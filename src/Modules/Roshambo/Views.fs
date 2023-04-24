module Roshambo.Views
open DSharpPlus
open FsharpMyExtension

open Extensions.Interaction
open Types

let createSimpleView str =
    let b = Entities.DiscordMessageBuilder()
    b.WithAllowedMentions(Entities.Mentions.All) |> ignore
    b.Content <- str
    b

// TODO: refact
// type ComponentStateParser<'State> = (int32 * string) -> Result<'State, string>
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module ComponentStateParser =
    let parseMap (formId: FormId) (parseState: ComponentStateParser<'State>) map =
        let f (pos, str) =
            match parseState (pos, str) with
            | Ok (x: 'State) ->
                Ok (map x)

            | Error(errorValue) ->
                sprintf "%s\n%s" formId errorValue
                |> Error

        f : ComponentStateParser<'NewState>

module FightView =
    open Model

    let viewId = "FightViewId"

    type ComponentId =
        | CreateSelectionGesture = 0

    [<RequireQualifiedAccessAttribute>]
    type ComponentState =
        | CreateSelectionGesture of FightState

    let handler: FormId * ComponentStateParsers<ComponentState> =
        let handlers: ComponentStateParsers<ComponentState> =
            let parse deserialize map =
                let parse (pos, str: string) =
                    deserialize str.[pos..]

                ComponentStateParser.parseMap viewId parse map

            [
                int ComponentId.CreateSelectionGesture, parse FightState.deserialize ComponentState.CreateSelectionGesture
            ]
            |> Map.ofList

        viewId, handlers

    let create (state: FightState) =
        let {
            User1Status = user1Id, user1Status
            User2Status = user2Id, user2Status
        } = state

        let b = Entities.DiscordMessageBuilder()

        let showUserStatus (userId, userStatus) =
            let showStatus (status: PlayerGestureStatus) =
                match status with
                | PlayerGestureStatus.None -> "?"
                | PlayerGestureStatus.Some _ -> "✔"

            sprintf "%s <@%d>" (showStatus userStatus) userId

        b.WithAllowedMentions(Entities.Mentions.All) |> ignore

        b.Content <-
            [
                sprintf "<@%d> бросил вызов <@%d> в \"Камень, ножницы, бумага\"!" user1Id user2Id
                ""
                showUserStatus (user1Id, user1Status)
                showUserStatus (user2Id, user2Status)
            ]
            |> String.concat "\n"

        let startMove =
            let id =
                ComponentState.create
                    viewId
                    ComponentId.CreateSelectionGesture
                    state
            Entities.DiscordButtonComponent(
                ButtonStyle.Primary,
                ComponentState.serialize FightState.Printer.showT id,
                "Выбрать жест!"
            )

        b.AddComponents [|
            startMove :> Entities.DiscordComponent
        |] |> ignore

        b

    let createResult (state: FightResultState) =
        let {
            Model.FightState = {
                Model.User1Status = user1Id, user1Status
                Model.User2Status = user2Id, user2Status
            }
            Model.Winner = winner
        } : Model.FightResultState = state

        let showUserStatus (userId, userStatus) =
            let showGesture (status: PlayerGestureStatus) =
                match status with
                | Some gesture ->
                    Core.PlayerGesture.getDescription gesture
                | None -> ""

            sprintf "<@%d> выбрал(а) %s" userId (showGesture userStatus)

        let res =
            match winner with
            | Core.DefineWinnerResult.Draw ->
                sprintf "Ничья!"
            | Core.DefineWinnerResult.FirstPlayerWin ->
                sprintf "<@%d> победил(а)!" user1Id
            | Core.DefineWinnerResult.SecondPlayerWin ->
                sprintf "<@%d> победал(а)!" user2Id
            | _ ->
                sprintf "Unknown %A!" winner

        let b = Entities.DiscordMessageBuilder()

        b.WithAllowedMentions(Entities.Mentions.All) |> ignore

        b.Content <-
            [
                sprintf "<@%d> бросил вызов <@%d> в \"Камень, ножницы, бумага\"!" user1Id user2Id
                ""
                showUserStatus (user1Id, user1Status)
                showUserStatus (user2Id, user2Status)
                ""
                res
            ]
            |> String.concat "\n"

        b

module GestureSelectionView =
    open Model

    let viewId = "GestureSelectionViewId"

    type ComponentId =
        | SelectGesture = 0

    [<RequireQualifiedAccessAttribute>]
    type ComponentState =
        | SelectGesture of GestureSelectionState

    let handler: FormId * ComponentStateParsers<ComponentState> =
        let handlers: ComponentStateParsers<ComponentState> =
            let parse parseState map =
                let parseState (pos, str: string) =
                    parseState str.[pos..]

                ComponentStateParser.parseMap viewId parseState map

            [
                int ComponentId.SelectGesture, parse GestureSelectionState.deserialize ComponentState.SelectGesture
            ]
            |> Map.ofList

        viewId, handlers

    let create (internalState: GestureSelectionState) =
        let {
            UserId = authorId
        } = internalState

        let b = Entities.DiscordMessageBuilder()

        b.Content <-
            sprintf "<@%d>, выбери жест, который хочешь показать:" authorId

        let gestureSelection =
            let id =
                ComponentState.create
                    viewId
                    ComponentId.SelectGesture
                    (GestureSelectionState.create authorId)
                |> ComponentState.serialize GestureSelectionState.Printer.showT

            let gestures =
                Core.PlayerGesture.descriptions
                |> Array.map (fun (value, label) ->
                    Entities.DiscordSelectComponentOption(label, Core.PlayerGesture.serialize value)
                )
            Entities.DiscordSelectComponent(
                id,
                "Выбери жест...",
                gestures
            )

        b.AddComponents [|
            gestureSelection :> Entities.DiscordComponent
        |] |> ignore

        b

let resultFightView (state: Model.FightResultState) =
    let {
        Model.FightState = {
            Model.User1Status = user1Id, _
            Model.User2Status = user2Id, _
        }
        Model.Winner = winner
    } : Model.FightResultState = state

    let res =
        match winner with
        | Core.DefineWinnerResult.Draw ->
            sprintf "Игра между <@%d> и <@%d> закончилась ничьей!" user1Id user2Id
        | Core.DefineWinnerResult.FirstPlayerWin ->
            sprintf "Игра между <@%d> и <@%d> закончилась победой <@%d>!" user1Id user2Id user1Id
        | Core.DefineWinnerResult.SecondPlayerWin ->
            sprintf "Игра между <@%d> и <@%d> закончилась победой <@%d>!" user1Id user2Id user2Id
        | _ ->
            sprintf "Unknown %A!" winner

    let b = Entities.DiscordMessageBuilder()
    b.WithAllowedMentions(Entities.Mentions.All) |> ignore
    b.Content <- res
    b
