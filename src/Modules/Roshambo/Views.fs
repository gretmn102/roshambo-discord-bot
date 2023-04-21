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

module FightView =
    open Model

    let viewId = "FightViewId"

    type ComponentId =
        | CreateSelectionGesture = 0

    type Action =
        | CreateSelectionGesture of FightState

    let handlers: Map<ComponentId, string -> Result<Action, string>> =
        let f deserialize handle str =
            match deserialize str with
            | Ok x ->
                Ok (handle x)

            | Error(errorValue) ->
                sprintf "%s" errorValue
                |> Error

        [
            ComponentId.CreateSelectionGesture, f FightState.deserialize CreateSelectionGesture
        ]
        |> Map.ofList

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

    type Action =
        | SelectGesture of GestureSelectionState

    let handlers: Map<ComponentId, string -> Result<Action, string>> =
        let f deserialize handle str =
            match deserialize str with
            | Ok x ->
                Ok (handle x)

            | Error(errorValue) ->
                sprintf "%s" errorValue
                |> Error

        [
            ComponentId.SelectGesture, f GestureSelectionState.deserialize SelectGesture
        ]
        |> Map.ofList

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
