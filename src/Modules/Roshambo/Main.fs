module Roshambo.Main
open DSharpPlus
open FsharpMyExtension
open FsharpMyExtension.Either
open Types
open Extensions

open Views

module LeaderboardComponent =
    open Shared.Ui.Table

    type SortBy =
        | SortByWins = 0
        | SortByLoses = 1

    let initSetting getState : Setting<_, SortBy, _, (GuildId * Model.GuildUserStats.GuildData)> =
        {
            Id = "RoshamboLeaderboardId"

            GetState = getState

            Title = fun _ _ -> "Рейтинг!"

            GetHeaders = fun sortBy ->
                match sortBy with
                | SortBy.SortByWins ->
                    [| "Игрок"; "Победы▼"; "Поражения" |]
                | SortBy.SortByLoses ->
                    [| "Игрок"; "Победы"; "Поражения▼" |]
                | x -> failwithf "RatingTable.SortBy %A" x

            GetItems = fun () (guildId, state) ->
                let state =
                    state.Cache
                    |> Seq.choose (fun (KeyValue(id, v)) ->
                        if id.GuildId = guildId then
                            Some v
                        else
                            None
                    )

                state
                |> Seq.toArray

            ItemsCountPerPage = 10

            SortBy = SortByContainer.Init [|
                SortBy.SortByWins, "Отсортировать по победам"
                SortBy.SortByLoses, "Отсортировать по поражениям"
            |]

            SortFunction = fun sortBy items ->
                match sortBy with
                | SortBy.SortByWins ->
                    Array.sortByDescending (fun x -> x.Data.Wins) items
                | SortBy.SortByLoses ->
                    Array.sortByDescending (fun x -> x.Data.Loses) items
                | x -> failwithf "RatingTable.SortBy %A" x

            MapFunction =
                fun _ i x ->
                    [|
                        sprintf "%d <@!%d>" i x.Id.UserId
                        string x.Data.Wins
                        string x.Data.Loses
                    |]
        }

    let createTable addComponents addEmbed state =
        createTable addComponents addEmbed 1 (None, ()) (initSetting state)

    let componentInteractionCreateHandle (client: DiscordClient) (e: EventArgs.ComponentInteractionCreateEventArgs) getState =
        let getState () =
            let state: Model.GuildUserStats.GuildData = getState ()
            e.Guild.Id, state

        componentInteractionCreateHandle client e (initSetting getState)

type SlashCommand =
    | ChallengeToFight of target: UserId
    | CreateLeaderboard

[<RequireQualifiedAccess>]
type FormComponentState =
    | Fight of FightView.ComponentState
    | GestureSelection of GestureSelectionView.ComponentState

module Interaction =
    module Form =
        // TODO: refact: use `DiscordBotExtensions.Extensions.Interaction.Form.map`
        let map (act: 'State -> 'NewState) (formId: Interaction.FormId, handlers: Interaction.ComponentStateParsers<'State>) : Interaction.FormId * Interaction.ComponentStateParsers<'NewState> =
            let handlers =
                handlers
                |> Map.map (fun _ dataParser ->
                    fun arg -> dataParser arg |> Result.map act
                )

            formId, handlers

let formComponentStates =
    [
        Interaction.Form.map FormComponentState.Fight FightView.handler
        Interaction.Form.map FormComponentState.GestureSelection GestureSelectionView.handler
    ]
    |> Map.ofList

type State =
    {
        GuildUserStats: Model.GuildUserStats.GuildData
        MvcState: Model.Mvc.Controller.State
    }

type Msg =
    | RequestSlashCommand of EventArgs.InteractionCreateEventArgs * SlashCommand
    | RequestInteraction of DiscordClient * EventArgs.ComponentInteractionCreateEventArgs * FormComponentState
    | GetState of AsyncReplyChannel<State>

let interp guildId sharedCmdHandle req state =
    let rec interp cmd state =
        let interpView (view: Model.ViewReq) =
            match view with
            | Model.ViewReq.GestureSelectionView(internalState) ->
                GestureSelectionView.create internalState
            | Model.ViewReq.FightView(internalState) ->
                FightView.create internalState
            | Model.ViewReq.FinishFightView internalState ->
                FightView.createResult internalState
            | Model.ViewReq.ResultFightView(internalState) ->
                resultFightView internalState
            | Model.ViewReq.SimpleView(str) ->
                createSimpleView str

        match cmd with
        | Model.UserStatsReq req ->
            let req, newGuildUserStats =
                Model.GuildUserStats.interp guildId req state.GuildUserStats

            let state =
                { state with
                    GuildUserStats = newGuildUserStats
                }
            interp req state

        | Model.RoshamboCmd.MvcCmd req ->
            let req, state =
                sharedCmdHandle interpView req state

            interp req state

        | Model.End -> state

    interp req state

let rec reduce (restClient: DiscordRestClient) (msg: Msg) (state: State): State =
    match msg with
    | RequestSlashCommand(e, act) ->
        let guildId = e.Interaction.Guild.Id

        let responseCreate isEphemeral (b: Entities.DiscordMessageBuilder) =
            let b = Entities.DiscordInteractionResponseBuilder(b)
            b.IsEphemeral <- isEphemeral
            let typ =
                InteractionResponseType.ChannelMessageWithSource
            awaiti <| e.Interaction.CreateResponseAsync (typ, b)
            None

        let interp =
            let sharedCmdHandle interpView req state =
                let req, state' =
                    let api =
                        Model.Mvc.Controller.createSlashCommandApi
                            interpView
                            (fun state ->
                                let req = Model.End
                                req, state
                            )
                            restClient
                            e

                    Model.Mvc.Controller.interp api req state.MvcState

                let state =
                    { state with
                        MvcState = state'
                    }

                req, state

            interp guildId sharedCmdHandle

        match act with
        | ChallengeToFight user2Id ->
            let user1Id = e.Interaction.User.Id
            interp (Model.challengeToFight user1Id user2Id) state

        | CreateLeaderboard ->
            let b = Entities.DiscordMessageBuilder()
            LeaderboardComponent.createTable
                b.AddComponents
                b.AddEmbed
                (fun () -> e.Interaction.Guild.Id, state.GuildUserStats)

            responseCreate false b |> ignore

            state

    | RequestInteraction(client, e, act) ->
        let guildId = e.Interaction.Guild.Id

        let interp =
            let sharedCmdHandle interpView req state =
                let req, state' =
                    let api =
                        Model.Mvc.Controller.createComponentInteractionApi
                            interpView
                            (fun state ->
                                let req = Model.End
                                req, state
                            )
                            restClient
                            e

                    Model.Mvc.Controller.interp api req state.MvcState

                let state =
                    { state with
                        MvcState = state'
                    }

                req, state

            interp guildId sharedCmdHandle

        match act with
        | FormComponentState.Fight act ->
            let userId = e.Interaction.User.Id
            match act with
            | FightView.ComponentState.CreateSelectionGesture internalState ->
                interp (Model.startSelectionGesture userId internalState) state

        | FormComponentState.GestureSelection act ->
            let channel = e.Message.Reference.Channel
            let message = e.Message.Reference.Message

            // referenced message don't contains components, so you need to get it
            let message =
                // maybe get from cache, because message have old component.CustomId
                // await <| channel.GetMessageAsync message.Id

                await <| restClient.GetMessageAsync(channel.Id, message.Id)

            let input =
                let firstRow =
                    message.Components
                    |> Seq.head

                let firstComponent =
                    firstRow.Components
                    |> Seq.head
                firstComponent.CustomId

            let restartComponent errMsg =
                try
                    DiscordMessage.Ext.clearComponents e.Message
                with e ->
                    printfn "%A" e.Message

                let b = Entities.DiscordInteractionResponseBuilder()
                b.Content <-
                    [
                        sprintf "Вызовите эту комманду еще раз, потому что-то пошло не так:"
                        "```"
                        sprintf "%s" errMsg
                        "```"
                    ] |> String.concat "\n"
                b.IsEphemeral <- true
                awaiti <| e.Interaction.CreateResponseAsync(InteractionResponseType.ChannelMessageWithSource, b)

            let gesture =
                e.Interaction.Data.Values
                |> Seq.head
                |> Core.PlayerGesture.deserialize
                |> function
                    | Ok x ->
                        Some x
                    | Error errMsg ->
                        sprintf "%A" errMsg
                        |> restartComponent
                        None

            let res =
                gesture
                |> Option.bind (fun gesture ->
                    match Interaction.parseForms formComponentStates input with
                    | Ok act ->
                        match act with
                        | FormComponentState.Fight x ->
                            match x with
                            | FightView.ComponentState.CreateSelectionGesture fightState ->
                                Some (gesture, fightState)
                        | x ->
                            sprintf "Expected ViewAction.Fight but %A" x
                            |> restartComponent

                            None
                    | Error _ ->
                        None
                )
                |> Option.map (fun (gesture, fightState) ->
                    let userId = e.User.Id
                    interp (Model.selectGesture userId gesture fightState) state
                )

            res
            |> Option.defaultValue state

    | GetState r ->
        r.Reply state

        state

let create (restClient: DiscordRestClient) db =
    let m =
        let init: State = {
            GuildUserStats = Model.GuildUserStats.GuildData.init "roshambos" db
            MvcState = Model.Mvc.Controller.State.empty
        }

        MailboxProcessor.Start (fun mail ->
            let rec loop (state: State) =
                async {
                    let! msg = mail.Receive()
                    let state =
                        try
                            reduce restClient msg state
                        with e ->
                            printfn "%A" e
                            state

                    return! loop state
                }
            loop init
        )

    let commands =
        let challengeToFight =
            let slashCommandName = "fight"
            let targetOptionName = "target"
            InteractionCommand.SlashCommand {|
                CommandName = slashCommandName
                Command =
                    let targetOption =
                        Entities.DiscordApplicationCommandOption(
                            targetOptionName,
                            "target",
                            ApplicationCommandOptionType.User,
                            required = true
                        )

                    new Entities.DiscordApplicationCommand(
                        slashCommandName,
                        "вызвать на бой",
                        ``type`` = ApplicationCommandType.SlashCommand,
                        options = [
                            targetOption
                        ]
                    )
                Handler = fun e ->
                    let getTargetId next =
                        let res =
                            e.Interaction.Data.Options
                            |> Seq.tryFind (fun x -> x.Name = targetOptionName)

                        match res with
                        | Some opt ->
                            let targetId = opt.Value :?> uint64
                            next targetId
                        | None -> ()

                    getTargetId <| fun targetId ->
                    m.Post(RequestSlashCommand(e, ChallengeToFight targetId))
            |}

        let challengeToFightMenu =
            let commandName = "challenge-to-fight"
            InteractionCommand.CommandMenu {|
                CommandName = commandName
                Command =
                    new Entities.DiscordApplicationCommand(
                        commandName,
                        null,
                        ``type`` = ApplicationCommandType.UserContextMenu
                    )
                Handler = fun e ->
                    let targetId = e.TargetUser.Id
                    m.Post(RequestSlashCommand(e, ChallengeToFight targetId))
            |}

        let createLeaderboard =
            let slashCommandName = "leaderboard"
            InteractionCommand.SlashCommand {|
                CommandName = slashCommandName
                Command =
                    new Entities.DiscordApplicationCommand(
                        slashCommandName,
                        "create leaderboard",
                        ``type`` = ApplicationCommandType.SlashCommand
                    )
                Handler = fun e ->
                    m.Post(RequestSlashCommand(e, CreateLeaderboard))
            |}

        [|
            challengeToFight
            challengeToFightMenu
            createLeaderboard
        |]

    let componentInteractionCreateHandler (client: DiscordClient, e: EventArgs.ComponentInteractionCreateEventArgs) =
        let testIsMessageBelongToBot () next =
            if e.Message.Author.Id = client.CurrentUser.Id then
                next ()
            else
                false

        let restartComponent errMsg =
            try
                DiscordMessage.Ext.clearComponents e.Message
            with e ->
                printfn "%A" e.Message

            let b = Entities.DiscordInteractionResponseBuilder()
            b.Content <-
                [
                    sprintf "Вызовите эту комманду еще раз, потому что-то пошло не так:"
                    "```"
                    sprintf "%s" errMsg
                    "```"
                ] |> String.concat "\n"
            b.IsEphemeral <- true
            awaiti <| e.Interaction.CreateResponseAsync(InteractionResponseType.ChannelMessageWithSource, b)

        pipeBackwardBuilder {
            do! testIsMessageBelongToBot ()

            let input = e.Id

            let isHandled =
                Interaction.handleForms
                    formComponentStates
                    restartComponent
                    (fun viewAction -> RequestInteraction(client, e, viewAction) |> m.Post)
                    input

            if isHandled then
                return isHandled
            else
                let isHandled =
                    LeaderboardComponent.componentInteractionCreateHandle
                        client
                        e
                        (fun () ->
                            let x = m.PostAndReply (fun r -> GetState r)
                            x.GuildUserStats
                        )

                return isHandled
        }

    { Shared.BotModule.empty with
        InteractionCommands =
            Some commands

        ComponentInteractionCreateHandle =
            Some componentInteractionCreateHandler
    }
