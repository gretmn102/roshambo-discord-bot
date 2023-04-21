module Roshambo.Main
open DSharpPlus
open FsharpMyExtension
open FsharpMyExtension.Either

open Types
open Extensions
open Views

type SlashCommand =
    | ChallengeToFight of target: UserId

[<RequireQualifiedAccess>]
type ViewAction =
    | Fight of FightView.Action
    | GestureSelection of GestureSelectionView.Action

let viewActions =
    let inline f handlers act componentId str =
        let componentId = enum componentId
        match Map.tryFind componentId handlers with
        | Some parse ->
            parse str
        | None ->
            sprintf "Not found '%A' ComponentId" componentId
            |> Error
        |> Result.map act

    [
        FightView.viewId, f FightView.handlers ViewAction.Fight
        GestureSelectionView.viewId, f GestureSelectionView.handlers ViewAction.GestureSelection
    ]
    |> Map.ofList

type Msg =
    | RequestSlashCommand of EventArgs.InteractionCreateEventArgs * SlashCommand
    | RequestInteraction of DiscordClient * EventArgs.ComponentInteractionCreateEventArgs * ViewAction

type State =
    {
        MarriedCouples: Model.MarriedCouples.GuildData
    }

// todo
module Builder =
    open Interaction.ComponentState.Parser

    open FSharp.Core

    let parseFormId handleError input next =
        match parseHeader input with
        | Some pos ->
            match parseFormId pos input with
            | Ok (formId, pos2) ->
                (formId, pos + pos2)
                |> next

            | Error (errMsg, _, _) ->
                handleError errMsg
                None
        | None ->
            None

    let parseComponentId handleError (pos, input) next =
        match parseComponentId pos input with
        | Ok (componentId, pos2) ->
            (componentId, pos + pos2)
            |> next
        | Error (errMsg, _, _) ->
            handleError errMsg
            None

    let handleForms viewActions restartComponent input =
        let f formId rawComponentId (pos, input: string) next =
            let handleActions formId rawComponentId str =
                match Map.tryFind formId viewActions with
                | Some parse ->
                    parse rawComponentId str
                | None ->
                    sprintf "Not found '%A' form" formId
                    |> Error

            let rawState = input.[pos..]
            match handleActions formId rawComponentId rawState with
            | Ok x ->
                next x
            | Error x ->
                restartComponent x
                None

        pipeBackwardBuilder {
            let! formId, pos =
                parseFormId restartComponent input
            let! rawComponentId, pos =
                parseComponentId restartComponent (pos, input)

            let! action = f formId rawComponentId (pos, input)

            return Some action
        }

// HACK
let restClient: DiscordRestClient option ref = ref None

// HACK
let lastInteraction: {| Id: uint64; Token: string |} option ref = ref None

let rec reduce (msg: Msg) (state: State): State =
    let interp guildId responseCreate responseUpdate updateMessage createMessage removeCurrent getReference getMemberAsync cmd state =
        let rec interp cmd state =
            let interpView (view: Model.ViewReq) =
                match view with
                | Model.GestureSelectionView(internalState) ->
                    GestureSelectionView.create internalState
                | Model.FightView(internalState) ->
                    FightView.create internalState
                | Model.FinishFightView internalState ->
                    FightView.createResult internalState
                | Model.ResultFightView(internalState) ->
                    resultFightView internalState
                | Model.SimpleView(str) ->
                    createSimpleView str

            match cmd with
            | Model.UserStatsReq req ->
                let req, newMarriedCouples =
                    Model.MarriedCouples.interp guildId req state.MarriedCouples

                let state =
                    { state with
                        MarriedCouples = newMarriedCouples
                    }
                interp req state

            | Model.ResponseCreateView(view, next) ->
                let messageId =
                    interpView view.View
                    |> responseCreate view.IsEphemeral

                interp (next messageId) state

            | Model.CreateView(opts, next) ->
                let messageId =
                    interpView opts.View
                    |> createMessage opts.Reference

                interp (next messageId) state

            | Model.ResponseUpdateCurrentView(view, next) ->
                let res =
                    interpView view
                    |> responseUpdate

                interp (next res) state

            | Model.UpdateView(opts, next) ->
                let res =
                    interpView opts.View
                    |> updateMessage opts.MessageId

                interp (next res) state
            | Model.RemoveCurrentView((), next) ->
                let req = removeCurrent ()
                interp (next req) state

            | Model.UserIsBot(userId, userIdBot) ->
                let user =
                    try
                        let guildMember: Entities.DiscordMember = await <| getMemberAsync userId
                        Ok guildMember
                    with e ->
                        Error e.Message

                match user with
                | Ok user ->
                    let req = userIdBot user.IsBot

                    interp req state
                | Error(errorValue) ->
                    let b = Entities.DiscordMessageBuilder()
                    b.Content <- sprintf "```\n%s\n```" errorValue
                    let messageId = responseCreate true b

                    state

            | Model.GetReferenceMessageId((), next) ->
                let req =
                    next (getReference ())

                interp req state

            | Model.End -> state

        interp cmd state

    match msg with
    | RequestSlashCommand(e, act) ->
        let user1Id = e.Interaction.User.Id
        let guildId = e.Interaction.Guild.Id

        let interp =
            let responseCreate isEphemeral (b: Entities.DiscordMessageBuilder) =
                let b = Entities.DiscordInteractionResponseBuilder(b)
                if isEphemeral then
                    b.AddMentions(Entities.Mentions.All) |> ignore
                    b.IsEphemeral <- isEphemeral
                let typ =
                    InteractionResponseType.ChannelMessageWithSource
                awaiti <| e.Interaction.CreateResponseAsync (typ, b)
                // HACK
                lastInteraction.Value <-
                    {| Id = e.Interaction.ApplicationId; Token = e.Interaction.Token |}
                    |> Some
                None

            let getMemberAsync userId =
                e.Interaction.Guild.GetMemberAsync userId

            interp
                guildId
                responseCreate
                (responseCreate false >> ignore)
                (fun messageId b ->
                    let restClient = restClient.Value.Value
                    awaiti <| restClient.EditMessageAsync(e.Interaction.Channel.Id, messageId, b)
                )
                (fun referenceMessageIdOpt b ->
                    referenceMessageIdOpt
                    |> Option.iter (fun messageId ->
                        b.WithReply(messageId, true)
                        |> ignore
                    )

                    let message = await <| e.Interaction.Channel.SendMessageAsync(b)
                    Some message.Id
                )
                ignore
                (fun () -> None)
                getMemberAsync

        match act with
        | ChallengeToFight user2Id ->
            interp (Model.challengeToFight user1Id user2Id) state

    | RequestInteraction(client, e, act) ->
        let interp =
            let responseCreate isEphemeral (b: Entities.DiscordMessageBuilder) =
                let b = Entities.DiscordInteractionResponseBuilder(b)
                if isEphemeral then
                    b.IsEphemeral <- isEphemeral
                else
                    b.AddMentions(Entities.Mentions.All) |> ignore

                let typ =
                    InteractionResponseType.ChannelMessageWithSource

                awaiti <| e.Interaction.CreateResponseAsync (typ, b) // todo: how to get ID from new message?
                None

            let responseUpdate (b: Entities.DiscordMessageBuilder) =
                let b = Entities.DiscordInteractionResponseBuilder(b)
                let typ =
                    InteractionResponseType.UpdateMessage
                awaiti <| e.Interaction.CreateResponseAsync (typ, b)

            // todo
            // let createFollowupMessage isEphemeral (b: Entities.DiscordMessageBuilder) =
            //     let b = Entities.DiscordFollowupMessageBuilder(b)
            //     b.AddMentions(Entities.Mentions.All) |> ignore
            //     b.IsEphemeral <- isEphemeral
            //     await <| e.Interaction.CreateFollowupMessageAsync(b)
            //     |> Some

            let getMemberAsync userId =
                e.Interaction.Guild.GetMemberAsync userId

            let guildId = e.Guild.Id

            let updateMessage (messageId: MessageId) (b: Entities.DiscordMessageBuilder) =
                e.Message.Reference
                |> Option.ofObj
                |> Option.iter (fun x ->
                    awaiti <| x.Message.ModifyAsync b
                )

            let createMessage (referenceMessageIdOpt: MessageId option) (b: Entities.DiscordMessageBuilder) =
                referenceMessageIdOpt
                |> Option.iter (fun messageId ->
                    b.WithReply(messageId, true)
                    |> ignore
                )

                let message = await <| e.Interaction.Channel.SendMessageAsync(b)
                Some message.Id

            let removeCurrent () =
                restClient.Value
                |> Option.iter (fun client ->
                    lastInteraction.Value
                    |> Option.iter (fun interaction ->
                        try
                            awaiti <| client.DeleteWebhookMessageAsync(interaction.Id, interaction.Token, e.Message.Id)
                        with e ->
                            ()
                    )
                )

            let getReference () =
                e.Message.Reference
                |> Option.ofObj
                |> Option.map (fun r -> r.Message.Id)

            interp guildId responseCreate responseUpdate updateMessage createMessage removeCurrent getReference getMemberAsync

        match act with
        | ViewAction.Fight act ->
            let userId = e.Interaction.User.Id
            match act with
            | FightView.CreateSelectionGesture internalState ->
                interp (Model.startSelectionGesture userId internalState) state

        | ViewAction.GestureSelection act ->
            let channel = e.Message.Reference.Channel
            let message = e.Message.Reference.Message

            // referenced message don't contains components, so you need to get it
            let restClient = restClient.Value |> Option.get

            let message =
                // maybe get from cache, because message have old component.CustomId
                // await <| channel.GetMessageAsync message.Id

                // HACK
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
                    Builder.handleForms viewActions restartComponent input
                    |> Option.bind (fun act ->
                        match act with
                        | ViewAction.Fight x ->
                            match x with
                            | FightView.CreateSelectionGesture fightState ->
                                Some (gesture, fightState)
                        | x ->
                            sprintf "Expected ViewAction.Fight but %A" x
                            |> restartComponent

                            None
                    )
                )
                |> Option.map (fun (gesture, fightState) ->
                    let userId = e.User.Id
                    interp (Model.selectGesture userId gesture fightState) state
                )

            res
            |> Option.defaultValue state

let create db =
    let m =
        let init: State = {
            MarriedCouples = Model.MarriedCouples.GuildData.init "roshambos" db
        }

        MailboxProcessor.Start (fun mail ->
            let rec loop (state: State) =
                async {
                    let! msg = mail.Receive()
                    let state =
                        try
                            reduce msg state
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

        [|
            challengeToFight
            challengeToFightMenu
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
                Extensions.Interaction.handleForms
                    viewActions
                    (fun viewAction -> RequestInteraction(client, e, viewAction) |> m.Post)
                    restartComponent
                    input

            return isHandled
        }

    { Shared.BotModule.empty with
        InteractionCommands =
            Some commands

        ComponentInteractionCreateHandle =
            Some componentInteractionCreateHandler
    }
