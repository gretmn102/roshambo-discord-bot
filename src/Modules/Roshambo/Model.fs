module Roshambo.Model
open FsharpMyExtension
open MongoDB.Driver
open MongoDB.Bson

open Types
open Db

module Mvc =
    module Model =
        type Req<'Arg, 'Res, 'Next> = 'Arg * ('Res -> 'Next)

        type InteractionData =
            {
                Id: uint64
                Token: string
            }
        [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
        [<RequireQualifiedAccess>]
        module InteractionData =
            let create applicationId token : InteractionData =
                {
                    Id = applicationId
                    Token = token
                }

        [<RequireQualifiedAccess>]
        type EphemeralResponsesReq<'Next> =
            | Add of Req<MessageId * InteractionData, unit, 'Next>
            | Get of Req<MessageId, InteractionData option, 'Next>
            | Remove of Req<MessageId, unit, 'Next>
        [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
        [<RequireQualifiedAccess>]
        module EphemeralResponsesReq =
            let add arg next =
                EphemeralResponsesReq.Add(arg, next)

            let get arg next =
                EphemeralResponsesReq.Get(arg, next)

            let remove arg next =
                EphemeralResponsesReq.Remove(arg, next)

        module EphemeralResponses =
            type LocalMessagePath =
                {
                    ChannelId: ChannelId
                    MessageId: MessageId
                }
            [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
            [<RequireQualifiedAccess>]
            module LocalMessagePath =
                let create channelId messageId =
                    {
                        ChannelId = channelId
                        MessageId = messageId
                    }

            type State = Map<LocalMessagePath, InteractionData>

            let empty: State = Map.empty

            let interp channelId (req: EphemeralResponsesReq<_>) (state: State) =
                match req with
                | EphemeralResponsesReq.Get(messageId, next) ->
                    let id = LocalMessagePath.create channelId messageId
                    let req =
                        Map.tryFind id state
                        |> next
                    req, state

                | EphemeralResponsesReq.Add((messageId, data), next) ->
                    let id = LocalMessagePath.create channelId messageId
                    let state =
                        Map.add id data state
                    next (), state

                | EphemeralResponsesReq.Remove(messageId, next) ->
                    let id = LocalMessagePath.create channelId messageId
                    let state =
                        Map.remove id state
                    next (), state

        [<RequireQualifiedAccess>]
        type Cmd<'View, 'Next> =
            | EphemeralResponsesReq of EphemeralResponsesReq<'Next>

            | GetCurrentMessageId of Req<unit, MessageId option, 'Next>
            | UserIsBot of Req<UserId, bool, 'Next>
            | GetReferenceMessageId of Req<unit, MessageId option, 'Next>
            | CreateView of Req<{| Reference: MessageId option; View: 'View |}, MessageId option, 'Next>
            | UpdateView of Req<{| MessageId: MessageId; View: 'View |}, unit, 'Next>
            | RemoveCurrentView of Req<InteractionData option, unit, 'Next>
            | GetInteractionData of Req<unit, InteractionData option, 'Next>

            | ResponseCreateView of Req<{| IsEphemeral: bool; View: 'View |}, MessageId option, 'Next>
            | ResponseUpdateCurrentView of Req<'View, unit, 'Next>

        [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
        [<RequireQualifiedAccess>]
        module Cmd =
            let ephemeralResponsesReq fn arg next =
                Cmd.EphemeralResponsesReq (fn arg (fun res ->
                    next res
                ))

            let getCurrentMessageId () next =
                Cmd.GetCurrentMessageId((), next)

            let userIsBot userId next =
                Cmd.UserIsBot(userId, next)

            let getReferenceMessageId arg next =
                Cmd.GetReferenceMessageId(arg, next)

            let getInteractionData arg next =
                Cmd.GetInteractionData(arg, next)

            let responseCreateView isEphemeral view next =
                let opts =
                    {| IsEphemeral = isEphemeral; View = view |}
                Cmd.ResponseCreateView(opts, next)

            let createView reference view next =
                let opts =
                    {| View = view; Reference = reference |}
                Cmd.CreateView(opts, next)

            let updateView messageId view next =
                let opts =
                    {| MessageId = messageId; View = view |}
                Cmd.UpdateView(opts, next)

            let removeCurrentView arg next =
                Cmd.RemoveCurrentView(arg, next)

            let responseUpdateCurrentView view next =
                Cmd.ResponseUpdateCurrentView(view, next)

    module Controller =
        open DSharpPlus

        type State =
            {
                EphemeralResponses: Model.EphemeralResponses.State
            }
        [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
        [<RequireQualifiedAccess>]
        module State =
            let empty: State =
                {
                    EphemeralResponses = Model.EphemeralResponses.empty
                }

        type Api<'View, 'Next> =
            abstract GetCurrentChannelId : unit -> ChannelId
            abstract GetCurrentMessageId : unit -> MessageId option
            abstract InterpView : 'View -> Entities.DiscordMessageBuilder
            abstract ReturnError : State -> 'Next * State
            abstract ResponseCreate : bool -> Entities.DiscordMessageBuilder -> option<MessageId>
            abstract ResponseUpdate : Entities.DiscordMessageBuilder -> unit
            abstract UpdateMessage : MessageId -> Entities.DiscordMessageBuilder -> unit
            abstract CreateMessage : option<MessageId> -> Entities.DiscordMessageBuilder -> option<MessageId>
            abstract RemoveCurrent : option<Model.InteractionData> -> unit
            abstract GetReference : unit -> option<MessageId>
            abstract GetMemberAsync : UserId -> System.Threading.Tasks.Task<Entities.DiscordMember>
            abstract GetInteractionData : unit -> option<Model.InteractionData>

        let interp (api: Api<'View, 'Next>) (cmd: Model.Cmd<'View,'Next>) state =
            let interp (cmd: Model.Cmd<'View,'Next>) state =
                match cmd with
                | Model.Cmd.EphemeralResponsesReq req ->
                    let req, ephemeralResponses =
                        let channelId = api.GetCurrentChannelId()
                        Model.EphemeralResponses.interp channelId req state.EphemeralResponses

                    let state =
                        { state with
                            EphemeralResponses = ephemeralResponses
                        }

                    req, state

                | Model.Cmd.ResponseCreateView(view, next) ->
                    let messageId =
                        api.InterpView view.View
                        |> api.ResponseCreate view.IsEphemeral

                    let req =
                        next messageId

                    req, state

                | Model.Cmd.CreateView(opts, next) ->
                    let messageId =
                        api.InterpView opts.View
                        |> api.CreateMessage opts.Reference

                    let req =
                        next messageId

                    req, state

                | Model.Cmd.ResponseUpdateCurrentView(view, next) ->
                    let req =
                        api.InterpView view
                        |> api.ResponseUpdate
                        |> next

                    req, state

                | Model.Cmd.UpdateView(opts, next) ->
                    let req =
                        api.InterpView opts.View
                        |> api.UpdateMessage opts.MessageId
                        |> next

                    req, state

                | Model.Cmd.RemoveCurrentView(interactionDataOpt, next) ->
                    let req =
                        api.RemoveCurrent interactionDataOpt
                        |> next

                    req, state

                | Model.Cmd.GetCurrentMessageId((), next) ->
                    let req = next (api.GetCurrentMessageId ())

                    req, state

                | Model.Cmd.UserIsBot(userId, userIdBot) ->
                    let user =
                        try
                            let guildMember: Entities.DiscordMember = await <| api.GetMemberAsync userId
                            Ok guildMember
                        with e ->
                            Error e.Message

                    match user with
                    | Ok user ->
                        let req = userIdBot user.IsBot

                        req, state
                    | Error(errorValue) ->
                        let b = Entities.DiscordMessageBuilder()
                        b.Content <- sprintf "```\n%s\n```" errorValue
                        let messageId = api.ResponseCreate true b

                        api.ReturnError state

                | Model.Cmd.GetReferenceMessageId((), next) ->
                    let req =
                        next (api.GetReference ())

                    req, state

                | Model.Cmd.GetInteractionData((), next) ->
                    let req =
                        next (api.GetInteractionData ())

                    req, state

            interp cmd state

        let createSlashCommandApi interpView returnError (e: EventArgs.InteractionCreateEventArgs) =
            let responseCreate isEphemeral (b: Entities.DiscordMessageBuilder) =
                let b = Entities.DiscordInteractionResponseBuilder(b)
                b.IsEphemeral <- isEphemeral
                let typ =
                    InteractionResponseType.ChannelMessageWithSource
                awaiti <| e.Interaction.CreateResponseAsync (typ, b)
                None

            { new Api<'View, 'Next> with
                member _.GetCurrentMessageId(): ChannelId option =
                    None

                member _.CreateMessage(referenceMessageIdOpt: MessageId option) (b: Entities.DiscordMessageBuilder): MessageId option =
                    referenceMessageIdOpt
                    |> Option.iter (fun messageId ->
                        b.WithReply(messageId, true)
                        |> ignore
                    )

                    let message = await <| e.Interaction.Channel.SendMessageAsync(b)
                    Some message.Id

                member _.GetCurrentChannelId(): ChannelId =
                    e.Interaction.ChannelId

                member _.GetInteractionData(): Model.InteractionData option =
                    Model.InteractionData.create
                        e.Interaction.ApplicationId
                        e.Interaction.Token
                    |> Some

                member _.GetMemberAsync(userId: UserId): System.Threading.Tasks.Task<Entities.DiscordMember> =
                    e.Interaction.Guild.GetMemberAsync userId

                member _.GetReference(): MessageId option =
                    None

                member _.InterpView(arg1: 'View): Entities.DiscordMessageBuilder =
                    interpView arg1

                member _.RemoveCurrent(arg1: Model.InteractionData option): unit =
                    ()

                member _.ResponseCreate(isEphemeral: bool) (b: Entities.DiscordMessageBuilder): MessageId option =
                    responseCreate isEphemeral b

                member _.ResponseUpdate(b: Entities.DiscordMessageBuilder): unit =
                    responseCreate false b |> ignore

                member _.ReturnError(state: State): 'Next * State =
                    returnError state

                member _.UpdateMessage(arg1: MessageId) (arg2: Entities.DiscordMessageBuilder): unit =
                    ()
            }

        let createComponentInteractionApi interpView returnError (restClient: DiscordRestClient) (e: EventArgs.ComponentInteractionCreateEventArgs) =
            { new Api<'View, 'Next> with
                member _.GetCurrentMessageId(): ChannelId option =
                    Some e.Message.Id

                member _.CreateMessage(referenceMessageIdOpt: MessageId option) (b: Entities.DiscordMessageBuilder): MessageId option =
                    referenceMessageIdOpt
                    |> Option.iter (fun messageId ->
                        b.WithReply(messageId, true)
                        |> ignore
                    )

                    let message = await <| e.Interaction.Channel.SendMessageAsync(b)
                    Some message.Id

                member _.GetCurrentChannelId(): ChannelId =
                    e.Interaction.ChannelId

                member _.GetInteractionData(): Model.InteractionData option =
                    Model.InteractionData.create
                        e.Interaction.ApplicationId
                        e.Interaction.Token
                    |> Some

                member _.GetMemberAsync(userId: UserId): System.Threading.Tasks.Task<Entities.DiscordMember> =
                    e.Interaction.Guild.GetMemberAsync userId

                member _.GetReference(): MessageId option =
                    e.Message.Reference
                    |> Option.ofObj
                    |> Option.map (fun r -> r.Message.Id)

                member _.InterpView(arg1: 'View): Entities.DiscordMessageBuilder =
                    interpView arg1

                member _.RemoveCurrent(interactionDataOpt: Model.InteractionData option): unit =
                    match interactionDataOpt with
                    | Some interactionData ->
                        try
                            awaiti <| restClient.DeleteWebhookMessageAsync(interactionData.Id, interactionData.Token, e.Message.Id)
                        with e ->
                            ()
                    | None ->
                        try
                            awaiti <| e.Message.DeleteAsync()
                        with e ->
                            ()

                member _.ResponseCreate(isEphemeral: bool) (b: Entities.DiscordMessageBuilder): MessageId option =
                    let emptyResponseWithEphemeral = Entities.DiscordInteractionResponseBuilder()
                    emptyResponseWithEphemeral.IsEphemeral <- isEphemeral
                    let typ =
                        InteractionResponseType.DeferredChannelMessageWithSource
                    awaiti <| e.Interaction.CreateResponseAsync(typ, emptyResponseWithEphemeral)

                    let b = Entities.DiscordFollowupMessageBuilder(b)
                    let message =
                        await <| e.Interaction.CreateFollowupMessageAsync b

                    Some message.Id

                member _.ResponseUpdate(b: Entities.DiscordMessageBuilder): unit =
                    let b = Entities.DiscordInteractionResponseBuilder(b)
                    let typ =
                        InteractionResponseType.UpdateMessage
                    awaiti <| e.Interaction.CreateResponseAsync (typ, b)

                member _.ReturnError(state: State): 'Next * State =
                    returnError state

                member _.UpdateMessage(messageId: MessageId) (b: Entities.DiscordMessageBuilder): unit =
                    // TODO: fix: get message by ID instead by reference
                    e.Message.Reference
                    |> Option.ofObj
                    |> Option.iter (fun x ->
                        awaiti <| x.Message.ModifyAsync b
                    )
            }

open Mvc.Model

type UserStatsReq<'Next> =
    | AddWin of Req<UserId, unit, 'Next>
    | AddLose of Req<UserId, unit, 'Next>
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module UserStatsReq =
    let addWin arg next =
        AddWin(arg, next)

    let addLose arg next =
        AddLose(arg, next)

type PlayerGestureStatus = Core.PlayerGesture Option
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module PlayerGestureStatus =
    module Printer =
        open FsharpMyExtension.ShowList

        let showT (status: PlayerGestureStatus) =
            match status with
            | Some x -> Core.PlayerGesture.Printer.showT x
            | None -> shows "null"

    let serialize (status: PlayerGestureStatus) =
        Printer.showT status |> FsharpMyExtension.ShowList.show

    module Parser =
        open FParsec

        let parser<'UserState> : Parser<_, 'UserState> =
            pstring "null" >>% None
            <|> (Core.PlayerGesture.Parser.parser |>> Some)

    let deserialize str =
        FParsecExt.runEither Parser.parser str

type FightState =
    {
        /// challenger
        User1Status: UserId * PlayerGestureStatus
        /// target
        User2Status: UserId * PlayerGestureStatus
    }
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module FightState =
    let createEmpty user1Id user2Id =
        {
            User1Status = user1Id, PlayerGestureStatus.None
            User2Status = user2Id, PlayerGestureStatus.None
        }

    let create user1Status user2Status =
        {
            User1Status = user1Status
            User2Status = user2Status
        }

    module Printer =
        open FsharpMyExtension.ShowList

        let showT (p: FightState) =
            let showUser (userId: UserId) = shows userId
            let showUserIdState (userId, status) =
                showUser userId << showSpace << PlayerGestureStatus.Printer.showT status

            showUserIdState p.User1Status << nl
            << showUserIdState p.User2Status

    module Parser =
        open FParsec

        let parse<'UserState> : Parser<_, 'UserState> =
            let puser = puint64
            let parseUserIdState =
                tuple2 (puser .>> spaces) PlayerGestureStatus.Parser.parser

            pipe2
                (parseUserIdState .>> newline)
                parseUserIdState
                create

    let deserialize =
        FParsecExt.runResult Parser.parse

type GestureSelectionState =
    {
        UserId: UserId
    }
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module GestureSelectionState =
    let create userId =
        {
            UserId = userId
        }

    module Printer =
        open FsharpMyExtension.ShowList

        let showT (state: GestureSelectionState) =
            let showUser (userId: UserId) = shows userId

            showUser state.UserId

    module Parser =
        open FParsec

        let parse<'UserState> : Parser<_, 'UserState> =
            let puser = puint64

            puser |>> create

    let deserialize =
        FParsecExt.runResult Parser.parse

type FightResultState =
    {
        FightState: FightState
        Winner: Core.DefineWinnerResult
    }
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module FightResultState =
    let create fightState winner : FightResultState =
        {
            FightState = fightState
            Winner = winner
        }

[<RequireQualifiedAccess>]
type ViewReq =
    | SimpleView of string
    | GestureSelectionView of GestureSelectionState
    | FightView of FightState
    | FinishFightView of FightResultState
    | ResultFightView of FightResultState
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module ViewReq =
    let fightView state =
        ViewReq.FightView state

    let fightEmptyView user1Id user2Id =
        ViewReq.FightView(FightState.createEmpty user1Id user2Id)

    let gestureSelectionView userId =
        ViewReq.GestureSelectionView(GestureSelectionState.create userId)

    let finishFightView fightState winner =
        ViewReq.FinishFightView(FightResultState.create fightState winner)

    let resultFightView fightState winner =
        ViewReq.ResultFightView(FightResultState.create fightState winner)

type RoshamboCmd =
    | UserStatsReq of UserStatsReq<RoshamboCmd>

    | MvcCmd of Mvc.Model.Cmd<ViewReq, RoshamboCmd>

    | End

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module RoshamboCmd =
    let userStatsReq fn arg next =
        UserStatsReq (fn arg (fun res ->
            next res
        ))

    let mvcCmd fn next =
        MvcCmd (fn (fun res ->
            next res
        ))

    let responsePrint isEphemeral description =
        mvcCmd (Cmd.responseCreateView isEphemeral (ViewReq.SimpleView description))

let challengeToFight user1Id user2Id =
    let testChallengeToFight user1Id user2Id next =
        let testChallengeYourselfToFight user1Id user2Id next =
            pipeBackwardBuilder {
                if user1Id = user2Id then
                    let! _ = "Нельзя самого себя вызвать на бой!" |> RoshamboCmd.responsePrint true
                    return End
                else
                    return next ()
            }

        let testChallengeBotToFight user2Id next =
            pipeBackwardBuilder {
                let! isBot =
                    RoshamboCmd.mvcCmd (Cmd.userIsBot user2Id)

                if isBot then
                    let! _ = sprintf "С ботом <@%d> низзя сражаться!" user2Id |> RoshamboCmd.responsePrint true
                    return End
                else
                    return next ()
            }

        pipeBackwardBuilder {
            do! testChallengeYourselfToFight user1Id user2Id
            do! testChallengeBotToFight user2Id
            return next ()
        }

    pipeBackwardBuilder {
        do! testChallengeToFight user1Id user2Id
        let! messageId =
            RoshamboCmd.mvcCmd (Cmd.responseCreateView false (ViewReq.fightEmptyView user1Id user2Id))
        return End
    }

let testIsAlreadyChose userStatus next =
    pipeBackwardBuilder {
        match userStatus with
        | None ->
            return next ()
        | Some _ ->
            let! _ = sprintf "Ты уже выбрал(а) жест." |> RoshamboCmd.responsePrint true
            return End
    }

let startSelectionGesture (currentUserId: UserId) (internalState: FightState) =
    pipeBackwardBuilder {
        let {
            User1Status = user1Id, user1Status
            User2Status = user2Id, user2Status
        } = internalState

        let f () next =
            pipeBackwardBuilder {
                let! interactionData = RoshamboCmd.mvcCmd (Cmd.getInteractionData ())
                let interactionData =
                    interactionData
                    |> Option.defaultWith (fun () -> failwithf "Internal error: not found interaction data!")

                let! messageId =
                    RoshamboCmd.mvcCmd (Cmd.responseCreateView true (ViewReq.gestureSelectionView currentUserId))
                match messageId with
                | Some messageId ->
                    do! RoshamboCmd.mvcCmd (Cmd.ephemeralResponsesReq EphemeralResponsesReq.add (messageId, interactionData))
                    return next ()
                | None ->
                    return next ()
            }

        if currentUserId = user1Id then
            do! testIsAlreadyChose user1Status

            do! f ()

            return End
        elif currentUserId = user2Id then
            do! testIsAlreadyChose user2Status

            do! f ()

            return End
        else
            let! _ =
                sprintf "На эту кнопку должен нажать либо <@%d>, либо <@%d>." user1Id user2Id
                |> RoshamboCmd.responsePrint true
            return End
    }

let selectGesture (currentUserId: UserId) (gesture: Core.PlayerGesture) (internalState: FightState) =
    let testCurrentUserIsValid (currentUserId: UserId) (gesture: Core.PlayerGesture) (internalState: FightState) next =

        pipeBackwardBuilder {
            let {
                User1Status = user1Id, user1Status
                User2Status = user2Id, user2Status
            } = internalState

            if currentUserId = user1Id then
                do! testIsAlreadyChose user1Status
                let internalState =
                    { internalState with
                        User1Status = user1Id, Some gesture
                    }
                return next internalState
            elif currentUserId = user2Id then
                do! testIsAlreadyChose user2Status
                let internalState =
                    { internalState with
                        User2Status = user2Id, Some gesture
                    }
                return next internalState
            else
                let _ =
                    sprintf "На эту кнопку должен нажать либо <@%d>, либо <@%d>." user1Id user2Id
                    |> RoshamboCmd.responsePrint true
                return End
        }

    let getReferenceMessageId () next =
        pipeBackwardBuilder {
            let! referenceMessageId = RoshamboCmd.mvcCmd (Cmd.getReferenceMessageId ())
            match referenceMessageId with
            | Some referenceMessageId ->
                return next referenceMessageId
            | None ->
                return End
        }

    let removeCurrentView () next =
        pipeBackwardBuilder {
            let! messageId = RoshamboCmd.mvcCmd (Cmd.getCurrentMessageId ())
            match messageId with
            | Some messageId ->
                let! interactionData =
                    RoshamboCmd.mvcCmd (Cmd.ephemeralResponsesReq EphemeralResponsesReq.get messageId)
                match interactionData with
                | Some interactionData ->
                    do! RoshamboCmd.mvcCmd (Cmd.removeCurrentView (Some interactionData))
                    do! RoshamboCmd.mvcCmd (Cmd.ephemeralResponsesReq EphemeralResponsesReq.remove messageId)
                    return next ()
                | None ->
                    return next ()
            | None ->
                return next ()
        }

    pipeBackwardBuilder {
        let! {
            User1Status = user1Id, user1Status
            User2Status = user2Id, user2Status
        } as internalState = testCurrentUserIsValid currentUserId gesture internalState

        let setWinnerAndLoserToDb res next =
            pipeBackwardBuilder {
                match res with
                | Core.DefineWinnerResult.FirstPlayerWin ->
                    do! RoshamboCmd.userStatsReq UserStatsReq.addWin user1Id
                    do! RoshamboCmd.userStatsReq UserStatsReq.addLose user2Id
                    return next ()
                | Core.DefineWinnerResult.SecondPlayerWin ->
                    do! RoshamboCmd.userStatsReq UserStatsReq.addWin user2Id
                    do! RoshamboCmd.userStatsReq UserStatsReq.addLose user1Id
                    return next ()
                | _ ->
                    return next ()
            }

        let! referenceMessageId = getReferenceMessageId ()
        match user1Status, user2Status with
        | PlayerGestureStatus.Some gesture1, PlayerGestureStatus.Some gesture2 ->
            let res = Core.defineWinner gesture1 gesture2

            do! setWinnerAndLoserToDb res

            do! RoshamboCmd.mvcCmd (Cmd.updateView referenceMessageId (ViewReq.finishFightView internalState res))
            let! messageId =
                RoshamboCmd.mvcCmd (Cmd.createView (Some referenceMessageId) (ViewReq.resultFightView internalState res))

            do! removeCurrentView ()
            return End
        | _ ->
            do! RoshamboCmd.mvcCmd (Cmd.updateView referenceMessageId (ViewReq.fightView internalState))
            do! removeCurrentView ()
            return End
    }

module GuildUserStats =
    type MainData =
        {
            Wins: int
            Loses: int
        }
        static member Init wins loses : MainData =
            {
                Wins = wins
                Loses = loses
            }
        static member Empty : MainData =
            {
                Wins = 0
                Loses = 0
            }
        static member Serialize (data: MainData) =
            data |> Json.ser
        static member Deserialize json =
            try
                let res: MainData = Json.des json
                Ok res
            with e ->
                Error e.Message

    type Version =
        | V0 = 0

    type Id =
        {
            GuildId: GuildId
            UserId: UserId
        }

    type Data = CommonDb.Data<Id, Version, MainData>
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    [<RequireQualifiedAccess>]
    module Data =
        let create id data : Data =
            CommonDb.Data.create id Version.V0 data

    type GuildData = CommonDb.GuildData<Id, Version, MainData>
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    [<RequireQualifiedAccess>]
    module GuildData =
        let createData id =
            Data.create id MainData.Empty

        let init collectionName (db: IMongoDatabase): GuildData =
            CommonDb.GuildData.init
                createData
                (fun ver x ->
                    match Option.get ver with
                    | Version.V0 ->
                        None, Serialization.BsonSerializer.Deserialize<Data>(x)
                    | x ->
                        failwithf "Version = %A not implemented" x
                )
                collectionName
                db

        let set id setAdditionParams (guildData: GuildData) =
            CommonDb.GuildData.set
                createData
                id
                setAdditionParams
                guildData

        let sets (items: Data seq) db =
            CommonDb.GuildData.sets
                items
                db

        let drop (db: IMongoDatabase) (items: GuildData) =
            CommonDb.GuildData.drop db items

        let tryFindById id (items: GuildData): Data option =
            CommonDb.GuildData.tryFind id items

        let removeByIds ids (items: GuildData) =
            CommonDb.GuildData.removeByIds ids items

    let interp guildId req (guildUserData: GuildData) : _ * GuildData =
        let createId id = { GuildId = guildId; UserId = id }

        match req with
        | AddWin(userId, next) ->
            let id = createId userId
            let guildUserStats =
                guildUserData
                |> GuildData.set id (fun x ->
                    { x with Wins = x.Wins + 1 }
                )
            let req = next ()
            req, guildUserStats
        | AddLose(userId, next) ->
            let id = createId userId
            let guildUserStats =
                guildUserData
                |> GuildData.set id (fun x ->
                    { x with Loses = x.Loses + 1 }
                )
            let req = next ()
            req, guildUserStats
