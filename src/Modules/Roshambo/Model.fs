module Roshambo.Model
open FsharpMyExtension
open MongoDB.Driver
open MongoDB.Bson

open Types
open Db

type Req<'Arg, 'Res, 'Next> = 'Arg * ('Res -> 'Next)

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
        User1Id: UserId
        User2Id: UserId
        Winner: Core.DefineWinnerResult
    }
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module FightResultState =
    let create user1Id user2Id winner : FightResultState =
        {
            User1Id = user1Id
            User2Id = user2Id
            Winner = winner
        }

type ViewReq =
    | SimpleView of string
    | GestureSelectionView of GestureSelectionState
    | FightView of FightState
    | ResultFightView of FightResultState
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module ViewReq =
    let fightView state =
        FightView state

    let fightEmptyView user1Id user2Id =
        FightView(FightState.createEmpty user1Id user2Id)

    let gestureSelectionView userId =
        GestureSelectionView(GestureSelectionState.create userId)

    let resultFightView user1Id user2Id res =
        ResultFightView(FightResultState.create user1Id user2Id res)

type RoshamboCmd =
    | UserStatsReq of UserStatsReq<RoshamboCmd>
    | UserIsBot of Req<UserId, bool, RoshamboCmd>
    | CreateView of Req<{| IsEphemeral: bool; View: ViewReq |}, MessageId option, RoshamboCmd>
    | UpdateCurrentView of Req<ViewReq, unit, RoshamboCmd>
    | UpdateReferenceView of Req<ViewReq, unit, RoshamboCmd>
    | RemoveCurrentView of Req<unit, unit, RoshamboCmd>
    | End

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module RoshamboCmd =
    let apply fn arg next =
        UserStatsReq (fn arg (fun res ->
            next res
        ))

    let userIsBot userId next =
        UserIsBot(userId, next)

    let createView isEphemeral view next =
        CreateView({| IsEphemeral = isEphemeral; View = view |}, next)

    let print isEphemeral description next =
        createView isEphemeral (ViewReq.SimpleView description) next

    let updateCurrentView view next =
        UpdateCurrentView(view, next)

    let updateReferenceView view next =
        UpdateReferenceView(view, next)

    let removeCurrentView () next =
        RemoveCurrentView((), next)

let challengeToFight user1Id user2Id =
    let testChallengeToFight user1Id user2Id next =
        let testChallengeYourselfToFight user1Id user2Id next =
            pipeBackwardBuilder {
                if user1Id = user2Id then
                    let! _ = "Нельзя самого себя вызвать на бой!" |> RoshamboCmd.print true
                    return End
                else
                    return next ()
            }

        let testChallengeBotToFight user2Id next =
            pipeBackwardBuilder {
                let! isBot =
                    RoshamboCmd.userIsBot user2Id

                if isBot then
                    let! _ = sprintf "С ботом <@%d> низзя сражаться!" user2Id |> RoshamboCmd.print true
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
        let! messageId = RoshamboCmd.createView false (ViewReq.fightEmptyView user1Id user2Id)
        return End
    }

let testIsAlreadyChose userStatus next =
    pipeBackwardBuilder {
        match userStatus with
        | None ->
            return next ()
        | Some _ ->
            let! _ = sprintf "Ты уже выбрал(а) жест." |> RoshamboCmd.print true
            return End
    }

let startSelectionGesture (currentUserId: UserId) (internalState: FightState) =
    pipeBackwardBuilder {
        let {
            User1Status = user1Id, user1Status
            User2Status = user2Id, user2Status
        } = internalState

        if currentUserId = user1Id then
            do! testIsAlreadyChose user1Status

            let! messageId = RoshamboCmd.createView true (ViewReq.gestureSelectionView currentUserId)

            return End
        elif currentUserId = user2Id then
            do! testIsAlreadyChose user2Status

            let! messageId = RoshamboCmd.createView true (ViewReq.gestureSelectionView currentUserId)

            return End
        else
            let! _ =
                sprintf "На эту кнопку должен нажать либо <@%d>, либо <@%d>." user1Id user2Id
                |> RoshamboCmd.print true
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
                    |> RoshamboCmd.print true
                return End
        }

    pipeBackwardBuilder {
        let! {
            User1Status = user1Id, user1Status
            User2Status = user2Id, user2Status
        } as internalState = testCurrentUserIsValid currentUserId gesture internalState

        match user1Status, user2Status with
        | PlayerGestureStatus.Some gesture1, PlayerGestureStatus.Some gesture2 ->
            let res = Core.defineWinner gesture1 gesture2
            let! messageId = RoshamboCmd.createView false (ViewReq.resultFightView user1Id user2Id res)
            return End
        | _ ->
            do! RoshamboCmd.updateReferenceView (ViewReq.fightView internalState)
            do! RoshamboCmd.removeCurrentView ()
            return End
    }

module MarriedCouples =
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
            let marriedCouples =
                guildUserData
                |> GuildData.set id (fun x ->
                    { x with Wins = x.Wins + 1 }
                )
            let req = next ()
            req, marriedCouples
        | AddLose(userId, next) ->
            let id = createId userId
            let marriedCouples =
                guildUserData
                |> GuildData.set id (fun x ->
                    { x with Loses = x.Loses + 1 }
                )
            let req = next ()
            req, marriedCouples