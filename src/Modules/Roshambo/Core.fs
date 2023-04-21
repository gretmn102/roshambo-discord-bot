module Roshambo.Core
open FsharpMyExtension

type PlayerGesture =
    | Scissor = 0
    | Rock = 1
    | Pepper = 2
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module PlayerGesture =
    let descriptions =
        [|
            PlayerGesture.Pepper, "Бумага"
            PlayerGesture.Rock, "Камень"
            PlayerGesture.Scissor, "Ножницы"
        |]

    let getDescription =
        let m = Map.ofArray descriptions
        fun gesture -> m.[gesture]

    module Printer =
        open FsharpMyExtension.ShowList

        let showT (gesture: PlayerGesture) =
            int gesture |> shows

    let serialize =
        Printer.showT >> FsharpMyExtension.ShowList.show

    module Parser =
        open FParsec

        let parser<'UserState> : Parser<_, 'UserState> =
            pint32 |>> enum<PlayerGesture>

    let deserialize =
        FParsecExt.runResult Parser.parser

type DefineWinnerResult =
    | FirstPlayerWin = -1
    | Draw = 0
    | SecondPlayerWin = 1

let defineWinner player1Gesture player2Gesture =
    match player1Gesture, player2Gesture with
    | PlayerGesture.Scissor, PlayerGesture.Scissor
    | PlayerGesture.Rock, PlayerGesture.Rock
    | PlayerGesture.Pepper, PlayerGesture.Pepper ->
        DefineWinnerResult.Draw
    | PlayerGesture.Pepper, PlayerGesture.Rock
    | PlayerGesture.Rock, PlayerGesture.Scissor
    | PlayerGesture.Scissor, PlayerGesture.Pepper ->
        DefineWinnerResult.FirstPlayerWin
    | _ ->
        DefineWinnerResult.SecondPlayerWin
