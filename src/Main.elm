module Main exposing (main)

import Browser
import Html exposing (Html, button, div, text, input, br)
import Html.Attributes exposing(type_, checked)
import Html.Events exposing (onClick)
import List.Extra exposing (updateAt,zip)
import Random
import Random.List exposing (shuffle)


-- MODEL --

type Suit
    = Spade
    | Heart
    | Diamond
    | Club

suitToString : Suit -> String
suitToString suit =
    case suit of
        Spade   -> "♠︎"
        Heart   -> "♡"
        Diamond -> "♢"
        Club    -> "♣︎"

type Role
    = RoyalFlush
    | StraightFlush
    | FourCard
    | FullHouse
    | Flush
    | Straight
    | ThreeCard
    | Pair2
    | Pair1
    | None

toOdds : Role -> Int
toOdds role =
    case role of
        RoyalFlush    -> 100
        StraightFlush -> 50
        FourCard      -> 20
        FullHouse     -> 7
        Flush         -> 5
        Straight      -> 4
        ThreeCard     -> 3
        Pair2         -> 2
        Pair1         -> 1
        None          -> 0

roleToString role =
    case role of
        RoyalFlush    -> "RoyalFlush !!"
        StraightFlush -> "StraightFlush"
        FourCard      -> "4 Card"
        FullHouse     -> "FullHouse"
        Flush         -> "Flush"
        Straight      -> "Straight"
        ThreeCard     -> "3 Cards"
        Pair2         -> "2 Pairs"
        Pair1         -> "1 Pair"
        None          -> "None"

type alias Card = ( Suit, Int )

type Phase
    = BetPhase
    | SelectPhase
    | ResultPhase
    | GameOver

type alias Model =
    { phase : Phase
    , deck : List Card
    , hand : List Card
    , select : List Bool
    , money : Int
    , bet : Int
    , deckWave : Int
    }

numberToString : Int -> String
numberToString n =
    case n of
        1 -> "A"
        11 -> "J"
        12 -> "Q"
        13 -> "K"
        _  -> String.fromInt n

refreshDeck = Random.generate Refresh (shuffle initialDeck)

init () =
    ( initialModel, refreshDeck )

initialModel : Model
initialModel =
    { phase = BetPhase
    , deck = initialDeck
    , hand = []
    , select = False |> List.repeat 5
    , money = 10
    , bet = 0
    , deckWave = 1
    }

initialDeck : List Card
initialDeck =
    cross [Spade, Heart, Diamond, Club] (List.range 1 13)



-- UPDATE --

type Msg
    = Refresh (List Card)
    | Check Int
    | Trash
    | Finish
    | Continue
    | Bet Int
    | Decide


update : Msg -> Model -> (Model ,Cmd Msg)
update msg model =
    let
        newModel =
            case msg of
                Refresh deck -> { model | deck = List.drop 5 deck, hand = List.take 5 deck, deckWave = 1 }
                Check n -> { model | select = updateAt n not model.select}
                Trash ->
                    let
                        trashedHand : List Card
                        trashedHand = zip model.hand model.select
                            |> List.filter (Tuple.second >> not)
                            |> List.map Tuple.first
                        refreshHand = List.append trashedHand
                            <|List.take (5-List.length trashedHand) model.deck
                    in
                        { model | phase = ResultPhase, select = List.repeat 5 False, hand = refreshHand, deck = List.drop (5-List.length trashedHand) model.deck }

                Finish ->
                    let newMoney = (toOdds <| evaluateRole <| model.hand)*model.bet + model.money in
                    { model | phase = if newMoney > 0 then BetPhase else GameOver, deck = List.drop 5 model.deck, hand = List.take 5 model.deck, money = newMoney, bet = 1, deckWave = model.deckWave + 1}
                Continue -> initialModel
                Bet n-> { model | bet = model.bet + n }
                Decide -> { model | phase = SelectPhase, money = model.money - model.bet }

        cmd =
            case msg of
                Finish ->
                    case model.deckWave of
                        3 -> refreshDeck
                        _ -> Cmd.none
                Continue -> refreshDeck
                _ -> Cmd.none
    in
        ( newModel, cmd )


evaluateRole : List Card -> Role
evaluateRole hand =
    let
        handToInt = List.reverse <| Tuple.second <| List.unzip <|  List.sortBy Tuple.second <| hand
        difference = handToInt
            |> List.map (\n -> n - (Maybe.withDefault 0 (List.head handToInt)))
            |> List.map ((+)13)

        isFlush = (hand |> List.unzip |> Tuple.first |> toBag |> List.length)==1

        isStraight =
            difference == [13,12,11,10,9]
            || difference == [13,12,11,10,1] -- Ace high

        aceHigh = difference == [13,12,11,10,1]

        sortedHandNumbers =
            hand
                |> List.unzip
                |> Tuple.second
                |> toBag
                |> List.unzip
                |> Tuple.second
                |> List.sort

    in
        case sortedHandNumbers of
            [1,1,1,1,1] ->
                case (isFlush,isStraight) of
                    (True , True ) -> if aceHigh then RoyalFlush else StraightFlush
                    (True , False) -> Flush
                    (False, True ) -> Straight
                    (False, False) -> None

            [1,1,1,2] -> Pair1
            [1,2,2]   -> Pair2
            [1,1,3]   -> ThreeCard
            [1,4]     -> FourCard
            [2,3]     -> FullHouse
            _         -> None



-- VIEW --

view : Model -> Html Msg
view model =
    let
        waveDeckText =
            text
                <| "Wave : " ++ (String.fromInt model.deckWave) ++ " / 3, Deck : "
                    ++ (String.fromInt <| List.length <| model.deck)

        coinText =
            text <| "Coin : " ++ (model.money * 100 |> String.fromInt)

        betText = text <| "Bet  : " ++ (model.bet * 100 |> String.fromInt)

        handDiv = model.hand |> List.map viewCard |> div[]

        role = model.hand |> evaluateRole
        roleText = text <| ("Role : " ++ (role |> roleToString))
    in
        case model.phase of
            BetPhase ->
                let
                    logCoin = Basics.floor <| Basics.logBase 10 (Basics.toFloat (model.money - model.bet))
                    addButton n = button
                        [ onClick <| Bet <| 10^n ]
                        [ "+" ++ (10^n |> String.fromInt) ++ "00" |> text ]
                    betButtons =
                        (List.range 0 logCoin |> List.map addButton)
                            ++ [ button[ onClick <| Bet <| (model.money - model.bet) ][ text  "Bet All" ] ]

                    betAndDraw =
                        button
                            [ onClick <| Decide ]
                            [ text "click to bet" ]
                in
                    div []
                        [ waveDeckText, br[][]
                        , coinText, br[][]
                        , betText, br[][]
                        , div[] betButtons
                        , br[][]
                        , betAndDraw, br[][]
                        , br[][]
                        ]

            SelectPhase ->
                let
                    selectInput n c = input[type_ "checkbox",onClick <| Check n,checked c][]
                    selectDiv = model.select |> List.indexedMap selectInput |> div[]
                    trushButton = button [ onClick <| Trash ] [ text "trash" ]
                in
                    div []
                        [ waveDeckText, br[][]
                        , coinText, br[][]
                        , betText, br[][]
                        , handDiv
                        , selectDiv
                        , roleText, br[][]
                        , trushButton, br[][]
                        , br[][]
                        ]

            ResultPhase ->
                let
                    odds = role |> toOdds
                    resultText = text <|
                        (if role == None then "Lose : " else "Win : ")
                            ++ (model.bet * 100 |> String.fromInt)
                            ++ " x " ++ (odds |> String.fromInt)
                            ++ " = " ++ (model.bet * odds * 100 |> String.fromInt)

                    nextButton = button [ onClick <| Finish ] [ text "click to next"]
                in
                    div []
                        [ waveDeckText, br[][]
                        , coinText, br[][]
                        , resultText, br[][]
                        , handDiv
                        , roleText, br[][]
                        , nextButton, br[][]
                        , br[][]
                        ]

            GameOver ->
                div []
                    [ text "Game Over !!!"
                    , br[][]
                    , button[onClick <| Continue][text "Click to continue"]
                    ]


viewCard : Card -> Html Msg
viewCard ( suit, number ) =
    text <| (suit |> suitToString) ++ (number |> numberToString)



 -- UTILITY --

cross : List a -> List b -> List ( a, b )
cross list1 list2 =
    list1
        |> List.concatMap (\e1 ->
            list2 |> List.map (\e2 -> ( e1, e2 )))


toBag : List a -> List ( a, Int )
toBag arg =
    toBagHelper [] arg

toBagHelper : List ( a, Int ) ->  List a  -> List ( a, Int )
toBagHelper  result rest =
    case rest of
        [] ->
            result
        a :: tl ->
            let
                newResult = result |> increment a []
            in
                toBagHelper newResult tl

increment : a -> List ( a, Int ) -> List ( a, Int ) -> List ( a, Int )
increment a result rest =
    case rest of
        [] ->
            (a, 1) :: result
        (b, n) :: tl ->
            if a == b
            then List.append tl (( a, n+1 ) :: result)
            else increment a (( b, n ) :: result) tl



main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ ->Sub.none
        }
