module Main exposing (main)

import Browser
import Browser.Events exposing (onAnimationFrame)
import Html exposing (Html, button, div, text, input, br, img)
import Html.Attributes exposing(type_, checked, class, src, style, disabled)
import Html.Events exposing (onClick)
import Process
import Task
import Svg exposing (svg, rect, text_)
import Svg.Attributes as SvgAttr exposing (x, y, width, height, rx, ry, fill, fillOpacity, stroke, textAnchor, fontFamily, fontSize)
import List.Extra exposing (updateAt,zip)
import Time exposing (posixToMillis)
import Dict exposing (Dict)
import Set exposing (Set)
import Random
import Random.List exposing (shuffle)
import Heap exposing (Heap, smallest, by)


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
    Heart   -> "♥"
    Diamond -> "♦"
    Club    -> "♣︎"

suitToColor : Suit -> String
suitToColor suit =
  case suit of
    Spade   -> "#bbb"
    Heart   -> "#f88"
    Diamond -> "#f88"
    Club    -> "#bbb"

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
    RoyalFlush  -> 100
    StraightFlush -> 50
    FourCard    -> 20
    FullHouse   -> 7
    Flush     -> 5
    Straight    -> 4
    ThreeCard   -> 3
    Pair2     -> 2
    Pair1     -> 1
    None      -> 0

roleToString role =
  case role of
    RoyalFlush  -> "RoyalFlush !!"
    StraightFlush -> "StraightFlush"
    FourCard    -> "4 Card"
    FullHouse   -> "FullHouse"
    Flush     -> "Flush"
    Straight    -> "Straight"
    ThreeCard   -> "3 Cards"
    Pair2     -> "2 Pairs"
    Pair1     -> "1 Pair"
    None      -> "None"

type alias Card = ( Suit, Int )

type Phase
  = BetPhase
  | SelectPhase (Set Int) -- trash
  | ResultPhase
  | GameOver
  | InvalidState

type alias Model =
  { phase : Phase
  , deck : List Card
  , hand : Hand
  , money : Int
  , bet : Int
  , deckWave : Int
  , animationWaiting : Bool
  , animationQueue : Heap ( Int, AnimationTask ) -- millis, task
  , cardEffects : CardEffects
  }

type alias Hand = Dict Int Card

type AnimationTask
  = CardEffectUpdate (CardEffects -> CardEffects)
  | Resume Model

type alias CardEffects = Dict Int CardEffect -- index, state

type CardEffect
  = RollOpen
  | BeatOut
  | ZoomInClose

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
  , hand = Dict.empty
  , money = 10
  , bet = 1
  , deckWave = 1
  , animationWaiting = False
  , animationQueue = Heap.empty (smallest |> by Tuple.first)
  , cardEffects = Dict.empty
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
  | AnimationFire
  | ReserveAnimation Int AnimationTask
  | DoNothing


update : Msg -> Model -> (Model ,Cmd Msg)
update msg model =
  case (model.phase, msg) of
    ( BetPhase, Bet n ) ->
      ( { model | bet = model.bet + n }
      , Cmd.none
      )

    ( BetPhase, Decide ) ->
      let
        newModel =
          { model
          | phase = SelectPhase <| Set.empty
          , money = model.money - model.bet
          }

        animationCommand =
          List.range 0 5
            |> List.map (\i ->
              reserveAnimation (i*300) i RollOpen)
            |> Cmd.batch
      in
        ( newModel, animationCommand )

    ( SelectPhase select, Check n ) ->
      ( { model
        | phase =
            SelectPhase
              <|( if select |> Set.member n
                  then select |> Set.remove n
                  else select |> Set.insert n
                )
        }
      , Cmd.none
      )
    ( SelectPhase select, Trash ) ->
      let
        trashCount =
          select
            |> Set.size

        refreshHand =
          zip
          (model.deck |> List.take trashCount)
          (select |> Set.toList)
            |> List.foldl (\(c, i) d -> Dict.insert i c d) model.hand

        newDeck =
          model.deck
            |> List.drop trashCount

        newModel =
          { model
          | phase = ResultPhase
          , hand = refreshHand
          , deck = newDeck
          }

        cardEffects =
          select
            |> Set.toList
            |> List.foldl (\i -> Dict.insert i BeatOut) model.cardEffects

        oldModel =
          { model | cardEffects = cardEffects }
      in
        animationWait 500 newModel oldModel

    ( ResultPhase, Finish ) ->
      let
        newMoney =
          model.hand
            |> evaluateRole
            |> toOdds
            |> (*) model.bet
            |> (+) model.money

        newHand =
          model.deck
            |> List.take 5
            |> List.indexedMap Tuple.pair
            |> Dict.fromList

        newDeck =
          model.deck
            |> List.drop 5

        nextPhase =
          if newMoney > 0
          then BetPhase
          else GameOver

        refreshIfNeed =
          if model.deckWave == 3
          then refreshDeck
          else Cmd.none

        cardEffects = Dict.empty
      in
        ( { model
          | phase = nextPhase
          , deck = newDeck
          , hand = newHand
          , money = newMoney
          , bet = 1
          , deckWave = model.deckWave + 1
          , cardEffects = cardEffects
          }
        , refreshIfNeed )

    ( ResultPhase, Check _ ) ->
      ( model, Cmd.none )

    ( GameOver, Continue ) ->
      ( initialModel
      , refreshDeck
      )

    ( _, Refresh deck ) ->
      ( { model
        | deck =
            deck
              |> List.drop 5

        , hand =
            deck
              |> List.take 5
              |> List.indexedMap Tuple.pair
              |> Dict.fromList

        , deckWave = 1
        }
      , Cmd.none
      )

    ( _, AnimationFire ) ->
      case model.animationQueue |> Heap.pop of
        Nothing ->
          ( model, Cmd.none )

        Just ( ( _, task ), tl) ->
          let
            newModel =
              case task of
                CardEffectUpdate updater ->
                  { model
                  | animationQueue = tl
                  , cardEffects = updater model.cardEffects
                  }

                Resume reserved ->
                  reserved
          in
            ( newModel, Cmd.none )

    ( _, ReserveAnimation millis animation ) ->
      let
        newModel =
          { model | animationQueue =
            model.animationQueue
              |> Heap.push ( millis, animation )}
      in
        ( newModel, Cmd.none )

    ( _, DoNothing ) ->
      ( model, Cmd.none )

    _ ->
      ( { model | phase = InvalidState }, Cmd.none )

evaluateRole : Hand -> Role
evaluateRole hand_ =
  let
    hand =
      hand_
        |> Dict.values

    handToInt =
      hand
        |> List.sortBy Tuple.second
        |> List.unzip
        |> Tuple.second
        |> List.reverse

    difference = handToInt
      |> List.map (\n ->
        n - (Maybe.withDefault 0 (List.head handToInt)))
      |> List.map ((+)13)

    isFlush =
      hand
        |> List.unzip
        |> Tuple.first
        |> toBag
        |> List.length
        |> (==) 1

    isStraight =
      difference == [13,12,11,10,9] ||
      difference == [13,12,11,10,1] -- Ace high

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
          (True , True ) ->
            if aceHigh
            then RoyalFlush
            else StraightFlush
          (True , False) -> Flush
          (False, True ) -> Straight
          (False, False) -> None

      [1,1,1,2] -> Pair1
      [1,2,2]   -> Pair2
      [1,1,3]   -> ThreeCard
      [1,4]   -> FourCard
      [2,3]   -> FullHouse
      _     -> None

reserveTask: Int -> AnimationTask -> Cmd Msg
reserveTask delay task =
  let
    content posix =
      ReserveAnimation
      (Time.posixToMillis posix + delay)
      task
  in
    Task.perform content Time.now

reserveAnimation: Int -> Int -> CardEffect -> Cmd Msg
reserveAnimation delay index state =
  reserveTask delay <| CardEffectUpdate <| createAnimation index state

createAnimation : Int -> CardEffect -> CardEffects -> CardEffects
createAnimation =
  Dict.insert

animationWait : Int -> Model -> Model -> ( Model, Cmd Msg )
animationWait waitTime nextModel currentModel =
  let
    cmd =
      reserveTask waitTime
        <| Resume nextModel
  in
    ( { currentModel | animationWaiting = True }, cmd )



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

    handDiv ss =
      model.hand
        |> Dict.toList
        |> List.map (\(i, c) ->
            viewCard
            i
            (ss |> Set.member i)
            (model.cardEffects |> Dict.get i)
            (Just c))
        |> div[]

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

      SelectPhase select ->
        let
          trushButton =
            button
            [ onClick <| Trash
            , disabled <| model.animationWaiting
            ]
            [ text "trash" ]
        in
          div []
            [ waveDeckText, br[][]
            , coinText, br[][]
            , betText, br[][]
            , handDiv select
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
            , handDiv Set.empty
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

      InvalidState ->
        text "!! Error !!"

viewCard : Int -> Bool -> Maybe CardEffect -> Maybe Card -> Html Msg
viewCard n darken maybeState maybeCard =
  let
    atrrList =
      maybeState
        |> Maybe.map (cardEffectToClassName >> class >> List.singleton)
        |> Maybe.withDefault []

    content =
      case maybeCard of
        Nothing ->
          []
        Just ( suit, number ) ->
          [ div atrrList
            [ div [ class "card-front" ]
              [ img [ src "img/cardBack.svg" ][] ]
            , div [ class "card-back" ]
              [ svg [ width "84", height "119" ]
                [ cardFrontImage
                , text_
                  [ x "42"
                  , y "90"
                  , SvgAttr.fill (suit |> suitToColor)
                  , SvgAttr.class "suit"
                  ]
                  [ text (suit |> suitToString) ]
                , text_
                  [ x "42"
                  , y "85"
                  , SvgAttr.class "number"
                  ]
                  [ text (number |> numberToString) ]
                , rect
                  (overlayStyle darken)
                  []
                ]
              ]
            ]
          ]

  in
    div
    [ class "card" ,width "84", height "119", onClick <| Check n]
    content

cardEffectToClassName : CardEffect -> String
cardEffectToClassName state =
  case state of
    RollOpen ->
      "rollOpen inner"

    BeatOut ->
      "beatOut inner"

    ZoomInClose ->
      "zoomInClose inner"

cardFrontImage =
  rect
  [ x "0"
  , y "0"
  , width "84"
  , height "119"
  , rx "8"
  , ry "8"
  , fill "white"
  , stroke "black"
  ]
  []

overlayStyle darken =
  [ x "0"
  , y "0"
  , width "84"
  , height "119"
  , rx "8"
  , ry "8"
  , fill "black"
  , fillOpacity (if darken then "0.5" else "0.0")
  ]



 -- SUBSCRIPTIONS --

subscriptions: Model -> Sub Msg
subscriptions model =
  onAnimationFrame (\posix ->
    case model.animationQueue |> Heap.peek of
      Nothing ->
        DoNothing

      Just ( time, _ ) ->
        if time < posixToMillis posix
        then AnimationFire
        else DoNothing
  )



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
    , subscriptions = subscriptions
    }
