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
        Spade -> "♠︎"
        Heart -> "♡"
        Diamond -> "♢"
        Club -> "♣︎"
        
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
toOdds n =
    case n of
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
        
type alias Card = ( Suit, Int)
type alias Model =
    { deck : List Card
    , hand : List Card
    , select : List Bool
    , coin : (Int, Int)
    , finishCheck : Bool
    , wave : Int
    }

numberToString : Int -> String
numberToString n =
    case n of 
        14 -> "A"
        11 -> "J"
        12 -> "Q"
        13 -> "K"
        m -> String.fromInt m
        
init () =
    ( initialModel, Random.generate Refresh (shuffle initialDeck) )

initialModel : Model
initialModel =
    { deck = initialDeck
    , hand = []
    , select = List.repeat 5 False
    , coin = (10,0) 
    , finishCheck = False
    , wave = 1
    }

initialDeck : List Card
initialDeck =
    cross [Spade, Heart, Diamond, Club] (List.range 2 14)



-- UPDATE --

type Msg
    = Draw
    | Refresh (List Card)
    | Check Int
    | Trash
    | Finish Int
    | Continue
    | Bet Int


update : Msg -> Model -> (Model ,Cmd Msg)
update msg model =
    let
        newModel = 
            case msg of
                --Draw -> { model | deck = List.drop 1 model.deck }
                Draw -> { model | hand = [ (Spade,14), (Spade,13), (Spade,12), (Spade,11), (Spade,10) ] }
                Refresh deck -> { model | deck = List.drop 5 deck, hand = List.take 5 deck, coin = ((Tuple.first model.coin)-1,1), finishCheck = False}
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
                        { model | select = List.repeat 5 False, hand = refreshHand, deck = List.drop (5-List.length trashedHand) model.deck, finishCheck = True}
       
                Finish n ->
                    case  n of
                        3 ->
                            { model | coin = ((toOdds <| evaluateRole <| model.hand)*(Tuple.second model.coin)+(Tuple.first model.coin),1), finishCheck = False, wave=1}
                        _ ->
                            { model | deck = List.drop 5 model.deck, hand = List.take 5 model.deck, coin = ((toOdds <| evaluateRole <| model.hand)*(Tuple.second model.coin)+(Tuple.first model.coin)-1,1), finishCheck = False, wave = model.wave +1 }
                            
                Continue -> { model | coin = (10,0), select = List.repeat 5 False, wave =1 }
                Bet n-> { model | coin = ((Tuple.first model.coin)-n,(Tuple.second model.coin)+n)}
    
        cmd = 
            case msg of
                Finish wave ->
                    case wave of
                        3 -> Random.generate Refresh (shuffle initialDeck)
                        _ -> Cmd.none
                Continue -> Random.generate Refresh (shuffle initialDeck)
                _ -> Cmd.none
    in
        ( newModel, cmd )
        

evaluateRole : List Card -> Role
evaluateRole hand =
    let
        handToInt = Tuple.second <| List.unzip <|  List.sortBy Tuple.second <| hand
        difference = handToInt
            |> List.map ((-)(Maybe.withDefault 0 (List.head handToInt)))
            |> List.map ((+)14)
        
        result = toBag <|
            Tuple.second (List.unzip hand)
            
        sortedNumber = List.sort <| Tuple.second <| List.unzip result
        
        suitToBag = toBag <| Tuple.first <| List.unzip <| hand

        isFlush = (suitToBag |> List.length)==1
                
        isStraight =
            case difference of
                [14,13,12,11,10] -> True
                [14,13,12,11,2]  -> True
                _ -> False
                      
        isMin10 = (Maybe.withDefault 0 (List.head handToInt))==10
    in
        case sortedNumber of
            [1,1,1,1,1] -> 
                case (isFlush,isStraight) of
                    (True,True)  -> if isMin10 then RoyalFlush else StraightFlush
                    (True,False) -> Flush
                    (False,True) -> Straight
                    (_,_)        -> None
                    
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
        top = case model.deck of
            [] -> text <| "error"
            card :: _ ->viewCard card
        allHand = List.map viewCard  model.hand
        selectInput n c = input[type_ "checkbox",onClick <| Check n,checked c][]
        allSelects = model.select
            |> List.indexedMap selectInput
            
        roleText = case model.hand |> evaluateRole of
            RoyalFlush -> "RoyalFlush !!"
            StraightFlush->"StraightFlush"
            FourCard  -> "4 Card"
            FullHouse -> "FullHouse"
            Flush -> "Flush"
            Straight -> "Straight"
            ThreeCard -> "3 Card"
            Pair2 -> "2 Pairs"
            Pair1 -> "1 Pair"
            None  -> "None"
        
        betButton =
            let
                logCoin = Basics.floor <| Basics.logBase 10 (Basics.toFloat <| Tuple.first <| model.coin)
                addButton n = 
                     button[onClick <| Bet <| 10^n]
                        [text 
                            <| String.append "+"
                            <| String.append (String.fromInt <| (10^n))
                            "00"
                        ]
                    
            in
                if model.finishCheck |> not then
                    if (Tuple.first <| model.coin) == 0 then 
                        []
                    else
                        List.append 
                            (List.map addButton (List.range 0 logCoin))
                            [button[onClick <| Bet <| Tuple.first <| model.coin][text  "Bet All"]]
                else
                    []
                
        changeButton : Bool -> List (Html Msg)
        changeButton n=
            if n then
                [button[onClick <| Finish model.wave][text "click to next"]]
            else
                [button[onClick <| Trash][text "trash"]]
                
        odds = toOdds<|evaluateRole<|model.hand
        
        win = (Tuple.second model.coin)*(odds)
                
        betText = 
            if model.finishCheck then
                if (win /= 0) then
                    [text <| String.append "Win  : " 
                        <| String.append (String.fromInt<|Tuple.second<|model.coin)
                        <| String.append "00*"
                        <| String.append (String.fromInt odds)
                        <| String.append "="
                        <| String.append (String.fromInt win)
                        "00"
                    ]
                else
                    [text <| String.append "Lose : " 
                        <| String.append (String.fromInt<|Tuple.second<|model.coin)
                        "00*0=0"
                    ]
            else
                [text <| String.append "Bet  : " 
                    <| String.append (String.fromInt<|Tuple.second<|model.coin)
                    "00"
                ]
                
        coinString = 
            if (((model.coin |> Tuple.first) == 0) |> not) then
                String.append "Coin : " 
                    <| String.append (String.fromInt<|Tuple.first <|model.coin)
                    "00"
            else 
                "Coin : 0"
                
        waveString = 
            [text 
                <| String.append "Wave : "
                <| String.append (String.fromInt model.wave)
                <| String.append " / 3, Deck : "
                (String.fromInt <| List.length <| model.deck)
            ]
                
            
    in 
        if (model.coin |> Tuple.first) >= 0 then
            div[]
                <| List.append waveString
                <| List.append [br[][]]
                <| List.append allHand 
                <| List.append [br[][]]
                <| List.append allSelects
                <| List.append [br[][]]
                <| List.append [text <| "Role : "++roleText]
                <| List.append [br[][]]
                <| List.append [text <| coinString]
                <| List.append [br[][],br[][]]
                <| List.append (changeButton model.finishCheck)
                <| List.append [br[][],br[][]]
                <| List.append betText
                <| List.append [br[][]]
            betButton
            
        else
            div[]
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
