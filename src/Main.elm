module Main exposing (main)

import Browser exposing (element)
import Dict
import Html as H exposing (..)
import Random
import Random.List exposing (shuffle)
import Set



-- 第1章


oneZero : String
oneZero =
    String.reverse "stressed"


oneOne : String
oneOne =
    let
        base =
            "パタトクカシーー"

        toIndexedList =
            String.toList >> List.indexedMap Tuple.pair

        firstIsEven =
            Tuple.first >> modBy 2 >> (\n -> n == 0)

        toNormalString =
            List.map Tuple.second >> String.fromList
    in
    base |> toIndexedList |> List.filter firstIsEven |> toNormalString


oneTwo : String
oneTwo =
    let
        base1 =
            "パトカー"

        base2 =
            "タクシー"

        base1List =
            String.toList base1

        base2List =
            String.toList base2

        joinChars a b =
            String.cons a <| String.fromChar b
    in
    List.map2 joinChars base1List base2List |> String.concat


oneThree : String
oneThree =
    let
        base =
            "Now I need a drink, alcoholic of course, after the heavy lectures involving quantum mechanics."

        removeNotLatinChars : List Char -> List Char
        removeNotLatinChars =
            List.filter Char.isAlpha

        countLatinChars =
            String.toList >> removeNotLatinChars >> List.length
    in
    base
        |> String.words
        |> List.map countLatinChars
        |> Debug.toString


oneFour : Dict.Dict String Int
oneFour =
    let
        base =
            "Hi He Lied Because Boron Could Not Oxidize Fluorine. New Nations Might Also Sign Peace Security Clause. Arthur King Can."

        toIndexedWords =
            String.words >> List.indexedMap Tuple.pair

        takeFirstSet =
            Set.fromList [ 1, 5, 6, 7, 8, 9, 15, 16, 19 ] |> Set.map (\n -> n - 1)

        extractString i word =
            if Set.member i takeFirstSet then
                String.left 1 word

            else
                String.left 2 word

        insertDict ( i, word ) dict =
            Dict.insert (extractString i word) (i + 1) dict
    in
    base |> toIndexedWords |> List.foldl insertDict Dict.empty


oneFive : Int -> List a -> List (List a)
oneFive n list =
    let
        canMake =
            List.length list - n + 1

        createGram =
            List.drop >> (<<) (List.take n)
    in
    List.repeat canMake list |> List.indexedMap createGram


oneSix : Html msg
oneSix =
    let
        xBase =
            "paraparaparadise"

        yBase =
            "paragraph"

        stringToBigramSet =
            String.toList >> oneFive 2 >> Set.fromList

        x =
            stringToBigramSet xBase

        y =
            stringToBigramSet yBase

        applyAndToString f =
            f x y |> Debug.toString
    in
    H.div []
        [ H.p []
            [ H.text <|
                "和集合は"
                    ++ applyAndToString Set.union
                    ++ "です"
            ]
        , H.p []
            [ H.text <|
                "積集合は"
                    ++ applyAndToString Set.intersect
                    ++ "です"
            ]
        , H.p []
            [ H.text <|
                "差集合は"
                    ++ applyAndToString Set.diff
                    ++ "です"
            ]
        ]


oneSeven : String -> String -> String -> String
oneSeven x y z =
    x ++ "時の" ++ y ++ "は" ++ z


oneEight : String -> String
oneEight =
    let
        convert c =
            if Char.isLower c then
                Char.toCode c |> (-) 219 |> Char.fromCode

            else
                c
    in
    String.map convert


oneNine : String -> Random.Generator String
oneNine =
    let
        extractLettersToShuffle : String -> String
        extractLettersToShuffle =
            String.dropLeft 1 >> String.dropRight 1

        concatShuffled : String -> List Char -> String
        concatShuffled word shuffled =
            String.left 1 word ++ String.fromList shuffled ++ String.right 1 word

        shuffleWord : String -> Random.Generator String
        shuffleWord word =
            if String.length word < 5 then
                Random.constant word

            else
                extractLettersToShuffle word
                    |> String.toList
                    |> shuffle
                    |> Random.andThen (concatShuffled word >> Random.constant)

        foldGenerator : List (Random.Generator String) -> Random.Generator (List String)
        foldGenerator =
            List.foldr (Random.map2 (::)) (Random.constant [])

        concatWords : Random.Generator (List String) -> Random.Generator String
        concatWords =
            Random.map <| String.join " "
    in
    String.words
        >> List.map shuffleWord
        >> foldGenerator
        >> concatWords


type alias Model =
    String


type Msg
    = GotOneNine String


type alias Flags =
    ()


main : Program Flags Model Msg
main =
    let
        oneNineCmd =
            Random.generate GotOneNine <| oneNine "I couldn't believe that I could actually understand what I was reading : the phenomenal power of the human mind ."
    in
    element
        { init = always ( "", oneNineCmd )
        , view = view
        , update = update
        , subscriptions = always Sub.none
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg _ =
    case msg of
        GotOneNine string ->
            ( string, Cmd.none )


view : Model -> Html Msg
view m =
    H.div []
        [ H.div []
            [ H.h1 [] [ H.text "第1章" ]
            , H.h2 [] [ H.text "00" ]
            , H.text oneZero
            , H.h2 [] [ H.text "01" ]
            , H.text oneOne
            , H.h2 [] [ H.text "02" ]
            , H.text oneTwo
            , H.h2 [] [ H.text "03" ]
            , H.text oneThree
            , H.h2 [] [ H.text "04" ]
            , H.ul [] <|
                List.map
                    (\( k, v ) ->
                        k ++ " => " ++ String.fromInt v |> H.text |> List.singleton |> H.li []
                    )
                    (Dict.toList oneFour)
            ]
        , H.h2 [] [ H.text "05" ]
        , H.p [] [ H.text <| "単語bi-gram: " ++ (Debug.toString <| oneFive 2 (String.words "I am an NLPer")) ]
        , H.p []
            [ H.text <|
                "文字bi-gram: "
                    ++ (Debug.toString <|
                            oneFive 2
                                (List.filter Char.isAlphaNum <| String.toList "I am an NLPer")
                       )
            ]
        , H.h2 [] [ H.text "06" ]
        , oneSix
        , H.h2 [] [ H.text "07" ]
        , H.p [] [ H.text <| oneSeven "12" "気温" "22.4" ]
        , H.h2 [] [ H.text "08" ]
        , H.p [] [ H.text <| oneEight "No, I am your father" ]
        , H.h2 [] [ H.text "09" ]
        , H.p [] [ H.text m ]
        ]
