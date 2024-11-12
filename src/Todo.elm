module Todo exposing (..)

import Array exposing (Array)
import Html as H
import List exposing (..)
import Regex exposing (Regex)


type Status
    = Belum
    | Sedang
    | Sudah


showStatus : Status -> String
showStatus status =
    case status of
        Belum ->
            "belum"

        Sedang ->
            "sedang"

        Sudah ->
            "sudah"


readStatus : String -> Status
readStatus str =
    case str of
        "sudah" ->
            Sudah

        "sedang" ->
            Sedang

        "belum" ->
            Belum

        _ ->
            Belum


enumerateStatus : Status -> Int
enumerateStatus status =
    case status of
        Belum ->
            0

        Sedang ->
            1

        Sudah ->
            2


type Skala
    = S
    | M
    | L
    | XL


showSkala : Skala -> String
showSkala skala =
    case skala of
        S ->
            "S"

        M ->
            "M"

        L ->
            "L"

        XL ->
            "XL"


readSkala : String -> Skala
readSkala str =
    case str of
        "S" ->
            S

        "M" ->
            M

        "L" ->
            L

        "XL" ->
            XL

        _ ->
            S


type alias Deskripsi =
    String


type alias ID =
    Int


type alias Prioritas =
    Float


type alias Todo =
    { id : ID
    , deskripsi : Deskripsi
    , status : Status
    , prioritas : Prioritas
    , skala : Skala
    }


urutkan : Todo -> Todo -> Order
urutkan todo1 todo2 =
    let
        s1 =
            todo1.status

        s2 =
            todo2.status
    in
    if s1 == s2 then
        EQ

    else
        case ( s1, s2 ) of
            ( Sudah, _ ) ->
                GT

            ( _, Sudah ) ->
                LT

            ( Belum, _ ) ->
                GT

            ( _, Belum ) ->
                LT

            _ ->
                EQ


normalize : List Todo -> List Todo
normalize todos =
    let
        updatePrioritas : Prioritas -> Todo -> Todo
        updatePrioritas prioritas todo =
            { todo | prioritas = prioritas }
    in
    todos
        |> sortBy .prioritas
        |> map2 updatePrioritas
            (range 1 (length todos) |> map toFloat)


showTodos : List Todo -> String
showTodos =
    map showTodo >> String.join "\n"


showTodo : Todo -> String
showTodo todo =
    interpolate "{0} -- {1} [{2}] <{3}>"
        [ todo.deskripsi
        , todo.status |> showStatus
        , todo.skala |> showSkala
        , todo.prioritas |> String.fromFloat
        ]


main : H.Html msg
main =
    H.text "Hello"


{-| stolen from String.Interpolate
-}
interpolate : String -> List String -> String
interpolate string args =
    let
        asArray =
            Array.fromList args
    in
    Regex.replace interpolationRegex (applyInterpolation asArray) string


interpolationRegex : Regex
interpolationRegex =
    Regex.fromString "\\{\\d+\\}" |> Maybe.withDefault Regex.never


applyInterpolation : Array String -> Regex.Match -> String
applyInterpolation replacements { match } =
    let
        ordinalString =
            (String.dropLeft 1 << String.dropRight 1) match
    in
    ordinalString
        |> String.toInt
        |> Maybe.andThen (\value -> Array.get value replacements)
        |> Maybe.withDefault ""
