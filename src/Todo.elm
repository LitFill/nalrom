module Todo exposing (..)

import Html as H


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


zipWith : (a -> b -> c) -> List a -> List b -> List c
zipWith fn xs ys =
    case ( xs, ys ) of
        ( [], _ ) ->
            []

        ( _, [] ) ->
            []

        ( [ x ], [ y ] ) ->
            [ fn x y ]

        ( x :: xx, y :: yy ) ->
            fn x y :: zipWith fn xx yy


normalize : List Todo -> List Todo
normalize todos =
    let
        updatePrioritas prioritas todo =
            { todo | prioritas = prioritas }
    in
    todos
        |> List.sortBy .prioritas
        |> zipWith
            updatePrioritas
            (List.range 1 (List.length todos) |> List.map toFloat)


main : H.Html msg
main =
    H.text "Hello"
