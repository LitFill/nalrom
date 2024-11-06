port module Main exposing (main)

import Browser
import FeatherIcons as F
import Html as H
import Html.Attributes as A
import Html.Events as E
import Json.Decode as JD
import Json.Encode as JE


type TodoStatus
    = Belum
    | Sedang
    | Sudah


showStatus : TodoStatus -> String
showStatus status =
    case status of
        Belum ->
            "belum"

        Sedang ->
            "sedang"

        Sudah ->
            "sudah"


readStatus : String -> TodoStatus
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


enumerateStatus : TodoStatus -> Int
enumerateStatus status =
    case status of
        Belum ->
            0

        Sedang ->
            1

        Sudah ->
            2


type alias Deskripsi =
    String


type alias ID =
    Int


type alias Prioritas =
    Float


type alias Todo =
    { id : ID
    , deskripsi : Deskripsi
    , status : TodoStatus
    , prioritas : Prioritas
    }


type alias Model =
    { todos : List Todo
    , idSelanjutnya : ID
    , teksInput : String
    , prioritasInput : Float
    }


init : JE.Value -> ( Model, Cmd Msg )
init flag =
    ( case JD.decodeValue decoder flag of
        Ok model ->
            model

        Err _ ->
            { todos = []
            , idSelanjutnya = 1
            , teksInput = ""
            , prioritasInput = 0
            }
    , Cmd.none
    )


type Msg
    = TambahTodo Deskripsi Prioritas
    | SimpanTeksInput String
    | SimpanPrioritasInput Float
    | GantiStatusTodo ID TodoStatus
    | HapusTodo ID


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TambahTodo desk prioritas ->
            let
                todoBaru : Todo
                todoBaru =
                    { id = model.idSelanjutnya
                    , deskripsi = desk
                    , status = Belum
                    , prioritas = prioritas
                    }
            in
            ( if String.isEmpty desk then
                model

              else
                { model
                    | todos =
                        todoBaru
                            :: model.todos
                            |> normalizeTodos
                            |> List.reverse
                            |> List.sortWith urutkanTodos
                    , idSelanjutnya = model.idSelanjutnya + 1
                }
            , Cmd.none
            )

        GantiStatusTodo id status ->
            ( { model
                | todos =
                    model.todos
                        |> List.map
                            (\todo ->
                                if todo.id == id then
                                    { todo | status = status }

                                else
                                    todo
                            )
                        |> normalizeTodos
                        |> List.reverse
                        |> List.sortWith urutkanTodos
              }
            , Cmd.none
            )

        HapusTodo id ->
            ( { model
                | todos =
                    model.todos
                        |> List.filter (\todo -> todo.id /= id)
                        |> normalizeTodos
                        |> List.reverse
                        |> List.sortWith urutkanTodos
              }
            , Cmd.none
            )

        SimpanTeksInput teks ->
            ( { model | teksInput = teks }, Cmd.none )

        SimpanPrioritasInput prioritas ->
            ( { model | prioritasInput = prioritas }, Cmd.none )


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


normalizeTodos : List Todo -> List Todo
normalizeTodos todos =
    let
        updatePrioritas prioritas todo =
            { todo | prioritas = prioritas }
    in
    todos
        |> List.sortBy .prioritas
        |> zipWith
            updatePrioritas
            (List.range 1 (List.length todos) |> List.map toFloat)


urutkanTodos : Todo -> Todo -> Order
urutkanTodos todo1 todo2 =
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


viewTodo : Todo -> H.Html Msg
viewTodo todo =
    let
        ( msg, label ) =
            case todo.status of
                Belum ->
                    ( GantiStatusTodo todo.id Sedang, "lakukan" )

                Sedang ->
                    ( GantiStatusTodo todo.id Sudah, "selesai" )

                Sudah ->
                    ( GantiStatusTodo todo.id Belum, "ulangi" )
    in
    H.article [ A.class ("todo " ++ showStatus todo.status) ]
        [ H.nav []
            [ H.ul []
                [ H.li []
                    [ H.text todo.deskripsi
                    , H.text <| " (" ++ showStatus todo.status ++ ") "
                    , H.text <| " <" ++ String.fromFloat todo.prioritas ++ "> "
                    ]
                ]
            , H.ul []
                [ H.progress
                    [ A.value
                        (todo.status
                            |> enumerateStatus
                            |> String.fromInt
                        )
                    , A.max "2"
                    ]
                    []
                , H.li []
                    [ H.button [ A.class "steps", E.onClick msg ]
                        [ (case todo.status of
                            Sudah ->
                                F.rotateCcw

                            Belum ->
                                F.play

                            Sedang ->
                                F.fastForward
                          )
                            |> F.toHtml []
                        , H.text label
                        ]
                    ]
                , H.li []
                    [ H.button [ A.class "hapus", E.onClick (HapusTodo todo.id) ]
                        [ F.trash |> F.toHtml [], H.text "hapus" ]
                    ]
                ]
            ]
        ]


view : Model -> H.Html Msg
view model =
    H.main_ []
        [ H.h1 [] [ H.text "Todo App" ]
        , H.form [ E.onSubmit (TambahTodo model.teksInput model.prioritasInput) ]
            [ H.fieldset [ A.attribute "role" "group" ]
                [ H.input [ A.placeholder "todo baru", E.onInput SimpanTeksInput ] []
                , H.input
                    [ A.placeholder "prioritas"
                    , E.onInput
                        (String.toFloat
                            >> Maybe.withDefault 0
                            >> SimpanPrioritasInput
                        )
                    ]
                    []
                , H.input [ A.value "Tambah", A.type_ "submit" ] []
                ]
            ]
        , H.div [] <| List.map viewTodo model.todos
        , H.br [] []
        , H.footer [] [ H.text "Copyright (c) 2024 LitFill. All Rights Reserved." ]
        ]


port setStorage : JE.Value -> Cmd msg


updateDanStorage : Msg -> Model -> ( Model, Cmd Msg )
updateDanStorage msg modelLama =
    let
        ( modelBaru, cmd ) =
            update msg modelLama
    in
    ( modelBaru
    , Cmd.batch
        [ encode modelBaru |> setStorage
        , cmd
        ]
    )


encodeTodo : Todo -> JE.Value
encodeTodo todo =
    JE.object
        [ ( "id", JE.int todo.id )
        , ( "deskripsi", JE.string todo.deskripsi )
        , ( "status", JE.string <| showStatus todo.status )
        , ( "prioritas", JE.float todo.prioritas )
        ]


encode : Model -> JE.Value
encode model =
    JE.object
        [ ( "todos", JE.list encodeTodo model.todos )
        , ( "id_berikutnya", JE.int model.idSelanjutnya )
        , ( "teks_input", JE.string model.teksInput )
        , ( "prioritas_input", JE.float model.prioritasInput )
        ]


statusDecoder : JD.Decoder TodoStatus
statusDecoder =
    JD.map readStatus JD.string


todoDecoder : JD.Decoder Todo
todoDecoder =
    JD.map4 Todo
        (JD.field "id" JD.int)
        (JD.field "deskripsi" JD.string)
        (JD.field "status" statusDecoder)
        (JD.field "prioritas" JD.float)


decoder : JD.Decoder Model
decoder =
    JD.map4 Model
        (JD.field "todos" <| JD.list todoDecoder)
        (JD.field "id_berikutnya" JD.int)
        (JD.field "teks_input" JD.string)
        (JD.field "prioritas_input" JD.float)


main : Program JE.Value Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = updateDanStorage
        , subscriptions = \_ -> Sub.none
        }
