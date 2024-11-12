port module Main exposing (main)

import Browser
import FeatherIcons as F
import File.Download as Download
import Html as H
import Html.Attributes as A
import Html.Events as E
import Json.Decode as JD
import Json.Encode as JE
import Todo exposing (Todo)


type alias Model =
    { todos : List Todo
    , idSelanjutnya : Todo.ID
    , teksInput : String
    , prioritasInput : Float
    , skalaInput : String
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
            , skalaInput = ""
            }
    , Cmd.none
    )


type Msg
    = TambahTodo Todo.Deskripsi Todo.Prioritas Todo.Skala
    | SimpanTeksInput String
    | SimpanPrioritasInput Float
    | SimpanSkalaInput String
    | GantiStatusTodo Todo.ID Todo.Status
    | HapusTodo Todo.ID
    | DownloadTodos


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TambahTodo desk prioritas skala ->
            let
                todoBaru : Todo
                todoBaru =
                    { id = model.idSelanjutnya
                    , deskripsi = desk
                    , status = Todo.Belum
                    , prioritas = prioritas
                    , skala = skala
                    }
            in
            ( if String.isEmpty desk then
                model

              else
                { model
                    | todos =
                        todoBaru
                            :: model.todos
                            |> Todo.normalize
                            |> List.reverse
                            |> List.sortWith Todo.urutkan
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
                        |> Todo.normalize
                        |> List.reverse
                        |> List.sortWith Todo.urutkan
              }
            , Cmd.none
            )

        HapusTodo id ->
            ( { model
                | todos =
                    model.todos
                        |> List.filter (\todo -> todo.id /= id)
                        |> Todo.normalize
                        |> List.reverse
                        |> List.sortWith Todo.urutkan
              }
            , Cmd.none
            )

        SimpanTeksInput teks ->
            ( { model | teksInput = teks }, Cmd.none )

        SimpanPrioritasInput prioritas ->
            ( { model | prioritasInput = prioritas }, Cmd.none )

        SimpanSkalaInput skala ->
            ( { model | skalaInput = skala }, Cmd.none )

        DownloadTodos ->
            ( model
            , model.todos |> unduhTodos
            )


viewTodo : Todo -> H.Html Msg
viewTodo todo =
    let
        ( msg, label ) =
            case todo.status of
                Todo.Belum ->
                    ( GantiStatusTodo todo.id Todo.Sedang, " lakukan" )

                Todo.Sedang ->
                    ( GantiStatusTodo todo.id Todo.Sudah, " selesai" )

                Todo.Sudah ->
                    ( GantiStatusTodo todo.id Todo.Belum, " ulangi" )
    in
    H.article [ A.class ("todo " ++ Todo.showStatus todo.status) ]
        [ H.nav []
            [ H.ul [ A.class "teks" ]
                [ H.li []
                    [ H.text todo.deskripsi
                    , H.text <| " (" ++ Todo.showStatus todo.status ++ ") "
                    , H.text <| " <" ++ String.fromFloat todo.prioritas ++ "> "
                    , H.text <| " [" ++ Todo.showSkala todo.skala ++ "] "
                    ]
                ]
            , H.ul [ A.class "kontrol" ]
                [ H.progress
                    [ A.value
                        (todo.status
                            |> Todo.enumerateStatus
                            |> String.fromInt
                        )
                    , A.max "2"
                    ]
                    []
                , H.li []
                    [ H.button [ A.class "steps", E.onClick msg ]
                        [ (case todo.status of
                            Todo.Sudah ->
                                F.rotateCcw

                            Todo.Belum ->
                                F.play

                            Todo.Sedang ->
                                F.fastForward
                          )
                            |> F.toHtml []
                        , H.text label
                        ]
                    ]
                , H.li []
                    [ H.button [ A.class "hapus", E.onClick (HapusTodo todo.id) ]
                        [ F.trash |> F.toHtml [], H.text " hapus" ]
                    ]
                ]
            ]
        ]


view : Model -> H.Html Msg
view model =
    let
        skala =
            Todo.readSkala model.skalaInput
    in
    H.main_ []
        [ H.h1 [] [ H.text "Todo App" ]
        , H.form
            [ E.onSubmit
                (TambahTodo model.teksInput model.prioritasInput skala)
            ]
            [ H.fieldset [ A.attribute "role" "group" ]
                [ H.input [ A.placeholder "todo baru", E.onInput SimpanTeksInput ] []
                , H.input
                    [ A.placeholder "prioritas (0.0 - ∞)"
                    , E.onInput
                        (String.toFloat
                            >> Maybe.withDefault 0
                            >> SimpanPrioritasInput
                        )
                    ]
                    []
                ]
            , H.fieldset [ A.attribute "role" "group" ]
                [ H.select [ A.name "skala", E.onInput SimpanSkalaInput, onSelect SimpanSkalaInput ]
                    [ H.option [ A.selected True, A.disabled True, A.value "" ]
                        [ H.text "skala" ]
                    , H.option [] [ H.text "S" ]
                    , H.option [] [ H.text "M" ]
                    , H.option [] [ H.text "L" ]
                    , H.option [] [ H.text "XL" ]
                    ]
                , H.input [ A.value "Tambah", A.type_ "submit" ] []
                ]
            ]
        , H.div [] <| List.map viewTodo model.todos
        , H.div
            [ A.style "display" "flex"
            , A.style "justify-content" "flex-end"
            ]
            [ H.button [ E.onClick DownloadTodos ] [ H.text "Simpan Todos" ] ]
        , H.br [] []
        , H.footer []
            [ H.text "Copyright (c) 2024 "
            , H.a [ A.href "https://github.com/LitFill" ] [ H.text "LitFill" ]
            , H.text ". All Rights are open source."
            ]
        ]



-- NOTE: this is from elm Html.Events


onSelect : (String -> msg) -> H.Attribute msg
onSelect tagger =
    E.stopPropagationOn "input" <|
        JD.map alwaysStop (JD.map tagger E.targetValue)


alwaysStop : a -> ( a, Bool )
alwaysStop x =
    ( x, True )


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
        , ( "status", JE.string <| Todo.showStatus todo.status )
        , ( "prioritas", JE.float todo.prioritas )
        , ( "skala", JE.string <| Todo.showSkala todo.skala )
        ]


encode : Model -> JE.Value
encode model =
    JE.object
        [ ( "todos", JE.list encodeTodo model.todos )
        , ( "id_berikutnya", JE.int model.idSelanjutnya )
        , ( "teks_input", JE.string model.teksInput )
        , ( "prioritas_input", JE.float model.prioritasInput )
        , ( "skala_input", JE.string model.skalaInput )
        ]


statusDecoder : JD.Decoder Todo.Status
statusDecoder =
    JD.map Todo.readStatus JD.string


skalaDecoder : JD.Decoder Todo.Skala
skalaDecoder =
    JD.map Todo.readSkala JD.string


todoDecoder : JD.Decoder Todo
todoDecoder =
    JD.map5 Todo
        (JD.field "id" JD.int)
        (JD.field "deskripsi" JD.string)
        (JD.field "status" statusDecoder)
        (JD.field "prioritas" JD.float)
        (JD.field "skala" skalaDecoder)


decoder : JD.Decoder Model
decoder =
    JD.map5 Model
        (JD.field "todos" <| JD.list todoDecoder)
        (JD.field "id_berikutnya" JD.int)
        (JD.field "teks_input" JD.string)
        (JD.field "prioritas_input" JD.float)
        (JD.field "skala_input" JD.string)


unduhTodos : List Todo -> Cmd msg
unduhTodos =
    Todo.showTodos >> Download.string "todo.txt" "text/plain"


main : Program JE.Value Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = updateDanStorage
        , subscriptions = \_ -> Sub.none
        }
