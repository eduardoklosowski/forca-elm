port module Forca exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


main =
    program
        { init = ( initialState, Cmd.none )
        , update = update
        , subscriptions = \_ -> setPalavraLimpa SetPalavraLimpa
        , view = view
        }



-- Aux


port limparPalavra : String -> Cmd msg


port setPalavraLimpa : (String -> msg) -> Sub msg



-- Model


type alias Model =
    { escondePalavra : Bool
    , status : Status
    , palavra : String
    , palavraLimpa : List Char
    , letra : String
    , letras : List Char
    , vidas : Int
    }


type Status
    = LendoPalavra
    | Iniciando
    | Jogando
    | Finalizado


initialState : Model
initialState =
    { escondePalavra = True
    , status = LendoPalavra
    , palavra = ""
    , palavraLimpa = []
    , letra = ""
    , letras = []
    , vidas = 0
    }



-- Msg


type Msg
    = SetEscondePalavra Bool
    | UpdatePalavra String
    | SetPalavraLimpa String
    | StartForca
    | EndForca
    | AddLetra String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetEscondePalavra escondePalavra ->
            ( { model | escondePalavra = escondePalavra }, Cmd.none )

        UpdatePalavra palavra ->
            ( { model | palavra = palavra }, Cmd.none )

        SetPalavraLimpa palavraLimpa ->
            ( { model
                | status = Jogando
                , palavraLimpa = String.toList palavraLimpa
              }
            , Cmd.none
            )

        StartForca ->
            let
                palavra =
                    String.trim model.palavra
            in
                if String.isEmpty palavra then
                    ( model, Cmd.none )
                else
                    ( { model
                        | status = Iniciando
                        , palavra = palavra
                        , letras = []
                        , vidas = 6
                      }
                    , limparPalavra palavra
                    )

        EndForca ->
            ( { model
                | status = LendoPalavra
                , palavra = ""
              }
            , Cmd.none
            )

        AddLetra letraInput ->
            let
                l =
                    List.head (letraInput |> String.trim |> String.toLower |> String.toList)
            in
                case l of
                    Just letra ->
                        if List.any (\a -> a == letra) model.letras then
                            ( { model | letra = "" }, Cmd.none )
                        else
                            let
                                letras =
                                    model.letras ++ [ letra ]

                                inPalavra =
                                    List.any (\a -> a == letra) model.palavraLimpa

                                vidas =
                                    if inPalavra then
                                        model.vidas
                                    else
                                        model.vidas - 1

                                faltando =
                                    List.length <| List.filter (\a -> a /= ' ' && not (List.any (\b -> b == a) letras)) model.palavraLimpa

                                status =
                                    if vidas == 0 || faltando == 0 then
                                        Finalizado
                                    else
                                        model.status
                            in
                                ( { model
                                    | status = status
                                    , letra = ""
                                    , letras = letras
                                    , vidas = vidas
                                  }
                                , Cmd.none
                                )

                    Nothing ->
                        ( { model | letra = "" }, Cmd.none )



-- View


view : Model -> Html Msg
view model =
    let
        conteudo =
            case model.status of
                LendoPalavra ->
                    [ palavraInputView model ]

                Iniciando ->
                    [ text "Iniciando Jogo..." ]

                Jogando ->
                    [ forcaView model
                    , palavraView model
                    , letraInputView model
                    , letrasView model
                    ]

                Finalizado ->
                    [ forcaView model
                    , palavraView model
                    , letrasView model
                    , button [ onClick EndForca ] [ text "Jogar Novamente" ]
                    ]
    in
        div [ class "app" ] conteudo


palavraInputView : Model -> Html Msg
palavraInputView { escondePalavra, palavra } =
    let
        inputType =
            if escondePalavra then
                "password"
            else
                "text"
    in
        div
            [ class "form" ]
            [ input
                [ placeholder "Digite uma palavra"
                , type_ inputType
                , value palavra
                , onInput UpdatePalavra
                ]
                []
            , button [ onClick StartForca ] [ text "Iniciar" ]
            , label
                []
                [ input
                    [ type_ "checkbox"
                    , checked escondePalavra
                    , onCheck SetEscondePalavra
                    ]
                    []
                , text "Esconder Palavra?"
                ]
            ]


forcaView : Model -> Html Msg
forcaView { vidas } =
    div
        [ class "forca" ]
        [ text "Vida: ", text (vidas |> toString) ]


palavraView : Model -> Html Msg
palavraView { status, palavra, palavraLimpa, letras } =
    let
        aux i l =
            if l == ' ' then
                li [ class "espaco" ] [ text " " ]
            else
                let
                    acertada =
                        List.any (\a -> a == l) letras

                    className =
                        if not acertada && status == Finalizado then
                            "faltou"
                        else
                            ""

                    letra =
                        if acertada || status == Finalizado then
                            String.slice i (i + 1) palavra
                        else
                            "_"
                in
                    li [ class className ] [ text letra ]
    in
        div
            [ class "palavra" ]
            [ ul [] (List.indexedMap aux palavraLimpa) ]


letraInputView : Model -> Html Msg
letraInputView { letra } =
    div
        [ class "form" ]
        [ input
            [ type_ "text"
            , placeholder "Letra"
            , maxlength 1
            , value letra
            , onInput AddLetra
            ]
            []
        ]


letrasView : Model -> Html Msg
letrasView { palavraLimpa, letras } =
    let
        aux l =
            let
                className =
                    if List.any (\a -> a == l) palavraLimpa then
                        "certa"
                    else
                        "errada"
            in
                li [ class className ] [ text (String.fromChar l) ]
    in
        div
            [ class "letras" ]
            [ div [ class "title" ] [ text "Letras" ]
            , ul [] (List.map aux letras)
            ]
