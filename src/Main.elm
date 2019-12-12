module Main exposing (main)

import Browser
import Html exposing (Html, div, text)


type Msg
    = Noop


type alias Flags =
    {}


type alias Model =
    String


view : Model -> Html Msg
view model =
    div []
        [ text <| "Hello " ++ model ]


init : Flags -> ( Model, Cmd Msg )
init _ =
    ( "Chris"
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        _ ->
            ( model, Cmd.none )


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
