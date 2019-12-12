module Main exposing (main)

import Browser
import Html exposing (Html, div, text)
import Html.Attributes exposing (class)


type Msg
    = Noop


type alias Flags =
    {}


type Interactor
    = One
    | Two
    | Three
    | Four
    | Arrow Direction


type Direction
    = Up
    | Down
    | Left
    | Right


type Projectile
    = Projectile Direction


type alias Board =
    List (List (Maybe Interactor))


type alias Model =
    { board : Board
    , projectiles : List Projectile
    }


view : Model -> Html Msg
view { board } =
    div [ class "board" ] <| List.concatMap viewRow board


viewRow : List (Maybe Interactor) -> List (Html Msg)
viewRow row =
    List.map viewCell row


viewCell : Maybe Interactor -> Html Msg
viewCell mInteractor =
    case mInteractor of
        Nothing ->
            div [ class "cell" ] [ text "empty" ]

        Just _ ->
            div [ class "cell" ] [ text "interactor" ]


initialBoard : Board
initialBoard =
    [ [ Nothing, Nothing, Just One, Nothing ]
    , [ Nothing, Nothing, Nothing, Nothing ]
    , [ Nothing, Nothing, Just One, Nothing ]
    , [ Nothing, Nothing, Nothing, Nothing ]
    ]


init : Flags -> ( Model, Cmd Msg )
init _ =
    ( { board = initialBoard, projectiles = [] }
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
