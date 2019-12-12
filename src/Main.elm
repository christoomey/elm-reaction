module Main exposing (main)

import Browser
import Html exposing (Html, div, img, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)


type Msg
    = Click Interactor


type alias Flags =
    {}


type InteractorKind
    = One
    | Two
    | Three
    | Four
    | Boom
    | Arrow Direction


type alias Interactor =
    { id : Int
    , kind : InteractorKind
    }


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
            div [ class "cell" ] []

        Just ({ kind } as interactor) ->
            div [ onClick (Click interactor), class <| "cell " ++ interactorClass kind ] []


interactorClass : InteractorKind -> String
interactorClass kind =
    case kind of
        One ->
            "interactor-one"

        Two ->
            "interactor-two"

        Three ->
            "interactor-three"

        Four ->
            "interactor-four"

        _ ->
            ""


initialBoard : Board
initialBoard =
    [ [ Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Just { id = 1, kind = One }, Nothing ]
    , [ Nothing, Nothing, Nothing, Nothing, Just { id = 2, kind = Three }, Nothing, Nothing, Nothing ]
    , [ Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Just { id = 3, kind = Two }, Nothing ]
    , [ Nothing, Nothing, Nothing, Nothing, Nothing, Just { id = 4, kind = Four }, Nothing, Nothing ]
    , [ Nothing, Nothing, Nothing, Nothing, Just { id = 5, kind = Three }, Nothing, Nothing, Nothing ]
    , [ Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Just { id = 6, kind = Two }, Nothing ]
    , [ Nothing, Just { id = 7, kind = Three }, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing ]
    , [ Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Just { id = 8, kind = Two }, Nothing ]
    ]


init : Flags -> ( Model, Cmd Msg )
init _ =
    ( { board = initialBoard, projectiles = [] }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Click { id, kind } ->
            let
                _ =
                    Debug.log "clicked a thing" ( id, kind )
            in
            ( model, Cmd.none )


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
