module Main exposing (main)

import Browser
import Html exposing (Html, button, div, img, span, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import List.Extra as List


type Msg
    = Click Interactor
    | Tick


type alias Flags =
    {}


type InteractorKind
    = One
    | Two
    | Three
    | Four
    | Boom
    | Arrow Direction


type alias Cell =
    ( Maybe Interactor, List Projectile )


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
    List (List Cell)


type alias Model =
    { board : Board
    , projectiles : List Projectile
    }


view : Model -> Html Msg
view { board } =
    div []
        [ div [ class "board" ] <| List.concatMap viewRow board
        , button [ onClick Tick ] [ text "Tick" ]
        ]


viewRow : List Cell -> List (Html Msg)
viewRow row =
    List.map viewCell row


viewCell : Cell -> Html Msg
viewCell ( mInteractor, projectiles ) =
    case mInteractor of
        Nothing ->
            div [ class "cell" ] <| List.map viewProjectile projectiles

        Just ({ kind } as interactor) ->
            div [ onClick (Click interactor), class <| "cell " ++ interactorClass kind ] <| List.map viewProjectile projectiles


viewProjectile : Projectile -> Html Msg
viewProjectile projectile =
    span [ class <| "projectile-" ++ directionToString projectile ] []


directionToString : Projectile -> String
directionToString projectile =
    case projectile of
        Projectile Up ->
            "up"

        Projectile Down ->
            "down"

        Projectile Left ->
            "left"

        Projectile Right ->
            "right"


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
    [ [ ( Nothing, [ Projectile Down, Projectile Right ] ), ( Nothing, [] ), ( Nothing, [] ), ( Nothing, [] ), ( Nothing, [] ), ( Nothing, [] ), ( Just { id = 1, kind = One }, [] ), ( Nothing, [] ) ]
    , [ ( Nothing, [] ), ( Nothing, [] ), ( Nothing, [] ), ( Nothing, [] ), ( Just { id = 2, kind = Three }, [] ), ( Nothing, [] ), ( Nothing, [] ), ( Nothing, [] ) ]
    , [ ( Nothing, [ Projectile Up ] ), ( Nothing, [] ), ( Nothing, [] ), ( Nothing, [] ), ( Nothing, [] ), ( Nothing, [] ), ( Just { id = 3, kind = Two }, [] ), ( Nothing, [] ) ]
    , [ ( Nothing, [] ), ( Nothing, [] ), ( Nothing, [] ), ( Nothing, [] ), ( Nothing, [] ), ( Just { id = 4, kind = Four }, [] ), ( Nothing, [] ), ( Nothing, [] ) ]
    , [ ( Nothing, [] ), ( Nothing, [] ), ( Nothing, [] ), ( Nothing, [] ), ( Just { id = 5, kind = Three }, [] ), ( Nothing, [] ), ( Nothing, [] ), ( Nothing, [] ) ]
    , [ ( Nothing, [ Projectile Right ] ), ( Nothing, [] ), ( Nothing, [] ), ( Nothing, [] ), ( Nothing, [] ), ( Nothing, [] ), ( Just { id = 6, kind = Two }, [] ), ( Nothing, [] ) ]
    , [ ( Nothing, [] ), ( Just { id = 7, kind = Three }, [] ), ( Nothing, [] ), ( Nothing, [] ), ( Nothing, [] ), ( Nothing, [] ), ( Nothing, [] ), ( Nothing, [] ) ]
    , [ ( Nothing, [ Projectile Left ] ), ( Nothing, [] ), ( Nothing, [] ), ( Nothing, [] ), ( Nothing, [] ), ( Nothing, [] ), ( Just { id = 8, kind = Two }, [] ), ( Nothing, [] ) ]
    ]


init : Flags -> ( Model, Cmd Msg )
init _ =
    ( { board = initialBoard, projectiles = [] }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Click ({ id } as interactor) ->
            let
                newBoard : Board
                newBoard =
                    List.map (updateInRow id (interact interactor)) model.board
            in
            ( { model | board = newBoard }, Cmd.none )

        Tick ->
            let
                newBoard =
                    generateNewBoard model.board
            in
            ( { model | board = newBoard }, Cmd.none )


interact : Interactor -> Interactor
interact ({ id, kind } as interactor) =
    case kind of
        One ->
            Interactor id Two

        Two ->
            Interactor id Three

        Three ->
            Interactor id Four

        _ ->
            interactor


generateNewBoard : Board -> Board
generateNewBoard board =
    let
        indexedRows =
            List.indexedMap Tuple.pair board

        indexedCells =
            List.concatMap (\( i, row ) -> List.indexedMap (fn i) row) indexedRows

        indexedProjectiles =
            List.filter (\( _, _, ps ) -> not <| List.isEmpty ps) indexedCells

        flattenedIndexedProjectiles =
            List.concatMap (\( x, y, ps ) -> List.map (\p -> ( x, y, p )) ps) indexedProjectiles

        updatedProjectiles =
            List.filterMap identity (List.map updateProjectile flattenedIndexedProjectiles)

        cleanBoard =
            boardWithoutProjectiles board

        updatedBoard =
            List.foldl buildNewBoard cleanBoard updatedProjectiles
    in
    updatedBoard


type alias PositionedProjectile =
    ( Int, Int, Projectile )


buildNewBoard : PositionedProjectile -> Board -> Board
buildNewBoard ( x, y, projectile ) board =
    List.updateAt y (List.updateAt x (\cell -> updateCell cell projectile)) board


updateCell : Cell -> Projectile -> Cell
updateCell ( mInteractor, projectiles ) projectile =
    case mInteractor of
        Nothing ->
            ( mInteractor, projectile :: projectiles )

        Just interactor ->
            ( Just (interact interactor), [] )


boardWithoutProjectiles : Board -> Board
boardWithoutProjectiles board =
    let
        cleanRow =
            List.map cleanCell

        cleanCell ( mi, _ ) =
            ( mi, [] )
    in
    List.map cleanRow board


updateProjectile : PositionedProjectile -> Maybe PositionedProjectile
updateProjectile ( x, y, projectile ) =
    let
        newProjectile =
            case projectile of
                Projectile Up ->
                    ( x, y - 1, projectile )

                Projectile Left ->
                    ( x - 1, y, projectile )

                Projectile Down ->
                    ( x, y + 1, projectile )

                Projectile Right ->
                    ( x + 1, y, projectile )
    in
    if isOffTheScreen newProjectile then
        Nothing

    else
        Just newProjectile


isOffTheScreen : PositionedProjectile -> Bool
isOffTheScreen ( x, y, _ ) =
    x < 0 || x > 7 || y < 0 || y > 7


fn : Int -> Int -> Cell -> ( Int, Int, List Projectile )
fn y x ( _, projectiles ) =
    ( x, y, projectiles )


updateInRow : Int -> Interactor -> List Cell -> List Cell
updateInRow id newInteractor row =
    List.updateIf (interactorMatches id) (\( _, projectiles ) -> ( Just newInteractor, projectiles )) row


interactorMatches : Int -> Cell -> Bool
interactorMatches id ( mInteractor, _ ) =
    case mInteractor of
        Nothing ->
            False

        Just interactor ->
            id == interactor.id


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
