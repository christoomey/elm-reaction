module Main exposing (main)

import Browser
import Html exposing (Html, button, div, span, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import List.Extra as List


type Msg
    = Click Position
    | Tick


type alias Position =
    ( Int, Int )


type alias Flags =
    {}


type InteractorKind
    = One
    | Two
    | Three
    | Four



-- type Interaction
--     = Clicked Interactor
--     | Collide Projectile Interactor


type Positioned a
    = Positioned Int Int a


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
    { interactors : List (Positioned Interactor)
    , projectiles : List (Positioned Projectile)
    }


type alias Model =
    { board : Board
    , dimension : Int
    }


view : Model -> Html Msg
view model =
    div []
        [ div [ class "board" ] <| renderableBoard model
        , button [ onClick Tick ] [ text "Tick" ]
        ]


renderableBoard : Model -> List (Html Msg)
renderableBoard model =
    let
        range =
            List.range 0 <| model.dimension - 1
    in
    List.andThen (\x -> List.map (\y -> viewCell model.board ( x, y )) range) range


viewCell : Board -> Position -> Html Msg
viewCell board ( x, y ) =
    let
        mInteractor =
            List.find (elementIsAtPosition ( x, y )) board.interactors

        projectiles =
            List.filter (elementIsAtPosition ( x, y )) board.projectiles

        renderedProjectiles =
            List.map (getPositionedElement >> viewProjectile) projectiles
    in
    case mInteractor of
        Nothing ->
            div [ class "cell" ] renderedProjectiles

        Just (Positioned _ _ interactor) ->
            div [ onClick (Click ( x, y )), class <| "cell " ++ interactorClass interactor.kind ] <| renderedProjectiles


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


initialBoard : Board
initialBoard =
    { interactors =
        [ Positioned 0 0 { id = 1, kind = One }
        , Positioned 2 6 { id = 1, kind = Four }
        , Positioned 7 7 { id = 2, kind = Three }
        ]
    , projectiles =
        [ Positioned 2 2 (Projectile Right)
        , Positioned 1 4 (Projectile Left)
        , Positioned 4 2 (Projectile Down)
        , Positioned 3 3 (Projectile Up)
        ]
    }


init : Flags -> ( Model, Cmd Msg )
init _ =
    ( { board = initialBoard, dimension = 8 }
    , Cmd.none
    )


elementIsAtPosition : Position -> Positioned a -> Bool
elementIsAtPosition ( x, y ) (Positioned px py _) =
    px == x && py == y


getPositionedElement : Positioned a -> a
getPositionedElement (Positioned _ _ a) =
    a


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Click ( x, y ) ->
            let
                board =
                    model.board

                mInteractor =
                    List.find (elementIsAtPosition ( x, y )) board.interactors

                ( updatedMaybeInteractor, projectiles ) =
                    case mInteractor of
                        Nothing ->
                            ( Nothing, [] )

                        Just interactor ->
                            interact interactor

                updatedInteractors =
                    case updatedMaybeInteractor of
                        Nothing ->
                            List.filter (not << elementIsAtPosition ( x, y )) board.interactors

                        Just interactor ->
                            List.updateIf (elementIsAtPosition ( x, y )) (always interactor) board.interactors

                updatedProjectiles =
                    List.append projectiles board.projectiles

                newBoard : Board
                newBoard =
                    { board | interactors = updatedInteractors, projectiles = updatedProjectiles }
            in
            ( { model | board = newBoard }, Cmd.none )

        Tick ->
            ( { model | board = updateBoard model }, Cmd.none )


interact : Positioned Interactor -> ( Maybe (Positioned Interactor), List (Positioned Projectile) )
interact (Positioned x y { id, kind }) =
    let
        inPlace =
            Positioned x y
    in
    case kind of
        One ->
            ( Just (inPlace (Interactor id Two)), [] )

        Two ->
            ( Just (inPlace (Interactor id Three)), [] )

        Three ->
            ( Just (inPlace (Interactor id Four)), [] )

        Four ->
            ( Nothing, [ inPlace <| Projectile Up, inPlace <| Projectile Down, inPlace <| Projectile Left, inPlace <| Projectile Right ] )


updateBoard : Model -> Board
updateBoard { dimension, board } =
    let
        updatedProjectiles : List (Positioned Projectile)
        updatedProjectiles =
            List.filterMap updateProjectile board.projectiles

        range : List Int
        range =
            List.range 0 <| dimension - 1

        grid : List Position
        grid =
            List.andThen (\x -> List.map (Tuple.pair x) range) range

        updatedCells : List ( Maybe (Positioned Interactor), List (Positioned Projectile) )
        updatedCells =
            List.map (updateCell { board | projectiles = updatedProjectiles }) grid

        updatedInteractors : List (Positioned Interactor)
        updatedInteractors =
            List.filterMap Tuple.first updatedCells

        positionedProjectiles : List (Positioned Projectile)
        positionedProjectiles =
            List.concatMap Tuple.second updatedCells
    in
    { board | projectiles = positionedProjectiles, interactors = updatedInteractors }


updateCell : Board -> Position -> ( Maybe (Positioned Interactor), List (Positioned Projectile) )
updateCell board ( x, y ) =
    let
        mInteractor =
            List.find (elementIsAtPosition ( x, y )) board.interactors

        projectiles =
            List.filter (elementIsAtPosition ( x, y )) board.projectiles
    in
    List.foldl collide ( mInteractor, [] ) projectiles


collide : Positioned Projectile -> ( Maybe (Positioned Interactor), List (Positioned Projectile) ) -> ( Maybe (Positioned Interactor), List (Positioned Projectile) )
collide projectile ( mInteractor, projectiles ) =
    case mInteractor of
        Nothing ->
            ( Nothing, projectile :: projectiles )

        Just interactor ->
            let
                ( newMInteractor, newProjectiles ) =
                    interact interactor
            in
            ( newMInteractor, List.append newProjectiles projectiles )


updateProjectile : Positioned Projectile -> Maybe (Positioned Projectile)
updateProjectile (Positioned x y projectile) =
    let
        ( newX, newY ) =
            case projectile of
                Projectile Up ->
                    ( x - 1, y )

                Projectile Left ->
                    ( x, y - 1 )

                Projectile Down ->
                    ( x + 1, y )

                Projectile Right ->
                    ( x, y + 1 )
    in
    if isOffTheScreen newX newY then
        Nothing

    else
        Just (Positioned newX newY projectile)


isOffTheScreen : Int -> Int -> Bool
isOffTheScreen x y =
    x < 0 || x > 7 || y < 0 || y > 7


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
