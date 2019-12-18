module Main exposing (main)

import Browser
import Browser.Events exposing (onAnimationFrameDelta)
import Html exposing (Html, button, div, h3, span, text)
import Html.Attributes exposing (class, style)
import Html.Events exposing (onClick)
import List.Extra as List


percentagePerTick : Float
percentagePerTick =
    0.1


type Msg
    = Click Interactor
    | Frame Float
    | Tick
    | Reset
    | TogglePause


type alias Position =
    ( Int, Int )


type alias Flags =
    {}


type CoreSize
    = One
    | Two
    | Three
    | Four


type Interactor
    = Core CoreSize Position
    | Reverse Position
    | Arrow Direction Position
    | Energizer Position
    | BlackHole Position


type Direction
    = Up
    | Down
    | Left
    | Right


type Projectile
    = Projectile Direction Position Float


type alias Board =
    { interactors : List Interactor
    , projectiles : List Projectile
    }


type alias Model =
    { board : Board
    , dimension : Int
    , isPaused : Bool
    , clickCount : Int
    }


type alias CellState =
    ( Maybe Interactor, List Projectile )


view : Model -> Html Msg
view model =
    div []
        [ controls model
        , div [ class "board" ] <| renderableBoard model
        ]


controls : Model -> Html Msg
controls model =
    div [ class "controls" ]
        [ h3 []
            [ text <|
                if isComplete model.board then
                    "COMPLETE"

                else
                    "Playing"
            ]
        , text <| "Click count: " ++ String.fromInt model.clickCount
        , div [ class "button-group" ]
            [ pauseButton model.isPaused
            , button [ onClick Reset ] [ text "Reset" ]
            , tickButton
            ]
        ]


isComplete : Board -> Bool
isComplete board =
    not <| List.any isClickable board.interactors


isClickable : Interactor -> Bool
isClickable interactor =
    case interactor of
        Core _ _ ->
            True

        _ ->
            False


tickButton : Html Msg
tickButton =
    button [ onClick Tick ] [ text "Tick" ]


pauseButton : Bool -> Html Msg
pauseButton isPaused =
    button [ onClick TogglePause ]
        [ text <|
            if isPaused then
                "Start"

            else
                "Pause"
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
            List.find (interactorIsAtPosition ( x, y )) board.interactors

        projectiles =
            List.filter (projectileIsAtPosition ( x, y )) board.projectiles

        renderedProjectiles =
            List.map viewProjectile projectiles
    in
    case mInteractor of
        Nothing ->
            div [ class "cell cell-empty" ] renderedProjectiles

        Just interactor ->
            if isClickable interactor then
                div [ onClick (Click interactor), interactorClass interactor ] renderedProjectiles

            else
                div [ interactorClass interactor ] renderedProjectiles


coreSizeToString : CoreSize -> String
coreSizeToString coreSize =
    case coreSize of
        One ->
            "one"

        Two ->
            "two"

        Three ->
            "three"

        Four ->
            "Four"


interactorClass : Interactor -> Html.Attribute Msg
interactorClass interactor =
    case interactor of
        Core size _ ->
            class <| "cell interactor-" ++ coreSizeToString size

        Reverse _ ->
            class "cell interactor-reverse"

        Arrow dir _ ->
            class <| "cell interactor-arrow-" ++ directionToString dir

        Energizer _ ->
            class "cell interactor-energizer"

        BlackHole _ ->
            class "cell interactor-black-hole"


viewProjectile : Projectile -> Html Msg
viewProjectile (Projectile dir _ percentage) =
    let
        positioning =
            case dir of
                Right ->
                    style "left" <| String.fromFloat (percentage * 100) ++ "%"

                Down ->
                    style "top" <| String.fromFloat (percentage * 100) ++ "%"

                Left ->
                    style "left" <| String.fromFloat (percentage * 100) ++ "%"

                Up ->
                    style "top" <| String.fromFloat (percentage * 100) ++ "%"
    in
    span [ class <| "projectile-" ++ directionToString dir, positioning ] []


directionToString : Direction -> String
directionToString direction =
    case direction of
        Up ->
            "up"

        Down ->
            "down"

        Left ->
            "left"

        Right ->
            "right"


initialBoard : Board
initialBoard =
    { interactors =
        [ Core Three ( 0, 0 )
        , Arrow Down ( 1, 2 )
        , BlackHole ( 6, 5 )
        , Core Two ( 3, 7 )
        , Reverse ( 4, 1 )
        , Core Three ( 7, 6 )
        , Arrow Up ( 7, 6 )
        , Core Two ( 6, 1 )
        , Reverse ( 0, 7 )
        , Core Four ( 2, 4 )
        , Core One ( 4, 7 )
        , Energizer ( 3, 6 )
        , Core Four ( 0, 4 )
        , Core Four ( 2, 6 )
        , Arrow Right ( 3, 2 )
        , Arrow Right ( 5, 4 )
        , Arrow Up ( 5, 7 )
        , Core Three ( 6, 3 )
        , Core Three ( 1, 4 )
        , Core Three ( 2, 5 )
        , Arrow Right ( 7, 4 )
        , Arrow Up ( 5, 7 )
        , Core Four ( 4, 4 )
        , Arrow Right ( 6, 0 )
        , Core Four ( 7, 2 )
        , Arrow Down ( 2, 0 )
        ]
    , projectiles = []
    }


initialModel : Model
initialModel =
    { board = initialBoard, dimension = 8, isPaused = False, clickCount = 0 }


init : Flags -> ( Model, Cmd Msg )
init _ =
    ( initialModel, Cmd.none )


projectileIsAtPosition : Position -> Projectile -> Bool
projectileIsAtPosition ( x, y ) (Projectile _ ( px, py ) _) =
    x == px && y == py


interactorIsAtPosition : Position -> Interactor -> Bool
interactorIsAtPosition ( x, y ) interactor =
    let
        ( ix, iy ) =
            getInteractorPosition interactor
    in
    x == ix && y == iy


getInteractorPosition : Interactor -> Position
getInteractorPosition interactor =
    case interactor of
        Core _ pos ->
            pos

        Arrow _ pos ->
            pos

        BlackHole pos ->
            pos

        Reverse pos ->
            pos

        Energizer pos ->
            pos


handleClick : Interactor -> Model -> ( Model, Cmd Msg )
handleClick interactor ({ board } as model) =
    let
        ( updatedMaybeInteractor, projectiles ) =
            interact interactor Nothing

        updatedInteractors =
            case updatedMaybeInteractor of
                Nothing ->
                    List.filter (not << interactorIsAtPosition (getInteractorPosition interactor)) board.interactors

                Just newInteractor ->
                    List.updateIf (interactorIsAtPosition (getInteractorPosition interactor)) (always newInteractor) board.interactors

        updatedProjectiles =
            List.append projectiles board.projectiles

        newBoard =
            { board | interactors = updatedInteractors, projectiles = updatedProjectiles }
    in
    ( { model | board = newBoard, clickCount = model.clickCount + 1 }, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Click positionedInteractor ->
            handleClick positionedInteractor model

        Frame _ ->
            ( { model | board = updateBoard model }, Cmd.none )

        Tick ->
            ( { model | board = updateBoard model }, Cmd.none )

        Reset ->
            ( { initialModel | isPaused = model.isPaused }, Cmd.none )

        TogglePause ->
            ( { model | isPaused = not model.isPaused }, Cmd.none )


reversedDirection : Direction -> Direction
reversedDirection direction =
    case direction of
        Up ->
            Down

        Down ->
            Up

        Left ->
            Right

        Right ->
            Left


shouldInteract : Maybe Projectile -> Bool
shouldInteract maybeProjectile =
    case maybeProjectile of
        Nothing ->
            True

        Just (Projectile _ _ percentage) ->
            percentage < 0.01 && percentage > -0.01


interact : Interactor -> Maybe Projectile -> CellState
interact interactor maybeProjectile =
    if shouldInteract maybeProjectile then
        case interactor of
            Core One position ->
                ( Just (Core Two position), [] )

            Core Two position ->
                ( Just (Core Three position), [] )

            Core Three position ->
                ( Just (Core Four position), [] )

            Core Four position ->
                ( Nothing, List.map (\dir -> Projectile dir position 0) [ Up, Down, Left, Right ] )

            Reverse position ->
                case maybeProjectile of
                    Nothing ->
                        ( Just interactor, [] )

                    Just (Projectile dir _ _) ->
                        ( Just interactor, [ Projectile (reversedDirection dir) position 0 ] )

            Arrow dir position ->
                case maybeProjectile of
                    Nothing ->
                        ( Just interactor, [] )

                    Just _ ->
                        ( Just interactor, [ Projectile dir position 0 ] )

            Energizer position ->
                case maybeProjectile of
                    Nothing ->
                        ( Just interactor, [] )

                    Just (Projectile dir _ _) ->
                        ( Just interactor, List.map (\newDir -> Projectile newDir position 0) (withLateralDirections dir) )

            BlackHole _ ->
                ( Just interactor, [] )

    else
        case maybeProjectile of
            Nothing ->
                ( Just interactor, [] )

            Just projectile ->
                ( Just interactor, [ projectile ] )


withLateralDirections : Direction -> List Direction
withLateralDirections direction =
    case direction of
        Up ->
            [ Up, Left, Right ]

        Down ->
            [ Down, Left, Right ]

        Right ->
            [ Up, Down, Right ]

        Left ->
            [ Up, Left, Down ]


grid : Int -> List Position
grid dimension =
    let
        range =
            List.range 0 <| dimension - 1
    in
    List.andThen (\x -> List.map (Tuple.pair x) range) range


getCell : Board -> Position -> CellState
getCell board ( x, y ) =
    ( List.find (interactorIsAtPosition ( x, y )) board.interactors
    , List.filter (projectileIsAtPosition ( x, y )) board.projectiles
    )


updateBoard : Model -> Board
updateBoard { board, dimension } =
    let
        movedProjectiles =
            List.filterMap updateProjectile board.projectiles

        cells =
            List.map (getCell { board | projectiles = movedProjectiles }) (grid dimension)

        updatedCells =
            List.map updateCell cells

        updatedInteractors =
            List.filterMap Tuple.first updatedCells

        positionedProjectiles =
            List.concatMap Tuple.second updatedCells
    in
    { board | projectiles = positionedProjectiles, interactors = updatedInteractors }


updateCell : CellState -> CellState
updateCell ( mInteractor, projectiles ) =
    case mInteractor of
        Just positionedInteractor ->
            List.foldl collide ( Just positionedInteractor, [] ) projectiles

        Nothing ->
            ( mInteractor, projectiles )


collide : Projectile -> CellState -> CellState
collide projectile ( mInteractor, projectiles ) =
    case mInteractor of
        Nothing ->
            ( Nothing, projectile :: projectiles )

        Just interactor ->
            let
                ( newMInteractor, newProjectiles ) =
                    interact interactor (Just projectile)
            in
            ( newMInteractor, List.append newProjectiles projectiles )


updateProjectile : Projectile -> Maybe Projectile
updateProjectile (Projectile direction ( x, y ) percentage) =
    let
        ( newX, newY, newPercentage ) =
            case direction of
                Up ->
                    let
                        tempNewPercentage =
                            percentage - percentagePerTick
                    in
                    if tempNewPercentage < -0.5 then
                        ( x - 1, y, 0.5 )

                    else
                        ( x, y, tempNewPercentage )

                Left ->
                    let
                        tempNewPercentage =
                            percentage - percentagePerTick
                    in
                    if tempNewPercentage < -0.5 then
                        ( x, y - 1, 0.5 )

                    else
                        ( x, y, tempNewPercentage )

                Down ->
                    let
                        tempNewPercentage =
                            percentage + percentagePerTick
                    in
                    if tempNewPercentage > 0.5 then
                        ( x + 1, y, -0.5 )

                    else
                        ( x, y, tempNewPercentage )

                Right ->
                    let
                        tempNewPercentage =
                            percentage + percentagePerTick
                    in
                    if tempNewPercentage > 0.5 then
                        ( x, y + 1, -0.5 )

                    else
                        ( x, y, tempNewPercentage )
    in
    if isOffTheScreen newX newY then
        Nothing

    else
        Just (Projectile direction ( newX, newY ) newPercentage)


isOffTheScreen : Int -> Int -> Bool
isOffTheScreen x y =
    x < 0 || x > 7 || y < 0 || y > 7


subscriptions : Model -> Sub Msg
subscriptions { isPaused } =
    if isPaused then
        Sub.none

    else
        onAnimationFrameDelta Frame


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
