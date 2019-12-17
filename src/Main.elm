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
    = Click (Positioned Interactor)
    | Frame Float
    | Tick
    | Reset
    | TogglePause


type alias Position =
    ( Int, Int )


type alias Flags =
    {}


type InteractorKind
    = One
    | Two
    | Three
    | Four
    | Reverse
    | Arrow Direction
    | Energizer
    | BlackHole


type Positioned a
    = Positioned Int Int Float a


type Interactor
    = Interactor InteractorKind


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
    , isPaused : Bool
    , clickCount : Int
    }


type alias CellState =
    ( Maybe (Positioned Interactor), List (Positioned Projectile) )


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
    not <| List.any (\(Positioned _ _ _ (Interactor kind)) -> isClickable kind) board.interactors


isClickable : InteractorKind -> Bool
isClickable kind =
    List.member kind [ One, Two, Three, Four ]


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
            List.find (elementIsAtPosition ( x, y )) board.interactors

        projectiles =
            List.filter (elementIsAtPosition ( x, y )) board.projectiles

        renderedProjectiles =
            List.map viewProjectile projectiles
    in
    case mInteractor of
        Nothing ->
            div [ class "cell cell-empty" ] renderedProjectiles

        Just ((Positioned _ _ _ (Interactor kind)) as positionedInteractor) ->
            if isClickable kind then
                div [ onClick (Click positionedInteractor), interactorClass kind ] renderedProjectiles

            else
                div [ interactorClass kind ] renderedProjectiles


interactorClass : InteractorKind -> Html.Attribute Msg
interactorClass kind =
    case kind of
        One ->
            class "cell interactor-one"

        Two ->
            class "cell interactor-two"

        Three ->
            class "cell interactor-three"

        Four ->
            class "cell interactor-four"

        Reverse ->
            class "cell interactor-reverse"

        Arrow dir ->
            class <| "cell interactor-arrow-" ++ directionToString dir

        Energizer ->
            class "cell interactor-energizer"

        BlackHole ->
            class "cell interactor-black-hole"


viewProjectile : Positioned Projectile -> Html Msg
viewProjectile (Positioned _ _ percentage (Projectile dir)) =
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
        [ Positioned 0 0 0 (Interactor Three)
        , Positioned 1 2 0 (Interactor (Arrow Down))
        , Positioned 6 5 0 (Interactor BlackHole)
        , Positioned 3 7 0 (Interactor Two)
        , Positioned 4 1 0 (Interactor Reverse)
        , Positioned 7 6 0 (Interactor Three)
        , Positioned 7 6 0 (Interactor (Arrow Up))
        , Positioned 6 1 0 (Interactor Two)
        , Positioned 0 7 0 (Interactor Reverse)
        , Positioned 2 4 0 (Interactor Four)
        , Positioned 4 7 0 (Interactor One)
        , Positioned 3 6 0 (Interactor Energizer)
        , Positioned 0 4 0 (Interactor Four)
        , Positioned 2 6 0 (Interactor Four)
        , Positioned 3 2 0 (Interactor (Arrow Right))
        , Positioned 5 4 0 (Interactor (Arrow Right))
        , Positioned 5 7 0 (Interactor (Arrow Up))
        , Positioned 6 3 0 (Interactor Three)
        , Positioned 1 4 0 (Interactor Three)
        , Positioned 2 5 0 (Interactor Three)
        , Positioned 7 4 0 (Interactor (Arrow Right))
        , Positioned 5 7 0 (Interactor (Arrow Up))
        , Positioned 4 4 0 (Interactor Four)
        , Positioned 6 0 0 (Interactor (Arrow Right))
        , Positioned 7 2 0 (Interactor Four)
        , Positioned 2 0 0 (Interactor (Arrow Down))
        ]
    , projectiles = []
    }


initialModel : Model
initialModel =
    { board = initialBoard, dimension = 8, isPaused = False, clickCount = 0 }


init : Flags -> ( Model, Cmd Msg )
init _ =
    ( initialModel, Cmd.none )


elementIsAtPosition : Position -> Positioned a -> Bool
elementIsAtPosition ( x, y ) (Positioned px py _ _) =
    px == x && py == y


handleClick : Positioned Interactor -> Model -> ( Model, Cmd Msg )
handleClick ((Positioned x y _ _) as positionedInteractor) ({ board } as model) =
    let
        ( updatedMaybeInteractor, projectiles ) =
            interact positionedInteractor Nothing

        updatedInteractors =
            case updatedMaybeInteractor of
                Nothing ->
                    List.filter (not << elementIsAtPosition ( x, y )) board.interactors

                Just newInteractor ->
                    List.updateIf (elementIsAtPosition ( x, y )) (always newInteractor) board.interactors

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


shouldInteract : Maybe (Positioned Projectile) -> Bool
shouldInteract maybeProjectile =
    case maybeProjectile of
        Nothing ->
            True

        Just (Positioned _ _ percentage _) ->
            percentage < 0.01 && percentage > -0.01


interact : Positioned Interactor -> Maybe (Positioned Projectile) -> CellState
interact ((Positioned x y _ (Interactor kind)) as positionedInteractor) maybeProjectile =
    let
        inPlace =
            Positioned x y 0
    in
    if shouldInteract maybeProjectile then
        case kind of
            One ->
                ( Just (inPlace (Interactor Two)), [] )

            Two ->
                ( Just (inPlace (Interactor Three)), [] )

            Three ->
                ( Just (inPlace (Interactor Four)), [] )

            Four ->
                ( Nothing, List.map (Projectile >> inPlace) [ Up, Down, Left, Right ] )

            Reverse ->
                case maybeProjectile of
                    Nothing ->
                        ( Just positionedInteractor, [] )

                    Just (Positioned _ _ _ (Projectile dir)) ->
                        ( Just positionedInteractor, [ inPlace (Projectile <| reversedDirection dir) ] )

            Arrow dir ->
                case maybeProjectile of
                    Nothing ->
                        ( Just positionedInteractor, [] )

                    Just _ ->
                        ( Just positionedInteractor, [ inPlace (Projectile dir) ] )

            Energizer ->
                case maybeProjectile of
                    Nothing ->
                        ( Just positionedInteractor, [] )

                    Just (Positioned _ _ _ (Projectile dir)) ->
                        ( Just positionedInteractor, List.map (Projectile >> inPlace) (withLateralDirections dir) )

            BlackHole ->
                ( Just positionedInteractor, [] )

    else
        case maybeProjectile of
            Nothing ->
                ( Just positionedInteractor, [] )

            Just positionedProjectile ->
                ( Just positionedInteractor, [ positionedProjectile ] )


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


getCell : Board -> ( Int, Int ) -> CellState
getCell board ( x, y ) =
    ( List.find (elementIsAtPosition ( x, y )) board.interactors
    , List.filter (elementIsAtPosition ( x, y )) board.projectiles
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


collide : Positioned Projectile -> CellState -> CellState
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


updateProjectile : Positioned Projectile -> Maybe (Positioned Projectile)
updateProjectile (Positioned x y percentage projectile) =
    let
        ( newX, newY, newPercentage ) =
            case projectile of
                Projectile Up ->
                    let
                        tempNewPercentage =
                            percentage - percentagePerTick
                    in
                    if tempNewPercentage < -0.5 then
                        ( x - 1, y, 0.5 )

                    else
                        ( x, y, tempNewPercentage )

                Projectile Left ->
                    let
                        tempNewPercentage =
                            percentage - percentagePerTick
                    in
                    if tempNewPercentage < -0.5 then
                        ( x, y - 1, 0.5 )

                    else
                        ( x, y, tempNewPercentage )

                Projectile Down ->
                    let
                        tempNewPercentage =
                            percentage + percentagePerTick
                    in
                    if tempNewPercentage > 0.5 then
                        ( x + 1, y, -0.5 )

                    else
                        ( x, y, tempNewPercentage )

                Projectile Right ->
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
        Just (Positioned newX newY newPercentage projectile)


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
