module Main exposing (main)

import Browser
import Browser.Events exposing (onAnimationFrameDelta)
import Html exposing (Html, a, button, details, div, h3, li, p, span, strong, summary, text, ul)
import Html.Attributes exposing (class, href, style)
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


type CoreDetails
    = CoreDetails CoreSize Position


type Interactor
    = Core CoreDetails
    | Reverse Position
    | Arrow Direction Position
    | Energizer Position
    | BlackHole Position
    | Portal Position Position


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
        , gameSummary
        , div [ class "board" ] <| renderableBoard model
        ]


gameSummary : Html Msg
gameSummary =
    details [ class "game-summary" ]
        [ summary [] [ text "Game notes" ]
        , p []
            [ text "This game was build as part of "
            , a [ href "https://thoughtbot.com" ] [ text "thoughtbot" ]
            , text "'s annual holiday hackathon. Visit "
            , a [ href "https://github.com/christoomey/elm-reaction" ] [ text "the repo" ]
            , text " to view the code (check out src/Main.elm for most of the action). "
            , text "The game was coded by "
            , a [ href "https://ctoomey.com" ] [ text "Chris Toomey" ]
            , text " and "
            , a [ href "https://twitter.com/gnfisher" ] [ text "Greg Fisher" ]
            , text ", with all artwork provided by the incomparable "
            , a [ href "https://twitter.com/matthewmsumner" ] [ text "Matt Sumner" ]
            , text "."
            ]
        , p []
            [ text "The game itself is fun to watch, but the functionality implemented thus far isn't too fancy. "
            , text "To play, click on any of the 'interactor' cells which will start the fun. The goal is to clear the board "
            , text "in as few clicks as possible. You'll know you're done when the "
            , strong [] [ text "Playing" ]
            , text " text in the top left changes to "
            , strong [] [ text "COMPLETE." ]
            ]
        , p [] [ strong [] [ text "Cell types:" ] ]
        , ul []
            [ li []
                [ span [ class "interactor-example interactor-one" ] []
                , strong [] [ text "One Core " ]
                , text " - click to increase to Two Core"
                ]
            , li []
                [ span [ class "interactor-example interactor-two" ] []
                , strong [] [ text "Two Core " ]
                , text " - click to increase to Three Core"
                ]
            , li []
                [ span [ class "interactor-example interactor-three" ] []
                , strong [] [ text "Three Core " ]
                , text " - click to increase to Four Core"
                ]
            , li []
                [ span [ class "interactor-example interactor-four" ] []
                , strong [] [ text "Four Core " ]
                , text " - click to explode, sending particles flying in each direction"
                ]
            , li []
                [ span [ class "interactor-example interactor-reverse" ] []
                , strong [] [ text "Reverse " ]
                , text " - (not clickable) reverses the direction of a particle that lands on it"
                ]
            , li []
                [ span [ class "interactor-example interactor-arrow-right" ] []
                , strong [] [ text "Arrow " ]
                , text " - (not clickable) forces a particle to switch to the arrow's direction"
                ]
            , li []
                [ span [ class "interactor-example interactor-energizer" ] []
                , strong [] [ text "Energizer " ]
                , text " - (not clickable) sends out two additional particles in the lateral directions"
                ]
            , li []
                [ span [ class "interactor-example interactor-black-hole" ] []
                , strong [] [ text "Black hole " ]
                , text " - (not clickable) swallows any particle that collides with it"
                ]
            ]
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
        Core _ ->
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
        Core (CoreDetails size _) ->
            class <| "cell interactor-" ++ coreSizeToString size

        Reverse _ ->
            class "cell interactor-reverse"

        Arrow dir _ ->
            class <| "cell interactor-arrow-" ++ directionToString dir

        Energizer _ ->
            class "cell interactor-energizer"

        BlackHole _ ->
            class "cell interactor-black-hole"

        Portal _ _ ->
            class "cell interactor-portal"


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
        [ Core <| CoreDetails Three ( 0, 0 )
        , Arrow Down ( 1, 2 )
        , BlackHole ( 6, 5 )
        , Core <| CoreDetails Two ( 3, 7 )
        , Reverse ( 4, 1 )
        , Core <| CoreDetails Three ( 7, 6 )
        , Arrow Up ( 7, 6 )
        , Core <| CoreDetails Two ( 6, 1 )
        , Reverse ( 0, 7 )
        , Core <| CoreDetails Four ( 2, 4 )
        , Core <| CoreDetails One ( 4, 7 )
        , Energizer ( 3, 6 )
        , Core <| CoreDetails Four ( 0, 4 )
        , Core <| CoreDetails Four ( 2, 6 )
        , Arrow Right ( 3, 2 )
        , Arrow Right ( 5, 4 )
        , Arrow Up ( 5, 7 )
        , Core <| CoreDetails Three ( 6, 3 )
        , Core <| CoreDetails Three ( 1, 4 )
        , Core <| CoreDetails Three ( 2, 5 )
        , Arrow Right ( 7, 4 )
        , Arrow Up ( 5, 7 )
        , Core <| CoreDetails Four ( 4, 4 )
        , Arrow Right ( 6, 0 )
        , Core <| CoreDetails Four ( 7, 2 )
        , Arrow Down ( 2, 0 )
        , Portal ( 5, 2 ) ( 0, 6 )
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
    case interactor of
        Portal ( ix1, iy1 ) ( ix2, iy2 ) ->
            (x == ix1 && y == iy1) || (x == ix2 && y == iy2)

        _ ->
            let
                ( ix, iy ) =
                    getInteractorPosition interactor
            in
            x == ix && y == iy


getInteractorPosition : Interactor -> Position
getInteractorPosition interactor =
    case interactor of
        Core (CoreDetails _ pos) ->
            pos

        Arrow _ pos ->
            pos

        BlackHole pos ->
            pos

        Reverse pos ->
            pos

        Energizer pos ->
            pos

        Portal pos _ ->
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
            Core (CoreDetails One position) ->
                ( Just <| Core (CoreDetails Two position), [] )

            Core (CoreDetails Two position) ->
                ( Just <| Core (CoreDetails Three position), [] )

            Core (CoreDetails Three position) ->
                ( Just <| Core (CoreDetails Four position), [] )

            Core (CoreDetails Four position) ->
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

            (Portal position1 position2) as portal ->
                case maybeProjectile of
                    Just projectile ->
                        let
                            movedProjectile =
                                interactWithPortal position1 position2 projectile
                        in
                        ( Just portal, [ movedProjectile ] )

                    Nothing ->
                        ( Just portal, [] )

    else
        case maybeProjectile of
            Nothing ->
                ( Just interactor, [] )

            Just projectile ->
                ( Just interactor, [ projectile ] )


interactWithPortal : Position -> Position -> Projectile -> Projectile
interactWithPortal position1 position2 (Projectile direction position percent) =
    let
        newPosition =
            if position == position1 then
                position2

            else
                position1
    in
    Projectile direction newPosition percent


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
