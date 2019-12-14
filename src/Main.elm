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
    = Click Position
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


type Positioned a
    = Positioned Int Int Float a


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
    , isPaused : Bool
    , clickCount : Int
    }


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
    not <| List.any (\(Positioned _ _ _ { kind }) -> isClickable kind) board.interactors


isClickable : InteractorKind -> Bool
isClickable kind =
    case kind of
        One ->
            True

        Two ->
            True

        Three ->
            True

        Four ->
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
            List.find (elementIsAtPosition ( x, y )) board.interactors

        projectiles =
            List.filter (elementIsAtPosition ( x, y )) board.projectiles

        renderedProjectiles =
            List.map viewProjectile projectiles
    in
    case mInteractor of
        Nothing ->
            div [ class "cell cell-empty" ] renderedProjectiles

        Just (Positioned _ _ _ interactor) ->
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

        Reverse ->
            "interactor-reverse"

        Arrow dir ->
            "interactor-arrow-" ++ directionToString dir

        Energizer ->
            "interactor-energizer"


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
        [ Positioned 0 0 0 { id = 1, kind = Three }
        , Positioned 1 2 0 { id = 1, kind = Arrow Down }
        , Positioned 3 7 0 { id = 2, kind = Two }
        , Positioned 4 1 0 { id = 1, kind = Reverse }
        , Positioned 7 6 0 { id = 2, kind = Three }
        , Positioned 7 6 0 { id = 2, kind = Arrow Up }
        , Positioned 6 1 0 { id = 2, kind = Two }
        , Positioned 0 7 0 { id = 2, kind = Reverse }
        , Positioned 2 4 0 { id = 1, kind = Four }
        , Positioned 4 7 0 { id = 1, kind = One }
        , Positioned 3 6 0 { id = 1, kind = Energizer }
        , Positioned 0 4 0 { id = 1, kind = Four }
        , Positioned 2 6 0 { id = 1, kind = Four }
        , Positioned 3 2 0 { id = 1, kind = Arrow Right }
        , Positioned 5 4 0 { id = 2, kind = Arrow Right }
        , Positioned 5 7 0 { id = 2, kind = Arrow Up }
        , Positioned 6 3 0 { id = 2, kind = Three }
        , Positioned 1 4 0 { id = 1, kind = Three }
        , Positioned 2 5 0 { id = 1, kind = Three }
        , Positioned 7 4 0 { id = 2, kind = Arrow Right }
        , Positioned 5 7 0 { id = 2, kind = Arrow Up }
        , Positioned 4 4 0 { id = 2, kind = Four }
        , Positioned 6 0 0 { id = 1, kind = Arrow Right }
        , Positioned 7 2 0 { id = 1, kind = Four }
        , Positioned 2 0 0 { id = 1, kind = Arrow Down }
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


handleClick : Position -> Model -> ( Model, Cmd Msg )
handleClick ( x, y ) model =
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
                    interact interactor Nothing

        newCount =
            case mInteractor of
                Nothing ->
                    model.clickCount

                Just _ ->
                    model.clickCount + 1

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
    ( { model | board = newBoard, clickCount = newCount }, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Click pos ->
            if model.isPaused then
                ( model, Cmd.none )

            else
                handleClick pos model

        Frame _ ->
            if model.isPaused then
                ( model, Cmd.none )

            else
                ( { model | board = updateBoard model }, Cmd.none )

        Tick ->
            ( { model | board = updateBoard model }, Cmd.none )

        Reset ->
            ( { initialModel | isPaused = model.isPaused }, Cmd.none )

        TogglePause ->
            ( { model | isPaused = not model.isPaused }, Cmd.none )


interact : Positioned Interactor -> Maybe (Positioned Projectile) -> ( Maybe (Positioned Interactor), List (Positioned Projectile) )
interact ((Positioned x y _ { id, kind }) as positionedInteractor) maybeProjectile =
    let
        inPlace =
            Positioned x y 0

        shouldInteract =
            case maybeProjectile of
                Nothing ->
                    True

                Just (Positioned _ _ percentage _) ->
                    percentage < 0.01 && percentage > -0.01
    in
    if shouldInteract then
        case kind of
            One ->
                ( Just (inPlace (Interactor id Two)), [] )

            Two ->
                ( Just (inPlace (Interactor id Three)), [] )

            Three ->
                ( Just (inPlace (Interactor id Four)), [] )

            Four ->
                ( Nothing
                , [ inPlace <| Projectile Up
                  , inPlace <| Projectile Down
                  , inPlace <| Projectile Left
                  , inPlace <| Projectile Right
                  ]
                )

            Reverse ->
                case maybeProjectile of
                    Nothing ->
                        ( Just positionedInteractor, [] )

                    Just (Positioned _ _ _ (Projectile dir)) ->
                        let
                            reversedDirection =
                                case dir of
                                    Up ->
                                        Down

                                    Down ->
                                        Up

                                    Left ->
                                        Right

                                    Right ->
                                        Left
                        in
                        ( Just positionedInteractor, [ Positioned x y 0 (Projectile reversedDirection) ] )

            Arrow dir ->
                case maybeProjectile of
                    Nothing ->
                        ( Just positionedInteractor, [] )

                    Just _ ->
                        ( Just positionedInteractor, [ Positioned x y 0 (Projectile dir) ] )

            Energizer ->
                case maybeProjectile of
                    Nothing ->
                        ( Just positionedInteractor, [] )

                    Just (Positioned _ _ _ (Projectile dir)) ->
                        ( Just positionedInteractor, List.map (\d -> Positioned x y 0 (Projectile d)) <| withLateralDirections dir )

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
subscriptions _ =
    onAnimationFrameDelta Frame


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
