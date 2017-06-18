module Main exposing (..)

import Html
import Mouse exposing (Position, downs, moves, ups)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Task
import Window exposing (Size, resizes)


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ downs MouseDowns
        , ups MouseUps
        , moves MouseMoves
        , resizes WindowResizes
        ]



-- Model


init : ( Model, Cmd Msg )
init =
    let
        initState =
            { points = []
            , mouse = MouseUp
            , window = Size 0 0
            , latency = 200
            }
    in
        ( initState
        , Task.perform WindowResizes Window.size
        )


type alias Model =
    { points : List Position
    , mouse : MouseState
    , window : Size
    , latency : Int
    }


type MouseState
    = MouseUp
    | MouseDown



-- Update


type Msg
    = MouseUps Position
    | MouseDowns Position
    | MouseMoves Position
    | WindowResizes Size


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ mouse, window } as model) =
    case msg of
        MouseUps _ ->
            ( { model | mouse = MouseUp }
            , Cmd.none
            )

        MouseDowns newPosition ->
            let
                cP =
                    centerPosition window newPosition

                newModel =
                    { model
                        | mouse = MouseDown
                        , points = [ cP ]
                    }
            in
                ( newModel, Cmd.none )

        MouseMoves newPosition ->
            let
                cP =
                    centerPosition window newPosition

                newPoints =
                    case model.mouse of
                        MouseDown ->
                            (cP :: model.points)
                                |> List.take model.latency

                        MouseUp ->
                            model.points
            in
                ( { model | points = newPoints }
                , Cmd.none
                )

        WindowResizes newWindow ->
            ( { model | window = newWindow }
            , Cmd.none
            )


centerPosition : Size -> Position -> Position
centerPosition { width, height } { x, y } =
    { x = x - width // 2, y = height // 2 - y }


uncenterPosition : Size -> Position -> Position
uncenterPosition { width, height } { x, y } =
    { x = x + width // 2, y = height // 2 - y }



-- View


view : Model -> Html.Html Msg
view ({ window } as model) =
    let
        mPY =
            model.points
                |> List.map (\{ x, y } -> Position -x y)

        mPX =
            model.points
                |> List.map (\{ x, y } -> Position x -y)

        mPXY =
            model.points
                |> List.map (\{ x, y } -> Position -x -y)

        line1 =
            drawLine window model.points

        line2 =
            drawLine window mPY

        line3 =
            drawLine window mPX

        line4 =
            drawLine window mPXY
    in
        svg
            [ width (toString window.width), height (toString window.height) ]
            [ line1, line2, line3, line4 ]


drawLine : Size -> List Position -> Svg Msg
drawLine window points_ =
    let
        points__ =
            points_
                |> List.map (uncenterPosition window)
                |> List.map (\{ x, y } -> (toString x) ++ "," ++ (toString y))
                |> String.join " "
    in
        polyline
            [ fill "none"
            , stroke "#00FFFF"
            , points points__
            ]
            []
