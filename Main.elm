module Main exposing (..)

import Html
import Mouse exposing (Position, downs, moves, ups)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Task
import Window exposing (Size, resizes)
import List


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
            , latency = 300
            , n = 4
            }
    in
        ( initState
        , Task.perform WindowResizes Window.size
        )


initPosition : Position
initPosition =
    { x = 0, y = 0 }


type alias Model =
    { points : List Position
    , mouse : MouseState
    , window : Size
    , latency : Int
    , n : Int
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
view ({ window, n, points } as model) =
    let
        lines =
            List.repeat n points
                |> List.indexedMap (\i ps -> getSymetries n i ps)
                |> List.map (drawLine window)
    in
        svg
            [ width (toString window.width), height (toString window.height) ]
            lines


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



-- Math


getSymetries : Int -> Int -> List Position -> List Position
getSymetries n i points =
    let
        angle =
            2 * pi / (toFloat n) * (toFloat i)

        a =
            Debug.log "i" angle

        mirror : Position -> Position
        mirror point =
            let
                ( r, psi ) =
                    getRPsi point

                psi2 =
                    psi + angle
            in
                getCoordinates ( r, psi2 )
    in
        List.map mirror points


getRPsi : Position -> ( Float, Float )
getRPsi { x, y } =
    let
        fx =
            toFloat x

        fy =
            toFloat y

        r =
            hypot fy fx

        psi =
            atan2 fy fx
    in
        ( r, psi )


hypot : Float -> Float -> Float
hypot y x =
    sqrt <| (x * x) + (y * y)


getCoordinates : ( Float, Float ) -> Position
getCoordinates ( r, psi ) =
    let
        x =
            round <| (cos psi) * r

        y =
            round <| (sin psi) * r
    in
        { x = x, y = y }
