module Main exposing (..)

import Html exposing (Html, button, div, h1, li, text, ul)
import Html.Attributes exposing (class, classList, href)
import Html.Events exposing (onClick)
import List
import Mouse exposing (Position, downs, moves, ups)
import Svg exposing (Svg, polyline, svg)
import Svg.Attributes exposing (fill, height, points, stroke, strokeWidth, width)
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
            { screen = Settings
            , points = []
            , mouse = MouseUp
            , window = Size 0 0
            , threshold = 300
            , n = 4
            , mirror = True
            }
    in
        ( initState
        , Task.perform WindowResizes Window.size
        )


initPosition : Position
initPosition =
    { x = 0, y = 0 }


type alias Model =
    { screen : Screen
    , points : List Position
    , mouse : MouseState
    , window : Size
    , threshold : Int
    , n : Int
    , mirror : Bool
    }


type MouseState
    = MouseUp
    | MouseDown


type Screen
    = Art
    | Settings



-- Update


type Msg
    = MouseUps Position
    | MouseDowns Position
    | MouseMoves Position
    | WindowResizes Size
    | SwitchScreen Screen
    | SetNgon Int
    | Mirror


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
                                |> List.take model.threshold

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

        SwitchScreen screen ->
            ( { model | screen = screen }
            , Cmd.none
            )

        SetNgon n ->
            ( { model | n = n }
            , Cmd.none
            )

        Mirror ->
            ( { model | mirror = not model.mirror }
            , Cmd.none
            )


centerPosition : Size -> Position -> Position
centerPosition { width, height } { x, y } =
    { x = x - width // 2, y = height // 2 - y }


uncenterPosition : Size -> Position -> Position
uncenterPosition { width, height } { x, y } =
    { x = x + width // 2, y = height // 2 - y }



-- View


view : Model -> Html Msg
view ({ screen } as model) =
    case screen of
        Art ->
            viewArt model

        Settings ->
            viewSettings model



-- Screens


viewSettings : Model -> Html Msg
viewSettings { n, mirror } =
    let
        header : Html Msg
        header =
            h1 [] [ text "Stars" ]

        mirrorSwitch : Html Msg
        mirrorSwitch =
            button [ onClick Mirror, classList [ ( "active", mirror ) ] ] [ text "Mirror" ]

        nOptions : Html Msg
        nOptions =
            List.range 2 20
                |> List.map (viewOption n)
                |> ul []

        close : Html Msg
        close =
            button [ onClick (SwitchScreen Art) ] [ text "Draw" ]
    in
        div [] <|
            header
                :: mirrorSwitch
                :: (div [ class "center" ] [ nOptions ])
                :: close
                :: []


viewArt : Model -> Html Msg
viewArt { window, n, points } =
    let
        lines =
            List.repeat n points
                |> List.indexedMap (\i ps -> getSymetries n i ps)
                |> List.map (drawLine window)
    in
        svg
            [ width (toString window.width), height (toString window.height) ]
            lines



-- Helpers


viewOption : Int -> Int -> Html Msg
viewOption selected v =
    li
        [ classList [ ( "selected", selected == v ) ], onClick (SetNgon v) ]
        [ text <| toString v ]


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
            , strokeWidth "2"
            ]
            []



-- Math


getSymetries : Int -> Int -> List Position -> List Position
getSymetries n i points =
    let
        angle =
            2 * pi / (toFloat n) * (toFloat i)

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
