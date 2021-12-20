module Main exposing (main)

---Main file which contains the MVC framework

import Board exposing (..)
import Browser
import Browser.Dom exposing (..)
import Browser.Events
import Collage.Render exposing (svg)
import Color exposing (Color)
import Computer exposing (..)
import Display exposing (..)
import Element exposing (Element, onLeft)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Html.Attributes as Attr
import Json.Decode as Decode
import Play exposing (..)
import Task
import Time


type alias Flags =
    ()



-- Human player


human =
    X



-- Gets the viewPort


getVP =
    Task.perform ScreenDim Browser.Dom.getViewport


init : Flags -> ( Model, Cmd Msg )
init () =
    ( initModel, getVP )


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { game : Game
    , turn : Sqr
    , screenDim : { width : Float, height : Float }
    , hover : Maybe Col
    , mode : Mode
    , computerMoveDelay : Bool
    , startGame : Bool
    }


initModel : Model
initModel =
    { game = newGame
    , turn = X
    , screenDim = { width = 0, height = 0 }
    , hover = Nothing
    , mode = Prompt
    , computerMoveDelay = True
    , startGame = False
    }


type alias MousePoint =
    { x : Int, y : Int }


type Mode
    = SinglePlay
    | MultiPlay
    | Prompt



-- UPDATE


type Msg
    = SetPiece Col
    | Undo
    | Noop
    | StartGame
    | ChooseMode Mode
    | NewGame
    | Delay
    | ComputerTurn (Maybe Col)
    | Hover (Maybe Col)
    | WindowRefresh
    | ScreenDim Viewport


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Noop ->
            ( model, Cmd.none )

        StartGame ->
            ( { model | startGame = True }, Cmd.none )

        WindowRefresh ->
            ( model, getVP )

        NewGame ->
            ( initModel, getVP )

        ComputerTurn maybeMove ->
            ( computerMoveUpdate maybeMove model, Cmd.none )

        ChooseMode mode ->
            ( { model | mode = mode }, Cmd.none )

        Undo ->
            ( undoUpdate model, Cmd.none )

        Delay ->
            ( { model | computerMoveDelay = True }, promptComputerMove model )

        SetPiece j ->
            ( moveUpdate j model, Cmd.none )

        Hover maybeCol ->
            ( hoverUpdate maybeCol model, Cmd.none )

        ScreenDim vp ->
            ( { model | screenDim = { width = vp.scene.width, height = vp.scene.height } }, Cmd.none )



-- takes a possible computer move and updates model


computerMoveUpdate : Maybe Col -> Model -> Model
computerMoveUpdate maybej model =
    case maybej of
        Nothing ->
            model

        Just col ->
            case makeMove model.game col of
                Just g ->
                    { model | game = g }

                _ ->
                    model



-- Shows a move preview in the column that the mouse is hovering over


hoverUpdate : Maybe Col -> Model -> Model
hoverUpdate maybej model =
    { model | hover = maybej }



-- Takes a column and makes a move in that column


moveUpdate : Col -> Model -> Model
moveUpdate j model =
    case ( makeMove model.game j, model.mode, model.game.turn == human ) of
        ( Just g, SinglePlay, True ) ->
            { model | game = g, computerMoveDelay = False }

        ( Just g, MultiPlay, _ ) ->
            { model | game = g }

        _ ->
            model



{- A command Msg for when it is the computer's turn to go -}


promptComputerMove : Model -> Cmd Msg
promptComputerMove model =
    Task.perform ComputerTurn (Task.succeed (computerMove model.game))



-- undoes previous move. Makes two undos in multiplayer


undoUpdate : Model -> Model
undoUpdate model =
    case ( model.game.winner, model.mode ) of
        ( Nothing, SinglePlay ) ->
            { model | game = makeUndo (makeUndo model.game) }

        ( Nothing, MultiPlay ) ->
            { model | game = makeUndo model.game }

        _ ->
            model



----------------  ---------------- ----------------
---------------- Display functions ----------------
----------------  ---------------- ----------------
-- Helper function for displaying either who one or whose turn it is.


showStatus txt sqr =
    let
        turnSVG =
            turnCircle sqr
                |> svg
                |> Element.html
    in
    Element.row
        sideBarAttributes
        [ Element.el [ Element.centerX ] (Element.text txt), turnSVG ]



-- shows either a winner or turn


turnOrWin model =
    case model.game.winner of
        Nothing ->
            showStatus "Turn  " model.game.turn

        Just sqr ->
            showStatus "Winner  " sqr



-- converts a collage color to an element color


collageToElementColor : Color -> Element.Color
collageToElementColor color =
    case Color.toRgba color of
        { blue, green, red } ->
            Element.rgb red green blue


sideBarAttributes : List (Element.Attribute msg)
sideBarAttributes =
    [ Background.color (Element.rgb255 255 255 255)
    , Border.width 5
    , Border.color (Element.rgb255 0 0 0)
    , Border.rounded 10
    , Element.padding 20
    , Element.centerX
    , Element.centerY
    , Font.color (Element.rgb255 45 45 45)
    , Font.size 30
    , Font.family
        [ Font.external
            { name = "Fascinate"
            , url = "https://fonts.googleapis.com/css2?family=Fascinate&display=swap"
            }
        ]
    ]


buttonAttributes : List (Element.Attribute msg)
buttonAttributes =
    Element.mouseOver
        [ Border.glow (collageToElementColor boardBackgroundColor) 2
        ]
        :: sideBarAttributes


view : Model -> Html Msg
view model =
    let
        styles =
            [ ( "background-color", backgroundColorString )
            ]

        showHover =
            if not model.computerMoveDelay then
                Nothing

            else
                model.hover

        boardDisplay =
            boardToCanvas model.game.board showHover model.game.turn
                |> svg
                |> Element.html
                |> Element.el [ Element.centerX, Element.centerY, Element.onRight sideBar, Element.padding 10, Element.above titleCard ]

        status =
            turnOrWin model

        titleCard =
            Element.el
                [ Element.centerX
                , Element.centerY
                , Font.color (Element.rgb255 255 255 255)
                , Font.size 60
                , Font.family
                    [ Font.external
                        { name = "Fascinate"
                        , url = "https://fonts.googleapis.com/css2?family=Fascinate&display=swap"
                        }
                    ]
                ]
                (Element.text "Connect 4")

        newGame =
            Input.button buttonAttributes
                { onPress = Just NewGame
                , label = Element.text "New Game"
                }

        undo =
            Input.button buttonAttributes
                { onPress = Just Undo
                , label = Element.text "Undo"
                }

        sideBar =
            Element.column
                [ Element.padding 30, Element.spacing 15 ]
                [ status
                , newGame
                , undo
                ]

        onePlayerButton =
            Input.button buttonAttributes
                { onPress = Just (ChooseMode SinglePlay)
                , label = Element.text "Single Player"
                }

        twoPlayerButton =
            Input.button buttonAttributes
                { onPress = Just (ChooseMode MultiPlay)
                , label = Element.text "Two Players"
                }

        selectPlayerButtons =
            Element.row
                [ Element.padding 30, Element.centerX, Element.spacing 15 ]
                [ onePlayerButton
                , twoPlayerButton
                ]

        allElementBoard =
            Element.layout
                [ Element.height (Element.px (round model.screenDim.height))
                ]
            <|
                boardDisplay

        allElementStartScreen =
            Element.layout
                [ Element.height (Element.px (round model.screenDim.height))
                ]
            <|
                Element.column [ Element.centerX, Element.centerY ] [ titleCard, selectPlayerButtons ]

        display =
            if model.mode == Prompt then
                allElementStartScreen

            else
                allElementBoard
    in
    Html.div (List.map (\( k, v ) -> Attr.style k v) styles) [ display ]



-- SUBSCRIPTIONS


clickDecoder : Decode.Decoder MousePoint
clickDecoder =
    Decode.map2
        (\x y -> { x = x, y = y })
        (Decode.field "clientX" Decode.int)
        (Decode.field "clientY" Decode.int)



-- converts a point to a msg
-- ensures that clicks are on the board


pointToMsg : Model -> MousePoint -> Msg
pointToMsg model p =
    let
        width =
            model.screenDim.width

        height =
            model.screenDim.height

        topBoard =
            (height - boardHeight) / 2

        bottomBoard =
            (height + boardHeight) / 2

        center =
            (width / 2) - (3.5 * gridWidth)

        px =
            toFloat p.x

        py =
            toFloat p.y
    in
    if -center + px < 0 || (py < topBoard) || (py > bottomBoard) || (model.mode == Prompt) then
        Noop

    else if not model.startGame then
        -- when selecting single player or multiplayer
        -- The click would accidentally place a piece
        -- so this is the work around
        StartGame

    else if -center + px < gridWidth then
        SetPiece 0

    else if -center + px < gridWidth * 2 then
        SetPiece 1

    else if -center + px < gridWidth * 3 then
        SetPiece 2

    else if -center + px < gridWidth * 4 then
        SetPiece 3

    else if -center + px < gridWidth * 5 then
        SetPiece 4

    else if -center + px < gridWidth * 6 then
        SetPiece 5

    else if -center + px < gridWidth * 7 then
        SetPiece 6

    else
        Noop


hoverToMsg : Model -> MousePoint -> Msg
hoverToMsg model p =
    case pointToMsg model p of
        SetPiece x ->
            Hover (Just x)

        _ ->
            Hover Nothing


delayComputerTurn : Model -> Sub Msg
delayComputerTurn model =
    if not model.computerMoveDelay && (model.game.turn == O) then
        Time.every
            200
            (always Delay)

    else
        Sub.none


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ delayComputerTurn model
        , Browser.Events.onResize (\_ _ -> WindowRefresh)
        , Browser.Events.onClick (Decode.map (pointToMsg model) clickDecoder)
        , Browser.Events.onMouseMove (Decode.map (hoverToMsg model) clickDecoder)
        ]
