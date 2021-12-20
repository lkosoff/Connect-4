module Display exposing (..)

-- Display.elm  contains functions for displaying the board

import Board exposing (..)
import Collage exposing (circle, dot, line, rectangle, thick, thin, traced, ultrathick, uniform)
import Collage.Layout exposing (impose)
import Color exposing (Color)
import Play exposing (..)



-- size of a box to place a circle


gridWidth : number
gridWidth =
    70



-- width of board


boardWidth : number
boardWidth =
    7 * gridWidth


boardHeight : Float
boardHeight =
    6.8 * gridWidth


lineColor : Color.Color
lineColor =
    Color.rgb 0 0 0



-- color of the board itself


boardBackgroundColor : Color.Color
boardBackgroundColor =
    Color.rgb255 51 153 255


backgroundColorString : String
backgroundColorString =
    "#2C4D7D"



-- color of any empty circle


emptyColor : Color.Color
emptyColor =
    Color.rgb255 44 77 125



-- color of the X piece


xColor : Color.Color
xColor =
    Color.rgb255 255 0 0



-- color of the O piece


oColor : Color.Color
oColor =
    Color.rgb255 230 230 0



-- converts a sqr to color


toColor : Sqr -> Color
toColor s =
    case s of
        X ->
            xColor

        O ->
            oColor

        E ->
            emptyColor



-- coverts a piece to a lighter version of the piece


previewColor : Sqr -> Color
previewColor s =
    case Color.toRgba (toColor s) of
        { alpha, blue, green, red } ->
            Color.rgba red green blue 0.25



-- converts position to point on the screen


indexToPosition : Pos -> ( Float, Float )
indexToPosition ( i, j ) =
    ( gridWidth * (-3 + toFloat j), gridWidth * (2.5 - toFloat i) )



-- Outlined rectangle


blankCanvas : Collage.Collage msg
blankCanvas =
    rectangle (boardWidth + 1 * gridWidth) boardHeight
        |> Collage.styled ( uniform boardBackgroundColor, Collage.solid ultrathick (uniform Color.black) )



-- converts a sqr to a circle


sqrToCircle : Sqr -> Collage.Collage msg
sqrToCircle x =
    circle (0.9 * gridWidth / 2)
        |> Collage.styled ( uniform (toColor x), Collage.solid thick (uniform Color.black) )



-- highlights the next free space in a given column


highlightCircle : Col -> Board -> Sqr -> Maybe (Collage.Collage msg)
highlightCircle col board sqr =
    case getFreeSpace board col of
        Nothing ->
            Nothing

        Just pos ->
            circle (0.9 * gridWidth / 2)
                |> Collage.styled ( uniform (previewColor sqr), Collage.solid thick (uniform Color.black) )
                |> Collage.shift (indexToPosition pos)
                |> Just



--tiny circle for showing turn and winner


turnCircle : Sqr -> Collage.Collage msg
turnCircle sqr =
    sqrToCircle sqr
        |> Collage.scale 0.5



-- converts a position on the board to a shape


posToCircle : Pos -> Board -> Maybe (Collage.Collage msg)
posToCircle pos board =
    case get pos board of
        Nothing ->
            Nothing

        Just x ->
            sqrToCircle x
                |> Collage.shift (indexToPosition pos)
                |> Just



-- Converts an entire board to a collage


boardToCanvas : Board -> Maybe Col -> Sqr -> Collage.Collage msg
boardToCanvas board hoverCol sqr =
    let
        circlesInRow row canvas =
            List.foldl
                (\j acc ->
                    case posToCircle ( row, j ) board of
                        Just circ ->
                            impose circ acc

                        Nothing ->
                            acc
                )
                canvas
                (List.range 0 6)

        newCanvas =
            List.foldl (\j canvas -> circlesInRow j canvas) blankCanvas (List.range 0 5)
    in
    -- Add the move preview if mouse is hovering
    case hoverCol of
        Nothing ->
            newCanvas

        Just col ->
            case highlightCircle col board sqr of
                Nothing ->
                    newCanvas

                Just selection ->
                    impose selection newCanvas
