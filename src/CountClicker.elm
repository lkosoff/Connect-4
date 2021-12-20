module CountClicker exposing (main)

import Browser
import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)



-- MAIN


main : Program () Model Msg
main =
    Browser.sandbox { init = initModel, view = view, update = update }



-- MODEL


type alias Model =
    { count : Int }


initModel =
    { count = 0 }



-- UPDATE


type Msg
    = Reset
    | Increment


update : Msg -> Model -> Model
update msg model =
    case msg of
        Reset ->
            initModel

        Increment ->
            { count = 1 + model.count }



-- VIEW


view : Model -> Html Msg
view model =
    let
        buttonAttributes =
            [ -- https://en.wikipedia.org/wiki/Web_colors
              Background.color (Element.rgb255 255 255 205)
            , Element.mouseOver
                [ Border.glow (Element.rgb255 135 206 235) 2
                ]
            , Border.width 5
            , Border.color (Element.rgb255 70 130 180)
            , Border.rounded 10
            , Element.padding 20
            ]

        reset =
            Input.button buttonAttributes
                { onPress = Just Reset
                , label = Element.text "Reset"
                }

        increment =
            Input.button buttonAttributes
                { onPress = Just Increment
                , label = Element.text "Increment"
                }

        display =
            Element.text ("Count: " ++ Debug.toString model.count)
    in
    Element.layout
        [ Font.family
            [ Font.external
                { name = "Noto Sans"
                , url = "https://fonts.googleapis.com/css?family=Noto+Sans"
                }
            ]
        , Element.focused []
        ]
    <|
        Element.row
            [ Element.centerX, Element.centerY, Element.spacing 15 ]
            [ reset
            , increment
            , display
            ]
