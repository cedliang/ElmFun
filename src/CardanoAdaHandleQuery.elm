module CardanoAdaHandleQuery exposing (..)

--pointfree
--compared to haskell, 
-- << is .
-- <| is $

import Browser
import Browser.Events
import Html exposing (Html)
import Html.Events exposing (keyCode, on)
import Http
import Json.Decode as Decode
import Types exposing (..)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Framework.Spinner exposing (..)
import Simple.Animation as Animation exposing (Animation)
import Simple.Animation.Animated as Animated
import Simple.Animation.Property as P
import CardanoTxQuery exposing (Model(..))

-- MAIN
main : Program () Model Msg
main = Browser.element { init = init, update = update, subscriptions = subscriptions, view = viewElem }



-- MODEL
type Model = 
  Unsubmitted String
  | HashNotFound
  | Loading 
  | HashFound String

init : () -> (Model, Cmd Msg)
init _ = (Unsubmitted "", Cmd.none)



-- UPDATE
type alias TxHash = String
type Msg
  = GotTx (Result Http.Error String)
  | BoxContents String
  | ChainQuery TxHash
  | Reset


update : Msg -> Model -> (Model, Cmd Msg)
update msg _ = 
  case msg of 
    GotTx result ->
      case result of 
        Ok txDetails ->
          (HashFound txDetails, Cmd.none)
        
        Err _ ->
          (HashNotFound, Cmd.none)

    BoxContents c -> (Unsubmitted c, Cmd.none)

    ChainQuery handleName -> (Loading, 
      Http.get
        { url = "http://127.0.0.1:3000/adahandle/" ++ handleName
        , expect = Http.expectString GotTx
        })
    
    Reset -> (Unsubmitted "", Cmd.none)



-- KEYPRESS DECODER
enterDecoder : Msg -> Decode.Decoder Msg
enterDecoder msg = 
    let isEnter code =
            if code == 13 then
                Decode.succeed msg
            else
                Decode.fail "not ENTER"
    in
      Decode.andThen isEnter keyCode



-- SUBSCRIPTIONS
subscriptions : Model -> Sub Msg
subscriptions m =
  case m of 
      Loading -> Sub.none
      Unsubmitted _ -> Sub.none
      _ -> (Browser.Events.onKeyPress << enterDecoder) Reset



-- VIEW
onEnter : Msg -> Html.Attribute Msg
onEnter = on "keydown" << enterDecoder


onEnterElem : Msg -> Element.Attribute Msg
onEnterElem = Element.htmlAttribute << on "keydown" << enterDecoder


blue : Color
blue = Element.rgb255 76 139 245
white : Color
white = Element.rgb255 230 230 230 

cardBlue : Color
cardBlue = Element.rgb255 42 113 208

black : Color
black = Element.rgb255 0 0 0

darkmodeBack : Color
darkmodeBack = Element.rgb255 20 21 23

darkmodeButtonFill : Color
darkmodeButtonFill = Element.rgb255 50 62 147







spinAndSlide : Animation
spinAndSlide =
    Animation.steps
        { startAt = [ P.rotate 0, P.x 0, P.y -100 ]
        , options = [ Animation.loop ]
        }
        [ Animation.step 300 [ P.rotate 180, P.x 0, P.y -100 ]
        , Animation.wait 100
        , Animation.step 300 [ P.rotate 540, P.x 0, P.y -100 ]
        , Animation.wait 350
        ]




animatedUi :
    (List (Element.Attribute msg) -> children -> Element msg)
    -> Animation
    -> List (Element.Attribute msg)
    -> children
    -> Element msg
animatedUi =
    Animated.ui
        { behindContent = Element.behindContent
        , htmlAttribute = Element.htmlAttribute
        , html = Element.html
        }

ael : Animation -> List (Element.Attribute msg) -> Element msg -> Element msg
ael =
    animatedUi Element.el


viewElem : Model -> Html Msg
viewElem model =
  case model of
    Loading ->
      layout [Background.color darkmodeBack] <| ael spinAndSlide [centerX, centerY, spacing 16] <| Framework.Spinner.spinner ThreeCircles 32 cardBlue

    _ -> layout [Background.color darkmodeBack] <| el [
                                          moveUp 100
                                        , centerX
                                        , centerY
                                        , width <| px 1300
                                        , height <| px 82
                                        , onRight
                                  (
                                    Input.button
                                        [ Background.color darkmodeButtonFill
                                        , Border.rounded 5
                                        , Border.width 1
                                        , Border.color cardBlue
                                        , centerY
                                        , moveRight 15
                                        , height fill
                                        , width <| px 100
                                        ]
                                        { onPress = (
                                                      case model of 
                                                        Unsubmitted boxContents -> Just <| ChainQuery boxContents
                                                        _ -> Just Reset
                                                    )
                                        , label = el [Font.family [Font.sansSerif], centerX, centerY, Font.semiBold, Font.color white] <| Element.text "Reset"
                                        }
                                  )]
                                        (Input.text 
                                          [ centerX
                                          , centerY
                                          , height fill
                                          , width fill
                                          , spacing 16
                                          , Font.family [Font.monospace]
                                          , Font.color white
                                          , onEnterElem (
                                                          case model of 
                                                            Unsubmitted boxContents -> ChainQuery boxContents
                                                            _ -> Reset
                                                        )
                                          , Background.color darkmodeBack
                                          ]
                                          { onChange = BoxContents
                                          , text = 
                                                (
                                                  case model of 
                                                    Unsubmitted boxContents -> boxContents
                                                    HashFound a -> String.replace "\"" "" a
                                                    _ -> ""
                                                )
                                          , placeholder = Just <| Input.placeholder [] <| Element.text "Enter a Handle"
                                          , label = Input.labelAbove [Font.family [Font.sansSerif], Font.color white] <| Element.text <| (case model of 
                                                                                                                                            Unsubmitted _ -> "Get Ada Handle Address"
                                                                                                                                            HashFound _ -> "Handle Address:"
                                                                                                                                            _ -> "Ada Handle was not found on the blockchain."
                                                                                                                                          )
                                          }
                                        )

