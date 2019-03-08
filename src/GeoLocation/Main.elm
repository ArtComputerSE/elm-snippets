port module GeoLocation.Main exposing (main)

import Browser
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Json.Decode as Decode exposing (Decoder)



-- MODEL


type alias Position =
    { lat : Float, long : Float }


type GeoData
    = NotAsked
    | Loading
    | Failure String
    | Success Position


type alias Model =
    { geo : GeoData }


initialModel : Model
initialModel =
    { geo = NotAsked }



-- UPDATE


type Msg
    = GetGeo
    | ReceiveGeo (Result Decode.Error GeoData)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GetGeo ->
            ( { model | geo = Loading }, getGeo )

        ReceiveGeo (Err error) ->
            ( { model | geo = Failure "failed to decode JSON" }, Cmd.none )

        ReceiveGeo (Ok geo) ->
            ( { model | geo = geo }, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ button [ onClick GetGeo ] [ text "Get Geo" ]
        , div [] [ text <| Debug.toString model.geo ]
        ]



-- PORTS


getGeo : Cmd a
getGeo =
    getGeoPort ()


port getGeoPort : () -> Cmd msg


port receiveGeoData : (Decode.Value -> msg) -> Sub msg



-- DECODERS


geoDecoder : Decoder GeoData
geoDecoder =
    Decode.oneOf [ geoSuccessDecoder, geoErrorDecoder ]


positionDecoder : Decoder Position
positionDecoder =
    Decode.map2 Position
        (Decode.at [ "coords", "latitude" ] Decode.float)
        (Decode.at [ "coords", "longitude" ] Decode.float)


geoSuccessDecoder : Decoder GeoData
geoSuccessDecoder =
    Decode.map Success positionDecoder


geoErrorDecoder : Decoder GeoData
geoErrorDecoder =
    Decode.map Failure Decode.string



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    receiveGeoData (ReceiveGeo << Decode.decodeValue geoDecoder)



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = always ( initialModel, Cmd.none )
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
