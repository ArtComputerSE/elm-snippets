module CustomComponent.Main exposing (Model, Msg(..), init, main, update, valueDecoder, view, viewDate)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as D



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }



-- MODEL


type alias Model =
    { language : String
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { language = "sr-RS" }
    , Cmd.none
    )



-- UPDATE


type Msg
    = LanguageChanged String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LanguageChanged language ->
            ( { model | language = language }
            , Cmd.none
            )



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ p [] [ viewDate model.language 2012 5 ]
        , select
            [ on "change" (D.map LanguageChanged valueDecoder)
            ]
            [ option [ value "sr-RS" ] [ text "sr-RS" ]
            , option [ value "en-GB" ] [ text "en-GB" ]
            , option [ value "en-US" ] [ text "en-US" ]
            ]
        ]



-- Use the Custom Element defined in index.html
--


viewDate : String -> Int -> Int -> Html msg
viewDate lang year month =
    node "intl-date"
        [ attribute "lang" lang
        , attribute "year" (String.fromInt year)
        , attribute "month" (String.fromInt month)
        ]
        []


valueDecoder : D.Decoder String
valueDecoder =
    D.field "currentTarget" (D.field "value" D.string)
