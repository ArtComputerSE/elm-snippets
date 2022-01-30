module Retry.Retry exposing (Model, Msg(..), init, initialModel, main)

import Browser
import Element exposing (Element, column, fill, height, layout, padding, row, text, width)
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html
import Http
import Json.Decode as Decoder exposing (Decoder, bool, decodeString, int, string)
import Json.Decode.Pipeline exposing (required)
import Retry
import Task


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


type Msg
    = ClickedFetch
    | PostFetched (Result Http.Error Post)


type alias Model =
    { post : Maybe Post
    , error : String
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( initialModel, Cmd.none )


initialModel : Model
initialModel =
    { post = Nothing
    , error = ""
    }



-- Update


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickedFetch ->
            ( model, fetchPost PostFetched )

        PostFetched (Ok post) ->
            ( { model | post = Just post, error = "" }, Cmd.none )

        PostFetched (Err err) ->
            ( { model | error = httpErrorString err }, Cmd.none )



-- View


view : Model -> Html.Html Msg
view model =
    layout [ width fill, height fill, padding 5, Font.size 24 ] <| viewAll model


viewAll : Model -> Element Msg
viewAll model =
    column []
        [ row [] [ text "Error: ", text model.error ]
        , Input.button [ Border.width 1, Border.rounded 5 ]
            { onPress = Just ClickedFetch
            , label = text "Fetch"
            }
        ]


type alias Post =
    { userId : Int
    , id : Int
    , title : String
    , completed : Bool
    }


fetchPost : (Result Http.Error Post -> msg) -> Cmd msg
fetchPost msg =
    let
        task : Task.Task Http.Error Post
        task =
            Http.task
                { method = "GET"
                , headers = []
                , url = "https://jsonplaceholder.typicode.com/todos/1"
                , body = Http.emptyBody
                , timeout = Nothing
                , resolver = Http.stringResolver <| handleJsonResponse <| decodePost
                }
    in
    task
        |> Retry.with
            [ Retry.maxDuration 7000
            , Retry.exponentialBackoff { interval = 500, maxInterval = 3000 }
            ]
        |> Task.attempt msg


handleJsonResponse : Decoder a -> Http.Response String -> Result Http.Error a
handleJsonResponse decoder response =
    case response of
        Http.BadUrl_ url ->
            Err (Http.BadUrl url)

        Http.Timeout_ ->
            Err Http.Timeout

        Http.BadStatus_ { statusCode } _ ->
            Err (Http.BadStatus statusCode)

        Http.NetworkError_ ->
            Err Http.NetworkError

        Http.GoodStatus_ _ body ->
            case decodeString decoder body of
                Err _ ->
                    Err (Http.BadBody body)

                Ok result ->
                    Ok result


decodePost : Decoder Post
decodePost =
    Decoder.succeed Post
        |> required "userId" int
        |> required "id" int
        |> required "title" string
        |> required "completed" bool


httpErrorString : Http.Error -> String
httpErrorString error =
    case error of
        Http.BadBody message ->
            message

        Http.BadStatus statusCode ->
            "Server responded: " ++ String.fromInt statusCode

        Http.BadUrl url ->
            "Invalid URL: " ++ url

        Http.NetworkError ->
            "Network problem"

        Http.Timeout ->
            "Request timeout"



-- Subscription


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
