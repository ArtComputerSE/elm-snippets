module EternalScroll exposing (Model, Msg(..), main, update, view)

import Browser
import Html exposing (div)
import InfiniteScroll
import Process
import Task


type Msg
    = InfiniteScrollMsg InfiniteScroll.Msg
    | OnDataRetrieved (Result String (List String))


type alias Model =
    { infiniteScroll : InfiniteScroll.Model Msg
    , content : List String
    }


main =
    Browser.document
        { init = always init
        , view = view
        , update = update
        , subscriptions = \x -> Sub.none
        }


init : ( Model, Cmd Msg )
init =
    ( { infiniteScroll = InfiniteScroll.init loadMore |> InfiniteScroll.offset 0 |> InfiniteScroll.direction InfiniteScroll.Bottom
      , content = []
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        InfiniteScrollMsg msg_ ->
            let
                ( infScroll, cmd ) =
                    InfiniteScroll.update InfiniteScrollMsg msg_ model.infiniteScroll
            in
            ( { model | infiniteScroll = infScroll }, cmd )

        OnDataRetrieved (Ok result) ->
            let
                content =
                    List.concat [ model.content, result ]

                infScroll =
                    InfiniteScroll.stopLoading model.infiniteScroll
            in
            ( { model | content = content, infiniteScroll = infScroll }, Cmd.none )

        OnDataRetrieved (Err _) ->
            ( { model | infiniteScroll = InfiniteScroll.stopLoading model.infiniteScroll }, Cmd.none )


view : Model -> Browser.Document Msg
view model =
    { title = "scroll"
    , body = [ div [] [] ]
    }


loadMore : InfiniteScroll.Direction -> Cmd Msg
loadMore dir =
    loadBook OnDataRetrieved


loadBook : (Result String (List String) -> Msg) -> Cmd Msg
loadBook msg =
    Task.perform msg <| slow <| Task.succeed messageLoader


slow : Task.Task x a -> Task.Task x a
slow task =
    Process.sleep 1000.0 |> Task.andThen (\_ -> task)


messageLoader : Result String (List String)
messageLoader =
    Result.Ok
        [ "abc"
        , "def"
        , "ghi"
        , "jkl"
        , "mno"
        , "pqr"
        , "stu"
        , "vwx"
        , "xyz"
        ]
