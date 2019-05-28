module ScrollToMessage exposing (scrollToMessage)

import Browser.Dom
import Task


scrollToMessage : String -> String -> Cmd msg
scrollToMessage id messageBox =
    Task.map2 Tuple.pair (Browser.Dom.getViewportOf messageBox) (Browser.Dom.getElement id)
        |> Task.andThen
            (\( info, e ) ->
                let
                    newY =
                        e.element.y + info.viewport.y - 175
                in
                Browser.Dom.setViewportOf messageBox 0 newY
            )
        |> Task.attempt (\_ -> NoOp)
