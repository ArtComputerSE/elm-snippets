port module Notification.Port exposing (notificationClicked, notificationError, permissionChanged, requestPermission, sendNotification)

import Json.Encode as Encode


port requestPermission : () -> Cmd msg


port sendNotification : Encode.Value -> Cmd msg


port permissionChanged : (String -> msg) -> Sub msg


port notificationClicked : (String -> msg) -> Sub msg


port notificationError : (String -> msg) -> Sub msg
