port module Notification.Port exposing (permissionChanged, requestPermission, sendNotification)

import Json.Encode as Encode


port sendNotification : Encode.Value -> Cmd msg


port permissionChanged : (String -> msg) -> Sub msg


port requestPermission : () -> Cmd msg
