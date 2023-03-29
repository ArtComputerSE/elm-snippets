port module Notification.Port exposing (requestPermission, sendNotification)

import Json.Encode as Encode


port sendNotification : Encode.Value -> Cmd msg


port requestPermission : () -> Cmd msg
