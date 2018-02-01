module HttpUtil exposing(explainHttp)

import Http

explainHttp : Http.Error -> String
explainHttp error =
    case error of
        Http.BadUrl u ->
            "That was a bad url: " ++ u
        Http.NetworkError ->
            "Network Error"
        _ ->
            "Other error"