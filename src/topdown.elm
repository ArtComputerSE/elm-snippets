-- Take an Json structure and parse it outermost to innermost

import Json.Decode as Decode
import Dict as Dict

-- Input data , see file site-ip.json

ourSitesBlob = """
    {  "Site1": {    "PC1": {      "ip": "x.x.x.x",      "version": "3"    },    "PC2": {     "ip": "x.x.x.x",      "version": "3"    }  },  "Site2": {    "PC1": {      "ip": "x.x.x.x",      "version": "3"    },    "PC2": {      "ip": "x.x.x.x",      "version": "3"    }  }}
    """

sites1 : Decode.Decoder (Dict.Dict String Decode.Value)
sites1 = Decode.dict Decode.value

{-
Decode.decodeString sites1 ourSitesBlob
result1 = Ok (Dict.fromList
    [
        ("Site1", { PC1 = { ip = "x.x.x.x", version = "3" }, PC2 = { ip = "x.x.x.x", version = "3" } })
        ,("Site2",{ PC1 = { ip = "x.x.x.x", version = "3" }, PC2 = { ip = "x.x.x.x", version = "3" } })
    ])
-}

sites2 : Decode.Decoder (Dict.Dict String (Dict.Dict String Decode.Value))
sites2 =
    Decode.dict (Decode.dict Decode.value)


-- Decode.decodeString sites2 ourSitesBlob
result2 =
 Ok (Dict.fromList
    [
        ("Site1",Dict.fromList
                [
                    ("PC1", { ip = "x.x.x.x", version = "3" })
                    ,("PC2",{ ip = "x.x.x.x", version = "3" })])
        ,("Site2",Dict.fromList
                [
                    ("PC1",{ ip = "x.x.x.x", version = "3" })
                    ,("PC2",{ ip = "x.x.x.x", version = "3" })])])


type alias Machine = { ip: String , version : String }

machine = Decode.map2 Machine (Decode.field "ip" Decode.string) (Decode.field "version" Decode.string)

sites3 : Decode.Decoder (Dict.Dict String (Dict.Dict String Machine))
sites3 =
    Decode.dict (Decode.dict machine)

-- Decode.decodeString sites3 ourSitesBlob
result3 =
    Ok (Dict.fromList
    [
        ("Site1",Dict.fromList
            [
                ("PC1",{ ip = "x.x.x.x", version = "3" })
                ,("PC2",{ ip = "x.x.x.x", version = "3" })])
        ,("Site2",Dict.fromList
            [
                ("PC1",{ ip = "x.x.x.x", version = "3" })
                ,("PC2",{ ip = "x.x.x.x", version = "3" })
            ]
        )
    ]
    )


