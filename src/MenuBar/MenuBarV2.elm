module MenuBar.MenuBarV1 exposing (Menu, MenuBar, MenuItem(..), viewMenuBar)

{-
   A menu bar implemented in elm-ui. This version has the presentation dynamics (i.e. show or hide) separate from
   the menu structure itself. The title of each item doubles as its identity.
-}

import Dict exposing (Dict)
import Element exposing (Element, below, column, el, fill, none, text, width, wrappedRow)
import Element.Input as Input


type alias Show =
    Dict String Bool


type alias MenuBar =
    { menus : List Menu
    , show : Show
    }


type alias Menu =
    { title : String
    , menuItems : List MenuItem
    }


type MenuItem
    = SubMenu Menu
    | Item String


type Msg
    = ItemSelected String
    | MenuSelected String


createMenuBar =
    MenuBar menus


menus : List Menu
menus =
    List.map createMenu [ ( "File", fileItems ), ( "Edit", editItems ) ]


createMenu : ( String, List MenuItem ) -> Menu
createMenu ( title, items ) =
    Menu title items


fileItems : List MenuItem
fileItems =
    [ Item "Open", Item "New..", Item "Save" ]


editItems : List MenuItem
editItems =
    [ Item "Cut", Item "Copy", Item "Paste" ]


viewMenuBar : MenuBar -> Element Msg
viewMenuBar menuBar =
    wrappedRow [ width fill ] (List.map (viewMenu menuBar.show) menuBar.menus)


viewMenu : Show -> Menu -> Element Msg
viewMenu show menu =
    column [ below <| viewMenuItems show menu ]
        [ Input.button []
            { onPress = Just (MenuSelected menu.title)
            , label = el [] (text menu.title)
            }
        ]


viewMenuItems : Show -> Menu -> Element Msg
viewMenuItems show menu =
    let
        visible =
            case Dict.get menu.title show of
                Just True ->
                    True

                _ ->
                    False
    in
    if visible then
        column [] (List.map (viewMenuItem show) menu.menuItems)

    else
        none


viewMenuItem : Show -> MenuItem -> Element Msg
viewMenuItem show menuItem =
    case menuItem of
        SubMenu menu ->
            viewMenu show menu

        Item title ->
            Input.button []
                { onPress = Just (ItemSelected title)
                , label = el [] (text title)
                }
