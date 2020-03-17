module MenuBar.MenuBarV1 exposing (Menu, MenuBar, MenuItem(..), viewMenuBar)

{-
   A menu bar implemented in elm-ui.
-}

import Element exposing (Element, below, column, el, fill, none, text, width, wrappedRow)
import Element.Input as Input


type alias MenuBar =
    { menus : List Menu
    }


type alias Menu =
    { id : Int
    , title : String
    , show : Bool
    , menuItems : List MenuItem
    }


type MenuItem
    = SubMenu Menu
    | Item String Int


type Msg
    = ItemSelected Int
    | MenuSelected Int


createMenuBar =
    MenuBar menus


menus : List Menu
menus =
    List.indexedMap createMenu [ ( "File", fileItems ), ( "Edit", editItems ) ]


createMenu : Int -> ( String, List MenuItem ) -> Menu
createMenu id ( title, items ) =
    Menu id title False items


fileItems : List MenuItem
fileItems =
    [ Item "Open" 1, Item "New.." 2, Item "Save" 3 ]


editItems : List MenuItem
editItems =
    [ Item "Cut" 1, Item "Copy" 2, Item "Paste" 3 ]


viewMenuBar : MenuBar -> Element Msg
viewMenuBar menuBar =
    wrappedRow [ width fill ] (List.map viewMenu menuBar.menus)


viewMenu : Menu -> Element Msg
viewMenu menu =
    column [ below <| viewMenuItems menu ]
        [ Input.button []
            { onPress = Just (MenuSelected menu.id)
            , label = el [] (text menu.title)
            }
        ]


viewMenuItems : Menu -> Element Msg
viewMenuItems menu =
    if menu.show then
        column [] (List.map viewMenuItem menu.menuItems)

    else
        none


viewMenuItem : MenuItem -> Element Msg
viewMenuItem menuItem =
    case menuItem of
        SubMenu menu ->
            viewMenu menu

        Item title id ->
            Input.button []
                { onPress = Just (ItemSelected id)
                , label = el [] (text title)
                }
