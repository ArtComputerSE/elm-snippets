module DropDownWithInit exposing
    ( State, init
    , Msg
    , Config, basic, filterable, multi
    , withContainerAttributes, withPromptElement, withFilterPlaceholder, withSelectAttributes, withSearchAttributes, withOpenCloseButtons, withListAttributes
    , update, view
    , onOutsideClick
    )

{-| Elm UI Dropdown.

@docs State, init
@docs Msg
@docs Config, basic, filterable, multi
@docs withContainerAttributes, withPromptElement, withFilterPlaceholder, withSelectAttributes, withSearchAttributes, withOpenCloseButtons, withListAttributes
@docs update, view
@docs onOutsideClick

-}

import Browser.Dom as Dom
import Browser.Events exposing (onMouseDown)
import Element exposing (..)
import Element.Events as Events exposing (onMouseEnter)
import Element.Input as Input
import Html.Attributes
import Html.Events
import Json.Decode as Decode
import Task


type DropdownType
    = Basic
    | Filterable
    | MultiSelect


type alias InternalState item =
    { id : String
    , isOpen : Bool
    , selectedItems : List item
    , filterText : String
    , focusedIndex : Int
    }


{-| Type that holds the handlers for single and multiple selects. |
-}
type OnSelectMsg msg item
    = SingleItem (item -> Element msg)
    | MultipleSelection (List item -> Element msg)


{-| Opaque type that holds the current state

    type alias Model =
        { dropdownState : Dropdown.State String
        }

-}
type State item
    = State (InternalState item)


type alias InternalConfig item msg =
    { dropdownType : DropdownType
    , promptElement : Element msg
    , filterPlaceholder : Maybe String
    , dropdownMsg : Msg item -> msg
    , onSelectMsg : Maybe item -> msg
    , containerAttributes : List (Attribute msg)
    , selectAttributes : List (Attribute msg)
    , listAttributes : List (Attribute msg)
    , searchAttributes : List (Attribute msg)
    , selectionToPrompt : OnSelectMsg msg item
    , itemToElement : Bool -> Bool -> item -> Element msg
    , openButton : Element msg
    , closeButton : Element msg
    , itemToText : item -> String
    }


{-| Opaque type that holds the current config

    dropdownConfig =
        Dropdown.basic DropdownMsg OptionPicked Element.text Element.text

-}
type Config item msg
    = Config (InternalConfig item msg)


{-| Opaque type for the internal dropdown messages
-}
type Msg item
    = NoOp
    | OnBlur
    | OnClickPrompt
    | OnSelect item
    | OnFilterTyped String
    | OnKeyDown Key
    | OnClickOutside
    | HoverOver Int


type Key
    = ArrowDown
    | ArrowUp
    | Enter
    | Esc


{-| Create a new state. You must pass a unique identifier for each dropdown component.

    {
        ...
        dropdownState = Dropdown.init "country-dropdown"
    }

-}
init : String -> List item -> State item
init id initialSelections =
    State
        { id = id
        , isOpen = False
        , selectedItems = initialSelections
        , filterText = ""
        , focusedIndex = 0
        }


{-| Create a basic configuration. This takes:

    - The message to wrap all the internal messages of the dropdown
    - A message to trigger when an item is selected
    - A function to get the Element to display from an item, to be used in the select part of the dropdown
    - A function to get the Element to display from an item, to be used in the item list of the dropdown

    Dropdown.basic DropdownMsg OptionPicked Element.text Element.text

-}
basic :
    (Msg item -> msg)
    -> (Maybe item -> msg)
    -> (item -> Element msg)
    -> (Bool -> Bool -> item -> Element msg)
    -> Config item msg
basic dropdownMsg onSelectMsg itemToPrompt itemToElement =
    Config
        { closeButton = text "▲"
        , containerAttributes = []
        , dropdownMsg = dropdownMsg
        , dropdownType = Basic
        , filterPlaceholder = Nothing
        , itemToElement = itemToElement
        , selectionToPrompt = SingleItem itemToPrompt
        , itemToText = \_ -> ""
        , listAttributes = []
        , onSelectMsg = onSelectMsg
        , openButton = text "▼"
        , promptElement = el [ width fill ] (text "-- Select --")
        , searchAttributes = []
        , selectAttributes = []
        }


{-| Create a multiselect configuration. This takes:

    - The message to wrap all the internal messages of the dropdown
    - A message to trigger when an item is selected
    - A function to get the Element to display from the list of selected items, to be used in the select part of the dropdown
    - A function to get the Element to display from an item, to be used in the item list of the dropdown

    Dropdown.multiselect DropdownMsg OptionsPicked Element.text Element.text

-}
multi :
    (Msg item -> msg)
    -> (Maybe item -> msg)
    -> (List item -> Element msg)
    -> (Bool -> Bool -> item -> Element msg)
    -> Config item msg
multi dropdownMsg onSelectMsg itemsToPrompt itemToElement =
    Config
        { closeButton = text "▲"
        , containerAttributes = []
        , dropdownMsg = dropdownMsg
        , dropdownType = MultiSelect
        , filterPlaceholder = Nothing
        , itemToElement = itemToElement
        , selectionToPrompt = MultipleSelection itemsToPrompt
        , itemToText = \_ -> ""
        , listAttributes = []
        , onSelectMsg = onSelectMsg
        , openButton = text "▼"
        , promptElement = el [ width fill ] (text "-- Select --")
        , searchAttributes = []
        , selectAttributes = []
        }


{-| Create a filterable configuration. This takes:

    - The message to wrap all the internal messages of the dropdown
    - A message to trigger when an item is selected
    - A function to get the Element to display from an item, to be used in the select part of the dropdown
    - A function to get the Element to display from an item, to be used in the item list of the dropdown
    - A function to get the text representation from an item, to be used when filtering elements in the list

    Dropdown.basic DropdownMsg OptionPicked Element.text Element.text

-}
filterable :
    (Msg item -> msg)
    -> (Maybe item -> msg)
    -> (item -> Element msg)
    -> (Bool -> Bool -> item -> Element msg)
    -> (item -> String)
    -> Config item msg
filterable dropdownMsg onSelectMsg itemToPrompt itemToElement itemToText =
    Config
        { closeButton = text "▲"
        , containerAttributes = []
        , dropdownMsg = dropdownMsg
        , dropdownType = Filterable
        , filterPlaceholder = Just "Filter values"
        , itemToElement = itemToElement
        , selectionToPrompt = SingleItem itemToPrompt
        , itemToText = itemToText
        , listAttributes = []
        , onSelectMsg = onSelectMsg
        , openButton = text "▼"
        , promptElement = el [ width fill ] (text "-- Select --")
        , searchAttributes = []
        , selectAttributes = []
        }


{-| Sets the content of the Select, default is "-- Select --"

    Dropdown.withPromptElement (el [ Font.color (rgb255 123 123 123) ] <| text "Pick one") config

-}
withPromptElement : Element msg -> Config item msg -> Config item msg
withPromptElement promptElement (Config config) =
    Config { config | promptElement = promptElement }


{-| Sets the placeholder of the Filterable dropdown, default is "Filter values"

    Dropdown.withFilterPlaceholder "Type here..." config

-}
withFilterPlaceholder : String -> Config item msg -> Config item msg
withFilterPlaceholder placeholderString (Config config) =
    let
        placeholder =
            if String.isEmpty placeholderString then
                Nothing

            else
                Just placeholderString
    in
    Config { config | filterPlaceholder = placeholder }


{-| Sets the container visual attributes, default is empty

    Dropdown.withContainerAttributes [ width (px 300) ] config

-}
withContainerAttributes : List (Attribute msg) -> Config item msg -> Config item msg
withContainerAttributes attrs (Config config) =
    Config { config | containerAttributes = attrs }


{-| Sets the select visual attributes, default is empty

    Dropdown.withSelectAttributes [ Border.width 1, Border.rounded 5, paddingXY 16 8 ] config

-}
withSelectAttributes : List (Attribute msg) -> Config item msg -> Config item msg
withSelectAttributes attrs (Config config) =
    Config { config | selectAttributes = attrs }


{-| Sets the search visual attributes, default is empty

    Dropdown.withSearchAttributes [ Border.width 0, padding 0 ] config

-}
withSearchAttributes : List (Attribute msg) -> Config item msg -> Config item msg
withSearchAttributes attrs (Config config) =
    Config { config | searchAttributes = attrs }


{-| Sets the open and close buttons' visual attributes, default is empty

    Dropdown.withOpenCloseButtons { openButton = text "+", closeButton = "-" } config

-}
withOpenCloseButtons : { openButton : Element msg, closeButton : Element msg } -> Config item msg -> Config item msg
withOpenCloseButtons { openButton, closeButton } (Config config) =
    Config { config | openButton = openButton, closeButton = closeButton }


{-| Sets the item list visual attributes, default is empty

    Dropdown.withListAttributes [ Border.width 1, Border.rounded ] config

-}
withListAttributes : List (Attribute msg) -> Config item msg -> Config item msg
withListAttributes attrs (Config config) =
    Config { config | listAttributes = attrs }


closeOnlyIfNotMultiSelect : { a | dropdownType : DropdownType } -> { b | isOpen : Bool } -> Bool
closeOnlyIfNotMultiSelect config state =
    case config.dropdownType of
        MultiSelect ->
            state.isOpen

        _ ->
            False


{-| Update the component state

    DropdownMsg subMsg ->
        let
            ( updated, cmd ) =
                Dropdown.update dropdownConfig subMsg model.dropdownState model.items
        in
            ( { model | dropdownState = updated }, cmd )

-}
update : Config item msg -> Msg item -> State item -> List item -> ( State item, Cmd msg )
update (Config config) msg (State state) data =
    let
        ( newState, newCommand ) =
            case msg of
                NoOp ->
                    ( state, Cmd.none )

                OnBlur ->
                    ( { state | isOpen = closeOnlyIfNotMultiSelect config state }, Cmd.none )

                OnClickOutside ->
                    ( { state | isOpen = False }, Cmd.none )

                OnClickPrompt ->
                    let
                        isOpen =
                            not state.isOpen

                        cmd =
                            if isOpen then
                                Task.attempt (\_ -> NoOp) (Dom.focus (state.id ++ "input-search"))

                            else
                                Cmd.none
                    in
                    ( { state | isOpen = isOpen, focusedIndex = 0, filterText = "" }, Cmd.map config.dropdownMsg cmd )

                OnSelect item ->
                    let
                        cmd =
                            Task.succeed (Just item)
                                |> Task.perform config.onSelectMsg
                    in
                    ( { state
                        | isOpen = closeOnlyIfNotMultiSelect config state
                        , selectedItems =
                            case config.dropdownType of
                                MultiSelect ->
                                    -- if it was selected, we remove it from the list, otherwise include it
                                    if List.any ((==) item) state.selectedItems then
                                        List.filter ((/=) item) state.selectedItems

                                    else
                                        item :: state.selectedItems

                                _ ->
                                    [ item ]
                      }
                    , cmd
                    )

                OnFilterTyped val ->
                    ( { state | filterText = val }, Cmd.none )

                OnKeyDown key ->
                    let
                        newIndex =
                            case key of
                                ArrowUp ->
                                    if state.focusedIndex > 0 then
                                        state.focusedIndex - 1

                                    else
                                        0

                                ArrowDown ->
                                    if state.focusedIndex < List.length data - 1 then
                                        state.focusedIndex + 1

                                    else
                                        List.length data - 1

                                _ ->
                                    state.focusedIndex

                        isOpen =
                            case key of
                                Esc ->
                                    False

                                Enter ->
                                    False

                                _ ->
                                    True

                        focusedItem =
                            data
                                |> List.indexedMap (\i item -> ( i, item ))
                                |> List.filter (\( i, _ ) -> i == state.focusedIndex)
                                |> List.head
                                |> Maybe.map Tuple.second

                        ( cmd, newSelectedItem ) =
                            case key of
                                Enter ->
                                    ( Task.succeed focusedItem
                                        |> Task.perform config.onSelectMsg
                                    , case focusedItem of
                                        Nothing ->
                                            []

                                        Just item ->
                                            [ item ]
                                    )

                                _ ->
                                    ( Cmd.none, state.selectedItems )
                    in
                    ( { state | selectedItems = newSelectedItem, focusedIndex = newIndex, isOpen = isOpen }, cmd )

                HoverOver int ->
                    ( { state | focusedIndex = int }, Cmd.none )
    in
    ( State newState, newCommand )


{-| Render the view

    Dropdown.view dropdownConfig model.dropdownState model.items

-}
view : Config item msg -> State item -> List item -> Element msg
view (Config config) (State state) data =
    let
        containerAttrs =
            [ idAttr state.id
            , below body
            ]
                ++ config.containerAttributes

        filter item =
            String.contains (state.filterText |> String.toLower)
                (item |> config.itemToText |> String.toLower)

        filteredData =
            data
                |> List.filter filter

        trigger =
            triggerView config state

        body =
            bodyView config state filteredData
    in
    column
        containerAttrs
        [ el [ width fill, below body ] trigger
        ]


triggerView : InternalConfig item msg -> InternalState item -> Element msg
triggerView config state =
    let
        selectAttrs =
            [ onClick (config.dropdownMsg OnClickPrompt)
            , onKeyDown (config.dropdownMsg << OnKeyDown)
            , tabIndexAttr 0
            , referenceAttr state
            ]
                ++ (if config.dropdownType == Basic then
                        [ onBlurAttribute config state ]

                    else
                        []
                   )
                ++ config.selectAttributes

        prompt =
            el [ width fill ] <|
                case state.selectedItems of
                    [] ->
                        config.promptElement

                    xs ->
                        case config.selectionToPrompt of
                            SingleItem f ->
                                case List.head xs of
                                    Nothing ->
                                        config.promptElement

                                    Just x ->
                                        f x

                            MultipleSelection f ->
                                f xs

        search =
            case config.dropdownType of
                Basic ->
                    prompt

                MultiSelect ->
                    prompt

                Filterable ->
                    Input.search
                        ([ idAttr (state.id ++ "input-search")
                         , focused []
                         , onClickNoPropagation (config.dropdownMsg NoOp)
                         , onBlurAttribute config state
                         ]
                            ++ config.searchAttributes
                        )
                        { onChange = config.dropdownMsg << OnFilterTyped
                        , text = state.filterText
                        , placeholder =
                            config.filterPlaceholder
                                |> Maybe.map (text >> Input.placeholder [])
                        , label = Input.labelHidden "Filter List"
                        }

        ( promptOrSearch, button ) =
            if state.isOpen then
                ( search, el [] config.closeButton )

            else
                ( prompt, el [] config.openButton )
    in
    row selectAttrs [ promptOrSearch, button ]


bodyView : InternalConfig item msg -> InternalState item -> List item -> Element msg
bodyView config state data =
    if state.isOpen then
        let
            items =
                column
                    config.listAttributes
                    (List.indexedMap (itemView config state) data)

            body =
                el
                    [ height fill
                    , width fill
                    ]
                    items
        in
        body

    else
        none


itemView : InternalConfig item msg -> InternalState item -> Int -> item -> Element msg
itemView config state i item =
    let
        itemAttrs =
            [ onClick <| config.dropdownMsg (OnSelect item)
            , onMouseEnter <| config.dropdownMsg <| HoverOver i
            , referenceAttr state
            , tabIndexAttr -1
            , width fill
            , pointer
            ]

        selected =
            List.member item state.selectedItems

        highlighed =
            i == state.focusedIndex
    in
    el
        itemAttrs
        (config.itemToElement selected highlighed item)



-- helpers


idAttr : String -> Attribute msg
idAttr id =
    Html.Attributes.id id
        |> htmlAttribute


tabIndexAttr : Int -> Attribute msg
tabIndexAttr tabIndex =
    Html.Attributes.tabindex tabIndex
        |> htmlAttribute


referenceDataName : String
referenceDataName =
    "data-dropdown-id"


referenceAttr : InternalState item -> Attribute msg
referenceAttr model =
    Html.Attributes.attribute referenceDataName model.id
        |> htmlAttribute


onClick : msg -> Attribute msg
onClick message =
    Events.onClick message


onClickNoPropagation : msg -> Attribute msg
onClickNoPropagation msg =
    Html.Events.custom "click"
        (Decode.succeed
            { message = msg
            , stopPropagation = True
            , preventDefault = True
            }
        )
        |> htmlAttribute


onKeyDown : (Key -> msg) -> Attribute msg
onKeyDown msg =
    let
        stringToKey str =
            case str of
                "ArrowDown" ->
                    Decode.succeed ArrowDown

                "ArrowUp" ->
                    Decode.succeed ArrowUp

                "Enter" ->
                    Decode.succeed Enter

                "Escape" ->
                    Decode.succeed Esc

                _ ->
                    Decode.fail "not used key"

        keyDecoder =
            Decode.field "key" Decode.string
                |> Decode.andThen stringToKey
    in
    Html.Events.on "keydown" (Decode.map msg keyDecoder)
        |> htmlAttribute


onBlurAttribute : InternalConfig item msg -> InternalState item -> Attribute msg
onBlurAttribute config state =
    let
        -- relatedTarget only works if element has tabindex
        dataDecoder =
            Decode.at [ "relatedTarget", "attributes", referenceDataName, "value" ] Decode.string

        attrToMsg attr =
            if attr == state.id then
                config.dropdownMsg NoOp

            else
                config.dropdownMsg OnBlur

        blur =
            Decode.maybe dataDecoder
                |> Decode.map (Maybe.map attrToMsg)
                |> Decode.map (Maybe.withDefault <| config.dropdownMsg OnBlur)
    in
    Html.Events.on "blur" blur
        |> htmlAttribute


isOutsideDropdown : String -> Decode.Decoder Bool
isOutsideDropdown dropdownId =
    Decode.oneOf
        [ Decode.field "id" Decode.string
            |> Decode.andThen
                (\id ->
                    if dropdownId == id then
                        Decode.succeed False

                    else
                        Decode.fail "continue"
                )
        , Decode.lazy (\_ -> isOutsideDropdown dropdownId |> Decode.field "parentNode")
        , Decode.succeed True
        ]


{-| Emits the internal event 'OnClickOutside' and closes the multi select dropdown, works well with subscriptions. Takes:

    - dropdownId: Id of the HTML element from which you want to be notified whenever it is clicked outside!
    - dropdownMsg: The message to wrap all the internal messages of the dropdown

    subscriptions : Model -> Sub Msg
    subscriptions model =
        onMouseDown (Dropdown.outsideTarget "my-dropdown" DropdownMsg)

-}
outsideTarget : String -> (Msg item -> msg) -> Decode.Decoder msg
outsideTarget dropdownId dropdownMsg =
    Decode.field "target" (isOutsideDropdown dropdownId)
        |> Decode.andThen
            (\isOutside ->
                if isOutside then
                    Decode.succeed <| dropdownMsg OnClickOutside

                else
                    Decode.fail "inside dropdown"
            )


{-| Serves as a subscription to know when the user has clicked outside a certain dropdown

    - dropdownState: State of the dropdown we want to subscribe to
    - dropdownMsg: The message to wrap all the internal messages of the dropdown

    subscriptions : Model -> Sub Msg
    subscriptions model =
        Dropdown.onOutsideClick model.dropdownState DropdownMsg

-}
onOutsideClick : State item -> (Msg item -> msg) -> Sub msg
onOutsideClick (State state) dropdownMsg =
    onMouseDown (outsideTarget state.id dropdownMsg)
