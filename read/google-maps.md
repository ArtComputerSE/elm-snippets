# Elm + Google-map web component
In the Elm community ‘use a Port’ is a near-constant refrain, if you need to interoperate with Javascript. But more recently, web components have become a workable alternative. This post is a companion to Elm (0.18) & Ports - Google Maps quick and dirty, providing a guide to using the official Google-maps web component with Elm 0.19. The full code is here.

## Installing Elm
I’m going to use my own elm-webpack-starter, and to that I’m going to add support for the .env file with dotenv-webpack so as to keep my API key out of the repo. Up to you how you manage the key yourself.

## Installing the web component
The official Google-map web component is distributed as a Polymer project. It has fallen a little out of date and has limited functionality. It will not work out of the box with a Elm + Webpack dev environment, as the code foresees a now deprecated ‘html-import’ process, whereas we are using javascript imports.

So my repo has a copy of the original source code in the` /src/assets `directory. These have some dependencies and the easiest way to get them is to install the original library anyway:

`npm install --save @em-polymer/google-map`

The changes I’ve so far made to get the code working are to google-map/google-map.js and to google-map/google-map-marker.js:

to the import statements
adding html to the _template definition (a requirement for js imports)
changes to the marker events code per this unmerged PR, which helps with an Elm hot-loading environment such as we are using
Note the import statement in index.js

Using the web component in Elm
Adding custom markup to an Elm view is straightfoward - here we need a custom node and 3 custom attributes, as follows:

    gmap : Model -> Html Msg
    gmap model =
        Html.node "google-map"
            [ Attribute.attribute "api-key" model.gkey
            , Attribute.attribute "latitude" <| String.fromFloat model.center.lat
            , Attribute.attribute "longitude" <| String.fromFloat model.center.lng
            ]
            []

        
If you want to add markers as children, these can be created with

    gmarker : LatLng -> Html msg
    gmarker { lat, lng } =
        Html.node "google-map-marker"
            [ Attribute.attribute "latitude" <| String.fromFloat lat
            , Attribute.attribute "longitude" <| String.fromFloat lng
            ]
            []
## Adding more functionality
The web component comes with a certain amount of functionality, but to do more requires to you to manipulate the google map object directly in javascript. For Elm, that means going back to a port!

What we want is for the web component to emit better events, as we can access those directly in Elm. Let’s make the center and the bounds available. It will be simple enough for you to add more / other data if you wish.

When I looked at the code I was surprised that there seemed to be no data in the events already, but logging the event parameter here indicated that it was always null - I admit I do not know why.

      _forwardEvent(name) {
        this._listeners[name] = google.maps.event.addListener(this.map, name, (event) =>
            this.fire(`google-map-${name}`, event)
        );
      },
  
So I changed the code to:

      _forwardEvent(name) {
        this._listeners[name] = google.maps.event.addListener(this.map, name, (event) => {
          let bounds = this.map.getBounds().toJSON();
          let center = this.map.getCenter().toJSON();
    
          this.fire(`google-map-${name}`, {
            center,
            bounds
          });
        });
      },
  
Note that:

* drag events are only passed if the dragEvents attribute is set to true
* the raw event is prefixed with google-map-; and
* the data passed to fire will end up in the "detail" field of the event object.

Armed with that knowledge, we can build an Elm event handler

    view model =
        Html.node "google-map"
            [ Attribute.attribute "api-key" model.gkey
            , Attribute.attribute "latitude" <| String.fromFloat model.center.lat
            , Attribute.attribute "longitude" <| String.fromFloat model.center.lng
            , Attribute.attribute "drag-events" "true"
            , Events.on "google-map-dragend" (Decode.at [ "detail", "bounds" ] decodeMapBounds |> Decode.map OnDragEnd)
            ]
            [ gmarker model.center ]
    
    
    type alias MapBounds =
        { north : Float
        , east : Float
        , south : Float
        , west : Float
        }
    
    
    decodeMapBounds : Decoder MapBounds
    decodeMapBounds =
        Decode.map4 MapBounds
            (Decode.field "north" Decode.float)
            (Decode.field "east" Decode.float)
            (Decode.field "south" Decode.float)
            (Decode.field "west" Decode.float)
    
