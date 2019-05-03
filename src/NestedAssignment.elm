module NestedAssignment exposing (Bar, Foo, Model, initModel, setBar, setBaz, setFoo, update, updateBaz)


type alias Bar =
    { baz : String }


type alias Foo =
    { bar : Bar }


type alias Model =
    { foo : Foo }


initModel : Model
initModel =
    { foo = { bar = { baz = "hello world" } } }


setFoo : (Foo -> Foo) -> Model -> Model
setFoo fn model =
    { model | foo = fn model.foo }


setBar : (Bar -> Bar) -> Foo -> Foo
setBar fn foo =
    { foo | bar = fn foo.bar }


setBaz : String -> Bar -> Bar
setBaz str bar =
    { bar | baz = str }


updateBaz : String -> Model -> Model
updateBaz str =
    setFoo <| setBar <| setBaz str


update : Model
update =
    initModel |> updateBaz "foo bar baz"
