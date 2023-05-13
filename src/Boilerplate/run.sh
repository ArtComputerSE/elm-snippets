#!/usr/bin/env bash

# Change the start page to the one you use. May not want to use index.html as it is easy to overwrite
# it by mistake when running the compiler.

elm-live src/Main.elm --start-page=index-bootstrap.html --port 1337 -- --output=elm.js --debug
