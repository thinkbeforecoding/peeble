# Peeble. A F# -> Php transpiler

## Why

I wrote a game using Fable Elmish that has more that 2000LoC of non-trivial game logic executed both in the browser and on the server. When I needed to port it to a platform accepting only Php backend, the simplest option would have been to rewrite the server side to php. But it would have led to heavy code duplication in two different languages.

This is why I tried to transpile F# to Php the same way Fable does to Javascript

## How it works

Peeble use Fable transforms to parse F# files and create a simpler intermediate representation. It then convert this to a Php model before serializing it to text.

## Build it

