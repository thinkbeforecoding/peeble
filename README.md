# Peeble. A F# -> PHP transpiler

## Why

I wrote a game using Fable Elmish that has more that 2000LoC of non-trivial game logic executed both in the browser and on the server. When I needed to port it to a platform accepting only PHP backend, the simplest option would have been to rewrite the server side to php. But it would have led to heavy code duplication in two different languages.

This is why I tried to transpile F# to PHP the same way Fable does to JavaScript.

## How it works

Peeble use Fable transforms to parse F# files and create a simpler intermediate representation. It then convert this to a PHP model before serializing it to text.


## Quick start

You need first to get [dotnet cli installed](https://dotnet.microsoft.com/download).

The install peeble as a global dotnet tool:

    dotnet tool install --global peeble-cli --version 0.1.0-alpha

Once done, you can call it on a fsproj to convert it to php:

    peeble sample.fsproj --output sample.inc.php

If you don't specify the output, the result is printed to the console.

You can install it as a local tool if you prefere:

    dotnet new tool-manifest
    dotnet tool install peeble-cli --version 0.1.0-alpha 

You can the call it as a local tool:

    dotnet peeble sample.fsproj --output sample.inc.php

The result is a inc.php file containing the code converted to PHP.

As your code will make calls to FSharp.Core functions for standard operations on lists, maps and other system types, you need to include their implementation. Add an include on [FSharp.Core.php](./tree/master/src/FSharp.Core) that will reference other necessary files.

## Build

On windows, run :

    ./build.cmd

On linux:

    ./build.sh

## Interop

### Records

Records are compiled as PHP classes with public fields. It implemnents equality by value.

### Unions

Unions are compiled as a base class and derived classes for each cases, the same way F# does in .Net.

### Options

Like in Fable, Option are implemented using PHP NULL.

### Array

Peeble use PHP Arrays to implement F# arrays. The Array module is called FSharpArray to avoid name clash.

### List

F# Lists are implemented in FSharp List as a linked list.

### Map/Set

Peeble comes with FSharp Map and Set implemented with in depth equality.

### Emit

To call function that have no been defined in F#, create a function with the expeced signature and mark it with the emit attribute to indicate the PHP code to emit at the call site:

    type Php =
        [<Emit("_($0)")>]
        static member translate s = s

        [<Emit("sprintf(_($0),$1...)")>]
        static member translatef(fmt : string, [<ParamArray>]args : obj[]) = fmt

Here calling `Php.translate("Hello")` will generate:

    _("Hello");

## Contributing

There are still many functions missing in the PHP implementation of FSharp.Core functions. As you discover some functions missing, feel free to submit implementation.

If you find expressions that cannot be transpiled by Peeble, please report them an issue, with - if possible - a minimal example for the code that cannot compile and the expected result.
