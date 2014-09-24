# Haste TodoMVC Example

>  Haste is a dialect of the Haskell programming language geared towards web
>  applications. Haste supports the full Haskell language, including most GHC
>  extensions but comes with a different set of standard libraries, implementing
>  native support for modern web technologies such as WebSockets, LocalStorage,
>  Canvas and others.

> _[Haste Website](http://haste-lang.org/)_

## Learning how to use Haste

The [Haste Website](http://haste-lang.org/) is a great resource for getting started.

Here are some links you may find helpful:

* [Documentation](http://haste-lang.org/#docs)
* [API Reference](http://hackage.haskell.org/package/haste-compiler)
* [Haste on GitHub](https://github.com/valderman/haste-compiler)

## Implementation

How is the app structured? Are there deviations from the spec? If so, why?
Since this is not a conventional JavaScript application and but is written in
Haskell instead. The application logic likes in `src` directory.

  - `src/Data/Todo.hs`: Defines the relevant data structures.
  - `src/Todo/DOM.hs`: Defines DOM interactions and most of the business logic.
  - `src/Todo.hs`: Initializes the application.

Currently, out of MVC we don't really emulate a `C` but just `M` and `V`. `V`
has the logic of controller, router and view are all tangled in `DOM.hs`.


## Running

A recently built app will be available on the branch `gh-pages`.  To run the
app, spin up an HTTP server at any port and visit `http://localhost:port/`.

To make changes you'll need to have haste-compiler installed (`hastec` available
in your PATH). To build run: `bower install` and then `make`

## Credit

 - [Haste compiler](https://github.com/valderman/haste-compiler)
 - App design and templates from: <https://github.com/tastejs/todomvc/tree/master/template>
 - This TodoMVC application was created by [crodjer](https://github.com/crodjer/).
