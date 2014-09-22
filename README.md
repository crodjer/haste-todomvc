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
Since this is not a conventional JavaScript application and an Haskell one
instead, most of the application logic lies in `src` directory:

  - `src/Data/Todo.hs`: Defines the relevant data structures.
  - `src/Todo/DOM.hs`: Defines DOM interactions and most of the business logic.
  - `src/Todo.hs`: Initializes the application.


## Running
The app should run directly. But to make changes you'll need to have
haste-compiler installed (`hastec` available in your PATH). To build run: `make`

> Currently, there are a couple of patches that I need which are not merged
> yet. So, it will only function correctly with
> [my fork](https://github.com/crodjer/haste-compiler/) for now.

To run the app, spin up an HTTP server at any port and visit
`http://localhost:port/`.


## Credit

 - [Haste compiler](https://github.com/valderman/haste-compiler)
 - App design and templates from: <https://github.com/tastejs/todomvc/tree/master/template>
 - This TodoMVC application was created by [crodjer](https://github.com/crodjer/).
