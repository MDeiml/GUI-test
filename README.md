# gui-test

## Idea

The idea is to create a GUI library which is (mostly) written in Haskell and thus allows for a functional style when being used.
The main type of this library is `Widget p m i o` implementing the arrow class. A variable of type `Widget` is a computation which also some extra outputs and inputs.
A widget is essantialy a computation `i -> m o`, which has extra state and also outputs layout parameters of type `p`.

### Layouts

A Layout can be seen as a function `[p1] -> (Bounds -> [Bounds], p2)` which for children parameters of type `p1` can compute the childrens bounds from its own bounds and parameters of type `p2` for its parent.

### The GUI monad

The `m` type parameter is by standard `GUI t`, which is essantialy a combination of the IO monad, a `Reader (Globals t)` monad and a `Writer (Cmd t)` monad.
The `Globals t` variable contains input events, the current time and resources with textures of type `t`. A `Cmd t` can be an output to draw something or a IO Action.

### The Renderer typeclass

The backend for this library is fully abstracted and can be exchanged by using a different renderer. The renderer is responsible for drawing to the screen, input handling and resource loading.

## TODO

* More Drawables?
* Create components
* Reorganize files
* Rename some stuff
* Better design
* Don't redraw every frame
* Automatic font loading from font names (crossplatform)
* Concurrency
* Documentation
