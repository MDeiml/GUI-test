# gui-test

## Idea

The idea is to create a GUI library which is (mostly) written in Haskell and thus allows for a functional style when being used.
The main type of this library is ```Widget g r p i o``` implementing the arrow class. A variable of type ```Widget``` is a computation which also some extra outputs and inputs:
* ```i``` and ```o``` are the input and output types respectively
* ```r``` is the type of an extra output for IO commands (most often of type ```Cmd t```) like rendering stuff or loading resources
* ```g``` is the type of the current frame's global state of type which contains things like input events, loaded resources or the current time.
* to be able to compute layout, Widgets give layout paramters of type ```p``` to their parent nodes, which compute ```Bounds``` for each layout parater received. Layout parameters are often of type ```(LayoutParameter, Bool)```, where the ```Bool``` is ```True``` only if the respective bounds need to be recalulated.

### Layouts

A Layout can be seen as a function ```[p1] -> (Bounds -> [Bounds], p2)``` which for children parameters of type ```p1``` can compute the childrens bounds from its own bounds and parameters of type ```p2``` for its parent.
With those type signatures, the ```p``` output of widgets needs to be lazily independent of the bounds input. This decision was made as widgets need to be able to both make the parameters depend on its input (for example in a label, which can have variable length) and to make the output depend on its bounds (for example a button needs to know its size). 

## TODO

* More Drawables?
* Create components
* Reorganize files
* Rename some stuff
* Better design
    * Better labels
* Don't redraw every frame
* Automatic font loading from font names (crossplatform)
* Concurrency
* Documentation
