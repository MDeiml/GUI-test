# gui-test

## Idea

The idea is to create a GUI library which is (mostly) written in Haskell and thus allows for a functional style when being used.
The main type of this library is ```Widget g r p i o``` implementing the arrow class. A variable of type ```Widget``` is a computation taking a value of type ```i``` and outputing a value of type ```o``` while being able to send commands (mostly to render stuff) of type ```r``` which often is ```Cmd t```.
It can also read a global state of type ```g``` which contains things like input events, loaded resources or the current time.
To be able to compute layout, Widgets give layout paramters of type ```p``` to their parent nodes, which compute ```Bounds``` for each layout parater received. Layout parameters are often of type ```(LayoutParameter, Bool)```, where the ```Bool``` is ```True``` only if the respective bounds need to be recalulated.
A Layout can be seen as a function ```[p1] -> (Bounds -> [Bounds], p2)``` which for children parameters of type ```p1``` can compute the childrens bounds from its own bounds and parameters of type ```p2``` for its parent.
With those type signatures, the ```p``` output of widgets needs to be lazily independent of the bounds input. This decision was made as widgets need to be able to both make the parameters depend on its input (for example in a label, which can have variable length) and to make the output depend on its bounds (for example a button needs to know its size). 

## TODO

* More Drawables?
    * Lines
    * Rectangle outlines (~ lines)
* Create components
    * Add 9-patch resources
    * Text input
* Reorganize files
* Rename some stuff
* Better design
    * Better labels
* Resource loading during computations
* Don't redraw every frame
* Automatic font loading from font names (crossplatform)
* Concurrency
* Documentation
