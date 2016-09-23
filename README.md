# Bender

A solution to https://www.codingame.com/training/medium/bender-episode-1 from CodinGame, along with a simple terminal visualization of the game. A simple but non-trivial example of using pattern matching and the IO and State monads in Haskell.

## Overview

Bender begins from the starting point `@` in the map and seeks the suicide booth `$`, where he explodes. His course is completely deterministic and is defined by the different map symbols who change his direction or behavior, for a full description check https://www.codingame.com/training/medium/bender-episode-1.

In the beginning you are asked to pick one of the given maps (write the number and press `enter`). When selected, you can start with `enter` and then navigate the history of Bender's moves by `k` and `l`. `k` takes you one step to the past and `l` one step to the future. Have fun! (Warning! If you haven't tried to solve the puzzle you might find this very boring!)  

## Setup

Simply run the 'Bender-exe' file.

## Known issues

To exit the application one needs to break the process, i.e. press `ctrl + C`. In order to try a different map one needs to restart the application.

## Implementation

There are actually two data structures that keep the information of the game: `Bender` and the `CityMap`. The state is a stack of tuples `(Bender, CityMap)`. A new configuration is pushed when moving to the future and the last configuration pops when going back to the past. Those are controlled respectively by the `update` and `undo` instances of the State monad. There is a third instance, `doNothing`, which just preserves the current state. All those elements live in `src/Lib.hs`. One could seperate them in files, but they were deliberately put together in order to keep this an (almost) one page app.

The `singleAction` is the function which given a single state returns an IO monad which does the following:

1. Reads the user's key input
2. Maps the input to actions: `'l' -> undo, 'k' -> update, everything else -> doNothing`
3. Applies the action to the input state
4. Prints the map of the updated state
5. Returns the updated state

This function is run in an infinite recursion which feeds each time the new state to `singleAction`. All this happens in `app/Main.hs`.


After reviewing my code, I feel that manipulating the State monad inside the IO monad is the wrong way to approach things. The next attempt would be to do the opossite, loop in a state monad which has a default initial state and perform the IO actions there. Any ideas or corrections would be more than welcome :)  

## License

Copyright © 2016 Marios Koulakis

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>
