## Description

This is a very basic space invaders implementation modelled on a daring and  excellent livecoding talk given by Mary Rose Cook, http://vimeo.com/105955605. I thought it would be fun to 1) do it in Clojure, and 2) do it in a functional style by using the new fun-mode of quil introduced in 2.1.0.

## Getting it running

This project requires Leiningen; you can find instructions on how to install it here: http://www.leiningen.org/

Download the project to a local directory:

    `git clone https://github.com/quephird/space-invaders`

... and run the following:

    `lein repl`

Once in the Clojure REPL, issue the following:

    `(load-file "./src/space_invaders/core.clj")
    (in-ns 'function-plotter.core)
    (-main)`

## Current features

This is a _very_ basic game, my first actually, and so is nowhere near being a real game. So far, the only features that I've implemented are:

* A moving player
* A moving patrol of baddies
* SPRITES!!!!
* Firing bullets at the baddies
* Collision detection between player bullets and baddies
* Basic scoring

## Goals

I need to try to do at least some of the following:

* Bullets from the baddies
* A patrol that moves downward and with increasing speed
* SOUND
* Player death
* Limited player lives
* Obstacles
* Levels, with increasing difficulty
* Baddies that sweep down and then back up
* High score maintenance
* Background music
* Background graphics

## Useful links

The fabulous quil library written in Clojure, https://github.com/quil/quil 

How to do things The Functional Way, https://github.com/quil/quil/wiki/Functional-mode-(fun-mode)

The equally kewl Processing environment: http://www.processing.org/

## License

Copyright (C) 2014, ⅅ₳ℕⅈⅇℒℒⅇ Ƙⅇℱℱoℜⅆ Distributed under the Eclipse Public License.