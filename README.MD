A collection of code for playing around with an Arduino Uno.  
Arduino Sketches, C code, and Atom (Haskell EDSL) code is provided.

# Examples

## Blink

'blinks' the inbuild LED of the Arduino.

The C code was taken from [balau82's
post](https://balau82.wordpress.com/2011/03/29/programming-arduino-uno-in-pure-c/).

The Atom code was adapted from [this TechOverflow
post](https://techoverflow.net/blog/2014/07/28/yet-another-atom-arduino-blinker/)

## Spaceship Interface

Project 02 of the Arduino Projects Book, which comes with the Arduino Starter
Kit.  
Has a push-switch and some LEDs (green, red, red). The green LED is lit if
the switch is not pressed, red LEDs alternate if the switch is pressed.

# Usage Notes

### Arscons (SCons Script) Usage

Adapted from [suapapa/arscons](https://github.com/suapapa/arscons)
Assumes `scons`, and the Python package `pyserial` installed.

### Haskell Atom Usage

Assumes the cabal packages `atom` and `heredoc` are installed.

It can be easier to install these in a Cabal Sandbox (and the makefiles assume
these are created in the `atom/` folders).  
A sandbox can be created, with these dependencies installed by running:

```
cabal sandbox init
cabal install atom
cabal install heredoc
```

