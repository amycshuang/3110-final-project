# PoKaml Installation Instructions

## System Requirements
- Ocaml 4.10.x

***
## Installation Instructions
These instructions assume that you have OCaml and opam installed as per the
Install OCaml module in the CS 3110 Canvas.

1. Run the following command to install pkg-config.

    **Mac** (Pick One):  
    <code>brew install pkg-config</code>  
    <code>sudo port install pkgconfig</code>

2. You will need to install a couple packages in order to play the game. Run
the following command:  
`opam install oUnit yojson ASNITerminal graphics`

3. You will need an XServer to display the graphics:

    **Windows**: XMing [XMing] https://sourceforge.net/projects/xming/

    **Mac**: XQuartz [XQuartz] https://www.xquartz.org/

4. Run `make build` in the root directory and this will build the the necessary
modules needed to run this code.

5. Run `make play` to start playing!

***

## Commands for Building the System and Running

- `make build` builds the modules needed to run the project
- `make test` evaluates the test cases in test.ml
- `make play` initializes and plays the game