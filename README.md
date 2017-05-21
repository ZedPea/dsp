# dsp
Messing around with synthesizer-core

https://hackage.haskell.org/package/synthesizer-core-0.8.1

## Installation:

#### Install prerequisites
You need stack installed.

##### Debian based:
`sudo apt-get install haskell-stack sox libsndfile`

##### Arch based:
`sudo pacman -S stack sox libsndfile`

Commands should be similar for other distributions.

#### Clone the repository
git clone https://github.com/ZedPea/dsp.git && cd dsp

#### Setup stack and compile

`stack setup`

`stack install`

## Running

Either add ~/.local/bin to your path and run `dsp`

Or, run `stack exec dsp`

Make sure you are in the dsp folder, as this is where the sourced flacs are.
