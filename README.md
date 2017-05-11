# λsudoku

[![Travis](https://img.shields.io/travis/marcelmoosbrugger/hsudoku.svg)](https://travis-ci.org/marcelmoosbrugger/hsudoku)
[![GitHub stars](https://img.shields.io/github/stars/marcelmoosbrugger/hsudoku.svg)](https://github.com/marcelmoosbrugger/hsudoku/stargazers)
[![GitHub issues](https://img.shields.io/github/issues/marcelmoosbrugger/hsudoku.svg)](https://github.com/marcelmoosbrugger/hsudoku/issues)
![GTK version](https://img.shields.io/badge/GTK-3.20-blue.svg)
[![GitHub license](https://img.shields.io/badge/license-MIT-blue.svg)](https://raw.githubusercontent.com/marcelmoosbrugger/hsudoku/master/LICENSE)

-------
<p align="center">
    <a href="#appearance">Appearance</a> &bull;
    <a href="#motivation">Motivation</a> &bull;
    <a href="#installation">Installation</a> &bull;
    <a href="#installation">Usage</a> &bull;
    <a href="#tests-and-coverage">Tests</a> &bull;
    <a href="#documentation">Documentation</a>
</p>

-------

## Appearance

<h3 align="center">
  <img width="400px" src="gui/menu.png" alt="Menu" />
  <img width="400px" src="gui/play.gif" alt="Gameplay" />
</h3>

## Motivation
λsudoku was born in a haskell universty course at the technical university in Vienna.
The goal was to create a neat looking and user-friendly haskell game using modern technologies and libraries, namely GTK3 and [haskell-gi](https://github.com/haskell-gi/haskell-gi).
The sudokus get loaded from the internet, parsed in haskell and made playable through a native gtk interface.
Special thank goes to [Kjell Ericson](https://kjell.haxx.se/sudoku/) for letting me use is [sudoku generator web-app](https://kjell.haxx.se/sudoku/).

## Installation
λsudoku has a few prerequesites. If they are already installed on your system, you can skip the following steps.
- At least GHC 8.0.2 and cabal 1.24
```
sudo add-apt-repository -y ppa:hvr/ghc
sudo apt-get update
sudo apt-get install cabal-install-1.24 ghc-8.0.2
export PATH=/opt/ghc/8.0.2/bin:/opt/cabal/1.24/bin:$PATH
```
- GTK+3.20 and other packages needed for the UI
```
sudo add-apt-repository ppa:gnome3-team/gnome3-staging
sudo apt-get update
sudo apt-get install build-essential libgtk-3-dev libgirepository1.0-dev libcairo2-dev libgdk-pixbuf2.0-dev
```
- The *happy* haskell package
```
cabal update
cabal install happy
```

With all dependencies in place, the hsudoku game can be installed:
```
git clone git@github.com:marcelmoosbrugger/hsudoku.git
cd hsudoku
cabal install --only-dependencies
cabal build
```

## Usage
If everything is installed the game can be run from the installation folder:
```
dist/build/hsudoku/hsudoku
```

## Tests and Coverage
Tests can easily be run with:
```
cabal test
```

## Documentation
Documentation of the code and the hackage package can be found on [hackage/hsudoku](https://hackage.haskell.org/package/hsudoku)

## Licence
This project is licensed under the terms of the MIT license. See the LICENSE file.
