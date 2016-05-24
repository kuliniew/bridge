# Bridge

[![Build Status](https://travis-ci.org/kuliniew/bridge.svg?branch=master)](https://travis-ci.org/kuliniew/bridge)

A tool for practicing bridge against computer opponents.

## Live Demo

If you don't feel like running your own copy, you can [check out the live demo](https://kuliniew.github.io/bridge/).

## Running

You must have [Elm](http://elm-lang.org/) installed.

To compile:

```
./build.sh
```

To run, open `index.html` in your favorite browser.

To test (requires [elm-test](https://www.npmjs.com/package/elm-test)):

```
elm test
```

## Limitations

This is _very_ incomplete currently.  Only a small subset of [Standard American Yellow Card (SAYC)](https://en.wikipedia.org/wiki/Standard_American#SAYC) bidding is implemented so far.  The trick-taking part of the game is nonexistent.  The bots' hands are all exposed to facilitate debugging.

I'm writing this both as a way to learn Elm and as a way to learn bridge.  Maybe don't model your own Elm programs after this.
