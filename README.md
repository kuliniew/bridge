# Bridge

A tool for practicing bridge against computer opponents.

## Running

You must have [Elm](http://elm-lang.org/) installed.

To compile:

```
./build.sh
```

To run:

```
elm reactor
```

Then open http://localhost:8000/ in your favorite browser.

To test (requires [elm-test](https://www.npmjs.com/package/elm-test)):

```
elm test
```

## Limitations

This is _very_ incomplete currently.  Only a small subset of [Standard American Yellow Card (SAYC)](https://en.wikipedia.org/wiki/Standard_American#SAYC) bidding is implemented so far.  The trick-taking part of the game is nonexistent.  The bots' hands are all exposed to facilitate debugging.

I'm writing this both as a way to learn Elm and as a way to learn bridge.  Maybe don't model your own Elm programs after this.
