module Main exposing (main)

import ElmTest

import Tests


main : Program Never
main = ElmTest.runSuite Tests.all
