#!/bin/bash

set -o errexit

cd tests
elm make --output=tests.js --warn TestRunner.elm
node tests.js
