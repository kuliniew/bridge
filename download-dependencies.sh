#!/bin/bash

# Work around https://github.com/elm-lang/elm-package/issues/163 in CI

set -o nounset

max_attempts=5
delay=15

if [ $# -ne 1 ]; then
  echo "usage: $0 <directory>"
  exit 1
fi

cd $1

n=0
until [ $n -ge $max_attempts ]; do
  elm package install -y && break
  n=$[$n+1]
  echo "Waiting $delay seconds before trying again..."
  sleep $delay
done

if [ $n -ge $max_attempts ]; then
  echo "Tried $max_attempts times but still can't download!"
  echo "There's likely something actually wrong with the build."
  exit 1
fi
