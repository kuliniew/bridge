#!/bin/bash

set -o errexit
set -o nounset

deployable_branch=master

if [ "$TRAVIS_BRANCH" != "$deployable_branch" ]; then
  echo "only $deployable_branch gets deployed, not $TRAVIS_BRANCH"
  exit 0
fi

rev=$(git rev-parse --short=7 HEAD)

git clone "https://$GH_TOKEN@github.com/kuliniew/bridge.git" _deploy
cd _deploy
git config user.name "Paul Kuliniewicz"
git config user.email "paul@kuliniewicz.org"
git checkout gh-pages

cp -r ../index.html ../elm.js ../assets/ .
git add --all
git commit --message "CI-triggered update for $rev"
git push --quiet origin gh-pages
