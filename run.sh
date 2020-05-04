#!/usr/bin/env bash

cabal build && cabal exec diagrams-playground -- -o example.png -w 800
