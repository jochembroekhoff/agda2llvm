#!/bin/env sh
find {./{src,app},Setup.hs} -type f -name '*.hs' -exec hindent --line-length 120 {} \;
