#!/bin/bash

# ./day05_2.sh input.txt

grep -E '([a-z]{2})[a-z]*\1' $1 | grep -Ec '([a-z])[a-z]\1'
