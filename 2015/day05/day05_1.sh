#!/bin/bash

# ./day05_2.sh input.txt

grep -E '([a-z])\1' $1 | grep -E '[aeiou][a-z]*[aeiou][a-z]*[aeiou]' |
grep -Evc '(ab|cd|pq|xy)'
