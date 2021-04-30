#!/bin/bash

counter=0
while true; do
  result=$(echo -n "$1$counter" | md5sum)
  if [[ $result == 000000* ]]; then
    break
  fi
  let counter++
done

echo "$counter"
