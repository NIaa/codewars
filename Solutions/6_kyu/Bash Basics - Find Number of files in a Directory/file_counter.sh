#!/bin/bash
[ $1 ] || echo "Nothing to find" || exit 1
cd $1 || echo "Directory not found" || exit 1
echo "There are $(find . -maxdepth 1 -type f | wc -l) files in $(pwd)"