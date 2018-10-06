#!/bin/bash
if [ -z $1 ]; then echo "Nothing to find"; elif [ $(find * | grep $1) ]; then echo "Found $1"; else echo "Can't find $1"; fi