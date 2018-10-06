#!/bin/sh

((o=$1-2*$2))
if [ $o -lt  0 ]; then $((o=0-o)); fi
echo $o