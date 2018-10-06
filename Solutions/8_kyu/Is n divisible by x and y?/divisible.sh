#!/bin/bash
if (($1 % $2 == 0)) && (($1 % $3 == 0)); then echo true; else echo false; fi 