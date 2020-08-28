#!/bin/bash
# This will extract the first 500K from a large json file.  The output
#  will need to be cleaned using clean.sh
head -c 500000 $1 > $2
