#!/bin/bash
# This will take a raw or tiny json file and prepare it
#  for loading into SQL
sed 's/[^}]*$//;s/^\[//;s/}\,{/}\n{/g' $1 > $2
