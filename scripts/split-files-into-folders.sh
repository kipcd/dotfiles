#!/bin/bash
# outnum generates the name of the output directory
outnum=1
# n is the number of files we have moved
n=0

if [ "$#" -ne 2 ]; then
    echo Wrong number of args
    echo Usage: bash splitfiles.bash $PWD/directoryoffiles splitsize
    exit 1
fi

# Go through all files in the specified directory
for f in $1/*; do
   # Create new output directory if first of new batch
   if [ $n -eq 0 ]; then
      outdir=$1/$outnum
      mkdir $outdir
      ((outnum++))
   fi
   # Move the file to the new subdirectory
   mv "$f" "$outdir"

   # Count how many we have moved to there
   ((n++))

   # Start a new output directory if current new dir is full
   [ $n -eq $2 ] && n=0
done
