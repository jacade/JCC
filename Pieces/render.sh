#!/bin/sh
for ((SIZE=8; SIZE <= 100; SIZE = SIZE+2))
do
  if [ ! -d $SIZE"x"$SIZE  ]; then
    mkdir $SIZE"x"$SIZE 
  fi
  for file in *.svg; 
  do 
    inkscape -z -e $SIZE"x"$SIZE/${file%.svg}.png -w $SIZE -h $SIZE $file
   # do convert $file -size $SIZE"x"$SIZE $SIZE"x"$SIZE/${file%.svg}.png; <-- does not work
    echo $SIZE"x"$SIZE/${file%.svg}.png saved
  done
done
