#!/usr/bin/bash
if [ $# -lt 2 ]
then
    echo "Usage : compil.sh <input file.f90> <output file>"
    exit
fi
if [ -e $1 ]
then
    gfortran $1 -o $2 -llapack -lblas && ./$2
else
    echo "Bah $1 ça existe pas"
fi