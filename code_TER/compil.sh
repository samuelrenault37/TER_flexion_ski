#!/usr/bin/bash
if [ $# -lt 2 ]
then
    echo "Usage : compil.sh <input file.f90> <output file>"
    exit
fi
if [ -e $1 ]
then
    gfortran constantes.f90 fonction_pract.f90 initialisation.f90 methode.f90 $1 -o $2 -llapack -lblas -Wall -Wextra -g -O2&& ./$2
else
    echo "Bah $1 ça existe pas"
fi