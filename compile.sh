#!/bin/bash
gfortran -c main.f90 walkers.f90 arrayHandler.f90
gfortran main.o walkers.o arrayHandler.o
./a.exe $1
rm main.o walkers.o arrayHandler.o a.exe