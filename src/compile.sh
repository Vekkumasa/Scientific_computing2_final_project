#!/bin/bash
gfortran -c walkers.f90 arrayHandler.f90 walkerSimulation.f90 main.f90
gfortran main.o walkers.o arrayHandler.o walkerSimulation.o
./a.exe $1 $2 $3 $4 $5
rm a.exe main.o walkers.o arrayHandler.o walkerSimulation.o