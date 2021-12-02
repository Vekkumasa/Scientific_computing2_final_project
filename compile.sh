#!/bin/bash
gfortran -c main.f90 walkers.f90
gfortran main.o walkers.o
./a.exe