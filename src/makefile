prog: main.o walkers.o walkerSimulation.o arrayHandler.o
	gfortran -o prog main.o walkers.o walkerSimulation.o arrayHandler.o

main.o: main.f90
	gfortran -c main.f90

walkers.o: walkers.f90
	gfortran -c walkers.f90

walkerSimulation.o: walkerSimulation.f90
	gfortran -c walkerSimulation.f90

arrayHandler.o: arrayHandler.f90
	gfortran -c arrayHandler.f90

clean:
	rm -rf *.o prog