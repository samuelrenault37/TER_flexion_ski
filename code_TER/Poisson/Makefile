F90 = gfortran
FFLAGS = -O0 -g -Wall -fcheck=all -ffpe-trap=invalid,zero,overflow
EXE = run 

all : $(EXE)

$(EXE) : mod_constantes.o mod_functions.o mod_algebre.o main.o
	$(F90) -o $(EXE)  mod_constantes.o mod_functions.o mod_algebre.o main.o

mod_constantes.o : mod_constantes.f90
	$(F90) $(FFLAGS) -c mod_constantes.f90

mod_functions.o : mod_functions.f90 mod_constantes.o
	$(F90) $(FFLAGS) -c mod_functions.f90

mod_algebre.o : mod_algebre.f90 mod_constantes.o
	$(F90) $(FFLAGS) -c mod_algebre.f90

main.o : main.f90 mod_functions.o mod_constantes.o
	$(F90) $(FFLAGS) -c main.f90

clean :
	rm -f *.o *mod run
