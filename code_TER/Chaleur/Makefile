F90 = gfortran
FFLAGS = -O0 -g -Wall -fcheck=all -ffpe-trap=invalid,zero,overflow
EXE = run 

all : $(EXE)

$(EXE) : mod_constantes.o mod_functions.o mod_algebre.o chaleur.o
	$(F90) -o $(EXE)  mod_constantes.o mod_functions.o mod_algebre.o chaleur.o

mod_constantes.o : mod_constantes.f90
	$(F90) $(FFLAGS) -c mod_constantes.f90

mod_functions.o : mod_functions.f90 mod_constantes.o
	$(F90) $(FFLAGS) -c mod_functions.f90

mod_algebre.o : mod_algebre.f90 mod_constantes.o
	$(F90) $(FFLAGS) -c mod_algebre.f90

chaleur.o : chaleur.f90 mod_functions.o mod_algebre.o mod_constantes.o
	$(F90) $(FFLAGS) -c chaleur.f90

clean :
	rm -f *.o *mod run
