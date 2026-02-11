F90 = gfortran
FFLAGS = -O0 -g -Wall -fcheck=all -ffpe-trap=invalid,zero,overflow
EXE = run 

all : $(EXE)

$(EXE) : mod_constantes.o mod_function.o mod_df.o diff_finies.o
	$(F90) -o $(EXE) mod_constantes.o mod_function.o mod_df.o diff_finies.o

mod_constantes.o : mod_constantes.f90
	$(F90) $(FFLAGS) -c mod_constantes.f90

mod_function.o : mod_function.f90 mod_constantes.o
	$(F90) $(FFLAGS) -c mod_function.f90

mod_df.o : mod_df.f90 mod_function.o mod_constantes.o
	$(F90) $(FFLAGS) -c mod_df.f90

diff_finies.o : diff_finies.f90 mod_df.o mod_function.o mod_constantes.o
	$(F90) $(FFLAGS) -c diff_finies.f90

clean :
	rm -f *.o *mod run
