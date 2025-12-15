F90 = gfortran
FFLAGS = -O0 -Wall -Wextra -g -fcheck=all -ffpe-trap=invalid,zero,overflow 
LIBS = -llapack -lblas
EXE = exec

all : $(EXE)

$(EXE) : main.o constantes.o fonction_pract.o initialisation.o methode.o
	$(F90) -o $(EXE) main.o constantes.o fonction_pract.o initialisation.o methode.o erreur.o $(LIBS)

constantes.o : constantes.f90
	$(F90) $(FFLAGS) -c constantes.f90

fonction_pract.o : fonction_pract.f90 constantes.o
	$(F90) $(FFLAGS) -c fonction_pract.f90

initialisation.o : initialisation.f90 constantes.o fonction_pract.o
	$(F90) $(FFLAGS) -c initialisation.f90 

methode.o : methode.f90 constantes.o fonction_pract.o
	$(F90) $(FFLAGS) -c methode.f90

erreur.o : erreur.f90 constantes.o fonction_pract.o methode.o initialisation.o
	$(F90) $(FFLAGS) -c erreur.f90

main.o : main.f90 constantes.o fonction_pract.o initialisation.o methode.o erreur.o
	$(F90) $(FFLAGS) -c main.f90

clean : 
	rm -f *.o *.mod $(EXE)