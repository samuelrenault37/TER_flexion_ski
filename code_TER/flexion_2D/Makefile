F90 = gfortran
FFLAGS = -O0 -Wall -Wextra -g -fcheck=all -ffpe-trap=invalid,zero,overflow
LIBS = -llapack -lblas
EXE = exec

$(EXE) : main.o donnees.o fonctions.o post_traitement.o schema_DF.o mod_algebre.o
	$(F90) -o $(EXE) main.o donnees.o fonctions.o post_traitement.o schema_DF.o mod_algebre.o $(LIBS)

donnees.o : donnees.f90
	$(F90) $(FFLAGS) -c donnees.f90

fonctions.o : fonctions.f90 donnees.o
	$(F90) $(FFLAGS) -c fonctions.f90

schema_DF.o : schema_DF.f90 fonctions.o donnees.o mod_algebre.o
	$(F90) $(FFLAGS) -c schema_DF.f90

post_traitement.o : post_traitement.f90 fonctions.o donnees.o
	$(F90) $(FFLAGS) -c post_traitement.f90

main.o : main.f90 post_traitement.o schema_DF.o donnees.o
	$(F90) $(FFLAGS) -c main.f90

mod_algebre.o : mod_algebre.f90 donnees.o
	$(F90) $(FFLAGS) -c mod_algebre.f90

clean :
	rm -f *.o *.mod exec