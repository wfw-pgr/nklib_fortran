target  = ../main
object  = \
  mt19937i4Mod.o \
  randomGenMod.o \
  main.o

FC = gfortran

FFLAGS  = -O3 -fopenmp
LDFLAGS =


.SUFFIXES: .o .f90

%.o: %.f90
	$(FC) -c $< $(FFLAGS) $(LDFLAGS)

%.mod: %.f90 %.o
	@:

$(target): $(object)
	$(FC) -o $@ $(object) $(FFLAGS) $(LDFLAGS)


.PHONY:clean all

clean:
	$(RM) $(target) $(object) *.mod

all: clean $(target)
