target  = ../main
object  = \
  mt19937_original.o \
  mt19937Mod.o \
  mt19937Mod_omp.o \
  main.o


FC       = gfortran

FFLAGS   = -O3
FFLAGS  += -fno-range-check
LDFLAGS  =


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
