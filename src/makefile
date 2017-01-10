# makefile

# Choose desired compiler (compiler=gfortran/ifort)
ifndef $(compiler)
	compiler = ifort
endif

ifeq ($(compiler), gfortran)
	COMPILER = gfortran
	COMPILER_OPTS = -c -O3 -ffree-line-length-none	
else		
	COMPILER = ifort
	COMPILER_OPTS = -c -O3
endif

OBJECTS = types.o strength.o maths.o input_output.o build.o hamiltonian.o
FINAL_EXECUTABLE = diatomic
	
diatomic: $(OBJECTS)
	$(COMPILER) $(OBJECTS) -o $(FINAL_EXECUTABLE)
	cp $(FINAL_EXECUTABLE) ../
		
clean: 
	find . -maxdepth 2 -name "*.o" -delete ; find . -maxdepth 1 -name "*.mod" -delete ;
	find . -maxdepth 1 -name "*.f90~" -delete ; find . -maxdepth 2 -name "*.a" -delete ;
	
types.o: types.f90
	$(COMPILER) $(COMPILER_OPTS)  types.f90

maths.o: maths.f90 types.o
	$(COMPILER) $(COMPILER_OPTS)  maths.f90

input_output.o: input_output.f90 maths.o types.o
	$(COMPILER) $(COMPILER_OPTS)  input_output.f90
	
build.o: build.f90 types.o maths.o
	$(COMPILER) $(COMPILER_OPTS)  build.f90

strength.o: strength.f90 maths.o types.o build.o
	$(COMPILER) $(COMPILER_OPTS) strength.f90

hamiltonian.o: hamiltonian.f90 types.o maths.o input_output.o strength.o build.o
	$(COMPILER) $(COMPILER_OPTS) hamiltonian.f90
