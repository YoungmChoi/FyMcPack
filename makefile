#!/usr/bin/make

### Path -----------------------------------------------------------------------

export PROJECT_DIR:= $(dir $(abspath $(lastword $(MAKEFILE_LIST))))

export DIR_SRC=$(PROJECT_DIR)src/
export DIR_OBJ=$(PROJECT_DIR)obj/
export DIR_LIB=$(PROJECT_DIR)lib/
export DIR_EXE=$(PROJECT_DIR)

export DIR_SRCTEST=$(DIR_SRC)testCode/

export EXES=$(PROJECT_DIR)main

### compiler (gfortran) --------------------------------------------------------

export FC=gfortran
#export FC=ifort

# C flags
export CFLAGS = -fPIC -O2 -g

# linking flags
export LDFLAGS = -fPIC -O2 -c

#Flag for writing modules in $(OBJ) -J : write .mod file in given dir
export FLAGMOD1= -J $(DIR_LIB)

#Flag for reading modules in $(OBJ) - I : include .mod file in given dir
export FLAGMOD2= -I $(DIR_LIB)

export OPTSC0  = -c $(FLAGMOD1)
export OPTSL0  = $(FLAGMOD2)
export MKDIRS  = $(DIR_OBJ) $(DIR_LIB)

export OPTSC = $(OPTSC0)
export OPTSL = $(OPTSL0)

export COMPILE_OBJECT_RULE=@$(FC) $(CFLAGS) $(OPTSC) -o $@

### Targets for compilation ----------------------------------------------------

Release: cleanall
Release: $(MKDIRS)
Release: compileObject
Release: main

lib: cleanall
lib: $(MKDIRS)
lib: compileObject
lib: makelib

### auxiliary variables --------------------------------------------------------
COTEXT  = " - Compile  : '$(<F)'"
LITEXT  = " - Assemble : '$@'"

main: $(DIR_OBJ)main.o
	@echo $(COTEXT)
	@$(FC) -o $@ $(DIR_OBJ)*.o
EXES := $(DIR_EXE)main

makelib:
	@echo $(LITEXT)
	@ar cr $(DIR_LIB)libfymc.a $(DIR_OBJ)*.o

compileObject: compileObj

### compiling rules ------------------------------------------------------------

$(DIR_OBJ)main.o: $(DIR_SRC)main.f90 \
	$(DIR_OBJ)mfpGlobal.o
	@echo $(COTEXT)
	$(COMPILE_OBJECT_RULE) $< -o $@

compileObj: \
	$(DIR_OBJ)mfpGlobal.o
	$(MAKE) createObj -f $(DIR_SRC)general/makefile

$(DIR_OBJ)mfpGlobal.o: $(DIR_SRC)mfpGlobal.f90
	@echo $(COTEXT)
	$(COMPILE_OBJECT_RULE) $< -o $@

### phony auxiliary rules ------------------------------------------------------

.PHONY : $(MKDIRS)
$(MKDIRS):
	@mkdir -p $@
.PHONY : cleanobj

cleanobj:
	@echo deleting objects
	@rm -fr $(DIR_OBJ) @rm *.o

.PHONY : cleanmod
cleanlib:
	@echo deleting library
	@rm -fr $(DIR_LIB) @rm *.mod *.so *.a

.PHONY : cleanexe
cleanexe:
	@echo deleting exes
	@rm -f $(DIR_EXE), $(EXES)

.PHONY : clean
clean: cleanobj

.PHONY : cleanall
cleanall: clean cleanexe cleanlib
