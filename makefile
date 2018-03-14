#!/usr/bin/make
### Compiler and Compiling Rule ------------------------------------------------

export FC=gfortran
#export FC=ifort

include config.mk

### Targets for compilation ----------------------------------------------------

Release: cleanall
Release: $(MKDIRS)
Release: compileObject
Release: main
#Release: cleanCompileObj

lib: cleanall
lib: $(MKDIRS)
lib: compileObject
lib: makelib
#lib: cleanCompileObj

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
	$(MAKE) createObj -f $(DIR_SRC)geometry/makefile

$(DIR_OBJ)mfpGlobal.o: $(DIR_SRC)mfpGlobal.f90
	@echo $(COTEXT)
	$(COMPILE_OBJECT_RULE) $< -o $@


### phony auxiliary rules ------------------------------------------------------

.PHONY : cleanCompileObj
cleanCompileObj:
	@rm  compileObj

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
