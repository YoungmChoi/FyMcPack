#!/usr/bin/make

### auxiliary variables --------------------------------------------------------

COTEXT  = " - Compile  : '$(<F)'"
LITEXT  = " - Assemble : '$@'"

DIR_SUB_SRC:= $(dir $(abspath $(lastword $(MAKEFILE_LIST))))

### Compiling Rules ------------------------------------------------------------

createObj: $(DIR_OBJ)mfpGeometry.o

$(DIR_OBJ)mfpGeometry.o: $(DIR_SUB_SRC)mfpGeometry.f90
	@echo $(COTEXT)
	$(COMPILE_OBJECT_RULE) $< -o $@
