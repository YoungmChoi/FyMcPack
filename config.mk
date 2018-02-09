### Path -----------------------------------------------------------------------

export PROJECT_DIR:= $(dir $(abspath $(lastword $(MAKEFILE_LIST))))

export DIR_SRC=$(PROJECT_DIR)src/
export DIR_OBJ=$(PROJECT_DIR)obj/
export DIR_LIB=$(PROJECT_DIR)lib/
export DIR_EXE=$(PROJECT_DIR)

export DIR_SRCTEST=$(DIR_SRC)testCode/

export EXES=$(PROJECT_DIR)main

### compiling Rule --------------------------------------------------------

CFLAG1 = -fPIC -O2 -g -c

ifeq ($(FC),ifort)
	# Array Bound Check
	CFLAG2     = -CB
	# .mod output path
	FLAGMODOUT = -module
else
	# Array Bound Check
	CFLAG2     = -fbounds-check
	# .mod output path
	FLAGMODOUT = -J
endif

# C flags
CFLAGS = $(CFLAG1) $(CFLAG2)

# linking flags
LDFLAGS = -fPIC -O2 -c

#Flag for writing modules in $(DIR_LIB)
FLAGMOD1= $(FLAGMODOUT) $(DIR_LIB)

#Flag for reading modules in $(OBJ) - I : include .mod file in given dir
FLAGMOD2= -I $(DIR_LIB)

OPTSC0  = $(FLAGMOD1)
OPTSL0  = $(FLAGMOD2)
MKDIRS  = $(DIR_OBJ) $(DIR_LIB)

OPTSC = $(OPTSC0)
OPTSL = $(OPTSL0)

export COMPILE_OBJECT_RULE=@$(FC) $(CFLAGS) $(OPTSC)

### compiling Rule --------------------------------------------------------
