CPPFLAGS=-std=c++17 -fPIC
SOBJ=$(SWIPL_MODULE_DIR)/rolog.$(SWIPL_MODULE_EXT)
SWICPPFLAGS=-std=c++17
COFLAGS=-O2 -gdwarf-2 -g3
LIBS=

CC?=$(SWIPL_CC)
CXX?=$(SWIPL_CXX)
SWIPL?=swipl

ifeq ("$(R_HOME)","")
        RPATH=""
else
        ifeq ("$(SWIPL_ARCH)","x64-win64")
                RPATH="$(subst \,/,$(R_HOME))/bin/x64/"
        else
                RPATH="$(subst \,/,$(R_HOME))/bin/"
        endif
endif

RCPPFLAGS=$(shell "$(RPATH)R" CMD config --cppflags)
RLIBS=$(shell "$(RPATH)R" CMD config --ldflags)
INCLUDES2=-I$(shell "$(RPATH)Rscript" -e "cat(shQuote(system.file('include', package='Rcpp')))")
RINSIDECFLAGS=$(shell "$(RPATH)Rscript" -e "RInside:::CFlags()")
RINSIDELIBS=$(shell "$(RPATH)Rscript" -e "RInside:::LdFlags()")

ifeq ("$(SWIPL_ARCH)","x64-win64")
        ifeq ("$(RPATH)","")
                RDLL=$(shell which R.dll)
                RBLASSDLL=$(shell which Rblas.dll)
                RGRAPHAPPDLL=$(shell which Rgraphapp.dll)
                RICONVDLL=$(shell which Riconv.dll)
                RLAPACKDLL=$(shell which Rlapack.dll)
        else
                RDLL=$(RPATH)R.dll
                RBLASSDLL=$(RPATH)Rblas.dll
                RGRAPHAPPDLL=$(RPATH)Rgraphapp.dll
                RICONVDLL=$(RPATH)Riconv.dll
                RLAPACKDLL=$(RPATH)Rlapack.dll
        endif

        RSTUFF=$(RDLL) $(RBLASSDLL) $(RGRAPHAPPDLL) $(RICONVDLL) $(RLAPACKDLL)
endif


OBJ=src/rolog.o

all:	$(SOBJ)

$(SOBJ): $(OBJ)
	mkdir -p $(SWIPL_MODULE_DIR)
	$(CXX) $(SWIPL_MODULE_LDFLAGS) -o $@ $(OBJ) $(LIBS) $(SWIPL_MODULE_LIB) $(RLIBS) $(RINSIDELIBS)

src/rolog.o: src/rolog.cpp Makefile
	$(CXX) $(SWIPL_CFLAGS) $(COFLAGS) $(SWICPPFLAGS) $(RCPPFLAGS) $(INCLUDES2) $(RINSIDECFLAGS) -DPROLOGPACK -c -o $@ $<

clean:
	$(RM) src/rolog.o *~

distclean: clean
	$(RM) $(SOBJ) status.db buildenv.sh

check::

install:
ifeq ("$(RSTUFF)","")
else
	cp $(RSTUFF) $(SWIPL_MODULE_DIR)
endif
