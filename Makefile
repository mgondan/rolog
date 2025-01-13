CPPFLAGS=-std=c++17 -fPIC
SOBJ=$(SWIPL_MODULE_DIR)/rolog.$(SWIPL_MODULE_EXT)
SWICPPFLAGS=-std=c++17
COFLAGS=-O2 -gdwarf-2 -g3
LIBS=

CC?=$(SWIPL_CC)
CXX?=$(SWIPL_CXX)
SWIPL?=swipl

OBJ=src/rolog.o

all:	$(SOBJ)

$(SOBJ): $(OBJ)
	mkdir -p $(SWIPL_MODULE_DIR)
	$(CXX) $(SWIPL_MODULE_LDFLAGS) -o $@ $(OBJ) $(LIBS) $(SWIPL_MODULE_LIB) $(RLIBS) $(RINSIDELIBS)

src/rolog.o: src/rolog.cpp Makefile
	$(CXX) $(SWIPL_CFLAGS) $(COFLAGS) $(SWICPPFLAGS) $(RCPPFLAGS) $(INCLUDES2) $(RINSIDECFLAGS) -DPROLOGPACK -c -o $@ $<

clean:
	$(RM) cc/rolog.o *~

distclean: clean
	$(RM) $(SOBJ) status.db buildenv.sh

check::
install::

ifeq ($(R_HOME),)
	R_PATH=''
else
	ifeq ($(SWIARCH),x64-win64)
		R_PATH='$(R_HOME)/bin/x64/'
	else
		R_PATH='$(R_HOME)/bin/'
	endif	
endif
	
RCPPFLAGS=$(shell $(R_PATH)R CMD config --cppflags)
RLIBS=$(shell $(R_PATH)R CMD config --ldflags)
INCLUDES2=-I$(shell $(R_PATH)Rscript -e "cat(shQuote(system.file('include', package='Rcpp')))")
RINSIDECFLAGS=$(shell $(R_PATH)Rscript -e "RInside:::CFlags()")
RINSIDELIBS=$(shell $(R_PATH)Rscript -e "RInside:::LdFlags()")

ifeq ($(SWIARCH),x64-win64)
	ifeq ($(R_PATH),'')
		RDLL="$(shell which R.dll)"
		RBLASSDLL="$(shell which Rblas.dll)"
		RGRAPHAPPDLL="$(shell which Rgraphapp.dll)"
		RICONVDLL="$(shell which Riconv.dll)"
		RLAPACKDLL="$(shell which Rlapack.dll)"
	else
		RDLL=$(R_PATH)R.dll
		RBLASSDLL=$(R_PATH)Rblas.dll
		RGRAPHAPPDLL=$(R_PATH)Rgraphapp.dll
		RICONVDLL=$(R_PATH)Riconv.dll
		RLAPACKDLL=$(R_PATH)Rlapack.dll
	endif

	CP+=$(RDLL) $(RBLASSDLL) $(RGRAPHAPPDLL) $(RICONVDLL) $(RLAPACKDLL)
endif

ifeq ($(SWIARCH),x64-win64)
%.o: src/%.cpp
	$(CXX) $(CFLAGS) -D_REENTRANT -D__WINDOWS__ -D_WINDOWS -D__SWI_PROLOG__ -DROLOGPP $(RCPPFLAGS) $(INCLUDES2) $(RINSIDECFLAGS) $(LDSOFLAGS) -o $(SOBJ) src/$*.cpp $(RLIBS) $(RINSIDELIBS) $(SWILIB)
endif

