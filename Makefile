SOBJ=rolog.$(SOEXT)
SPATH=$(PACKSODIR)/$(SOBJ)

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

CP=$(SOBJ)

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

all: $(SPATH)

OBJ=rologpp.o

ifeq ($(SWIARCH),x64-win64)
%.o: src/%.cpp
	$(CXX) $(CFLAGS) -D_REENTRANT -D__WINDOWS__ -D_WINDOWS -D__SWI_PROLOG__ -DROLOGPP $(RCPPFLAGS) $(INCLUDES2) $(RINSIDECFLAGS) $(LDSOFLAGS) -o $(SOBJ) src/$*.cpp $(RLIBS) $(RINSIDELIBS) $(SWILIB)
endif

ifeq ($(SWIARCH),x86_64-linux)
%.o: src/%.cpp
	$(CC) $(CFLAGS) $(RCPPFLAGS) $(INCLUDES2) $(RINSIDECFLAGS) -DROLOGPP $(LDSOFLAGS) -o $(SOBJ) src/$*.cpp $(RLIBS) $(RINSIDELIBS)
endif

$(SPATH): $(OBJ)
	mkdir -p $(PACKSODIR)

install:
	cp $(CP) $(PACKSODIR)
	rm $(SOBJ)

check::

clean:
	rm -f $(OBJ)
	rm -f $(SOBJ)

distclean: clean
