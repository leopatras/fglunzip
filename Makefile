ifdef windir
WINDIR=$(windir)
endif

ifdef WINDIR
  FGLRUN=fglrun.exe
export LANG=.utf8

define _env
set $(1)=$(2)&&
endef

else
  FGLRUN=fglrun
export LC_ALL=en_US.UTF-8

define _env
export $(1)=$(2)&&
endef

endif
#check if we have a runtime supporting binary copyN
ifneq ($(wildcard $(FGLDIR)/lib/WSHelper.42m),)
  COMFLAGS= -D HAVE_COM
endif

%.42f: %.per 
	fglform -M $<

%.42m: %.4gl 
	fglcomp -M $(FGLFLAGS) $(COMFLAGS) -r -Wall -Wno-stdsql $*


MODS=$(patsubst %.4gl,%.42m,$(wildcard *.4gl))
FORMS=$(patsubst %.per,%.42f,$(wildcard *.per))

all:: $(MODS) $(FORMS)

GIT_LONG_VERSION=$(shell git describe --tags --long --abbrev=8)
GIT_CHECK:=$(shell fglcomp -M checkgit && $(FGLRUN) checkgit "$(GIT_LONG_VERSION)")

#$(info GIT_CHECK=$(GIT_CHECK))

futils.42m: fglunzip_version.inc

echo:
	echo "MODS: $(MODS), GIT_LONG_VERSION: $(GIT_LONG_VERSION)"

doc:
	fglcomp --build-doc location.4gl
	fglcomp --build-doc app.4gl

format:
	fglcomp -M $(FGLFLAGS) $(COMFLAGS) --format --fo-inplace futils.4gl
	fglcomp -M $(FGLFLAGS) $(COMFLAGS) --format --fo-inplace fglunzip.4gl
	fglcomp -M $(FGLFLAGS) $(COMFLAGS) --format --fo-inplace checkgit.4gl
	fglcomp -M $(FGLFLAGS) $(COMFLAGS) --format --fo-inplace mygetopt.4gl

clean:
	rm -f *.42? fglunzip_version.inc

format-clean: clean
	rm *.4gl~
