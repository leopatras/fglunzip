ifdef windir
WINDIR=$(windir)
endif


ifdef WINDIR
  FGLRUN=fglrun.exe
export LANG=.utf8

define _env
set $(1)=$(2)&&
endef
SHELL=cmd.exe
RM_F=del /s /q
RM_RF=rmdir /s /q
STD_DEV_NULL= >NUL
ERR_DEV_NULL= 2>NUL
SLASH=$(strip \)

else
  FGLRUN=fglrun
export LC_ALL=en_US.UTF-8

define _env
export $(1)=$(2)&&
endef
RM_F=rm -f
RM_RF=rm -rf
SLASH=/

endif

%.42f: %.per 
	fglform -M $<

%.42m: %.4gl 
	fglcomp -M $(FGLFLAGS) -r -Wall -Wno-stdsql $*


MODS=$(patsubst %.4gl,%.42m,$(wildcard *.4gl))
FORMS=$(patsubst %.per,%.42f,$(wildcard *.per))

all:: $(MODS) $(FORMS)

GIT_LONG_VERSION=$(shell git describe --tags --long --abbrev=8)
GIT_CHECK:=$(shell fglcomp -M checkgit && $(FGLRUN) checkgit "$(GIT_LONG_VERSION)")

#$(info GIT_CHECK=$(GIT_CHECK))

fglunzip.42m: fglunzip_version.inc

echo:
	echo "MODS: $(MODS), GIT_LONG_VERSION: $(GIT_LONG_VERSION)"

fglscriptify:
	git clone https://github.com/leopatras/fglscriptify.git

#bundles fglunzip as a single shell/bat script
dist: all fglscriptify
	fglscriptify$(SLASH)fglscriptify -o dist$(SLASH)fglunzip fglunzip_version.inc myassert.inc mygetopt.4gl fglunzip.4gl
	fglscriptify$(SLASH)fglscriptify -o dist$(SLASH)fglunzip.bat fglunzip_version.inc myassert.inc mygetopt.4gl fglunzip.4gl

format:
	fglcomp -M $(FGLFLAGS) $(COMFLAGS) --format --fo-inplace fglunzip.4gl
	fglcomp -M $(FGLFLAGS) $(COMFLAGS) --format --fo-inplace checkgit.4gl
	fglcomp -M $(FGLFLAGS) $(COMFLAGS) --format --fo-inplace mygetopt.4gl

clean:
	-$(RM_F) *.42? fglunzip_version.inc $(STD_DEV_NULL) $(ERR_DEV_NULL)


format-clean: clean
	-$(RM_F) *.4gl~ $(STD_DEV_NULL) $(ERR_DEV_NULL)

dist-clean: format-clean 
	-$(RM_RF) fglscriptify
