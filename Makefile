include Makefile.inc

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

test: all
	$(MAKE) -C tests test

clean:
	-$(RM_F) *.42? fglunzip_version.inc tests$(SLASH)*.42? $(STD_DEV_NULL) $(ERR_DEV_NULL)
	-$(MAKE) -C tests clean


format-clean: clean
	-$(RM_F) *.4gl~ $(STD_DEV_NULL) $(ERR_DEV_NULL)

dist-clean: format-clean 
	-$(RM_RF) fglscriptify
