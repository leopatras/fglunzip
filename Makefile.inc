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
