@echo off
@echo off
setlocal EnableExtensions

rem get unique file name 
:loop
set randbase=gen~%RANDOM%
set extractor="%tmp%\%randbase%.4gl"
set extractor42m="%tmp%\%randbase%.42m"
rem important: without quotes 
set _TMPDIR=%tmp%\%randbase%_d
set _IS_BAT_FILE=TRUE
if exist %extractor% goto :loop
if exist %extractor42m% goto :loop
if exist %_TMPDIR% goto :loop
rem echo tmp=%tmp%

set tmpdrive=%tmp:~0,2%
set _CATFILE=%~dpnx0
rem We use a small line extractor program in 4gl to a temp file
rem the bat only solutions at 
rem https://stackoverflow.com/questions/7954719/how-can-a-batch-script-do-the-equivalent-of-cat-eof
rem are too slow for bigger programs, so 4gl rules !

echo # Extractor coming from catsource.bat > %extractor%
echo --note: some 4gl constructs in this file are there to surround the pitfalls >> %extractor%
echo --of echo'ing this file with the windows echo command to a temp 4gl file >> %extractor%
echo --percent signs are avoided as well as or signs, thats why we avoid >> %extractor%
echo --the sfmt operator and the cat operator and mixing quotes with double quotes >> %extractor%
echo OPTIONS SHORT CIRCUIT >> %extractor%
echo IMPORT util >> %extractor%
echo IMPORT os >> %extractor%
echo DEFINE tmpdir,fname,full,lastmodule STRING >> %extractor%
echo DEFINE m_bat INT >> %extractor%
echo DEFINE singlequote,doublequote,backslash,percent,dollar STRING >> %extractor%
echo DEFINE m_binTypeArr,m_resTypeArr,m_imgarr,m_resarr DYNAMIC ARRAY OF STRING >> %extractor%
echo MAIN >> %extractor%
echo   DEFINE line,err,catfile STRING >> %extractor%
echo   DEFINE ch,chw base.Channel >> %extractor%
echo   DEFINE sb base.StringBuffer >> %extractor%
echo   DEFINE write,writebin INT >> %extractor%
echo   LET singlequote=ASCII(39) >> %extractor%
echo   LET doublequote=ASCII(34) >> %extractor%
echo   LET backslash=ASCII(92) --we must not use the literal here >> %extractor%
echo   LET percent=ASCII(37) >> %extractor%
echo   LET dollar=ASCII(36) >> %extractor%
echo   LET m_binTypeArr[m_binTypeArr.getLength()+1]='png'  >> %extractor%
echo   LET m_binTypeArr[m_binTypeArr.getLength()+1]='jpg' >> %extractor%
echo   LET m_binTypeArr[m_binTypeArr.getLength()+1]='bmp' >> %extractor%
echo   LET m_binTypeArr[m_binTypeArr.getLength()+1]='gif' >> %extractor%
echo   LET m_binTypeArr[m_binTypeArr.getLength()+1]='tiff' >> %extractor%
echo   LET m_binTypeArr[m_binTypeArr.getLength()+1]='wav' >> %extractor%
echo   LET m_binTypeArr[m_binTypeArr.getLength()+1]='mp3' >> %extractor%
echo   LET m_binTypeArr[m_binTypeArr.getLength()+1]='aiff' >> %extractor%
echo   LET m_binTypeArr[m_binTypeArr.getLength()+1]='mpg' >> %extractor%
echo -- >> %extractor%
echo   LET m_resTypeArr[m_resTypeArr.getLength()+1]='per'  >> %extractor%
echo   LET m_resTypeArr[m_resTypeArr.getLength()+1]='4st' >> %extractor%
echo   LET m_resTypeArr[m_resTypeArr.getLength()+1]='4tb' >> %extractor%
echo   LET m_resTypeArr[m_resTypeArr.getLength()+1]='4tm' >> %extractor%
echo   LET m_resTypeArr[m_resTypeArr.getLength()+1]='4sm' >> %extractor%
echo   LET m_resTypeArr[m_resTypeArr.getLength()+1]='iem' >> %extractor%
echo   LET sb=base.StringBuffer.create() >> %extractor%
echo   LET catfile=fgl_getenv("_CATFILE") --set by calling script >> %extractor%
echo   LET tmpdir=fgl_getenv("_TMPDIR") --set by calling script >> %extractor%
echo   LET m_bat=fgl_getenv("_IS_BAT_FILE") IS NOT NULL >> %extractor%
echo   IF catfile IS NULL OR tmpdir IS NULL THEN >> %extractor%
echo     CALL myerr("_CATFILE or _TMPDIR not set") >> %extractor%
echo   END IF >> %extractor%
echo   IF catfile IS NULL THEN >> %extractor%
echo     LET catfile=arg_val(1) >> %extractor%
echo     LET tmpdir=arg_val(2) >> %extractor%
echo   END IF >> %extractor%
echo   IF NOT m_bat THEN --windows fullPath is clumsy >> %extractor%
echo     LET tmpdir=os.Path.fullPath(tmpdir) >> %extractor%
echo   END IF >> %extractor%
echo   LET ch=base.Channel.create() >> %extractor%
echo   LET chw=base.Channel.create() >> %extractor%
echo   IF NOT os.Path.exists(tmpdir) THEN >> %extractor%
echo     IF NOT os.Path.mkdir(tmpdir) THEN >> %extractor%
echo       LET err="Can't mkdir :",tmpdir >> %extractor%
echo       CALL myerr(err) >> %extractor%
echo     END IF >> %extractor%
echo   END IF >> %extractor%
echo   CALL ch.openFile(catfile,"r") >> %extractor%
echo   WHILE (line:=ch.readLine()) IS NOT NULL >> %extractor%
echo     CASE >> %extractor%
echo        WHEN m_bat AND line.getIndexOf("rem __CAT_EOF_BEGIN__:",1)==1 >> %extractor%
echo          LET fname=line.subString(23,line.getLength()) >> %extractor%
echo          GOTO mark1 >> %extractor%
echo        WHEN (NOT m_bat) AND  line.getIndexOf("#__CAT_EOF_BEGIN__:",1)==1 >> %extractor%
echo          LET fname=line.subString(20,line.getLength()) >> %extractor%
echo        LABEL mark1: >> %extractor%
echo          LET full=os.Path.join(tmpdir,fname) >> %extractor%
echo          CALL checkSubdirs() >> %extractor%
echo          IF isBinary(fname) THEN >> %extractor%
echo            LET writebin=TRUE >> %extractor%
echo            CALL addDir(m_imgarr,os.Path.dirName(fname)) >> %extractor%
echo            CALL sb.clear() >> %extractor%
echo          ELSE >> %extractor%
echo            IF isResource(fname) THEN >> %extractor%
echo              CALL addDir(m_resarr,os.Path.dirName(fname)) >> %extractor%
echo            END IF >> %extractor%
echo            LET write=TRUE >> %extractor%
echo            CALL chw.openFile(full,"w") >> %extractor%
echo          END IF >> %extractor%
echo        WHEN ((NOT m_bat) AND line=="#__CAT_EOF_END__") OR >> %extractor%
echo             (m_bat AND line=="rem __CAT_EOF_END__") >> %extractor%
echo          IF writebin THEN >> %extractor%
echo            LET writebin=FALSE >> %extractor%
echo            CALL util.Strings.base64Decode(sb.toString(),full) >> %extractor%
echo          ELSE >> %extractor%
echo            LET write=FALSE >> %extractor%
echo            CALL chw.close() >> %extractor%
echo            CALL eventuallyCompileFile() >> %extractor%
echo          END IF >> %extractor%
echo        WHEN writebin >> %extractor%
echo          CALL sb.append(line.subString(IIF(m_bat,5,2),line.getLength())) >> %extractor%
echo        WHEN write >> %extractor%
echo          CALL chw.writeLine(line.subString(IIF(m_bat,5,2),line.getLength())) >> %extractor%
echo     END CASE >> %extractor%
echo   END WHILE >> %extractor%
echo   CALL ch.close() >> %extractor%
echo   CALL runLastModule() >> %extractor%
echo END MAIN >> %extractor%
echo -- >> %extractor%
echo FUNCTION addDir(arr,dirname) >> %extractor%
echo   DEFINE arr DYNAMIC ARRAY OF STRING >> %extractor%
echo   DEFINE dirname STRING >> %extractor%
echo   DEFINE i INT >> %extractor%
echo   FOR i=1 TO arr.getLength() >> %extractor%
echo     IF arr[i]=dirname THEN >> %extractor%
echo       RETURN --already contained >> %extractor%
echo     END IF >> %extractor%
echo   END FOR >> %extractor%
echo   LET arr[arr.getLength()+1]=dirname >> %extractor%
echo END FUNCTION >> %extractor%
echo -- >> %extractor%
echo FUNCTION setPathFor(arr,envName,cmd) >> %extractor%
echo   DEFINE arr DYNAMIC ARRAY OF STRING >> %extractor%
echo   DEFINE envName,tmp STRING >> %extractor%
echo   DEFINE cmd STRING >> %extractor%
echo   DEFINE i INT >> %extractor%
echo   IF arr.getLength()>0 THEN >> %extractor%
echo     LET tmp=envName,"=" >> %extractor%
echo     LET cmd=cmd,IIF(m_bat,"set ",""),tmp >> %extractor%
echo     IF fgl_getenv(envName) IS NOT NULL THEN >> %extractor%
echo       IF m_bat THEN >> %extractor%
echo         LET cmd=percent,envName,percent,";" >> %extractor%
echo       ELSE >> %extractor%
echo         LET cmd=dollar,envName,":" >> %extractor%
echo       END IF >> %extractor%
echo     END IF >> %extractor%
echo     FOR i=1 TO arr.getLength() >> %extractor%
echo         IF i>1 THEN >> %extractor%
echo           LET cmd=cmd,IIF(m_bat,";",":") >> %extractor%
echo         END IF >> %extractor%
echo         LET cmd=cmd,quotePath(os.Path.join(tmpdir,arr[i])) >> %extractor%
echo     END FOR >> %extractor%
echo     LET cmd=cmd,IIF(m_bat,"&&"," ") >> %extractor%
echo   END IF >> %extractor%
echo   RETURN cmd >> %extractor%
echo END FUNCTION >> %extractor%
echo -- >> %extractor%
echo FUNCTION runLastModule() --we must get argument quoting right >> %extractor%
echo   DEFINE i INT >> %extractor%
echo   DEFINE arg,cmd,cmdsave,image2font STRING >> %extractor%
echo   IF lastmodule IS NULL THEN RETURN END IF >> %extractor%
echo   LET cmd=setPathFor(m_resarr,"FGLRESOURCEPATH",cmd) >> %extractor%
echo   LET image2font=os.Path.join(os.Path.join(fgl_getenv("FGLDIR"),"lib"),"image2font.txt") >> %extractor%
echo   LET cmdsave=cmd >> %extractor%
echo   LET cmd=setPathFor(m_imgarr,"FGLIMAGEPATH",cmd) >> %extractor%
echo   IF cmd!=cmdsave AND os.Path.exists(image2font) THEN >> %extractor%
echo     IF m_bat THEN >> %extractor%
echo       LET cmd=cmd.subString(1,cmd.getLength()-2),";",quotePath(image2font),"&&" >> %extractor%
echo     ELSE >> %extractor%
echo       LET cmd=cmd.subString(1,cmd.getLength()-1),":",quotePath(image2font)," " >> %extractor%
echo     END IF >> %extractor%
echo   END IF >> %extractor%
echo   LET cmd=cmd,"fglrun ",os.Path.join(tmpdir,lastmodule) >> %extractor%
echo   FOR i=1 TO num_args() >> %extractor%
echo     LET arg=arg_val(i) >> %extractor%
echo     CASE >> %extractor%
echo       WHEN m_bat AND arg.getIndexOf(' ',1)==0 AND  >> %extractor%
echo                      arg.getIndexOf(doublequote,1)==0 >> %extractor%
echo         LET cmd=cmd,' ',arg --we don't need quotes >> %extractor%
echo       WHEN m_bat OR arg.getIndexOf(singlequote,1)!=0  >> %extractor%
echo         --we must use double quotes on windows >> %extractor%
echo         LET cmd=cmd,' ',doublequote,quoteDouble(arg),doublequote >> %extractor%
echo       OTHERWISE >> %extractor%
echo         --sh: you can't quote single quotes inside single quotes >> %extractor%
echo         --everything else does not need to be quoted >> %extractor%
echo         LET cmd=cmd,' ',singlequote,arg,singlequote >> %extractor%
echo     END CASE >> %extractor%
echo   END FOR >> %extractor%
echo   --DISPLAY "cmd:",cmd >> %extractor%
echo   CALL myrun(cmd) >> %extractor%
echo END FUNCTION >> %extractor%
echo -- >> %extractor%
echo FUNCTION quotePath(p) >> %extractor%
echo   DEFINE p STRING >> %extractor%
echo   --TODO: quote space with backlash space >> %extractor%
echo   --IF NOT m_bat AND p.getIndexOf(" ",1)!=0 >> %extractor%
echo     --RETURN quoteSpace(p) >> %extractor%
echo   --END IF >> %extractor%
echo   RETURN p >> %extractor%
echo END FUNCTION >> %extractor%
echo -- >> %extractor%
echo FUNCTION myerr(err) >> %extractor%
echo   DEFINE err STRING >> %extractor%
echo   DISPLAY "ERROR:",err >> %extractor%
echo   EXIT PROGRAM 1 >> %extractor%
echo END FUNCTION >> %extractor%
echo -- >> %extractor%
echo FUNCTION eventuallyCompileFile() >> %extractor%
echo   DEFINE cmd STRING >> %extractor%
echo   CASE >> %extractor%
echo     WHEN os.Path.extension(fname)=="4gl" >> %extractor%
echo       LET cmd="cd ",tmpdir," && fglcomp -M ",fname >> %extractor%
echo       CALL myrun(cmd) >> %extractor%
echo       --DISPLAY "dirname:",fname,",basename:",os.Path.baseName(fname) >> %extractor%
echo       LET lastmodule=os.Path.baseName(fname) >> %extractor%
echo       --cut extension >> %extractor%
echo       LET lastmodule=lastmodule.subString(1,lastmodule.getLength()-4) >> %extractor%
echo       --DISPLAY "lastmodule=",lastmodule >> %extractor%
echo     WHEN os.Path.extension(fname)=="per" >> %extractor%
echo       LET cmd="cd ",tmpdir," && fglform -M ",fname >> %extractor%
echo       CALL myrun(cmd) >> %extractor%
echo     --other (resource) files are just copied >> %extractor%
echo   END CASE >> %extractor%
echo END FUNCTION >> %extractor%
echo -- >> %extractor%
echo FUNCTION myrun(cmd) >> %extractor%
echo   DEFINE cmd STRING, code INT >> %extractor%
echo   --DISPLAY "myrun:",cmd >> %extractor%
echo   RUN cmd RETURNING code >> %extractor%
echo   IF code THEN >> %extractor%
echo     EXIT PROGRAM 1 >> %extractor%
echo   END IF >> %extractor%
echo END FUNCTION >> %extractor%
echo -- >> %extractor%
echo FUNCTION checkSubdirs() >> %extractor%
echo   DEFINE i,found INT >> %extractor%
echo   DEFINE dir,err STRING >> %extractor%
echo   DEFINE dirs DYNAMIC ARRAY OF STRING >> %extractor%
echo   LET dir=os.Path.fullPath(os.Path.dirName(full)) >> %extractor%
echo   WHILE TRUE >> %extractor%
echo     CASE >> %extractor%
echo       WHEN dir IS NULL >> %extractor%
echo         EXIT WHILE >> %extractor%
echo       WHEN dir==tmpdir >> %extractor%
echo         LET found=true >> %extractor%
echo         EXIT WHILE >> %extractor%
echo       OTHERWISE >> %extractor%
echo         CALL dirs.insertElement(1) >> %extractor%
echo         LET dirs[1]=dir >> %extractor%
echo     END CASE >> %extractor%
echo     LET dir=os.Path.fullPath(os.Path.dirName(dir)) >> %extractor%
echo   END WHILE >> %extractor%
echo   IF NOT found THEN >> %extractor%
echo     --we can't use sfmt because of .bat echo pitfalls >> %extractor%
echo     LET err=singlequote,fname,singlequote,' does point outside' >> %extractor%
echo     CALL myerr(err) >> %extractor%
echo   END IF >> %extractor%
echo   FOR i=1 TO dirs.getLength() >> %extractor%
echo     LET dir=dirs[i] >> %extractor%
echo     IF NOT os.Path.exists(dir) THEN >> %extractor%
echo       IF NOT os.Path.mkdir(dir) THEN >> %extractor%
echo         LET err="Can't create directory:",dir >> %extractor%
echo         CALL myerr(err) >> %extractor%
echo       END IF >> %extractor%
echo     END IF >> %extractor%
echo   END FOR >> %extractor%
echo END FUNCTION >> %extractor%
echo -- >> %extractor%
echo FUNCTION quoteDouble(s) >> %extractor%
echo   DEFINE s STRING >> %extractor%
echo   DEFINE c STRING >> %extractor%
echo   DEFINE i INT >> %extractor%
echo   DEFINE sb base.StringBuffer >> %extractor%
echo   LET sb=base.StringBuffer.create() >> %extractor%
echo   FOR i=1 TO s.getLength() >> %extractor%
echo     LET c=s.getCharAt(i) >> %extractor%
echo     CASE >> %extractor%
echo       WHEN c==doublequote >> %extractor%
echo         CALL sb.append(backslash) >> %extractor%
echo       WHEN (NOT m_bat) AND  c==backslash >> %extractor%
echo         CALL sb.append(backslash) >> %extractor%
echo     END CASE >> %extractor%
echo     CALL sb.append(c) >> %extractor%
echo   END FOR >> %extractor%
echo   RETURN sb.toString() >> %extractor%
echo END FUNCTION >> %extractor%
echo -- >> %extractor%
echo FUNCTION isInArray(arr,fname) >> %extractor%
echo   DEFINE arr DYNAMIC ARRAY OF STRING >> %extractor%
echo   DEFINE fname,ext STRING >> %extractor%
echo   DEFINE i INT >> %extractor%
echo   LET ext=os.Path.extension(fname) >> %extractor%
echo   FOR i=1 TO arr.getLength() >> %extractor%
echo     IF arr[i]==ext THEN  >> %extractor%
echo       RETURN TRUE >> %extractor%
echo     END IF >> %extractor%
echo   END FOR >> %extractor%
echo   RETURN FALSE >> %extractor%
echo END FUNCTION >> %extractor%
echo -- >> %extractor%
echo FUNCTION isBinary(fname) >> %extractor%
echo   DEFINE fname STRING >> %extractor%
echo   RETURN isInArray(m_binTypeArr,fname) >> %extractor%
echo END FUNCTION >> %extractor%
echo -- >> %extractor%
echo FUNCTION isResource(fname) >> %extractor%
echo   DEFINE fname STRING >> %extractor%
echo   RETURN isInArray(m_resTypeArr,fname) >> %extractor%
echo END FUNCTION >> %extractor%
echo -- >> %extractor%
set mydir=%cd%
set mydrive=%~d0
%tmpdrive%
cd %tmp%
fglcomp -M %randbase%
if ERRORLEVEL 1 exit /b
del %extractor%
rem extract the 4gl code behind us to another 4GL file
%mydrive%
cd %mydir%
fglrun %extractor42m% %1 %2 %3 %4 %5
if ERRORLEVEL 1 exit /b
del %extractor42m%
exit /b
rem __CAT_EOF_BEGIN__:fglunzip_version.inc
rem public CONSTANT GIT_VERSION="1.00.00dev"
rem public CONSTANT GIT_COMMIT_COUNT=9
rem public CONSTANT GIT_REV="gd37392ae"
rem __CAT_EOF_END__
rem __CAT_EOF_BEGIN__:myassert.inc
rem &define MYERRCALL myErr
rem &define MYASSERT(x) IF NOT NVL(x,0) THEN CALL MYERRCALL("ASSERTION failed in line:"||__LINE__||":"||#x) END IF
rem &define MYASSERT_MSG(x,msg) IF NOT NVL(x,0) THEN CALL MYERRCALL("ASSERTION failed in line:"||__LINE__||":"||#x||","||msg) END IF
rem &define UNUSED_VAR(variable) IF (variable) IS NULL THEN END IF
rem __CAT_EOF_END__
rem __CAT_EOF_BEGIN__:mygetopt.4gl
rem #+ the fgl getopt module backported to 3.10
rem IMPORT os
rem 
rem #+ GetoptOptions.arg_type flag: Option has no value argument.
rem PUBLIC CONSTANT NONE = 1
rem #+ GetoptOptions.arg_type flag: Option has a mandatory value argument.
rem PUBLIC CONSTANT REQUIRED = 2
rem #+ GetoptOptions.arg_type flag: Option has an optional value argument.
rem PUBLIC CONSTANT OPTIONAL = 3
rem 
rem #+ getopt() method status: Last argument processing succeeded.
rem PUBLIC CONSTANT SUCCESS = 0
rem #+ getopt() method status: All possible options have been processed.
rem PUBLIC CONSTANT EOF = 1
rem #+ getopt() method status: The last option processing failed.
rem PUBLIC CONSTANT BAD_OPTION = 2
rem 
rem PRIVATE CONSTANT ALIGN_SIZE = 30 # Alignment of usage display
rem 
rem #+
rem #+ Getopt array of option definitions
rem #+
rem #+ This type defines a dynamic array of a record structure to hold command line
rem #+ options definitions information.
rem #+ Define a variable of the getopt.GetoptOptions type and fill it with an
rem #+ array initializer.
rem #+ Once the array is initialized, it can be passed to the initDefault() or
rem #+ initialize() method, to setup a Getopt variable in order to process
rem #+ command line arguments with the getopt() method.
rem #+
rem #+ Record members:
rem #+
rem #+ -  name: defines the long name of the command line option.
rem #+
rem #+ -  description: is the text to explain the command line option.
rem #+
rem #+ -  opt_char: is the single-char command line option name.
rem #+
rem #+ -  arg_type: can be one of getopt.NONE, getopt.OPTIONAL, getopt.REQUIRED
rem #+
rem PUBLIC TYPE GetoptOptions DYNAMIC ARRAY OF RECORD
rem   name STRING,
rem   description STRING,
rem   opt_char CHAR,
rem   arg_type INTEGER
rem END RECORD
rem 
rem #+
rem #+ Getopt object type
rem #+
rem #+ This type defines the Getopt record that is used with getopt methods to
rem #+ parse and validate command line options.
rem #+ A variable of the type Getopt must be defined and initialized with the
rem #+ initDefault() or initialize() method, before using the getopt() method
rem #+ in a WHILE loop, to process command line options.
rem #+ After initializing the Getopt variable, the argv dynamic array contains
rem #+ all command line arguments starting from the index offset provided to
rem #+ the initialize() method.
rem #+
rem #+ Record members:
rem #+
rem #+ -  opt_ind: Current command line argument index that is processed.
rem #+
rem #+ -  opt_char: The single-character short name of the current processed option.
rem #+
rem #+ -  opt_arg: If present, holds the value parameter of the processed option
rem #+             (--option=value). Otherwise, this member is NULL.
rem #+
rem PUBLIC TYPE Getopt RECORD
rem   # Private members - don't modify
rem   argv DYNAMIC ARRAY OF STRING,
rem   status INTEGER,
rem   _options GetoptOptions,
rem   prog_name STRING,
rem   next_char STRING,
rem   option_index INTEGER,
rem 
rem   # Public read only members
rem   opt_ind INTEGER,
rem   opt_char CHAR,
rem   opt_arg STRING
rem END RECORD
rem 
rem PUBLIC TYPE GetoptR DYNAMIC ARRAY OF Getopt
rem 
rem MAIN
rem   DEFINE gr GetoptR
rem   DEFINE _options GetoptOptions
rem   { =
rem       [(name: "version",
rem               description: "Version information",
rem               opt_char: 'v',
rem               arg_type: NONE),
rem           (name: "help",
rem               description: "This help page",
rem               opt_char: 'h',
rem               arg_type: NONE),
rem           (name: "hello",
rem               description: "Hello everybody!",
rem               opt_char: NULL,
rem               arg_type: REQUIRED),
rem           (name: "bonjour",
rem               description: "Bonjour tout le monde!",
rem               opt_char: 'B',
rem               arg_type: OPTIONAL),
rem           (name: "number",
rem               description: "Accept also negativ numbers",
rem               opt_char: 'n',
rem               arg_type: REQUIRED)]
rem 
rem 
rem   }
rem   DEFINE ind, option_index INTEGER
rem   DEFINE cnt INTEGER
rem   DEFINE opt_char, opt_arg STRING
rem 
rem   -- CALL g.initDefault(_options)
rem   CALL initialize(gr, arg_val(0), copyArguments(1), _options)
rem 
rem   WHILE getopt(gr) == SUCCESS
rem     LET opt_char = opt_char(gr)
rem     LET opt_arg = opt_arg(gr)
rem     LET option_index = option_index(gr)
rem 
rem     CASE opt_char
rem       WHEN 'v'
rem         DISPLAY "Version: 1.54"
rem       WHEN 'h'
rem         CALL displayUsage(gr, "filename ...")
rem       WHEN 'H'
rem         DISPLAY "Got option -H and value arg is ", opt_arg
rem       WHEN 'B'
rem         IF opt_arg IS NOT NULL THEN
rem           DISPLAY "Got option -B and value arg is ", opt_arg
rem         ELSE
rem           DISPLAY "Got option -B and no arg"
rem         END IF
rem       WHEN 'n'
rem         DISPLAY "Got option -n and value arg is ", opt_arg
rem       OTHERWISE
rem         IF opt_char IS NULL THEN
rem           DISPLAY "Got long option ", _options[option_index].name
rem           CASE _options[option_index].name
rem             WHEN "hello"
rem               DISPLAY "Got option --hello and arg is ", opt_arg
rem           END CASE
rem         END IF
rem     END CASE
rem   END WHILE
rem 
rem   IF invalidOptionSeen(gr) THEN # ERROR
rem     CALL displayUsage(gr, "filename ...")
rem     EXIT PROGRAM 1
rem   ELSE
rem     LET cnt = getMoreArgumentCount(gr)
rem     IF cnt > 0 THEN
rem       FOR ind = 1 TO cnt
rem         DISPLAY SFMT("Additional argument: %1", getMoreArgument(gr, ind))
rem       END FOR
rem     END IF
rem   END IF
rem 
rem END MAIN
rem 
rem PUBLIC FUNCTION opt_arg(gr GetoptR) RETURNS STRING
rem   RETURN gr[1].opt_arg
rem END FUNCTION
rem 
rem PUBLIC FUNCTION opt_char(gr GetoptR) RETURNS STRING
rem   RETURN gr[1].opt_char
rem END FUNCTION
rem 
rem PUBLIC FUNCTION option_index(gr GetoptR) RETURNS STRING
rem   RETURN gr[1].option_index
rem END FUNCTION
rem 
rem #+ Copy the command line arguments into an array of string
rem #+
rem #+ @param ind First argument to copy
rem #+
rem #+ @return An array of string
rem PUBLIC FUNCTION copyArguments(ind INTEGER) RETURNS DYNAMIC ARRAY OF STRING
rem   DEFINE argv DYNAMIC ARRAY OF STRING
rem   DEFINE i INTEGER
rem   DEFINE argc INTEGER
rem 
rem   LET argc = 0
rem   FOR i = ind TO num_args()
rem     LET argc = argc + 1
rem     LET argv[argc] = arg_val(i)
rem   END FOR
rem   RETURN argv
rem END FUNCTION
rem 
rem PUBLIC FUNCTION copyArgumentsFromArr(
rem     argsarr DYNAMIC ARRAY OF STRING, ind INTEGER)
rem     RETURNS DYNAMIC ARRAY OF STRING
rem   DEFINE argv DYNAMIC ARRAY OF STRING
rem   DEFINE i INTEGER
rem   DEFINE argc INTEGER
rem 
rem   LET argc = 0
rem   FOR i = ind TO argsarr.getLength()
rem     LET argc = argc + 1
rem     LET argv[argc] = argsarr[i]
rem   END FOR
rem   RETURN argv
rem END FUNCTION
rem 
rem #+ Expand argument list (@file)
rem #+
rem #+ @param argv The arguments list to expand
rem #+
rem #+ @return An array of string
rem PRIVATE FUNCTION expandArguments(
rem     argv DYNAMIC ARRAY OF STRING)
rem     RETURNS(INTEGER, DYNAMIC ARRAY OF STRING)
rem   DEFINE rv DYNAMIC ARRAY OF STRING
rem   DEFINE i INTEGER
rem   DEFINE argc INTEGER
rem   DEFINE arg STRING
rem   --DEFINE ch base.Channel
rem   --DEFINE fileName STRING
rem   --DEFINE ln STRING
rem 
rem   LET argc = 0
rem   FOR i = 1 TO argv.getLength()
rem     LET arg = argv[i]
rem     {IF arg.subString(1, 1) == '@' THEN
rem       LET fileName = arg.subString(2, arg.getLength())
rem       IF NOT os.Path.exists(fileName) THEN
rem         DISPLAY SFMT("getopt: File %1 not found.", fileName)
rem         RETURN BAD_OPTION, NULL
rem       END IF
rem       LET ch = base.Channel.create()
rem       CALL ch.openFile(fileName, "r")
rem       WHILE (ln := ch.readLine()) IS NOT NULL
rem         LET ln = ln.trim()
rem         IF ln.getLength() > 0 THEN
rem           LET argc = argc + 1
rem           LET rv[argc] = ln
rem         END IF
rem       END WHILE
rem       CALL ch.close()
rem     ELSE}
rem     LET argc = argc + 1
rem     LET rv[argc] = argv[i]
rem     {END IF}
rem   END FOR
rem 
rem   RETURN SUCCESS, rv
rem END FUNCTION
rem 
rem #+
rem #+ Getopt isAnOption method.
rem #+
rem #+ This method checks whether a given parameter is an option or not
rem #+
rem #+ @param param The parameter string you want to check if it is an option
rem #+
rem #+ @return TRUE if parameter is a valid option, FALSE otherwise
rem #+
rem PRIVATE FUNCTION isAnOption(gr GetoptR, param STRING) RETURNS BOOLEAN
rem   # An option must start with - or --
rem   IF param.getCharAt(1) == '-' THEN
rem     LET param = param.subString(2, param.getLength())
rem     IF param.getCharAt(1) == '-' THEN
rem       # Handle long option
rem       LET param = param.subString(2, param.getLength())
rem       IF gr[1]._options.search("name", param) > 0 THEN
rem         RETURN TRUE # Is a long option
rem       END IF
rem     ELSE
rem       IF param.getLength() == 1 THEN
rem         # Handle short option
rem         IF gr[1]._options.search("opt_char", param) > 0 THEN
rem           RETURN TRUE # Is a short option
rem         END IF
rem       END IF
rem     END IF
rem   END IF
rem   RETURN FALSE
rem END FUNCTION
rem 
rem #+
rem #+ Getopt object initialization method.
rem #+
rem #+ This method initializes the Getopt object by using the program name passed
rem #+ as parameter, a dynamic array of strings with the arguments to process, and
rem #+ the definition of the options of the program.
rem #+
rem #+ The second parameter (argv) can be used to implement command line syntax
rem #+ with a verb as first argument:
rem #+
rem #+ @code
rem #+ fglrun myprog capture --verbose --filename=file1
rem #+ fglrun myprog duplicate --source=file1 --destination=file2
rem #+
rem #+ The argv list can be provided with the copyArguments(index) function.
rem #+
rem #+ @param prog_name  The name of the program
rem #+ @param argv       The program arguments
rem #+ @param options    The GetoptOptions array of options definitions
rem #+
rem #+
rem PUBLIC FUNCTION initialize(
rem     gr GetoptR,
rem     prog_name STRING,
rem     argv DYNAMIC ARRAY OF STRING,
rem     options GetoptOptions)
rem   LET gr[1].prog_name = prog_name
rem   LET gr[1].next_char = NULL
rem   LET gr[1].opt_ind = 0
rem   LET gr[1].opt_char = ""
rem   LET gr[1].opt_arg = NULL
rem   LET gr[1].status = SUCCESS
rem   CALL options.copyTo(gr[1]._options)
rem   CALL expandArguments(argv) RETURNING gr[1].status, gr[1].argv
rem END FUNCTION
rem 
rem #+
rem #+ Default Getopt object initialization method.
rem #+
rem #+ This method initializes the Getopt object by using arg_val(0) as program
rem #+ name and starting command line argument processing at index 1.
rem #+
rem #+ @param options    The GetoptOptions array of options definitions
rem #+
rem PUBLIC FUNCTION initDefault(gr GetoptR, options GetoptOptions)
rem   CALL initialize(gr, os.Path.baseName(arg_val(0)), copyArguments(1), options)
rem END FUNCTION
rem 
rem #+
rem #+ Returns the number of arguments left, which are not part of the options,
rem #+ or -1 if the options are not yet fully parsed.
rem #+
rem #+ @return The number of additional arguments.
rem #+
rem PUBLIC FUNCTION getMoreArgumentCount(gr GetoptR) RETURNS INTEGER
rem   IF gr[1].status == EOF THEN
rem     RETURN gr[1].argv.getLength() - gr[1].opt_ind + 1
rem   END IF
rem   RETURN -1
rem END FUNCTION
rem 
rem #+
rem #+ Returns the argument not part of the options, at the specified index.
rem #+
rem #+ The index starts at 1, for the first argument after the last option
rem #+ (The index offset resulting from the used options is taken into account).
rem #+
rem #+ Returns NULL if the index is not valid.
rem #+
rem #+ @code
rem #+ LET cnt = g.getMoreArgumentCount()
rem #+ IF cnt THEN
rem #+     FOR ind = 1 TO cnt
rem #+         DISPLAY SFMT("Additional argument: %1", g.getMoreArgument(ind))
rem #+     END FOR
rem #+ END IF
rem #+
rem #+ @param ind The (offset-adapted) index of the additional argument.
rem #+
rem #+ @return The value of the additional argument.
rem #+
rem PUBLIC FUNCTION getMoreArgument(gr GetoptR, ind INTEGER) RETURNS STRING
rem   DEFINE x INTEGER
rem   LET x = gr[1].opt_ind + ind - 1
rem   IF x > 0 AND x <= gr[1].argv.getLength() THEN
rem     RETURN gr[1].argv[x]
rem   ELSE
rem     RETURN NULL
rem   END IF
rem END FUNCTION
rem 
rem #+
rem #+ This method can be used while processing command line options with getopt(),
rem #+ to check if there are more options to be processed.
rem #+
rem #+ @return TRUE if the option parsing is done.
rem #+
rem PUBLIC FUNCTION isEof(gr GetoptR) RETURNS BOOLEAN
rem   RETURN gr[1].status == EOF
rem END FUNCTION
rem 
rem #+
rem #+ This method can be used after processing the command line arguments with
rem #+ getopt() in a loop, to check if the processing failed because of an invalid
rem #+ options.
rem #+
rem #+ @return TRUE if the option parsing detected an invalid argument.
rem #+
rem PUBLIC FUNCTION invalidOptionSeen(gr GetoptR) RETURNS BOOLEAN
rem   RETURN gr[1].status == BAD_OPTION
rem END FUNCTION
rem 
rem #+
rem #+ This method can be used after processing the command line arguments with
rem #+ getopt() in a loop, to check if the processing succeeded.
rem #+
rem #+ @return TRUE if the option parsing succeeded.
rem #+
rem PUBLIC FUNCTION isSuccess(gr GetoptR) RETURNS BOOLEAN
rem   RETURN gr[1].status == SUCCESS
rem END FUNCTION
rem 
rem #+
rem #+ This is the main method to call in a loop, to parse command line arguments
rem #+ according to the GetoptOptions definitions of the Getopt object.
rem #+
rem #+ @code
rem #+ WHILE g.getopt() == getopt.SUCCESS
rem #+     CASE g.opt_char
rem #+         WHEN "v"
rem #+             DISPLAY "Version: 1.54"
rem #+             EXIT PROGRAM 0
rem #+         ...
rem #+     END CASE
rem #+ END WHILE
rem #+
rem #+ @return The processing status (getopt.SUCCESS | getopt.EOF | getopt.BAD_ARGUMENT)
rem #+
rem PUBLIC FUNCTION getopt(gr GetoptR) RETURNS INTEGER
rem   DEFINE arg STRING
rem 
rem   IF gr[1].status != SUCCESS THEN
rem     RETURN gr[1].status
rem   END IF
rem   IF gr[1].next_char IS NULL THEN
rem     LET gr[1].opt_ind = gr[1].opt_ind + 1
rem 
rem     # Check if EOF
rem     IF gr[1].opt_ind > gr[1].argv.getLength() THEN
rem       RETURN eof(gr)
rem     END IF
rem 
rem     LET arg = gr[1].argv[gr[1].opt_ind]
rem 
rem     # Parameter is not an option - end of option parsing
rem     IF arg.getCharAt(1) != '-' THEN
rem       RETURN eof(gr)
rem     END IF
rem 
rem     IF arg == "--" THEN
rem       LET gr[1].opt_ind = gr[1].opt_ind + 1 # Skip -- end of option marker
rem       RETURN eof(gr)
rem     END IF
rem 
rem     # Check for long option parameters
rem     IF arg.getCharAt(2) == '-' THEN
rem       RETURN parseLong(gr)
rem     ELSE
rem       RETURN parseShort(gr)
rem     END IF
rem   ELSE
rem     # Parse concatenated short options
rem     RETURN parseShort(gr)
rem   END IF
rem END FUNCTION
rem 
rem #+
rem #+ Displays the command line usage of a program.
rem #+
rem #+ @param more_args The non option string to add to usage
rem #+
rem PUBLIC FUNCTION displayUsage(gr GetoptR, more_args STRING)
rem   DEFINE ind INTEGER
rem   DEFINE delta INTEGER
rem   DEFINE sb base.StringBuffer
rem   LET sb = base.StringBuffer.create()
rem   IF more_args IS NOT NULL THEN
rem     DISPLAY SFMT("Usage: %1 [options] %2\n", gr[1].prog_name, more_args)
rem   ELSE
rem     DISPLAY SFMT("Usage: %1 [options]\n", gr[1].prog_name)
rem   END IF
rem   IF more_args IS NOT NULL THEN
rem     DISPLAY "Options:"
rem   END IF
rem   FOR ind = 1 TO gr[1]._options.getLength()
rem     CALL sb.clear()
rem     IF gr[1]._options[ind].opt_char IS NULL THEN
rem       CASE gr[1]._options[ind].arg_type
rem         WHEN NONE
rem           CALL sb.append(SFMT("    --%1", gr[1]._options[ind].name))
rem         WHEN OPTIONAL
rem           CALL sb.append(SFMT("    --%1 [<arg>]", gr[1]._options[ind].name))
rem         WHEN REQUIRED
rem           CALL sb.append(SFMT("    --%1 <arg>", gr[1]._options[ind].name))
rem       END CASE
rem     ELSE
rem       CASE gr[1]._options[ind].arg_type
rem         WHEN NONE
rem           CALL sb.append(
rem               SFMT("    -%1, --%2",
rem                   gr[1]._options[ind].opt_char, gr[1]._options[ind].name))
rem         WHEN OPTIONAL
rem           CALL sb.append(
rem               SFMT("    -%1, --%2 [<arg>]",
rem                   gr[1]._options[ind].opt_char, gr[1]._options[ind].name))
rem         WHEN REQUIRED
rem           CALL sb.append(
rem               SFMT("    -%1, --%2 <arg>",
rem                   gr[1]._options[ind].opt_char, gr[1]._options[ind].name))
rem       END CASE
rem     END IF
rem     IF sb.getLength() <= ALIGN_SIZE THEN
rem       LET delta = ALIGN_SIZE - sb.getLength()
rem       DISPLAY SFMT("%1%2%3",
rem           sb.toString(), delta SPACES, gr[1]._options[ind].description)
rem     ELSE
rem       DISPLAY SFMT("%1\n%2%3",
rem           sb.toString(), ALIGN_SIZE SPACES, gr[1]._options[ind].description)
rem     END IF
rem   END FOR
rem END FUNCTION
rem 
rem # Set status to EOF
rem PRIVATE FUNCTION eof(gr GetoptR) RETURNS INTEGER
rem   LET gr[1].opt_char = NULL
rem   LET gr[1].opt_arg = NULL
rem   LET gr[1].status = EOF
rem   RETURN gr[1].status
rem END FUNCTION
rem 
rem # Set status to BAD_OPTION
rem PRIVATE FUNCTION bad_option(gr GetoptR) RETURNS INTEGER
rem   LET gr[1].opt_char = NULL
rem   LET gr[1].opt_arg = NULL
rem   LET gr[1].status = BAD_OPTION
rem   RETURN gr[1].status
rem END FUNCTION
rem 
rem # Parse current argument as a long option name
rem PRIVATE FUNCTION parseLong(gr GetoptR) RETURNS INTEGER
rem   DEFINE arg STRING
rem   DEFINE equal_sign_index INTEGER
rem   DEFINE ind INTEGER
rem   DEFINE opt STRING
rem   DEFINE opt_found INTEGER
rem   DEFINE arg_key STRING
rem   DEFINE arg_val STRING
rem   DEFINE ambiguous BOOLEAN
rem   DEFINE exact BOOLEAN
rem 
rem   LET arg = gr[1].argv[gr[1].opt_ind]
rem 
rem   # Split arg in key=value or just key
rem   LET equal_sign_index = arg.getIndexOf("=", 3)
rem   IF equal_sign_index > 3 THEN
rem     LET arg_key = arg.subString(3, equal_sign_index - 1)
rem     LET arg_val = arg.subString(equal_sign_index + 1, arg.getLength())
rem   ELSE
rem     LET arg_key = arg.subString(3, arg.getLength())
rem     LET arg_val = NULL
rem   END IF
rem 
rem   # Search through all options to get an exact match, a partial match, and ensure
rem   # partial match is unique or else we have an ambiguous match, which has to be rejected.
rem   FOR ind = 1 TO gr[1]._options.getLength()
rem     LET opt = gr[1]._options[ind].name
rem     IF arg_key.equals(opt) THEN
rem       LET opt_found = ind
rem       LET exact = TRUE
rem       EXIT FOR
rem     ELSE
rem       IF opt.getIndexOf(arg_key, 1) > 0 THEN
rem         IF NOT opt_found THEN
rem           LET opt_found = ind
rem         ELSE
rem           LET ambiguous = TRUE
rem         END IF
rem       END IF
rem     END IF
rem   END FOR
rem 
rem   IF ambiguous AND NOT exact THEN
rem     DISPLAY SFMT("%1: ambiguous match: --%2", gr[1].prog_name, arg_key)
rem     RETURN bad_option(gr)
rem   END IF
rem 
rem   IF opt_found == 0 THEN
rem     DISPLAY SFMT("%1: invalid option: --%2", gr[1].prog_name, arg_key)
rem     RETURN bad_option(gr)
rem   END IF
rem 
rem   LET gr[1].option_index = opt_found
rem   # Check for argument value
rem   IF arg_val IS NOT NULL THEN
rem     CASE gr[1]._options[opt_found].arg_type
rem       WHEN NONE
rem         DISPLAY SFMT("%1: erroneous argument: --%2", gr[1].prog_name, arg_key)
rem         RETURN BAD_OPTION
rem       OTHERWISE
rem         LET gr[1].opt_char = gr[1]._options[opt_found].opt_char
rem         LET gr[1].opt_arg = arg_val
rem         RETURN SUCCESS
rem     END CASE
rem   END IF
rem 
rem   # Check there is no parameter
rem   IF gr[1]._options[opt_found].arg_type == NONE THEN
rem     LET gr[1].opt_char = gr[1]._options[opt_found].opt_char
rem     LET gr[1].opt_arg = NULL
rem     RETURN SUCCESS
rem   ELSE
rem     # Otherwise optional or required
rem     IF gr[1].opt_ind + 1 <= gr[1].argv.getLength() THEN
rem       IF NOT isAnOption(gr, gr[1].argv[gr[1].opt_ind + 1]) THEN
rem         # Found parameter
rem         LET gr[1].opt_ind = gr[1].opt_ind + 1
rem         LET gr[1].opt_char = gr[1]._options[opt_found].opt_char
rem         LET gr[1].opt_arg = gr[1].argv[gr[1].opt_ind]
rem         RETURN SUCCESS
rem       END IF
rem     END IF
rem     # Parameter optional, return success
rem     IF gr[1]._options[opt_found].arg_type == OPTIONAL THEN
rem       LET gr[1].opt_char = gr[1]._options[opt_found].opt_char
rem       LET gr[1].opt_arg = NULL
rem       RETURN SUCCESS
rem     END IF
rem   END IF
rem 
rem   DISPLAY SFMT("%1: missing argument: --%2", gr[1].prog_name, arg_key)
rem   RETURN bad_option(gr)
rem 
rem END FUNCTION
rem 
rem # Parse current argument as a short option name
rem PRIVATE FUNCTION parseShort(gr GetoptR) RETURNS INTEGER
rem   DEFINE arg STRING
rem   DEFINE equal_sign_index INTEGER
rem   DEFINE ind INTEGER
rem   DEFINE opt_found INTEGER
rem   DEFINE arg_key STRING
rem   DEFINE arg_val STRING
rem 
rem   IF gr[1].next_char IS NULL THEN
rem     LET arg = gr[1].argv[gr[1].opt_ind]
rem 
rem     # Split arg in key=value or just key
rem     LET equal_sign_index = arg.getIndexOf("=", 2)
rem     IF equal_sign_index > 2 THEN
rem       LET arg_key = arg.subString(2, equal_sign_index - 1)
rem       LET arg_val = arg.subString(equal_sign_index + 1, arg.getLength())
rem     ELSE
rem       LET arg_key = arg.subString(2, arg.getLength())
rem       IF arg_key.getLength() > 1 THEN
rem         LET gr[1].next_char = arg_key.subString(2, arg_key.getLength())
rem         LET arg_key = arg_key.subString(1, 1)
rem       ELSE
rem         LET gr[1].next_char = NULL
rem       END IF
rem       LET arg_val = NULL
rem     END IF
rem   ELSE
rem     # Parse concatenated short option
rem     LET arg_key = gr[1].next_char.subString(1, 1)
rem     IF gr[1].next_char.getLength() > 1 THEN
rem       LET gr[1].next_char =
rem           gr[1].next_char.subString(2, gr[1].next_char.getLength())
rem     ELSE
rem       LET gr[1].next_char = NULL
rem     END IF
rem     LET arg_val = NULL
rem   END IF
rem 
rem   # Lookup for single char
rem   FOR ind = 1 TO gr[1]._options.getLength()
rem     IF arg_key.equals(gr[1]._options[ind].opt_char) THEN
rem       LET opt_found = ind
rem       EXIT FOR
rem     END IF
rem   END FOR
rem 
rem   IF opt_found == 0 THEN
rem     DISPLAY SFMT("%1: invalid option character: -%2", gr[1].prog_name, arg_key)
rem     RETURN bad_option(gr)
rem   END IF
rem 
rem   LET gr[1].option_index = opt_found
rem   # Check there is no parameter
rem   IF gr[1]._options[opt_found].arg_type == NONE THEN
rem     IF arg_val IS NOT NULL THEN
rem       DISPLAY SFMT("%1: erroneous argument: -%2", gr[1].prog_name, arg_key)
rem       RETURN bad_option(gr)
rem     ELSE
rem       LET gr[1].opt_char = gr[1]._options[opt_found].opt_char
rem       LET gr[1].opt_arg = NULL
rem       RETURN SUCCESS
rem     END IF
rem   ELSE
rem     # Otherwise optional or required
rem     IF arg_val IS NOT NULL THEN
rem       LET gr[1].opt_char = gr[1]._options[opt_found].opt_char
rem       LET gr[1].opt_arg = arg_val
rem       RETURN SUCCESS
rem     END IF
rem     IF gr[1].opt_ind + 1 <= gr[1].argv.getLength() THEN
rem       IF NOT isAnOption(gr, gr[1].argv[gr[1].opt_ind + 1]) THEN
rem         # Found parameter
rem         LET gr[1].opt_ind = gr[1].opt_ind + 1
rem         LET gr[1].opt_char = gr[1]._options[opt_found].opt_char
rem         LET gr[1].opt_arg = gr[1].argv[gr[1].opt_ind]
rem         RETURN SUCCESS
rem       END IF
rem     END IF
rem     # Parameter optional, return success
rem     IF gr[1]._options[opt_found].arg_type == OPTIONAL THEN
rem       LET gr[1].opt_char = gr[1]._options[opt_found].opt_char
rem       LET gr[1].opt_arg = NULL
rem       RETURN SUCCESS
rem     END IF
rem   END IF
rem 
rem   DISPLAY SFMT("%1: missing argument: -%2", gr[1].prog_name, arg_key)
rem   RETURN bad_option(gr)
rem 
rem END FUNCTION
rem __CAT_EOF_END__
rem __CAT_EOF_BEGIN__:fglunzip.4gl
rem OPTIONS
rem SHORT CIRCUIT
rem IMPORT os
rem IMPORT FGL mygetopt
rem &include "fglunzip_version.inc"
rem &include "myassert.inc"
rem DEFINE _product_zip STRING --the zip file to process
rem DEFINE _opt_verbose BOOLEAN
rem DEFINE _opt_in_FGLDIR BOOLEAN
rem DEFINE _opt_simulate BOOLEAN
rem DEFINE _opt_undo BOOLEAN
rem DEFINE _opt_plain BOOLEAN
rem --DEFINE _stdoutNONL STRING
rem --TODO
rem --DEFINE _opt_quiet BOOLEAN
rem --DEFINE _opt_logfile STRING
rem --DEFINE _opt_ext_dir STRING
rem MAIN
rem   DEFINE argsarr DYNAMIC ARRAY OF STRING
rem   DEFINE root om.DomNode
rem   DEFINE numChildren INT
rem   IF yesno_mode() THEN
rem     RETURN
rem   END IF
rem   CALL checkTar()
rem   LET argsarr = setupArgs()
rem   --DISPLAY "argsarr:",util.JSON.stringify(argsarr)
rem   CALL parseArgs(argsarr)
rem   LET root = readFiles()
rem   LET numChildren = analyze(root)
rem   IF numChildren == 0 THEN
rem     CALL userError(SFMT("no entries found in:%1", _product_zip))
rem   END IF
rem   CALL doit(root, numChildren)
rem END MAIN
rem 
rem FUNCTION tarExe()
rem   RETURN IIF(isWin(), "tar.exe", "tar")
rem END FUNCTION
rem 
rem FUNCTION checkTar()
rem   DEFINE tar STRING
rem   LET tar = tarExe()
rem   IF whichExe(tar) IS NULL THEN
rem     CALL userError(
rem         SFMT("Couldn't find program:%1 on your system, please install", tar))
rem   END IF
rem END FUNCTION
rem 
rem FUNCTION unzipList()
rem   DEFINE cmd STRING
rem   LET cmd = SFMT("%1 tf %2", tarExe(), quote(_product_zip))
rem   --DISPLAY "unzipList cmd:", cmd
rem   RETURN getProgramOutput(cmd)
rem END FUNCTION
rem 
rem FUNCTION setupArgs()
rem   DEFINE i INT
rem   DEFINE argsarr DYNAMIC ARRAY OF STRING
rem   FOR i = 1 TO num_args()
rem     LET argsarr[i] = arg_val(i)
rem   END FOR
rem   RETURN argsarr
rem END FUNCTION
rem 
rem PRIVATE FUNCTION parseArgs(argsarr)
rem   DEFINE argsarr DYNAMIC ARRAY OF STRING
rem   DEFINE gr mygetopt.GetoptR
rem   DEFINE o mygetopt.GetoptOptions
rem   DEFINE opt_arg STRING
rem   DEFINE i, cnt INT
rem   DEFINE listSeen INT
rem 
rem   LET i = o.getLength() + 1
rem   LET o[i].name = "version"
rem   LET o[i].description = "Version information"
rem   LET o[i].opt_char = "V"
rem   LET o[i].arg_type = mygetopt.NONE
rem 
rem   LET i = o.getLength() + 1
rem   LET o[i].name = "help"
rem   LET o[i].description = "program help"
rem   LET o[i].opt_char = "h"
rem   LET o[i].arg_type = mygetopt.NONE
rem 
rem   LET i = o.getLength() + 1
rem   LET o[i].name = "verbose"
rem   LET o[i].description = "detailed log"
rem   LET o[i].opt_char = "v"
rem   LET o[i].arg_type = mygetopt.NONE
rem 
rem   LET i = o.getLength() + 1
rem   LET o[i].name = "simulate"
rem   LET o[i].description = "simulates what would be extracted/undone"
rem   LET o[i].opt_char = "s"
rem   LET o[i].arg_type = mygetopt.NONE
rem 
rem   LET i = o.getLength() + 1
rem   LET o[i].name = "list"
rem   LET o[i].description = "Lists the archive content"
rem   LET o[i].opt_char = "l"
rem   LET o[i].arg_type = mygetopt.NONE
rem 
rem   LET i = o.getLength() + 1
rem   LET o[i].name = "use-FGLDIR"
rem   LET o[i].description =
rem       "installs over FGLDIR to make the product avaiable without further env settings"
rem   LET o[i].opt_char = "F"
rem   LET o[i].arg_type = mygetopt.NONE
rem 
rem   LET i = o.getLength() + 1
rem   LET o[i].name = "like-unzip"
rem   LET o[i].description =
rem       "extracts multiple files/directories in the root directory of the archive like unzip 'plain' in the current directory"
rem   LET o[i].opt_char = "i"
rem   LET o[i].arg_type = mygetopt.NONE
rem 
rem   LET i = o.getLength() + 1
rem   LET o[i].name = "undo"
rem   LET o[i].description = "Reverts the install"
rem   LET o[i].opt_char = "u"
rem   LET o[i].arg_type = mygetopt.NONE
rem 
rem   { --TODO
rem   LET i = o.getLength() + 1
rem   LET o[i].name = "quiet"
rem   LET o[i].description = "Does install quietly without asking yes/no"
rem   LET o[i].opt_char = "q"
rem   LET o[i].arg_type = mygetopt.NONE
rem 
rem   LET i = o.getLength() + 1
rem   LET o[i].name = "logfile"
rem   LET o[i].description = "File written for logs and success"
rem   LET o[i].opt_char = "L"
rem   LET o[i].arg_type = mygetopt.REQUIRED
rem 
rem   LET i = o.getLength() + 1
rem   LET o[i].name = "destination-dir"
rem   LET o[i].description =
rem       "choose another extraction directory than the current dir"
rem   LET o[i].opt_char = "d"
rem   LET o[i].arg_type = mygetopt.NONE
rem   }
rem 
rem   CALL mygetopt.initialize(gr, "fglunzip", argsarr, o)
rem   WHILE mygetopt.getopt(gr) == mygetopt.SUCCESS
rem     LET opt_arg = mygetopt.opt_arg(gr)
rem     CASE mygetopt.opt_char(gr)
rem       WHEN 'V'
rem         CALL printVersion()
rem         EXIT PROGRAM 0
rem       WHEN 'v'
rem         LET _opt_verbose = TRUE
rem       WHEN 'h'
rem         CALL mygetopt.displayUsage(gr, "fjs-<product>.zip")
rem         EXIT PROGRAM 0
rem       WHEN 'l'
rem         LET listSeen = TRUE
rem       WHEN 'F'
rem         LET _opt_in_FGLDIR = TRUE
rem       WHEN 's'
rem         LET _opt_simulate = TRUE
rem       WHEN 'u'
rem         LET _opt_undo = TRUE
rem       WHEN 'i'
rem         LET _opt_plain = TRUE
rem         { --TODO
rem         WHEN 'q'
rem           LET _opt_quiet = TRUE
rem         WHEN 'L'
rem           LET _opt_logfile = opt_arg
rem         WHEN 'd'
rem           LET _opt_ext_dir = opt_arg
rem         }
rem     END CASE
rem   END WHILE
rem   IF (cnt := mygetopt.getMoreArgumentCount(gr)) <> 1 THEN
rem     CALL mygetopt.displayUsage(gr, "fjs-<product>.zip")
rem     EXIT PROGRAM 1
rem   END IF
rem   LET _product_zip = mygetopt.getMoreArgument(gr, 1)
rem   LET _product_zip = os.Path.fullPath(_product_zip)
rem   IF listSeen THEN
rem     DISPLAY unzipList()
rem     EXIT PROGRAM 0
rem   END IF
rem   { --TODO
rem   IF _opt_in_FGLDIR AND _opt_ext_dir IS NOT NULL THEN
rem     CALL userError(
rem         "option --use-FGLDIR(-F) and --destination-dir(-d) are mutually exclusive")
rem   END IF
rem   }
rem END FUNCTION
rem 
rem PRIVATE FUNCTION readFiles()
rem   DEFINE raw STRING
rem   DEFINE tok base.StringTokenizer
rem   DEFINE path STRING
rem   DEFINE doc om.DomDocument
rem   DEFINE root {,lastNode} om.DomNode
rem   LET doc = om.DomDocument.create("Files")
rem   LET root = doc.getDocumentElement()
rem   LET raw = unzipList()
rem   LET raw = replace(raw, "\r\n", "\n") --windows
rem   LET tok = base.StringTokenizer.create(raw, "\n")
rem   WHILE tok.hasMoreTokens()
rem     LET path = tok.nextToken()
rem     IF path.getLength() > 0 THEN
rem       CALL addFile(root, path)
rem     END IF
rem   END WHILE
rem   --DISPLAY "readFiles did get:",root.toString()
rem   RETURN root
rem END FUNCTION
rem 
rem --loop thru the path parts
rem PRIVATE FUNCTION findFileNode(parent, path, createIfNotFound)
rem   DEFINE path, part, tName, full STRING
rem   DEFINE parent, child, newchild om.DomNode
rem   DEFINE createIfNotFound, found BOOLEAN
rem   DEFINE tok base.StringTokenizer
rem   LET tok = base.StringTokenizer.create(path, "/")
rem   WHILE tok.hasMoreTokens()
rem     LET found = FALSE
rem     LET part = tok.nextToken()
rem     IF createIfNotFound THEN
rem       LET full = IIF(full IS NULL, part, SFMT("%1/%2", full, part))
rem     END IF
rem     --DISPLAY "begin handle part:",part," for parent:",parent.getTagName(),",full:",full
rem     LET child = parent.getFirstChild()
rem     WHILE child IS NOT NULL
rem       IF child.getAttribute("name") == part THEN
rem         LET found = TRUE
rem         --DISPLAY sfmt("found childTag:%1 for parent:%2",part,parent.getTagName())
rem         LET parent = child
rem         EXIT WHILE
rem       ELSE
rem         LET child = child.getNext()
rem       END IF
rem     END WHILE
rem     IF NOT found THEN
rem       IF NOT createIfNotFound THEN
rem         RETURN NULL
rem       END IF
rem       IF path.getCharAt(full.getLength() + 1)
rem           == "/" THEN --this full part ends with a slash
rem         LET tName = "Dir"
rem       ELSE
rem         LET tName = "File"
rem       END IF
rem       LET newchild = parent.createChild(tName)
rem       CALL newchild.setAttribute("name", part)
rem       CALL parent.setAttribute("isDir", "1")
rem       --DISPLAY sfmt("created newchild for tag:%1,parentName:%2",part,parent.getAttribute("name"))
rem       LET parent = newchild
rem     END IF
rem   END WHILE
rem   RETURN parent
rem END FUNCTION
rem 
rem PRIVATE FUNCTION addFile(parent, path)
rem   DEFINE parent, node om.DomNode
rem   DEFINE path STRING
rem   --DISPLAY "addFile:",path
rem   LET node = findFileNode(parent, path, TRUE)
rem   --DISPLAY "added:",node.toString()
rem END FUNCTION
rem 
rem PRIVATE FUNCTION fileExists(root, name)
rem   DEFINE root, node om.DomNode
rem   DEFINE name STRING
rem   LET node = findFileNode(root, name, FALSE)
rem   RETURN node IS NOT NULL
rem END FUNCTION
rem 
rem PRIVATE FUNCTION isDir(root, name)
rem   DEFINE root, node om.DomNode
rem   DEFINE name STRING
rem   LET node = findFileNode(root, name, FALSE)
rem   RETURN node IS NOT NULL AND node.getTagName() == "Dir"
rem END FUNCTION
rem 
rem PRIVATE FUNCTION analyze(root)
rem   DEFINE root, child {, lastChild} om.DomNode
rem   DEFINE numChildren INT
rem   --DEFINE children DYNAMIC ARRAY OF om.DomNode
rem   LET child = root.getFirstChild()
rem   WHILE child IS NOT NULL
rem     LET numChildren = numChildren + 1
rem     --LET children[children.getLength()+1]=child.getAttribute("name")
rem     --LET lastChild = child
rem     LET child = child.getNext()
rem   END WHILE
rem   CALL root.setAttribute("numChildren", numChildren)
rem   RETURN numChildren
rem END FUNCTION
rem 
rem PRIVATE FUNCTION getNumChildren(root)
rem   DEFINE root om.DomNode
rem   RETURN root.getAttribute("numChildren")
rem END FUNCTION
rem 
rem PRIVATE FUNCTION doit(root, numChildren)
rem   DEFINE root om.DomNode
rem   DEFINE numChildren INT
rem   DEFINE defRoot STRING
rem   {
rem   IF _opt_ext_dir IS NOT NULL THEN
rem     CALL mkdirp(_opt_ext_dir)
rem     MYASSERT(os.Path.chDir(_opt_ext_dir) == TRUE)
rem   END IF
rem   }
rem   IF numChildren == 1 THEN --single root , no need to compute one
rem     IF _opt_in_FGLDIR THEN
rem       CALL userError(
rem           "This package is not prepared to be installed over FGLDIR(yet).")
rem     END IF
rem     CALL unzip(root)
rem   ELSE
rem     IF _opt_in_FGLDIR THEN
rem       CALL unzipOverFGLDIR(root)
rem     ELSE
rem       IF NOT _opt_plain THEN --by default create a single root directory to avoid cluttering the current directory with multiple files and extract the zip beneath that single root directory
rem         LET defRoot = computeDefName()
rem         IF NOT os.Path.exists(defRoot) THEN
rem           CALL mkdirp(defRoot)
rem           IF _opt_verbose THEN
rem             DISPLAY "created extraction root:", os.Path.fullPath(defRoot)
rem           END IF
rem         END IF
rem         MYASSERT(os.Path.chDir(defRoot) == TRUE)
rem       ELSE
rem         IF NOT _opt_simulate AND NOT _opt_undo THEN
rem           CALL checkFilesExisting(root)
rem         END IF
rem       END IF
rem       CALL unzip(root)
rem     END IF
rem   END IF
rem END FUNCTION
rem 
rem PRIVATE FUNCTION unzip(root)
rem   DEFINE root om.DomNode
rem   DEFINE cmd STRING
rem   DEFINE code INT
rem   IF _opt_simulate THEN
rem     IF _opt_undo THEN
rem       DISPLAY "Would remove in:", os.Path.pwd()
rem       DISPLAY "(N no file/dir) (D remove dir if empty) (F remove File) (C conflict)"
rem     ELSE
rem       DISPLAY "Would extract in:", os.Path.pwd()
rem       DISPLAY "(N new file/dir) (D overwrite dir) (F overwrite File) (C conflict)"
rem     END IF
rem     CALL simulate(root, os.Path.pwd())
rem     RETURN
rem   END IF
rem   IF _opt_undo THEN
rem     CALL undo(root, os.Path.pwd())
rem     RETURN
rem   END IF
rem   --CALL generateUndoScript(root)
rem   LET cmd = SFMT("%1 xf %2", tarExe(), quote(_product_zip))
rem   IF _opt_verbose THEN
rem     DISPLAY "unzip cmd:", cmd
rem   END IF
rem   RUN cmd RETURNING code
rem   IF code THEN
rem     EXIT PROGRAM code
rem   END IF
rem   CALL verify(root, os.Path.pwd())
rem END FUNCTION
rem 
rem PRIVATE FUNCTION undo(parent, parentDir)
rem   DEFINE parent, child om.DomNode
rem   DEFINE parentDir, path, tag STRING
rem   LET child = parent.getFirstChild()
rem   WHILE child IS NOT NULL
rem     LET path = os.Path.join(parentDir, child.getAttribute("name"))
rem     LET tag = child.getTagName()
rem     IF tag == "File" THEN
rem       IF os.Path.exists(path) THEN
rem         IF NOT os.Path.delete(path) THEN
rem           DISPLAY "couldn't delete:", path
rem         ELSE
rem           IF _opt_verbose THEN
rem             DISPLAY "deleted file:", path
rem           END IF
rem         END IF
rem       END IF
rem     END IF
rem     CALL undo(child, path)
rem     IF tag == "Dir" THEN
rem       IF os.Path.exists(path) AND os.Path.isDirectory(path) THEN
rem         IF NOT os.Path.delete(path) THEN
rem           DISPLAY "Could not delete dir:", path, ",probably not empty"
rem         ELSE
rem           IF _opt_verbose THEN
rem             DISPLAY "deleted dir:", path, "/"
rem           END IF
rem         END IF
rem       END IF
rem     END IF
rem     LET child = child.getNext()
rem   END WHILE
rem END FUNCTION
rem 
rem #+check if the unzip command did work
rem PRIVATE FUNCTION verify(parent, parentDir)
rem   DEFINE parent, child om.DomNode
rem   DEFINE parentDir, path, tag STRING
rem   LET child = parent.getFirstChild()
rem   WHILE child IS NOT NULL
rem     LET path = os.Path.join(parentDir, child.getAttribute("name"))
rem     LET tag = child.getTagName()
rem     CASE
rem       WHEN tag == "Dir"
rem         MYASSERT(os.Path.isDirectory(path))
rem       WHEN tag == "File"
rem         MYASSERT(os.Path.isFile(path))
rem       OTHERWISE
rem         CALL myErr(SFMT("unexpected tagName:%1", tag))
rem     END CASE
rem     IF _opt_verbose THEN
rem       DISPLAY "verified:", path
rem     END IF
rem     CALL verify(child, path)
rem     LET child = child.getNext()
rem   END WHILE
rem END FUNCTION
rem 
rem #+check if the unzip command did work
rem PRIVATE FUNCTION simulate(parent, parentDir)
rem   DEFINE parent, child om.DomNode
rem   DEFINE parentDir, path, tag, marker STRING
rem   LET child = parent.getFirstChild()
rem   WHILE child IS NOT NULL
rem     LET path = os.Path.join(parentDir, child.getAttribute("name"))
rem     LET tag = child.getTagName()
rem     CASE
rem       WHEN tag == "Dir"
rem         IF os.Path.exists(path) THEN
rem           LET marker = IIF(os.Path.isFile(path), "C", "D")
rem         ELSE
rem           LET marker = "N"
rem         END IF
rem         DISPLAY SFMT("%1 %2%3", marker, path, os.Path.separator())
rem         IF marker == "C" THEN
rem           DISPLAY "  expected: directory, actual: file"
rem         END IF
rem       WHEN tag == "File"
rem         IF os.Path.exists(path) THEN
rem           LET marker = IIF(os.Path.isDirectory(path), "C", "F")
rem         ELSE
rem           LET marker = "N"
rem         END IF
rem         DISPLAY SFMT("%1 %2", marker, path)
rem         IF marker == "C" THEN
rem           DISPLAY "  expected: file, actual: directory"
rem         END IF
rem       OTHERWISE
rem         CALL myErr(SFMT("unexpected tagName:%1", tag))
rem     END CASE
rem     CALL simulate(child, path)
rem     LET child = child.getNext()
rem   END WHILE
rem END FUNCTION
rem 
rem PRIVATE FUNCTION checkFilesExisting(root)
rem   DEFINE root om.DomNode
rem   DEFINE dh INT
rem   DEFINE fname, ans STRING
rem   DEFINE foundEntries INT
rem   LET dh = os.Path.dirOpen(os.Path.pwd())
rem   IF dh == 0 THEN
rem     CALL userError(SFMT("Can't open directory '%1'", os.Path.pwd()))
rem   END IF
rem   WHILE (fname := os.Path.dirNext(dh)) IS NOT NULL
rem     IF fname == "." OR fname == ".." THEN
rem       CONTINUE WHILE
rem     END IF
rem     LET foundEntries = foundEntries + 1
rem   END WHILE
rem   CALL os.Path.dirClose(dh)
rem   IF foundEntries > 0 THEN
rem     LET ans =
rem         yesno(
rem             SFMT("There are already %1 files, folders or links in this directory...\nContinue extracting %2 files ?",
rem                 foundEntries, getNumChildren(root)))
rem     --DISPLAY "ans:",ans
rem     IF NOT ans.equals("yes") THEN
rem       EXIT PROGRAM 1
rem     END IF
rem   END IF
rem END FUNCTION
rem 
rem PRIVATE FUNCTION isGBC(root)
rem   DEFINE root om.DomNode
rem   RETURN fileExists(root, "VERSION")
rem       AND fileExists(root, "PRODUCTINFO")
rem       AND fileExists(root, "index.html")
rem       AND fileExists(root, "js/gbc.js")
rem END FUNCTION
rem 
rem PRIVATE FUNCTION myChdir(path)
rem   DEFINE path STRING
rem   IF NOT os.Path.chDir(path) THEN
rem     CALL myErr(SFMT("Can't chdir to:%1", path))
rem   END IF
rem END FUNCTION
rem 
rem PRIVATE FUNCTION getFglDir()
rem   RETURN base.Application.getFglDir()
rem END FUNCTION
rem 
rem PRIVATE FUNCTION unzipGBCoverFGLDIR(root)
rem   DEFINE root om.DomNode
rem   DEFINE gbcDir STRING
rem   LET gbcDir = SFMT("%1/web_utilities/gbc/gbc", getFglDir())
rem   CALL mkdirp(gbcDir)
rem   CALL myChdir(gbcDir)
rem   CALL unzip(root)
rem END FUNCTION
rem 
rem PRIVATE FUNCTION unzipOverFGLDIR(root)
rem   DEFINE root om.DomNode
rem   IF isGBC(root) THEN
rem     CALL unzipGBCoverFGLDIR(root)
rem   ELSE
rem     CALL myChdir(getFglDir())
rem     CALL unzip(root)
rem   END IF
rem END FUNCTION
rem 
rem PRIVATE FUNCTION generateUndoScript(root)
rem   DEFINE root om.DomNode
rem   DEFINE ch_sh, ch_bat base.Channel
rem   DEFINE name_sh, name_bat STRING
rem   LET ch_sh = base.Channel.create()
rem   LET name_sh = SFMT("rm-%1.sh", computeDefName())
rem   CALL ch_sh.openFile(name_sh, "w")
rem   CALL ch_sh.writeLine("#!/bin/bash")
rem   IF isWin() THEN
rem     LET ch_bat = base.Channel.create()
rem     LET name_bat = SFMT("rm-%1.bat", computeDefName())
rem     CALL ch_bat.openFile(name_bat, "w")
rem     CALL ch_bat.writeLine("@echo off")
rem   END IF
rem   CALL add_rm(root, ".", ch_sh, ch_bat)
rem   CALL ch_sh.writeLine(SFMT("rm -f %1", quote(name_sh)))
rem   CALL ch_sh.close()
rem   MYASSERT(os.Path.chRwx(name_sh, 484) == TRUE) --u+x
rem END FUNCTION
rem 
rem PRIVATE FUNCTION add_rm(parent, parentDir, ch_sh, ch_bat)
rem   DEFINE parent, child om.DomNode
rem   DEFINE parentDir, path, winpath STRING
rem   DEFINE ch_sh, ch_bat base.Channel
rem   LET child = parent.getFirstChild()
rem   WHILE child IS NOT NULL
rem     LET path = SFMT("%1/%2", parentDir, child.getAttribute("name"))
rem     LET winpath = replace(path, "/", "\\")
rem     IF child.getTagName() == "File" THEN
rem       CALL ch_sh.writeLine(SFMT("rm -f %1", quote(path)))
rem       IF isWin() THEN
rem         CALL ch_bat.writeLine(SFMT("del /Q %1", quote(winpath)))
rem       END IF
rem     END IF
rem     CALL add_rm(child, path, ch_sh, ch_bat)
rem     IF child.getTagName() == "Dir" THEN
rem       CALL ch_sh.writeLine(SFMT("rmdir %1", quote(path)));
rem       IF isWin() THEN
rem         CALL ch_bat.writeLine(SFMT("rmdir %1", quote(winpath)));
rem       END IF
rem     END IF
rem     LET child = child.getNext()
rem   END WHILE
rem END FUNCTION
rem 
rem #+ for zip archives not having a single root we create a root dir named after the product file name (similar to what desktop extraction tools do)
rem PRIVATE FUNCTION computeDefName()
rem   DEFINE def, b STRING
rem   DEFINE idx1, idx2 INT
rem   LET b = os.Path.baseName(_product_zip)
rem   IF b.getIndexOf("fjs-", 1) == 1 THEN
rem     LET idx1 = b.getIndexOf("-", 5)
rem     MYASSERT(idx1 != 0)
rem     LET idx2 = b.getIndexOf("-", idx1 + 1)
rem     LET def = b.subString(5, idx2 - 1)
rem   ELSE
rem     LET def = removeExtension(b)
rem   END IF
rem   --DISPLAY "defname:", def
rem   RETURN def
rem END FUNCTION
rem 
rem --utils
rem 
rem PRIVATE FUNCTION isWin() RETURNS BOOLEAN
rem   RETURN os.Path.separator().equals("\\")
rem END FUNCTION
rem 
rem PRIVATE FUNCTION printStderr(errstr STRING)
rem   DEFINE ch base.Channel
rem   LET ch = base.Channel.create()
rem   CALL ch.openFile("<stderr>", "w")
rem   CALL ch.writeLine(errstr)
rem   CALL ch.close()
rem END FUNCTION
rem {
rem PRIVATE FUNCTION printStdout(str STRING, noNewLine BOOLEAN)
rem   IF noNewLine THEN
rem     LET _stdoutNONL = _stdoutNONL, str
rem   ELSE
rem     LET str = _stdoutNONL, str
rem     LET _stdoutNONL = ""
rem     DISPLAY str
rem   END IF
rem END FUNCTION
rem }
rem 
rem PRIVATE FUNCTION myErr(errstr STRING)
rem   CALL printStderr(
rem       SFMT("ERROR:%1 stack:\n%2", errstr, base.Application.getStackTrace()))
rem   EXIT PROGRAM 1
rem END FUNCTION
rem {
rem PRIVATE FUNCTION myWarning(errstr STRING)
rem   CALL printStderr(SFMT("Warning %1:%2", progName(), errstr))
rem END FUNCTION
rem 
rem PRIVATE FUNCTION log(msg STRING)
rem   IF fgl_getenv("VERBOSE") IS NOT NULL THEN
rem     DISPLAY "log:", msg
rem   END IF
rem END FUNCTION
rem 
rem --for dev: replace log() with dlog() for simply write to stdout
rem PRIVATE FUNCTION dlog(s STRING)
rem   DISPLAY s
rem END FUNCTION
rem }
rem 
rem PRIVATE FUNCTION already_quoted(path) RETURNS BOOLEAN
rem   DEFINE path, first, last STRING
rem   LET first = NVL(path.getCharAt(1), "NULL")
rem   LET last = NVL(path.getCharAt(path.getLength()), "NULL")
rem   IF isWin() THEN
rem     RETURN (first == '"' AND last == '"')
rem   END IF
rem   RETURN (first == "'" AND last == "'") OR (first == '"' AND last == '"')
rem END FUNCTION
rem 
rem PRIVATE FUNCTION quote(path STRING) RETURNS STRING
rem   RETURN quoteInt(path, FALSE)
rem END FUNCTION
rem 
rem PRIVATE FUNCTION quoteForce(path STRING) RETURNS STRING
rem   RETURN quoteInt(path, TRUE)
rem END FUNCTION
rem 
rem PRIVATE FUNCTION quoteInt(path STRING, force BOOLEAN) RETURNS STRING
rem   IF force OR path.getIndexOf(" ", 1) > 0 THEN
rem     IF NOT already_quoted(path) THEN
rem       LET path = '"', path, '"'
rem     END IF
rem   ELSE
rem     IF already_quoted(path) AND isWin() THEN --remove quotes(Windows)
rem       LET path = path.subString(2, path.getLength() - 1)
rem     END IF
rem   END IF
rem   RETURN path
rem END FUNCTION
rem 
rem PRIVATE FUNCTION replace(
rem     src STRING, oldStr STRING, newString STRING)
rem     RETURNS STRING
rem   DEFINE b base.StringBuffer
rem   LET b = base.StringBuffer.create()
rem   CALL b.append(src)
rem   CALL b.replace(oldStr, newString, 0)
rem   RETURN b.toString()
rem END FUNCTION
rem 
rem PRIVATE FUNCTION backslash2slash(src STRING) RETURNS STRING
rem   RETURN replace(src, "\\", "/")
rem END FUNCTION
rem 
rem #+case insensitive variant of getIndexOf
rem PRIVATE FUNCTION getIndexOfI(src, pattern, idx) RETURNS INT
rem   DEFINE src, pattern STRING
rem   DEFINE idx INTEGER
rem   LET src = src.toLowerCase()
rem   RETURN src.getIndexOf(pattern.toLowerCase(), idx)
rem END FUNCTION
rem 
rem PRIVATE FUNCTION getProgramOutputWithErr(cmd STRING) RETURNS(STRING, STRING)
rem   DEFINE cmdOrig, tmpName, errStr STRING
rem   DEFINE txt TEXT
rem   DEFINE ret STRING
rem   DEFINE code INT
rem   LET cmdOrig = cmd
rem   LET tmpName = makeTempName()
rem   LET cmd = cmd, ">", tmpName, " 2>&1"
rem   --CALL log(sfmt("run:%1", cmd))
rem   RUN cmd RETURNING code
rem   LOCATE txt IN FILE tmpName
rem   LET ret = txt
rem   CALL os.Path.delete(tmpName) RETURNING status
rem   IF code THEN
rem     LET errStr = ",\noutput:", ret
rem     CALL os.Path.delete(tmpName) RETURNING code
rem   ELSE
rem     --remove \r\n
rem     IF ret.getCharAt(ret.getLength()) == "\n" THEN
rem       LET ret = ret.subString(1, ret.getLength() - 1)
rem     END IF
rem     IF ret.getCharAt(ret.getLength()) == "\r" THEN
rem       LET ret = ret.subString(1, ret.getLength() - 1)
rem     END IF
rem   END IF
rem   RETURN ret, errStr
rem END FUNCTION
rem 
rem PRIVATE FUNCTION getProgramOutput(cmd STRING) RETURNS STRING
rem   DEFINE result, err STRING
rem   CALL getProgramOutputWithErr(cmd) RETURNING result, err
rem   IF err IS NOT NULL THEN
rem     CALL userError(SFMT("failed to RUN:%1%2", cmd, err))
rem   END IF
rem   RETURN result
rem END FUNCTION
rem 
rem #+computes a temporary file name
rem PRIVATE FUNCTION makeTempName() RETURNS STRING
rem   DEFINE tmpDir, tmpName, sbase, curr STRING
rem   DEFINE sb base.StringBuffer
rem   DEFINE i INT
rem   IF isWin() THEN
rem     LET tmpDir = fgl_getenv("TEMP")
rem   ELSE
rem     LET tmpDir = "/tmp"
rem   END IF
rem   LET curr = CURRENT
rem   LET sb = base.StringBuffer.create()
rem   CALL sb.append(curr)
rem   CALL sb.replace(" ", "_", 0)
rem   CALL sb.replace(":", "_", 0)
rem   CALL sb.replace(".", "_", 0)
rem   CALL sb.replace("-", "_", 0)
rem   LET sbase = SFMT("fgl_%1_%2", fgl_getpid(), sb.toString())
rem   LET sbase = os.Path.join(tmpDir, sbase)
rem   FOR i = 1 TO 10000
rem     LET tmpName = SFMT("%1%2.tmp", sbase, i)
rem     IF NOT os.Path.exists(tmpName) THEN
rem       RETURN tmpName
rem     END IF
rem   END FOR
rem   CALL myErr("makeTempName:Can't allocate a unique name")
rem   RETURN NULL
rem END FUNCTION
rem 
rem -- returns
rem -- -<count>-g<SHA> for a non release
rem -- -<SHA> for a release (GIT_COMMIT_COUNT==0)
rem PRIVATE FUNCTION adjustGitCountAndRev(cnt INT, rev STRING)
rem   --don't display 0 and g in the revision for official releases
rem   VAR countInfo = IIF(cnt == 0, "", SFMT("-%1", cnt))
rem   LET rev = IIF(cnt == 0, rev.subString(2, rev.getLength()), rev)
rem   VAR ret = SFMT("%1-%2", countInfo, rev)
rem   VAR warn
rem       = IIF(cnt == 0,
rem           "",
rem           " (nightly build - not suitable for production purposes)")
rem   LET ret = SFMT("%1%2", ret, warn)
rem   RETURN ret
rem END FUNCTION
rem 
rem PRIVATE FUNCTION removeExtension(fname STRING) RETURNS STRING
rem   VAR ext = os.Path.extension(fname)
rem   IF ext.getLength() > 0 THEN
rem     LET fname = fname.subString(1, fname.getLength() - (ext.getLength() + 1))
rem   END IF
rem   RETURN fname
rem END FUNCTION
rem 
rem PRIVATE FUNCTION printVersion()
rem   VAR prog = removeExtension(os.Path.baseName(arg_val(0)))
rem   DISPLAY SFMT("%1 %2 rev%3",
rem       prog, GIT_VERSION, adjustGitCountAndRev(GIT_COMMIT_COUNT, GIT_REV))
rem   EXIT PROGRAM 0
rem END FUNCTION
rem 
rem PRIVATE FUNCTION isLetter(c STRING)
rem   VAR letters = "abcdefghijklmnopqrstuvwxyz"
rem   RETURN getIndexOfI(src: letters, pattern: c, idx: 1) > 0
rem END FUNCTION
rem 
rem PRIVATE FUNCTION isWinDriveInt(path STRING)
rem   RETURN isWin()
rem       AND path.getCharAt(2) == ":"
rem       AND (path.getCharAt(3) == "\\" OR path.getCharAt(3) == "/")
rem       AND isLetter(path.getCharAt(1))
rem END FUNCTION
rem {
rem PRIVATE FUNCTION isWinDriveRoot(path STRING)
rem   RETURN path.getLength() == 3 AND isWinDriveInt(path)
rem END FUNCTION
rem }
rem 
rem PRIVATE FUNCTION pathStartsWithWinDrive(path STRING)
rem   RETURN path.getLength() >= 3 AND isWinDriveInt(path)
rem END FUNCTION
rem 
rem #creates a directory path recursively like mkdir -p
rem PRIVATE FUNCTION mkdirp(path STRING)
rem   VAR winbase = FALSE
rem   VAR level = 0
rem   IF isWin() AND path.getIndexOf("\\", 1) > 0 THEN
rem     LET path = backslash2slash(path)
rem   END IF
rem   VAR basedir = "."
rem   CASE
rem     WHEN path.getCharAt(1) == "/"
rem       LET basedir = "/"
rem       --check for driveletter: as path start
rem     WHEN pathStartsWithWinDrive(path)
rem       LET basedir = path.subString(1, 2)
rem       --DISPLAY "winbase:",basedir
rem       LET winbase = TRUE
rem   END CASE
rem   VAR tok = base.StringTokenizer.create(path, "/")
rem   VAR part = basedir
rem   WHILE tok.hasMoreTokens()
rem     LET level = level + 1
rem     VAR next = tok.nextToken()
rem     --DISPLAY "part0:",part,",next:",next
rem     IF level == 1 AND winbase THEN
rem       MYASSERT(basedir == next)
rem       --DISPLAY "next level"
rem       CONTINUE WHILE
rem     END IF
rem     LET part = os.Path.join(part, next)
rem     --DISPLAY "part1:",part
rem     IF NOT os.Path.exists(part) THEN
rem       IF NOT os.Path.mkdir(part) THEN
rem         CALL myErr(SFMT("can't create directory:%1", part))
rem         {ELSE
rem           DISPLAY "did mkdir:", part}
rem       END IF
rem     ELSE
rem       IF NOT os.Path.isDirectory(part) THEN
rem         CALL myErr(SFMT("mkdirp: sub path:'%1' is not a directory", part))
rem         {ELSE
rem           DISPLAY "part next:", part, " is a dir"}
rem       END IF
rem     END IF
rem   END WHILE
rem END FUNCTION
rem 
rem PRIVATE FUNCTION progName() RETURNS STRING
rem   VAR ret = os.Path.baseName(arg_val(0))
rem   VAR ext = os.Path.extension(ret)
rem   IF ext.getLength() > 0 THEN
rem     LET ret = ret.subString(1, ret.getLength() - ext.getLength() - 1)
rem   END IF
rem   RETURN ret
rem END FUNCTION
rem 
rem PRIVATE FUNCTION userError(err STRING)
rem   CALL printStderr(SFMT("Error %1:%2", progName(), err))
rem   EXIT PROGRAM 1
rem END FUNCTION
rem 
rem PRIVATE FUNCTION whichExe(prog STRING) RETURNS STRING
rem   DEFINE exe, err, cmd STRING
rem   LET cmd = IIF(isWin(), "where", "which")
rem   CALL getProgramOutputWithErr(SFMT("%1 %2", cmd, quote(prog)))
rem       RETURNING exe, err
rem   IF err IS NOT NULL THEN
rem     --DISPLAY SFMT("which error for '%1':%2", prog, err)
rem     RETURN NULL
rem   END IF
rem   RETURN exe
rem END FUNCTION
rem 
rem PRIVATE FUNCTION fullfglrunEXE()
rem   RETURN os.Path.join(
rem       os.Path.join(base.Application.getFglDir(), "bin"),
rem       SFMT("fglrun%1", IIF(isWin(), ".exe", "")))
rem END FUNCTION
rem 
rem PRIVATE FUNCTION fullProgName()
rem   RETURN os.Path.join(
rem       base.Application.getProgramDir(), os.Path.baseName(arg_val(0)))
rem END FUNCTION
rem 
rem PRIVATE FUNCTION writeStringToFile(file STRING, content STRING)
rem   DEFINE ch base.Channel
rem   LET ch = base.Channel.create()
rem   CALL ch.openFile(file, "w")
rem   CALL ch.writeNoNL(content)
rem   CALL ch.close()
rem END FUNCTION
rem 
rem FUNCTION readTextFile(filename) RETURNS STRING
rem   DEFINE filename STRING
rem   DEFINE content STRING
rem   DEFINE t TEXT
rem   LOCATE t IN FILE filename
rem   LET content = t
rem   RETURN content
rem END FUNCTION
rem 
rem FUNCTION yesno_mode()
rem   DEFINE yesno_msg STRING
rem   LET yesno_msg = fgl_getenv("__FGL_UNZIP_YESNO_MESSAGE__")
rem   IF yesno_msg IS NOT NULL THEN --we did invoke us for yesno
rem     CALL yesno(yesno_msg) RETURNING status
rem     RETURN TRUE
rem   END IF
rem   RETURN FALSE
rem END FUNCTION
rem 
rem PRIVATE FUNCTION yesno_cmd(message)
rem   DEFINE message STRING
rem   DEFINE cmd, ans, resfile STRING
rem   DEFINE code INT
rem   CALL fgl_setenv("FGLGUI", "0")
rem   CALL fgl_setenv("__FGL_UNZIP_YESNO_MESSAGE__", message)
rem   LET resfile = makeTempName()
rem   CALL fgl_setenv("__FGL_UNZIP_RESULT_FILE__", resfile)
rem   LET cmd = SFMT("%1 %2", quote(fullfglrunEXE()), quote(fullProgName()))
rem   --DISPLAY "cmd:",cmd
rem   RUN cmd RETURNING code
rem   IF code THEN
rem     LET ans = "failed"
rem   ELSE
rem     LET ans = readTextFile(resfile)
rem   END IF
rem   CALL os.Path.delete(resfile) RETURNING status
rem   RETURN ans
rem END FUNCTION
rem 
rem --displays a multiline message in a temp .42f
rem --and calls a MENU with yes no
rem PRIVATE FUNCTION yesno(message)
rem   DEFINE message STRING
rem   DEFINE fglgui, ans, resfile, frmfile STRING
rem   LET fglgui = fgl_getenv("FGLGUI")
rem   IF NOT fglgui.equals("0") THEN --run sub process
rem     RETURN yesno_cmd(message)
rem   END IF
rem   LET resfile = fgl_getenv("__FGL_UNZIP_RESULT_FILE__")
rem   LET frmfile = makeTempName(), ".42f"
rem   CALL writeStringToFile(frmfile, fglunzip_42f())
rem   OPTIONS MENU LINE 5
rem   OPTIONS COMMENT LINE 6
rem   OPEN FORM f FROM frmfile
rem   DISPLAY FORM f
rem   CALL os.Path.delete(frmfile) RETURNING status
rem   DISPLAY message TO msg
rem   MENU " Please answer"
rem     COMMAND "yes"
rem       LET ans = "yes"
rem       EXIT MENU
rem     COMMAND "no"
rem       LET ans = "no"
rem       EXIT MENU
rem   END MENU
rem   IF resfile IS NOT NULL THEN
rem     CALL writeStringToFile(resfile, content: ans)
rem   END IF
rem   RETURN ans
rem END FUNCTION
rem 
rem --returns fglunzip.per ready compiled
rem FUNCTION fglunzip_42f()
rem   RETURN '<?xml version=\'1.0\' encoding=\'UTF-8\'?>\n<Form name="fglunzip" build="5.01.02" width="80" height="2" delimiters="">\n<Screen width="80" height="2">\n<FormField name="formonly.msg" colName="msg" fieldId="0" sqlTabName="formonly" tabIndex="1">\n<TextEdit wantReturns="1" scrollBars="none" scroll="0" height="2" width="78" posY="0" posX="1" gridWidth="78" gridHeight="2"/>\n</FormField>\n</Screen>\n<RecordView tabName="formonly">\n<Link colName="msg" fieldIdRef="0"/>\n</RecordView>\n</Form>'
rem END FUNCTION
rem --SCREEN
rem --{
rem --[msg                                                                           ]
rem --[msg                                                                           ]
rem --}
rem --END
rem --ATTRIBUTES
rem 
rem --msg=FORMONLY.msg,WORDWRAP;
rem --INSTRUCTIONS
rem --DELIMITERS "";
rem __CAT_EOF_END__
