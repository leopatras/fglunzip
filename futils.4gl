OPTIONS
SHORT CIRCUIT
IMPORT os
IMPORT util
&include "fglunzip_version.inc"
&define FGLUNZIP_UTILS_MODULE
&include "myassert.inc"
DEFINE _isMac INT
DEFINE _isMacInit BOOLEAN
DEFINE _isLinux INT
DEFINE _isLinuxInit BOOLEAN
DEFINE _clientQAActive BOOLEAN
DEFINE _stdoutNONL STRING
DEFINE _gbc_dir STRING
DEFINE _gbc_version STRING
DEFINE _tmpDir STRING

PUBLIC TYPE TStartEntries RECORD
  port INT,
  pid INT,
  url STRING,
  searchCmd STRING
END RECORD

PUBLIC TYPE TExitHandler FUNCTION()
DEFINE _exitHandler TExitHandler

PUBLIC TYPE F_DirWalkFile
    FUNCTION(level INT,
    srcDir STRING,
    dstDir STRING,
    relPath STRING,
    fname STRING)

PUBLIC TYPE TAppFile RECORD
  name STRING,
  size INT,
  md5 STRING,
  hashName STRING
END RECORD

PUBLIC TYPE TAppFiles DYNAMIC ARRAY OF TAppFile

PUBLIC TYPE F_DirWalkDir
    FUNCTION(level INT, fname STRING, fullName STRING) RETURNS BOOLEAN

FUNCTION setExitHandler(exitHandler TExitHandler)
  LET _exitHandler = exitHandler
END FUNCTION

FUNCTION isWin() RETURNS BOOLEAN
  RETURN os.Path.separator().equals("\\")
END FUNCTION

FUNCTION isMac() RETURNS BOOLEAN
  IF _isMacInit == FALSE THEN
    LET _isMacInit = TRUE
    LET _isMac = isMacInt()
  END IF
  RETURN _isMac
END FUNCTION

PRIVATE FUNCTION isMacInt() RETURNS BOOLEAN
  IF NOT isWin() THEN
    RETURN getProgramOutput("uname") == "Darwin"
  END IF
  RETURN FALSE
END FUNCTION

FUNCTION isLinux() RETURNS BOOLEAN
  IF _isLinuxInit == FALSE THEN
    LET _isLinuxInit = TRUE
    LET _isLinux = isLinuxInt()
  END IF
  RETURN _isLinux
END FUNCTION

PRIVATE FUNCTION isLinuxInt() RETURNS BOOLEAN
  IF NOT isWin() THEN
    RETURN getProgramOutput("uname") == "Linux"
  END IF
  RETURN FALSE
END FUNCTION

FUNCTION file_get_output(program STRING, arr DYNAMIC ARRAY OF STRING)
  DEFINE linestr STRING
  DEFINE mystatus, idx INTEGER
  DEFINE c base.Channel
  LET c = base.Channel.create()
  WHENEVER ERROR CONTINUE
  CALL c.openPipe(program, "r")
  LET mystatus = status
  WHENEVER ERROR STOP
  IF mystatus THEN
    CALL myErr(SFMT("program:%1, error:%2", program, err_get(mystatus)))
  END IF
  CALL arr.clear()
  WHILE (linestr := c.readLine()) IS NOT NULL
    LET idx = idx + 1
    LET arr[idx] = linestr
  END WHILE
  CALL c.close()
END FUNCTION

FUNCTION printStderr(errstr STRING)
  DEFINE ch base.Channel
  LET ch = base.Channel.create()
  CALL ch.openFile("<stderr>", "w")
  CALL ch.writeLine(errstr)
  CALL ch.close()
END FUNCTION

FUNCTION printStdout(str STRING, noNewLine BOOLEAN)
  IF noNewLine THEN
    LET _stdoutNONL = _stdoutNONL, str
  ELSE
    LET str = _stdoutNONL, str
    LET _stdoutNONL = ""
    DISPLAY str
  END IF
END FUNCTION

FUNCTION myErr(errstr STRING)
  CALL printStderr(
      SFMT("ERROR:%1 stack:\n%2", errstr, base.Application.getStackTrace()))
  IF _exitHandler IS NOT NULL THEN
    CALL _exitHandler()
  END IF
  EXIT PROGRAM 1
END FUNCTION

FUNCTION myWarning(errstr STRING)
  CALL printStderr(SFMT("Warning %1:%2", progName(), errstr))
END FUNCTION

FUNCTION log(msg STRING)
  IF fgl_getenv("VERBOSE") IS NOT NULL THEN
    DISPLAY "log:", msg
  END IF
END FUNCTION

--for dev: replace log() with dlog() for simply write to stdout
FUNCTION dlog(s STRING)
  DISPLAY s
END FUNCTION

FUNCTION already_quoted(path) RETURNS BOOLEAN
  DEFINE path, first, last STRING
  LET first = NVL(path.getCharAt(1), "NULL")
  LET last = NVL(path.getCharAt(path.getLength()), "NULL")
  IF isWin() THEN
    RETURN (first == '"' AND last == '"')
  END IF
  RETURN (first == "'" AND last == "'") OR (first == '"' AND last == '"')
END FUNCTION

FUNCTION quote(path STRING) RETURNS STRING
  RETURN quoteInt(path, FALSE)
END FUNCTION

FUNCTION quoteForce(path STRING) RETURNS STRING
  RETURN quoteInt(path, TRUE)
END FUNCTION

PRIVATE FUNCTION quoteInt(path STRING, force BOOLEAN) RETURNS STRING
  IF force OR path.getIndexOf(" ", 1) > 0 THEN
    IF NOT already_quoted(path) THEN
      LET path = '"', path, '"'
    END IF
  ELSE
    IF already_quoted(path) AND isWin() THEN --remove quotes(Windows)
      LET path = path.subString(2, path.getLength() - 1)
    END IF
  END IF
  RETURN path
END FUNCTION

FUNCTION quoteUrl(url) RETURNS STRING
  DEFINE url STRING
  IF isWin() THEN
    RETURN winQuoteUrl(url)
  END IF
  IF url.getIndexOf(" ", 1) > 0
      OR url.getIndexOf("?", 1) > 0
      OR url.getIndexOf("&", 1) > 0 THEN
    LET url = '"', url, '"'
  END IF
  RETURN url
END FUNCTION

FUNCTION winQuoteUrl(url STRING) RETURNS STRING
  LET url = replace(url, "%", "^%")
  LET url = replace(url, "&", "^&")
  RETURN url
END FUNCTION

FUNCTION replace(src STRING, oldStr STRING, newString STRING) RETURNS STRING
  DEFINE b base.StringBuffer
  LET b = base.StringBuffer.create()
  CALL b.append(src)
  CALL b.replace(oldStr, newString, 0)
  RETURN b.toString()
END FUNCTION

FUNCTION backslash2slash(src STRING) RETURNS STRING
  RETURN replace(src, "\\", "/")
END FUNCTION

#+returns the last matching index
FUNCTION lastIndexOf(s STRING, sub STRING) RETURNS INT
  DEFINE startpos, idx, lastidx INT
  LET startpos = 1
  WHILE (idx := s.getIndexOf(sub, startpos)) > 0
    LET lastidx = idx
    LET startpos = idx + 1
  END WHILE
  RETURN lastidx
END FUNCTION

#+case insensitive variant of getIndexOf
FUNCTION getIndexOfI(src, pattern, idx) RETURNS INT
  DEFINE src, pattern STRING
  DEFINE idx INTEGER
  LET src = src.toLowerCase()
  RETURN src.getIndexOf(pattern.toLowerCase(), idx)
END FUNCTION

FUNCTION getProgramOutputWithErr(cmd STRING) RETURNS(STRING, STRING)
  DEFINE cmdOrig, tmpName, errStr STRING
  DEFINE txt TEXT
  DEFINE ret STRING
  DEFINE code INT
  LET cmdOrig = cmd
  LET tmpName = makeTempName()
  LET cmd = cmd, ">", tmpName, " 2>&1"
  --CALL log(sfmt("run:%1", cmd))
  RUN cmd RETURNING code
  LOCATE txt IN FILE tmpName
  LET ret = txt
  CALL os.Path.delete(tmpName) RETURNING status
  IF code THEN
    LET errStr = ",\n  output:", ret
    CALL os.Path.delete(tmpName) RETURNING code
  ELSE
    --remove \r\n
    IF ret.getCharAt(ret.getLength()) == "\n" THEN
      LET ret = ret.subString(1, ret.getLength() - 1)
    END IF
    IF ret.getCharAt(ret.getLength()) == "\r" THEN
      LET ret = ret.subString(1, ret.getLength() - 1)
    END IF
  END IF
  RETURN ret, errStr
END FUNCTION

FUNCTION getProgramOutput(cmd STRING) RETURNS STRING
  DEFINE result, err STRING
  CALL getProgramOutputWithErr(cmd) RETURNING result, err
  IF err IS NOT NULL THEN
    CALL myErr(SFMT("failed to RUN:%1%2", cmd, err))
  END IF
  RETURN result
END FUNCTION

#+computes a temporary file name
FUNCTION makeTempName() RETURNS STRING
  DEFINE tmpDir, tmpName, sbase, curr STRING
  DEFINE sb base.StringBuffer
  DEFINE i INT
  IF isWin() THEN
    LET tmpDir = fgl_getenv("TEMP")
  ELSE
    LET tmpDir = "/tmp"
  END IF
  LET curr = CURRENT
  LET sb = base.StringBuffer.create()
  CALL sb.append(curr)
  CALL sb.replace(" ", "_", 0)
  CALL sb.replace(":", "_", 0)
  CALL sb.replace(".", "_", 0)
  CALL sb.replace("-", "_", 0)
  LET sbase = SFMT("fgl_%1_%2", fgl_getpid(), sb.toString())
  LET sbase = os.Path.join(tmpDir, sbase)
  FOR i = 1 TO 10000
    LET tmpName = SFMT("%1%2.tmp", sbase, i)
    IF NOT os.Path.exists(tmpName) THEN
      RETURN tmpName
    END IF
  END FOR
  CALL myErr("makeTempName:Can't allocate a unique name")
  RETURN NULL
END FUNCTION

FUNCTION makeTempNameWithExt(extension STRING) RETURNS STRING
  --should be a really unique file name
  VAR tmpName
      = SFMT("%1_fgl_%2_%3_%4.%5",
          os.Path.makeTempName(),
          fgl_getpid(),
          currentStr(),
          util.Math.rand(10000),
          extension)
  MYASSERT(os.Path.exists(tmpName) == FALSE)
  RETURN tmpName
END FUNCTION

FUNCTION trimWhiteSpace(s STRING) RETURNS STRING
  LET s = s.trim()
  LET s = replace(s, "\n", "")
  LET s = replace(s, "\r", "")
  RETURN s
END FUNCTION

FUNCTION trimWhiteSpaceAndLower(s STRING) RETURNS STRING
  LET s = trimWhiteSpace(s)
  LET s = s.toLowerCase()
  RETURN s
END FUNCTION

FUNCTION readTextFile(filename STRING) RETURNS STRING
  DEFINE content STRING
  DEFINE t TEXT
  IF NOT os.Path.exists(filename) THEN
    CALL myErr(SFMT("can't open:%1", filename))
  END IF
  TRY
    LOCATE t IN FILE filename
    LET content = t
  CATCH
    DISPLAY "readTextFile error:", err_get(status)
  END TRY
  RETURN content
END FUNCTION

FUNCTION readTextFileC(filename STRING) RETURNS STRING
  DEFINE content, line STRING
  IF NOT os.Path.exists(filename) THEN
    CALL myErr(SFMT("can't open:%1", filename))
  END IF
  VAR c = base.Channel.create()
  CALL c.openFile(filename, "r")
  WHILE (line := c.readLine()) IS NOT NULL
    LET content = content.append(line)
  END WHILE
  RETURN content
END FUNCTION

FUNCTION writeStringToFile(file STRING, content STRING)
  DEFINE ch base.Channel
  LET ch = base.Channel.create()
  CALL ch.openFile(file, "w")
  CALL ch.writeNoNL(content)
  CALL ch.close()
END FUNCTION

FUNCTION readBlob(fname) RETURNS BYTE
  DEFINE fname STRING
  DEFINE blob BYTE
  LOCATE blob IN FILE fname
  RETURN blob
END FUNCTION

FUNCTION checkClientQA()
  --CLIENTQA_GWA_ACTIVE is set in <clientqa>/Makefile.inc
  --for the gwarun/gwafast/gwatest rules
  LET _clientQAActive = fgl_getenv("CLIENTQA_GWA_ACTIVE") IS NOT NULL
  IF _clientQAActive THEN
    VAR qadir = fgl_getenv("FGLQADIR")
    IF qadir IS NULL THEN
      CALL myErr("FGLQADIR not set")
    END IF
    IF NOT os.Path.exists(qadir) OR NOT os.Path.isDirectory(qadir) THEN
      CALL myErr("FGLQADIR '%1' does not exist or isn't a directory")
    END IF
  END IF
END FUNCTION

FUNCTION clientQA()
  RETURN _clientQAActive
END FUNCTION

-- returns
-- -<count>-g<SHA> for a non release
-- -<SHA> for a release (GIT_COMMIT_COUNT==0)
FUNCTION adjustGitCountAndRev(cnt INT, rev STRING)
  --don't display 0 and g in the revision for official releases
  VAR countInfo = IIF(cnt == 0, "", SFMT("-%1", cnt))
  LET rev = IIF(cnt == 0, rev.subString(2, rev.getLength()), rev)
  VAR ret = SFMT("%1-%2", countInfo, rev)
  VAR warn
      = IIF(cnt == 0,
          "",
          " (nightly build - not suitable for production purposes)")
  LET ret = SFMT("%1%2", ret, warn)
  RETURN ret
END FUNCTION

FUNCTION removeExtension(fname STRING) RETURNS STRING
  VAR ext = os.Path.extension(fname)
  IF ext.getLength() > 0 THEN
    LET fname = fname.subString(1, fname.getLength() - (ext.getLength() + 1))
  END IF
  RETURN fname
END FUNCTION

FUNCTION printVersion()
  VAR prog = removeExtension(os.Path.baseName(arg_val(0)))
  DISPLAY SFMT("%1 %2 rev%3",
      prog, GIT_VERSION, adjustGitCountAndRev(GIT_COMMIT_COUNT, GIT_REV))
  EXIT PROGRAM 0
END FUNCTION

FUNCTION formatQAVersion() RETURNS STRING
  RETURN SFMT("%1 rev%2",
      GIT_VERSION, adjustGitCountAndRev(GIT_COMMIT_COUNT, GIT_REV))
END FUNCTION
{
FUNCTION formatEmbeddedVMVersion() RETURNS STRING
  RETURN SFMT("%1 rev%2",
      GIT_FGL_VERSION, adjustGitCountAndRev(GIT_FGL_COMMIT_COUNT, GIT_FGL_REV))
END FUNCTION
}

FUNCTION getGWAVersion() RETURNS STRING
  RETURN GIT_VERSION
END FUNCTION
{
FUNCTION getEmbeddedVMVersion() RETURNS STRING
  RETURN GIT_FGL_VERSION
END FUNCTION
}

FUNCTION printPackageName()
  --append sha without g (match GMI)
  RETURN SFMT("fjs-gwa-%1-build_%2",
      GIT_VERSION, GIT_REV.subString(2, GIT_REV.getLength()))
END FUNCTION

FUNCTION currentStr() RETURNS STRING
  RETURN underscoreDateTime(CURRENT)
END FUNCTION

FUNCTION underscoreDateTime(dt DATETIME YEAR TO FRACTION(3)) RETURNS STRING
  DEFINE s STRING
  LET s = dt
  LET s = replace(s, "-", "_")
  LET s = replace(s, " ", "_")
  LET s = replace(s, ":", "_")
  LET s = replace(s, ".", "_")
  RETURN s
END FUNCTION

FUNCTION kill(pid STRING)
  IF isWin() THEN
    RUN SFMT("taskkill /F /PID %1 > NUL", pid)
  ELSE
    RUN SFMT("kill %1", pid)
  END IF
END FUNCTION

FUNCTION kill9(pid STRING)
  IF isWin() THEN
    RUN SFMT("taskkill /F /PID %1 > NUL", pid)
  ELSE
    RUN SFMT("kill -9 %1", pid)
  END IF
END FUNCTION

FUNCTION interpretchars(s STRING) RETURNS STRING
  DEFINE c, hex STRING
  DEFINE i, len INT
  DEFINE sb base.StringBuffer
  CONSTANT ascji =
      "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz1234567890!@#$%^&*()-=_+~`{}[] |:;\"'<>,.?/\\"
  --CONSTANT  "\"\\"
  LET len = s.getLength()
  LET sb = base.StringBuffer.create()
  FOR i = 1 TO len
    LET c = s.getCharAt(i)
    CASE
      WHEN ascji.getIndexOf(c, 1) > 0
        CALL sb.append(c)
      WHEN c == '\r'
        CALL sb.append("\\r")
      WHEN c == '\b'
        CALL sb.append("\\b")
      WHEN c == '\t'
        CALL sb.append("\\t")
      WHEN c == '\n'
        CALL sb.append("\\n")
      OTHERWISE
        LET hex = util.Integer.toHexString(i: ORD(c))
        LET hex = IIF(hex.getLength() == 1, SFMT("0%1", hex), hex)
        CALL sb.append(SFMT("\\x%1", hex))
    END CASE
  END FOR
  RETURN sb.toString()
END FUNCTION

--in absence of a official flush method
--we read 0 bytes on the channel which causes a flush
FUNCTION myflush(ch base.Channel)
  CALL ch.readOctets(length: 0) RETURNING status
END FUNCTION

FUNCTION findFreeServerPort(start, end, local)
  DEFINE start, end, local, freeport INT
  DEFINE ch base.Channel
  DEFINE i INT
  LET ch = base.Channel.create()
  FOR i = start TO end
    TRY
      CALL ch.openServerSocket(IIF(local, "127.0.0.1", NULL), i, "u")
      LET freeport = i
      EXIT FOR
    CATCH
      CALL log(SFMT("can't bind port %1:%2", i, err_get(status)))
    END TRY
  END FOR
  IF freeport > 0 THEN
    CALL ch.close()
    CALL log(SFMT("found free port:%1", freeport))
    RETURN freeport
  END IF
  CALL myErr(SFMT("Can't find free port in the range %1-%2", start, end))
  RETURN -1
END FUNCTION

FUNCTION readPortFile(portfile STRING) RETURNS INT
  VAR portstr = readTextFile(portfile)
  VAR port = parseInt(portstr)
  MYASSERT(port IS NOT NULL AND port <> 0)
  RETURN port
END FUNCTION

FUNCTION startsWith(s STRING, sub STRING) RETURNS BOOLEAN
  RETURN s.getIndexOf(sub, 1) == 1
END FUNCTION

FUNCTION endsWith(s STRING, sub STRING) RETURNS STRING
  VAR idx = lastIndexOf(s, sub)
  IF idx < 1 THEN
    RETURN FALSE
  END IF
  VAR subLen = sub.getLength()
  RETURN idx + subLen - 1 == s.getLength()
END FUNCTION

FUNCTION parseInt(s STRING) RETURNS INT
  DEFINE intVal INT
  LET s = s.trimWhiteSpace()
  LET intVal = s
  RETURN intVal
END FUNCTION

--checked variant: bails out if we don't return a valid INT
FUNCTION parseIntChecked(s STRING) RETURNS INT
  VAR intVal = parseInt(s)
  IF intVal IS NULL THEN
    CALL myErr(SFMT("No valid conversion from:'%1' to INT", s))
  END IF
  RETURN intVal
END FUNCTION

FUNCTION EQI(s1 STRING, s2 STRING) RETURNS BOOLEAN
  DEFINE s1l, s2l STRING
  LET s1l = s1.toLowerCase()
  LET s2l = s2.toLowerCase()
  RETURN s1l.equals(s2l)
END FUNCTION

FUNCTION parseVersion(version STRING)
  DEFINE fversion, testversion FLOAT
  DEFINE pointpos, major, idx INTEGER
  LET pointpos = version.getIndexOf(".", 1)
  IF pointpos == 0 OR pointpos = 1 THEN
    --version string did not contain a '.' or no major number
    CALL myErr(SFMT("parseVersion: no valid version (wrong dot):%1", version))
  ELSE
    LET major = version.subString(1, pointpos - 1)
    IF major IS NULL
        OR major
            > 100 THEN --one needs to adjust the 100 hopefully only after 300 years
      CALL myErr(
          SFMT("parseVersion: no valid major number:'%1' in version:%2",
              version.subString(1, pointpos - 1), version))
    END IF
  END IF
  --go a long as possible thru the string after '.' and remember the last
  --valid conversion, so it doesn't matter if a '.' or something else is right hand side of major.minor
  LET idx = 1
  LET fversion = NULL
  WHILE (testversion := version.subString(1, pointpos + idx)) IS NOT NULL
      AND pointpos + idx <= version.getLength()
    LET fversion = testversion
    --DISPLAY "fversion:",fversion," out of:",version.subString(1,pointpos+idx)
    LET idx = idx + 1
  END WHILE
  IF fversion IS NULL OR fversion == 0.0 THEN --we had no valid conversion
    CALL myErr(SFMT("parseVersion: can't convert to float:%1", version))
  END IF
  RETURN fversion
END FUNCTION

FUNCTION limitPrintStr(s STRING) RETURNS STRING
  DEFINE len INT
  {
  IF NOT _verbose THEN
    RETURN ""
  END IF
  }
  LET len = s.getLength()
  IF len > 323 THEN
    RETURN s.subString(1, 160) || "..." || s.subString(len - 160, len)
  ELSE
    RETURN s
  END IF
END FUNCTION

FUNCTION getByte(x, pos) --pos may be 0..3
  DEFINE x, pos, b INTEGER
  LET b = util.Integer.shiftRight(x, 8 * pos)
  LET b = util.Integer.and(b, 255)
  RETURN b
END FUNCTION

FUNCTION hasPrefix(s STRING, prefix STRING)
  RETURN s.getIndexOf(prefix, 1) == 1
END FUNCTION

FUNCTION getLastModified(fn STRING) RETURNS INT
  DEFINE m INT
  DEFINE dt DATETIME YEAR TO SECOND
  LET dt = os.Path.mtime(fn)
  LET m = util.Datetime.toSecondsSinceEpoch(dt)
  MYASSERT(m IS NOT NULL)
  RETURN m
END FUNCTION

FUNCTION setLastModified(fn STRING, t INT)
  VAR dt = util.Datetime.fromSecondsSinceEpoch(t)
  MYASSERT(os.Path.setModificationTime(fn, dt) == TRUE)
END FUNCTION

FUNCTION isLetter(c STRING)
  VAR letters = "abcdefghijklmnopqrstuvwxyz"
  RETURN getIndexOfI(src: letters, pattern: c, idx: 1) > 0
END FUNCTION

FUNCTION isWinDriveInt(path STRING)
  RETURN isWin()
      AND path.getCharAt(2) == ":"
      AND (path.getCharAt(3) == "\\" OR path.getCharAt(3) == "/")
      AND isLetter(path.getCharAt(1))
END FUNCTION

FUNCTION isWinDriveRoot(path STRING)
  RETURN path.getLength() == 3 AND isWinDriveInt(path)
END FUNCTION

FUNCTION pathStartsWithWinDrive(path STRING)
  RETURN path.getLength() >= 3 AND isWinDriveInt(path)
END FUNCTION

#creates a directory path recursively like mkdir -p
FUNCTION mkdirp(path STRING)
  VAR winbase = FALSE
  VAR level = 0
  IF isWin() AND path.getIndexOf("\\", 1) > 0 THEN
    LET path = backslash2slash(path)
  END IF
  VAR basedir = "."
  CASE
    WHEN path.getCharAt(1) == "/"
      LET basedir = "/"
      --check for driveletter: as path start
    WHEN pathStartsWithWinDrive(path)
      LET basedir = path.subString(1, 2)
      --DISPLAY "winbase:",basedir
      LET winbase = TRUE
  END CASE
  VAR tok = base.StringTokenizer.create(path, "/")
  VAR part = basedir
  WHILE tok.hasMoreTokens()
    LET level = level + 1
    VAR next = tok.nextToken()
    --DISPLAY "part0:",part,",next:",next
    IF level == 1 AND winbase THEN
      MYASSERT(basedir == next)
      --DISPLAY "next level"
      CONTINUE WHILE
    END IF
    LET part = os.Path.join(part, next)
    --DISPLAY "part1:",part
    IF NOT os.Path.exists(part) THEN
      IF NOT os.Path.mkdir(part) THEN
        CALL myErr(SFMT("can't create directory:%1", part))
        {ELSE
          DISPLAY "did mkdir:", part}
      END IF
    ELSE
      IF NOT os.Path.isDirectory(part) THEN
        CALL myErr(SFMT("mkdirp: sub path:'%1' is not a directory", part))
        {ELSE
          DISPLAY "part next:", part, " is a dir"}
      END IF
    END IF
  END WHILE
END FUNCTION

FUNCTION writeStartFile(startfile STRING, port INT, pid INT)
  DEFINE entries TStartEntries
  LET entries.port = port
  LET entries.pid = pid
  CALL writeStartFileE(startfile, entries)
END FUNCTION

FUNCTION writeStartFileE(startfile STRING, entries TStartEntries INOUT)
  --DISPLAY util.JSON.stringify(entries)
  IF startfile IS NULL THEN
    RETURN
  END IF
  VAR s = util.JSON.stringify(entries)
  CALL log(SFMT("writeStartFileE:%1,entries:%2", startfile, s))
  VAR ch = base.Channel.create()
  CALL ch.openFile(startfile, "w")
  CALL ch.writeLine(s)
  CALL ch.close()
END FUNCTION

FUNCTION waitForStartFile(fname STRING, fromApp STRING) RETURNS TStartEntries
  DEFINE s STRING
  DEFINE entries TStartEntries
  DEFINE i INT
  FOR i = 1 TO 10
    IF NOT os.Path.exists(fname) THEN
      LET s = ""
    ELSE
      LET s = readTextFile(fname)
      LET s = s.trim()
    END IF
    --DISPLAY "s:", s
    TRY
      CALL util.JSON.parse(s, entries)
      RETURN entries
    CATCH
      DISPLAY "wait for pid in:", fname, " ..."
      SLEEP 1
    END TRY
  END FOR
  CALL myErr(SFMT("%1 didn't create the start file '%1'", fromApp, fname))
  RETURN entries
END FUNCTION

FUNCTION exitQA(err STRING)
  IF err IS NOT NULL THEN
    VAR pre = IIF(err.getIndexOf("ERROR: ", 1) <> 1, "ERROR: ", "")
    DISPLAY SFMT("%1%2", pre, err)
    EXIT PROGRAM 1
  ELSE
    DISPLAY "GWA Test OK"
    EXIT PROGRAM 0
  END IF
END FUNCTION

FUNCTION touch(fname STRING)
  VAR s = ""
  VAR c = base.Channel.create()
  CALL c.openFile(fname, "ab")
  CALL c.writeNoNL(s)
  CALL c.close()
END FUNCTION

FUNCTION RUNtouch(fname STRING)
  DEFINE cmd STRING
  VAR quoted = quote(fname)
  IF NOT isWin() THEN
    LET cmd = SFMT("touch %1", quoted)
  ELSE
    LET cmd = SFMT("type nul >> %1 && copy /b %2 +,,", quoted, quoted)
  END IF
  VAR code = 0
  RUN cmd RETURNING code
  IF code THEN
    CALL myErr(SFMT("RUNtouch failed to RUN:%1", cmd))
  END IF
END FUNCTION

FUNCTION checkRUN(cmd STRING)
  VAR code = 0
  RUN cmd RETURNING code
  IF code THEN
    CALL myErr(SFMT("RUN of:%1 failed with code:%2", cmd, code))
  END IF
END FUNCTION

--gives dirname with slashes regardless of the OS
FUNCTION dirnameSlash(fname STRING) RETURNS STRING
  VAR dirname = os.Path.dirName(fname)
  RETURN backslash2slash(dirname)
END FUNCTION

FUNCTION rename(oldName STRING, newName STRING)
  MYASSERT(os.Path.exists(oldName))
  IF isWin() AND os.Path.exists(newName) THEN
    MYASSERT(os.Path.delete(newName) == TRUE)
  END IF
  MYASSERT(os.Path.rename(oldName, newName) == TRUE)
END FUNCTION

FUNCTION fglrunEXE()
  RETURN SFMT("fglrun%1", IIF(isWin(), ".exe", ""))
END FUNCTION

FUNCTION cutStringFromEnd(s STRING, toCut STRING)
  VAR idx = lastIndexOf(s, toCut)
  IF idx <> 0 THEN
    LET s = s.subString(1, idx - 1)
  END IF
  RETURN s
END FUNCTION

FUNCTION cutExtension(fname STRING) RETURNS STRING
  VAR ext = os.Path.extension(fname)
  IF ext IS NOT NULL AND ext.getLength() > 0 THEN
    LET fname = cutStringFromEnd(fname, SFMT(".%1", ext))
  END IF
  RETURN fname
END FUNCTION

FUNCTION findPidOf(cmd STRING, errWhenNotFound BOOLEAN) RETURNS INT
  DEFINE pscmd, line STRING
  DEFINE c base.Channel
  DEFINE pid, pidIdx INT
  LET pid = -1
  LET pidIdx = -1
  IF NOT isWin() THEN --remove some quoting on Unix
    LET cmd = replace(src: cmd, oldStr: '"', newString: "")
    LET cmd = cutStringFromEnd(cmd, "&update=1")
  END IF
  LET cmd = cutStringFromEnd(cmd, "?iframe=true")
  DISPLAY progName(), ":findPidOf:'", cmd, "'"
  IF isMac() OR isLinux() THEN
    LET pscmd = 'ps -o "pid command"' --"ps -f"
  ELSE
    LET pscmd = 'wmic process get processid,commandline'
  END IF
  LET c = base.Channel.create()
  CALL c.openPipe(pscmd, "r")
  VAR found = FALSE
  IF isWin() THEN
    VAR firstline = c.readLine()
    LET pidIdx = firstline.getIndexOf("ProcessId", 1)
    MYASSERT(pidIdx > 0)
  END IF
  WHILE (line := c.readLine()) IS NOT NULL
    --DISPLAY "line:", line
    IF isWin()
        AND line.getIndexOf("cmd.exe /c", 1)
            > 0 THEN --filter out cmd for browser
      --DISPLAY "filter out:",line
      CONTINUE WHILE
    END IF
    IF line.getIndexOf("--type=", 1) > 0 THEN
      CONTINUE WHILE
    END IF
    --IF line.getIndexOf("--user-data-dir",1)>0 THEN
    --  DISPLAY "may be:",line
    --  DISPLAY "mustbe:",cmd
    --END IF
    IF line.getIndexOf(cmd, 1) <> 0 THEN
      LET pid = extractPidFromLine(line, pidIdx)
      DISPLAY "!!!!found pid:", pid, " in line:", line
      LET found = TRUE
      EXIT WHILE
    END IF
  END WHILE
  IF errWhenNotFound AND NOT found THEN
    CALL myErr(SFMT("didn't find browser cmdline '%1' in process list", cmd))
  END IF
  RETURN pid
END FUNCTION

FUNCTION extractPidFromLine(line STRING, pidIdx INT) RETURNS INT
  DEFINE tok base.StringTokenizer
  DEFINE first STRING
  DEFINE pid INT
  DISPLAY SFMT("extractPidFromLine:'%1',pidIdx:%2", line, pidIdx)
  IF pidIdx >= 0 THEN
    LET line = line.subString(pidIdx, line.getLength())
  END IF
  LET tok = base.StringTokenizer.create(line, " ")
  WHILE tok.hasMoreTokens()
    LET first = tok.nextToken()
    IF first.getLength() == 0 THEN
      CONTINUE WHILE
    END IF
    LET first = first.trimWhiteSpace()
    LET pid = first
    DISPLAY "first:", first, ",pid:", pid
    RETURN pid
  END WHILE
  CALL myErr("must not end here")
  RETURN NULL
END FUNCTION

FUNCTION progName() RETURNS STRING
  VAR ret = os.Path.baseName(arg_val(0))
  VAR ext = os.Path.extension(ret)
  IF ext.getLength() > 0 THEN
    LET ret = ret.subString(1, ret.getLength() - ext.getLength() - 1)
  END IF
  RETURN ret
END FUNCTION

FUNCTION qaTrace(msg STRING)
  DISPLAY progName(), ":", msg
END FUNCTION

FUNCTION getAppDirAsset(asset STRING) RETURNS STRING
  RETURN os.Path.join(os.Path.dirName(arg_val(0)), asset)
END FUNCTION

PRIVATE FUNCTION findGBCIn(dirname STRING)
  DEFINE exists INT
  LET exists =
      os.Path.exists(os.Path.join(dirname, "index.html"))
          AND os.Path.exists(os.Path.join(dirname, "VERSION"))
  IF exists THEN
    LET _gbc_dir = dirname
    LET _gbc_version = readTextFile(os.Path.join(dirname, "VERSION"))
    CALL log(SFMT("GBC version:%1", _gbc_version))
  END IF
  RETURN exists
END FUNCTION

FUNCTION checkCustomGBC(gbcdir STRING)
  IF NOT os.Path.isDirectory(gbcdir) THEN
    CALL userError(SFMT("custom GBC dir '%1' is not a directory", gbcdir))
  END IF
  IF NOT findGBCIn(gbcdir) THEN
    CALL userError(
        SFMT("Can't find index.html and VERSION in custom GBC dir '%1'",
            gbcdir))
  END IF
  CALL log(SFMT("set custom GBC to:%1", gbcdir))
  CALL checkGBCVersion()
END FUNCTION

FUNCTION getGBCDir(programdir STRING) RETURNS STRING
  IF _gbc_dir IS NOT NULL THEN
    RETURN _gbc_dir
  END IF
  VAR web_gdc_dir = os.Path.join(os.Path.join("web_utilities", "gbc"), "gbc")
  CASE
    WHEN findGBCIn(os.Path.join(programdir, "gbc"))
    WHEN findGBCIn(fgl_getenv("FGLGBCDIR"))
    WHEN findGBCIn(os.Path.join(fgl_getenv("FGLDIR"), web_gdc_dir))
    OTHERWISE
      CALL myErr("No GBC found")
  END CASE
  MYASSERT(_gbc_dir IS NOT NULL)
  CALL log(SFMT("found GBC in %1", _gbc_dir))
  CALL checkGBCVersion()
  RETURN _gbc_dir
END FUNCTION

FUNCTION getGBCVersion() RETURNS STRING
  IF _gbc_dir IS NULL THEN
    LET _gbc_dir = getGBCDir(os.Path.pwd())
  END IF
  RETURN _gbc_version
END FUNCTION

FUNCTION checkGBCVersion()
  DEFINE major, minor, build INT
  MYASSERT(_gbc_version IS NOT NULL)
  CALL parseMajorMinorBuild(_gbc_version, description: "GBC version")
      RETURNING major, minor, build
  IF major < 5 THEN
    CALL userError(
        SFMT("GBC version:%1 of GBC in:%2 is too old, must be >= 5.00.00",
            _gbc_version, _gbc_dir))
  END IF
  IF version1SmallerThanVersion2(_gbc_version, "5.00.11") THEN
    CALL myWarning(
        SFMT("GBC version:%1 of GBC in:%2 is not production ready with GWA, should be >= 5.00.11",
            _gbc_version, _gbc_dir))
  END IF
END FUNCTION

FUNCTION checkGWADIR()
  VAR owndir = os.Path.fullPath(os.Path.dirName(arg_val(0)))
  IF os.Path.exists(os.Path.join(owndir, "README.gwabuildtool")) THEN
    -- DISPLAY "in repo detected"
    RETURN
  END IF
  --just check if the GWADIR passed is matching with us
  VAR gwadir = fgl_getenv("GWADIR")
  IF gwadir IS NULL THEN
    RETURN
  END IF
  VAR gwabuildtooldir = os.Path.fullPath(os.Path.join(gwadir, "lib/gwa"))
  IF NOT owndir.equals(gwabuildtooldir) THEN
    CALL myErr(
        SFMT("$GWADIR/lib/gwa:%1<>directory:%2 of:%3",
            gwabuildtooldir, owndir, arg_val(0)))
  END IF
END FUNCTION

#+returns either the source dir or <gwainstalldir>/lib/gwa
FUNCTION getlibgwaDir() RETURNS STRING
  VAR owndir = os.Path.fullPath(os.Path.dirName(arg_val(0)))
  IF os.Path.exists(os.Path.join(owndir, "README.gwabuildtool")) THEN
    DISPLAY "in repo detected:", owndir
    RETURN owndir
  END IF
  VAR bas0 = os.Path.baseName(owndir)
  VAR dirname = os.Path.dirName(owndir)
  VAR bas = os.Path.baseName(dirname)
  IF bas == "lib" AND bas0 == "gwa" THEN
    RETURN owndir
  END IF
  CALL myErr(
      SFMT("getlibgwaDir of %1: bad owndir:%2,bas:%3,bas0:%4",
          arg_val(0), owndir, bas, bas0))
  RETURN NULL
END FUNCTION

#+ Can parse versions coming from git describe and returns major,minor,build,rev number,commit hash
FUNCTION parseExtendedVersion(
    version STRING, description STRING)
    RETURNS(INT, INT, INT, STRING, STRING)
  DEFINE major, minor, build INT
  DEFINE git_rev, git_build STRING
  VAR i = 1
  VAR tok = base.StringTokenizer.create(version, "-")
  WHILE tok.hasMoreTokens()
    VAR nextToken = tok.nextToken()
    CASE i
      WHEN 1
        CALL parseMajorMinorBuild(nextToken, description)
            RETURNING major, minor, build
      WHEN 2
        LET git_rev = nextToken
      WHEN 3
        LET git_build = nextToken
        EXIT WHILE
    END CASE
    LET i = i + 1
  END WHILE
  RETURN major, minor, build, git_rev, git_build
END FUNCTION

FUNCTION isDigit(c STRING)
  CONSTANT digits = "0123456789"
  RETURN digits.getIndexOf(c, 1) > 0
END FUNCTION

FUNCTION parseIntForVersion(
    version STRING,
    sub STRING,
    what STRING,
    partdesc STRING,
    canHavePre BOOLEAN,
    canHavePost BOOLEAN)
    RETURNS INT
  DEFINE i, num INT
  DEFINE c, accu, pre, post STRING
  FOR i = 1 TO sub.getLength()
    LET c = sub.getCharAt(i)
    CASE
      WHEN accu IS NULL
        IF isDigit(c) THEN
          LET accu = c
          IF pre.getLength() > 0 AND NOT canHavePre THEN
            CALL myErr(
                SFMT("%1 '%2': must not have leading characters('%3') for %4 part:'%5'",
                    what, version, pre, partdesc, sub))
          END IF
        ELSE
          LET pre = pre, c
        END IF
      WHEN accu IS NOT NULL
        IF isDigit(c) THEN
          LET accu = accu, c
        ELSE
          LET num = accu
          IF num IS NULL THEN
            DISPLAY "num NULL for accu:", accu, ",partdesc:", partdesc
          ELSE
            LET post = sub.subString(i, sub.getLength())
            IF post.getLength() > 0 AND NOT canHavePost THEN
              CALL myErr(
                  SFMT("%1 '%2': must not have trailing characters('%3') for %4 part:'%5'",
                      what, version, post, partdesc, sub))
            END IF
          END IF
          RETURN num
        END IF
    END CASE
  END FOR
  LET num = accu
  IF num IS NULL THEN
    CALL myErr(
        SFMT("%1 '%2': Can't find number in '%3' for %4 part",
            what, version, sub, partdesc))
  END IF
  RETURN num
END FUNCTION

FUNCTION parseMajorMinorBuild(version STRING, description STRING)
  DEFINE major, minor, build INT
  LET major = NULL
  LET minor = 0
  LET build = 0
  VAR i = 1
  VAR tok = base.StringTokenizer.create(version, ".")
  WHILE tok.hasMoreTokens()
    VAR nextToken = tok.nextToken()
    CASE i
      WHEN 1
        LET major =
            parseIntForVersion(
                version,
                nextToken,
                description,
                "major",
                canHavePre: TRUE,
                canHavePost: FALSE)
        IF nextToken.getIndexOf("v", 1) == 1 THEN
          LET nextToken = nextToken.subString(2, nextToken.getLength())
        END IF
      WHEN 2
        LET minor =
            parseIntForVersion(
                version,
                nextToken,
                description,
                "minor",
                canHavePre: FALSE,
                canHavePost: FALSE)
      WHEN 3
        LET build =
            parseIntForVersion(
                version,
                nextToken,
                description,
                "build",
                canHavePre: FALSE,
                canHavePost: TRUE)
        EXIT WHILE
    END CASE
    LET i = i + 1
  END WHILE
  IF major IS NULL THEN
    DISPLAY "no major number found in version:", version
  END IF

  RETURN major, minor, build
END FUNCTION

FUNCTION version1GreaterThanVersion2(version1 STRING, version2 STRING)
  DEFINE major1, major2, minor1, minor2, build1, build2 INT
  CALL parseMajorMinorBuild(version1, description: "version1")
      RETURNING major1, minor1, build1
  CALL parseMajorMinorBuild(version2, description: "version2")
      RETURNING major2, minor2, build2
  RETURN major1 > major2
      OR (major1 == major2 AND minor1 > minor2)
      OR (major1 == major2 AND minor1 = minor2 AND build1 > build2)
END FUNCTION

FUNCTION version1SmallerThanVersion2(version1 STRING, version2 STRING)
  DEFINE major1, major2, minor1, minor2, build1, build2 INT
  CALL parseMajorMinorBuild(version1, description: "version1")
      RETURNING major1, minor1, build1
  CALL parseMajorMinorBuild(version2, description: "version2")
      RETURNING major2, minor2, build2
  RETURN major1 < major2
      OR (major1 == major2 AND minor1 < minor2)
      OR (major1 == major2 AND minor1 = minor2 AND build1 < build2)
END FUNCTION

#+ by default all GWA tools and tests operate on 'localhost'
#+ can be configured to '127.0.0.1' when setting GWA_LOCALHOST
FUNCTION localhost() RETURNS STRING
  VAR lc = fgl_getenv("GWA_LOCALHOST")
  IF lc == "127.0.0.1" OR lc == "loopback" THEN
    RETURN "127.0.0.1"
  END IF
  RETURN "localhost"
END FUNCTION

FUNCTION chDirTemp() RETURNS STRING
  --VAR tmpDir=fgl_getenv("GWA_TOOL_TEMP_DIR")
  VAR tmpDir = _tmpDir
  IF tmpDir IS NULL THEN
    LET tmpDir = os.Path.makeTempName()
    MYASSERT_MSG(os.Path.mkdir(tmpDir) == TRUE, sfmt("chDirTemp: Can't create:%1", tmpDir))
    CALL log(SFMT("%1 chDirTemp created:%2", arg_val(0), tmpDir))
  END IF
  VAR pwd = os.Path.pwd()
  MYASSERT_MSG(os.Path.chDir(tmpDir) == TRUE, sfmt("chDirTemp: Can't chDir to:%1", tmpDir))
  IF _tmpDir IS NULL THEN
    LET _tmpDir = os.Path.pwd() --use the value the os reports
    CALL log(SFMT("%1 real _tmpDir:%2", arg_val(0), tmpDir))
  END IF

  CALL log(SFMT("%1 chDirTemp chDir from %2->%3", arg_val(0), pwd, tmpDir))
  RETURN pwd
END FUNCTION

FUNCTION cleanTempDir(prevCurr STRING)
  IF os.Path.chDir(prevCurr) AND _tmpDir IS NOT NULL THEN
    CALL log(SFMT("%1 cleanTempDir:%2", arg_val(0), _tmpDir))
    CALL rmrf(_tmpDir)
    LET _tmpDir = NULL
  END IF
END FUNCTION

FUNCTION isTempDir(dir STRING) RETURNS BOOLEAN
  RETURN dir IS NOT NULL AND dir.equals(_tmpDir)
END FUNCTION

FUNCTION getTempDir() RETURNS STRING
  RETURN _tmpDir
END FUNCTION

DEFINE _walkDirDstFull STRING
DEFINE _simulateDelete BOOLEAN

--enable a dry run for rmrf
FUNCTION checkSimulateDelete()
  LET _simulateDelete = fgl_getenv("GWA_SIMULATE_DELETE") IS NOT NULL
END FUNCTION

FUNCTION walkDir(
    srcDir STRING,
    dstDir STRING,
    recurse BOOLEAN,
    filecallback F_DirWalkFile,
    dircallback F_DirWalkDir)
  IF dstDir IS NOT NULL THEN
    LET _walkDirDstFull = os.Path.fullPath(dstDir)
  END IF
  CALL walkDirInt(
          srcDir, dstDir, ".", recurse, filecallback, dircallback, 0, FALSE)
      RETURNING status
  LET _walkDirDstFull = NULL
END FUNCTION

FUNCTION createDir(dir STRING, desc STRING)
  IF os.Path.exists(dir) AND os.Path.isDirectory(dir) THEN
    --DISPLAY "createDir:", os.Path.fullPath(dir), " exists.."
    RETURN
  END IF
  IF NOT os.Path.mkdir(dir) THEN
    CALL myErr(
        SFMT("Can't create %1 directory:'%2',fullPath:'%3' pwd:%4 ",
            desc, dir, os.Path.fullPath(dir), os.Path.pwd()))
  ELSE
    CALL log(SFMT("did createDir:%1", os.Path.fullPath(dir)))
  END IF
END FUNCTION

PRIVATE FUNCTION deleteInt(fullPath STRING)
  IF _simulateDelete THEN
    VAR kind = "file"
    CASE
      WHEN NOT os.Path.exists(fullPath)
        LET kind = "nonexisting"
      WHEN os.Path.isDirectory(fullPath)
        LET kind = "dir"
      WHEN os.Path.isLink(fullPath)
        LET kind = "link"
    END CASE
    CALL dlog(SFMT("would delete %1:%2", kind, fullPath))
  ELSE
    IF NOT os.Path.delete(fullPath) THEN
      CALL myWarning(SFMT("Can't delete '%1'", fullPath))
    END IF
  END IF
END FUNCTION

FUNCTION walkDirInt(
    srcDir STRING,
    dstDir STRING,
    relPath STRING,
    recurse BOOLEAN,
    filecallback F_DirWalkFile,
    dircallback F_DirWalkDir,
    level INT,
    rmrf BOOLEAN)
    RETURNS BOOLEAN
  DEFINE dh INT
  DEFINE fname STRING
  VAR dir = os.Path.join(srcDir, relPath)
  LET dir = os.Path.fullPath(dir)
  IF dir == _walkDirDstFull THEN
    CALL myWarning(
        SFMT("walkDirInt: don't recurse into destination dir:'%1', stack:\n%2",
            _walkDirDstFull, base.Application.getStackTrace()))
    RETURN FALSE
  END IF
  LET dh = os.Path.dirOpen(dir)
  IF rmrf AND isProtectedDir(level, dir) THEN
    RETURN FALSE
  END IF
  IF dh == 0 THEN
    CALL myErr(SFMT("Can't open directory '%1'", dir))
  ELSE
    CALL log(SFMT("entering '%1'", dir))
  END IF
  --DISPLAY SFMT(" walkDirInt:srcDir:%1,dstDir:%2,dir:%3,os.Path.exists dstDir:%3,relPath:%4",
  --    srcDir, dstDir, dir, os.Path.exists(dstDir), relPath)
  WHILE (fname := os.Path.dirNext(dh)) IS NOT NULL
    IF fname == "." OR fname == ".." THEN
      CONTINUE WHILE
    END IF
    VAR fullPath = os.Path.join(dir, fname)
    IF os.Path.isDirectory(fullPath) THEN
      --DISPLAY "is directory:", fullPath
      IF recurse THEN
        IF dircallback IS NOT NULL THEN
          IF NOT dircallback(level, fname, fullPath) THEN
            --exclude this dir from recursing
            CONTINUE WHILE
          END IF
        END IF
        VAR newRelPath = os.Path.join(relPath, fname)
        IF dstDir IS NOT NULL THEN
          VAR newDstDir = os.Path.join(dstDir, newRelPath)
          --DISPLAY "newDstDir1:", newDstDir
          LET newDstDir = os.Path.fullPath(newDstDir)
          --DISPLAY "newDstDir2:", newDstDir
          CALL createDir(newDstDir, "destination dir")
        END IF
        VAR ok
            = walkDirInt(
                srcDir: srcDir,
                dstDir: dstDir,
                relPath: newRelPath,
                recurse: TRUE,
                filecallback: filecallback,
                dircallback: dircallback,
                level: level + 1,
                rmrf: rmrf)
        IF ok AND rmrf THEN
          CALL deleteInt(fullPath)
        END IF
      ELSE
        --DISPLAY "omit dir:", fullPath
      END IF
    ELSE
      IF os.Path.isLink(fullPath) THEN
        CALL log(SFMT("isLink:%1", fullPath))
      END IF
      IF filecallback IS NOT NULL THEN
        CALL filecallback(level, srcDir, dstDir, relPath, fname)
      END IF
    END IF
  END WHILE
  CALL os.Path.dirClose(dh)
  RETURN TRUE
END FUNCTION

FUNCTION rmrf_file(
    level INT, srcDir STRING, dstDir STRING, relPath STRING, fname STRING)
  UNUSED_VAR(level)
  UNUSED_VAR(dstDir)
  VAR todel
      = os.Path.fullPath(os.Path.join(os.Path.join(srcDir, relPath), fname))
  CALL deleteInt(todel)
END FUNCTION

FUNCTION dontDelete(what STRING, fullName STRING, level INT) RETURNS BOOLEAN
  CALL myWarning(
      SFMT("Don't delete %1 dir: '%2', level:%3 stack:\n%4",
          what, fullName, level, base.Application.getStackTrace()))
  RETURN TRUE
END FUNCTION

FUNCTION isProtectedDir(level INT, fullName STRING) RETURNS BOOLEAN
  IF NOT base.Application.isGWA() THEN
    --check some critical directories which shouldn't be deleted by us
    CASE
      WHEN level == 0
          AND (fullName == "/"
              OR fullName.toLowerCase() == "c:\\"
              OR (isWin()
                  AND fullName.toLowerCase()
                      == SFMT("%1\\", fgl_getenv("HOMEDRIVE").toLowerCase())))
        RETURN dontDelete(what: "root", fullName, level)
      WHEN level < 2
          AND (fullName == "/Users"
              OR fullName == "/home"
              OR fullName == "/usr"
              OR (isWin()
                  AND fullName.toLowerCase()
                      == SFMT("%1\\users",
                          fgl_getenv("HOMEDRIVE").toLowerCase())))
        RETURN dontDelete(what: "user root", fullName, level)
      WHEN level < 3 AND (fullName == os.Path.fullPath(fgl_getenv("HOME")))
        RETURN dontDelete(what: "HOME", fullName, level)
      WHEN level < 3
          AND isWin()
          AND fullName
              == os.Path.fullPath(
                  SFMT("%1%2", fgl_getenv("HOMEDRIVE"), fgl_getenv("HOMEPATH")))
        RETURN dontDelete(what: "HOMEDRIVE+HOMEPATH", fullName, level)
    END CASE
  END IF
  RETURN FALSE
END FUNCTION

FUNCTION rmrf_dir(level INT, fname STRING, fullName STRING) RETURNS BOOLEAN
  UNUSED_VAR(fname)
  IF isProtectedDir(level, fullName) THEN
    RETURN FALSE
  END IF
  RETURN TRUE
END FUNCTION

--recursively deletes a directory
FUNCTION rmrf(dir STRING)
  IF NOT os.Path.exists(dir) THEN
    RETURN
  END IF
  LET _walkDirDstFull = NULL
  VAR ok
      = walkDirInt(
          srcDir: dir,
          dstDir: NULL,
          relPath: ".",
          recurse: TRUE,
          filecallback: FUNCTION rmrf_file,
          dircallback: FUNCTION rmrf_dir,
          level: 0,
          rmrf: TRUE)
  IF ok THEN
    CALL deleteInt(dir)
  END IF
END FUNCTION

FUNCTION isDirSubDirOf(dir1 STRING, dir2 STRING) RETURNS BOOLEAN
  VAR full1 = os.Path.fullPath(dir1)
  VAR full2 = os.Path.fullPath(dir2)
  --DISPLAY "full1:", full1, ",full2:", full2
  IF full1.getLength() > full2.getLength()
      AND full1.getIndexOf(full2, 1) == 1 THEN
    --sanitize a bit
    VAR parent = full1
    VAR prevParent = ""
    WHILE TRUE
      LET parent = os.Path.dirName(parent)
      --DISPLAY "parent:", parent, ",prevParent:", prevParent
      CASE
        WHEN parent IS NULL OR parent == "/" OR isWinDriveRoot(parent)
          --DISPLAY "top reached"
          RETURN FALSE
        WHEN parent.equals(full2)
          RETURN TRUE
          --safety belt
        WHEN parent.equals(prevParent)
          RETURN FALSE
      END CASE
      LET prevParent = parent
    END WHILE
  END IF
  RETURN FALSE
END FUNCTION

FUNCTION isDirEqualToDir(dir1 STRING, dir2 STRING) RETURNS BOOLEAN
  VAR full1 = os.Path.fullPath(dir1)
  VAR full2 = os.Path.fullPath(dir2)
  RETURN full1.equals(full2)
END FUNCTION

FUNCTION userError(err STRING)
  CALL printStderr(SFMT("Error %1:%2", progName(), err))
  EXIT PROGRAM 1
END FUNCTION

FUNCTION getAppFilesFromJSON(s STRING) RETURNS(TAppFiles, STRING)
  DEFINE err STRING
  DEFINE arr TAppFiles
  TRY
    CALL util.JSON.parse(s: s, variableRef: arr)
    --DISPLAY "arr.getLength:", arr.getLength()
  CATCH
    LET err = SFMT("getFiles:Can't parse s:%1,err:%2", s, err_get(status))
    CALL printStderr(
        SFMT("err:%1,stack:%2", err, base.Application.getStackTrace()))
  END TRY
  RETURN arr, err
END FUNCTION

FUNCTION sw_status() RETURNS STRING
  DEFINE ret, desc, s STRING
  TRY
    CALL ui.Interface.frontCall("gwa", "sw_controller", [], [s])
  CATCH
    LET s = "error1:", err_get(status)
  END TRY
  TRY
    CALL ui.Interface.frontCall("gwa", "sw_check", [], [ret, desc])
    LET s = s, ", currsw:", ret
    LET s = s, IIF(desc IS NOT NULL, SFMT(":%1", desc), "")
  CATCH
    LET s = "error2:", err_get(status)
  END TRY
  TRY
    CALL ui.Interface.frontCall("gwa", "sw_status_new", [], [ret, desc])
    LET s = s, " ,newsw:", ret
    LET s = s, IIF(desc IS NOT NULL, SFMT(":%1", desc), "")
  CATCH
    LET s = s, "error3:", err_get(status)
  END TRY
  RETURN s
END FUNCTION

FUNCTION fullfglrunEXE()
  RETURN os.Path.join(
      os.Path.join(base.Application.getFglDir(), "bin"),
      SFMT("fglrun%1", IIF(isWin(), ".exe", "")))
END FUNCTION

FUNCTION getFglrunVersion() RETURNS STRING
  VAR ret = getProgramOutput(SFMT("%1 -V", quote(fullfglrunEXE())))
  VAR tok = base.StringTokenizer.create(str: ret, delimiters: "\n")
  MYASSERT(tok.hasMoreTokens())
  VAR line = tok.nextToken()
  RETURN replace(line, oldStr: "fglrun ", newString: "")
END FUNCTION

FUNCTION millisecondsSinceEpochFromDT(
    dt DATETIME YEAR TO FRACTION(3))
    RETURNS BIGINT
  DEFINE tmp STRING
  DEFINE ms INT
  DEFINE secs, ret BIGINT
  LET secs = util.Datetime.toSecondsSinceEpoch(dt)
  LET tmp = EXTEND(dt, FRACTION TO FRACTION(3))
  VAR msStr = tmp.subString(2, 4)
  LET ms = msStr
  LET ret = (secs * 1000) + ms
  --MESSAGE sfmt("ret:%1",ret)
  RETURN ret
END FUNCTION

FUNCTION millisecondsSinceEpoch() RETURNS BIGINT
  RETURN millisecondsSinceEpochFromDT(CURRENT)
END FUNCTION

FUNCTION getMakeCmd() RETURNS STRING
  VAR makeEnv = fgl_getenv("MAKE")
  IF makeEnv IS NOT NULL THEN
    RETURN makeEnv
  END IF
  RETURN "make"
END FUNCTION

FUNCTION whichDaemonize() RETURNS STRING
  RETURN whichExe("daemonize")
END FUNCTION

FUNCTION whichExe(prog STRING) RETURNS STRING
  DEFINE exe, err,cmd STRING
  LET cmd=IIF(isWin(),"where","which")
  CALL getProgramOutputWithErr(SFMT("%1 %2", cmd, quote(prog))) RETURNING exe, err
  IF err IS NOT NULL THEN
    --DISPLAY SFMT("which error for '%1':%2", prog, err)
    RETURN NULL
  END IF
  RETURN exe
END FUNCTION

#+ 4GL draft variant for writing the current datetime to the caches
--PRIVATE FUNCTION writeTimeStamp(cacheKey STRING) RETURNS STRING
--  DEFINE timeStamp BIGINT
--  CALL ui.Interface.frontCall("gwa", "get_date_now", [], [timeStamp])
--  DISPLAY SFMT("dateNow:%1", timeStamp)
--  VAR dateObjStr = SFMT('{"time":%1}', timeStamp)
--  VAR tmpFile = writeToTmpFile(content: dateObjStr, fname: "TIMESTAMP")
--  VAR err
--      = saveFileToCache(
--          fname: tmpFile, url: "/private/TIMESTAMP", cacheKey: cacheKey)
--  RETURN err
--END FUNCTION

{
FUNCTION MAIN()
  DISPLAY isDirSubDirOf(".", "..")
  DISPLAY isDirSubDirOf("testxx", "..")
  DISPLAY isDirSubDirOf(".", "../demo")
  --DISPLAY isDirEqualToDir(".",os.Path.pwd())
END FUNCTION

FUNCTION MAIN()
  DISPLAY "endsWith foobar bar:",endsWith("foobar","bar")
  DISPLAY "endsWith foAobA A:",endsWith("foAobaA","A")
  DISPLAY "endsWith q q",endsWith("q","q")

END FUNCTION
}
