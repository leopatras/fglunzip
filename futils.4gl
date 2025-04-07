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
DEFINE _stdoutNONL STRING

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

FUNCTION touch(fname STRING)
  VAR s = ""
  VAR c = base.Channel.create()
  CALL c.openFile(fname, "ab")
  CALL c.writeNoNL(s)
  CALL c.close()
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

FUNCTION progName() RETURNS STRING
  VAR ret = os.Path.baseName(arg_val(0))
  VAR ext = os.Path.extension(ret)
  IF ext.getLength() > 0 THEN
    LET ret = ret.subString(1, ret.getLength() - ext.getLength() - 1)
  END IF
  RETURN ret
END FUNCTION

FUNCTION getAppDirAsset(asset STRING) RETURNS STRING
  RETURN os.Path.join(os.Path.dirName(arg_val(0)), asset)
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

FUNCTION isDirEqualToDir(dir1 STRING, dir2 STRING) RETURNS BOOLEAN
  VAR full1 = os.Path.fullPath(dir1)
  VAR full2 = os.Path.fullPath(dir2)
  RETURN full1.equals(full2)
END FUNCTION

FUNCTION userError(err STRING)
  CALL printStderr(SFMT("Error %1:%2", progName(), err))
  EXIT PROGRAM 1
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

FUNCTION whichExe(prog STRING) RETURNS STRING
  DEFINE exe, err, cmd STRING
  LET cmd = IIF(isWin(), "where", "which")
  CALL getProgramOutputWithErr(SFMT("%1 %2", cmd, quote(prog)))
      RETURNING exe, err
  IF err IS NOT NULL THEN
    --DISPLAY SFMT("which error for '%1':%2", prog, err)
    RETURN NULL
  END IF
  RETURN exe
END FUNCTION
