OPTIONS
SHORT CIRCUIT
IMPORT os
IMPORT FGL mygetopt
&include "fglunzip_version.inc"
&include "myassert.inc"
DEFINE _product_zip STRING --the zip file to process
DEFINE _opt_verbose BOOLEAN
DEFINE _opt_in_FGLDIR BOOLEAN
DEFINE _opt_simulate BOOLEAN
DEFINE _opt_undo BOOLEAN
DEFINE _opt_plain BOOLEAN
--DEFINE _stdoutNONL STRING
--TODO
--DEFINE _opt_quiet BOOLEAN
--DEFINE _opt_logfile STRING
--DEFINE _opt_ext_dir STRING
MAIN
  DEFINE argsarr DYNAMIC ARRAY OF STRING
  DEFINE root om.DomNode
  DEFINE numChildren INT
  IF yesno_mode() THEN
    RETURN
  END IF
  CALL checkTar()
  LET argsarr = setupArgs()
  --DISPLAY "argsarr:",util.JSON.stringify(argsarr)
  CALL parseArgs(argsarr)
  LET root = readFiles()
  LET numChildren = analyze(root)
  IF numChildren == 0 THEN
    CALL userError(SFMT("no entries found in:%1", _product_zip))
  END IF
  CALL doit(root, numChildren)
END MAIN

FUNCTION tarExe()
  RETURN IIF(isWin(), "tar.exe", "tar")
END FUNCTION

FUNCTION checkTar()
  DEFINE tar STRING
  LET tar = tarExe()
  IF whichExe(tar) IS NULL THEN
    CALL userError(
        SFMT("Couldn't find program:%1 on your system, please install", tar))
  END IF
END FUNCTION

FUNCTION unzipList()
  DEFINE cmd STRING
  LET cmd = SFMT("%1 tf %2", tarExe(), quote(_product_zip))
  --DISPLAY "unzipList cmd:", cmd
  RETURN getProgramOutput(cmd)
END FUNCTION

FUNCTION setupArgs()
  DEFINE i INT
  DEFINE argsarr DYNAMIC ARRAY OF STRING
  FOR i = 1 TO num_args()
    LET argsarr[i] = arg_val(i)
  END FOR
  RETURN argsarr
END FUNCTION

PRIVATE FUNCTION parseArgs(argsarr)
  DEFINE argsarr DYNAMIC ARRAY OF STRING
  DEFINE gr mygetopt.GetoptR
  DEFINE o mygetopt.GetoptOptions
  DEFINE opt_arg STRING
  DEFINE i, cnt INT
  DEFINE listSeen INT

  LET i = o.getLength() + 1
  LET o[i].name = "version"
  LET o[i].description = "Version information"
  LET o[i].opt_char = "V"
  LET o[i].arg_type = mygetopt.NONE

  LET i = o.getLength() + 1
  LET o[i].name = "help"
  LET o[i].description = "program help"
  LET o[i].opt_char = "h"
  LET o[i].arg_type = mygetopt.NONE

  LET i = o.getLength() + 1
  LET o[i].name = "verbose"
  LET o[i].description = "detailed log"
  LET o[i].opt_char = "v"
  LET o[i].arg_type = mygetopt.NONE

  LET i = o.getLength() + 1
  LET o[i].name = "simulate"
  LET o[i].description = "simulates what would be extracted/undone"
  LET o[i].opt_char = "s"
  LET o[i].arg_type = mygetopt.NONE

  LET i = o.getLength() + 1
  LET o[i].name = "list"
  LET o[i].description = "Lists the archive content"
  LET o[i].opt_char = "l"
  LET o[i].arg_type = mygetopt.NONE

  LET i = o.getLength() + 1
  LET o[i].name = "use-FGLDIR"
  LET o[i].description =
      "installs over FGLDIR to make the product avaiable without further env settings"
  LET o[i].opt_char = "F"
  LET o[i].arg_type = mygetopt.NONE

  LET i = o.getLength() + 1
  LET o[i].name = "like-unzip"
  LET o[i].description =
      "extracts multiple files/directories in the root directory of the archive like unzip 'plain' in the current directory"
  LET o[i].opt_char = "i"
  LET o[i].arg_type = mygetopt.NONE

  LET i = o.getLength() + 1
  LET o[i].name = "undo"
  LET o[i].description = "Reverts the install"
  LET o[i].opt_char = "u"
  LET o[i].arg_type = mygetopt.NONE

  { --TODO
  LET i = o.getLength() + 1
  LET o[i].name = "quiet"
  LET o[i].description = "Does install quietly without asking yes/no"
  LET o[i].opt_char = "q"
  LET o[i].arg_type = mygetopt.NONE

  LET i = o.getLength() + 1
  LET o[i].name = "logfile"
  LET o[i].description = "File written for logs and success"
  LET o[i].opt_char = "L"
  LET o[i].arg_type = mygetopt.REQUIRED

  LET i = o.getLength() + 1
  LET o[i].name = "destination-dir"
  LET o[i].description =
      "choose another extraction directory than the current dir"
  LET o[i].opt_char = "d"
  LET o[i].arg_type = mygetopt.NONE
  }

  CALL mygetopt.initialize(gr, "fglunzip", argsarr, o)
  WHILE mygetopt.getopt(gr) == mygetopt.SUCCESS
    LET opt_arg = mygetopt.opt_arg(gr)
    CASE mygetopt.opt_char(gr)
      WHEN 'V'
        CALL printVersion()
        EXIT PROGRAM 0
      WHEN 'v'
        LET _opt_verbose = TRUE
      WHEN 'h'
        CALL mygetopt.displayUsage(gr, "fjs-<product>.zip")
        EXIT PROGRAM 0
      WHEN 'l'
        LET listSeen = TRUE
      WHEN 'F'
        LET _opt_in_FGLDIR = TRUE
      WHEN 's'
        LET _opt_simulate = TRUE
      WHEN 'u'
        LET _opt_undo = TRUE
      WHEN 'i'
        LET _opt_plain = TRUE
        { --TODO
        WHEN 'q'
          LET _opt_quiet = TRUE
        WHEN 'L'
          LET _opt_logfile = opt_arg
        WHEN 'd'
          LET _opt_ext_dir = opt_arg
        }
    END CASE
  END WHILE
  IF (cnt := mygetopt.getMoreArgumentCount(gr)) <> 1 THEN
    CALL mygetopt.displayUsage(gr, "fjs-<product>.zip")
    EXIT PROGRAM 1
  END IF
  LET _product_zip = mygetopt.getMoreArgument(gr, 1)
  LET _product_zip = os.Path.fullPath(_product_zip)
  IF listSeen THEN
    DISPLAY unzipList()
    EXIT PROGRAM 0
  END IF
  { --TODO
  IF _opt_in_FGLDIR AND _opt_ext_dir IS NOT NULL THEN
    CALL userError(
        "option --use-FGLDIR(-F) and --destination-dir(-d) are mutually exclusive")
  END IF
  }
END FUNCTION

PRIVATE FUNCTION readFiles()
  DEFINE raw STRING
  DEFINE tok base.StringTokenizer
  DEFINE path STRING
  DEFINE doc om.DomDocument
  DEFINE root {,lastNode} om.DomNode
  LET doc = om.DomDocument.create("Files")
  LET root = doc.getDocumentElement()
  LET raw = unzipList()
  LET raw = replace(raw, "\r\n", "\n") --windows
  LET tok = base.StringTokenizer.create(raw, "\n")
  WHILE tok.hasMoreTokens()
    LET path = tok.nextToken()
    IF path.getLength() > 0 THEN
      CALL addFile(root, path)
    END IF
  END WHILE
  --DISPLAY "readFiles did get:",root.toString()
  RETURN root
END FUNCTION

--loop thru the path parts
PRIVATE FUNCTION findFileNode(parent, path, createIfNotFound)
  DEFINE path, part, tName, full STRING
  DEFINE parent, child, newchild om.DomNode
  DEFINE createIfNotFound, found BOOLEAN
  DEFINE tok base.StringTokenizer
  LET tok = base.StringTokenizer.create(path, "/")
  WHILE tok.hasMoreTokens()
    LET found = FALSE
    LET part = tok.nextToken()
    IF createIfNotFound THEN
      LET full = IIF(full IS NULL, part, SFMT("%1/%2", full, part))
    END IF
    --DISPLAY "begin handle part:",part," for parent:",parent.getTagName(),",full:",full
    LET child = parent.getFirstChild()
    WHILE child IS NOT NULL
      IF child.getAttribute("name") == part THEN
        LET found = TRUE
        --DISPLAY sfmt("found childTag:%1 for parent:%2",part,parent.getTagName())
        LET parent = child
        EXIT WHILE
      ELSE
        LET child = child.getNext()
      END IF
    END WHILE
    IF NOT found THEN
      IF NOT createIfNotFound THEN
        RETURN NULL
      END IF
      IF path.getCharAt(full.getLength() + 1)
          == "/" THEN --this full part ends with a slash
        LET tName = "Dir"
      ELSE
        LET tName = "File"
      END IF
      LET newchild = parent.createChild(tName)
      CALL newchild.setAttribute("name", part)
      CALL parent.setAttribute("isDir", "1")
      --DISPLAY sfmt("created newchild for tag:%1,parentName:%2",part,parent.getAttribute("name"))
      LET parent = newchild
    END IF
  END WHILE
  RETURN parent
END FUNCTION

PRIVATE FUNCTION addFile(parent, path)
  DEFINE parent, node om.DomNode
  DEFINE path STRING
  --DISPLAY "addFile:",path
  LET node = findFileNode(parent, path, TRUE)
  --DISPLAY "added:",node.toString()
END FUNCTION

PRIVATE FUNCTION fileExists(root, name)
  DEFINE root, node om.DomNode
  DEFINE name STRING
  LET node = findFileNode(root, name, FALSE)
  RETURN node IS NOT NULL
END FUNCTION

PRIVATE FUNCTION isDir(root, name)
  DEFINE root, node om.DomNode
  DEFINE name STRING
  LET node = findFileNode(root, name, FALSE)
  RETURN node IS NOT NULL AND node.getTagName() == "Dir"
END FUNCTION

PRIVATE FUNCTION analyze(root)
  DEFINE root, child {, lastChild} om.DomNode
  DEFINE numChildren INT
  --DEFINE children DYNAMIC ARRAY OF om.DomNode
  LET child = root.getFirstChild()
  WHILE child IS NOT NULL
    LET numChildren = numChildren + 1
    --LET children[children.getLength()+1]=child.getAttribute("name")
    --LET lastChild = child
    LET child = child.getNext()
  END WHILE
  CALL root.setAttribute("numChildren", numChildren)
  RETURN numChildren
END FUNCTION

PRIVATE FUNCTION getNumChildren(root)
  DEFINE root om.DomNode
  RETURN root.getAttribute("numChildren")
END FUNCTION

PRIVATE FUNCTION doit(root, numChildren)
  DEFINE root om.DomNode
  DEFINE numChildren INT
  DEFINE defRoot STRING
  {
  IF _opt_ext_dir IS NOT NULL THEN
    CALL mkdirp(_opt_ext_dir)
    MYASSERT(os.Path.chDir(_opt_ext_dir) == TRUE)
  END IF
  }
  IF numChildren == 1 THEN --single root , no need to compute one
    IF _opt_in_FGLDIR THEN
      CALL userError(
          "This package is not prepared to be installed over FGLDIR(yet).")
    END IF
    CALL unzip(root)
  ELSE
    IF _opt_in_FGLDIR THEN
      CALL unzipOverFGLDIR(root)
    ELSE
      IF NOT _opt_plain THEN --by default create a single root directory to avoid cluttering the current directory with multiple files and extract the zip beneath that single root directory
        LET defRoot = computeDefName()
        IF NOT os.Path.exists(defRoot) THEN
          CALL mkdirp(defRoot)
          IF _opt_verbose THEN
            DISPLAY "created extraction root:", os.Path.fullPath(defRoot)
          END IF
        END IF
        MYASSERT(os.Path.chDir(defRoot) == TRUE)
      ELSE
        IF NOT _opt_simulate AND NOT _opt_undo THEN
          CALL checkFilesExisting(root)
        END IF
      END IF
      CALL unzip(root)
    END IF
  END IF
END FUNCTION

PRIVATE FUNCTION unzip(root)
  DEFINE root om.DomNode
  DEFINE cmd STRING
  DEFINE code INT
  IF _opt_simulate THEN
    IF _opt_undo THEN
      DISPLAY "Would remove in:", os.Path.pwd()
      DISPLAY "(N no file/dir) (D remove dir if empty) (F remove File) (C conflict)"
    ELSE
      DISPLAY "Would extract in:", os.Path.pwd()
      DISPLAY "(N new file/dir) (D overwrite dir) (F overwrite File) (C conflict)"
    END IF
    CALL simulate(root, os.Path.pwd())
    RETURN
  END IF
  IF _opt_undo THEN
    CALL undo(root, os.Path.pwd())
    RETURN
  END IF
  --CALL generateUndoScript(root)
  LET cmd = SFMT("%1 xf %2", tarExe(), quote(_product_zip))
  IF _opt_verbose THEN
    DISPLAY "unzip cmd:", cmd
  END IF
  RUN cmd RETURNING code
  IF code THEN
    EXIT PROGRAM code
  END IF
  CALL verify(root, os.Path.pwd())
END FUNCTION

PRIVATE FUNCTION undo(parent, parentDir)
  DEFINE parent, child om.DomNode
  DEFINE parentDir, path, tag STRING
  LET child = parent.getFirstChild()
  WHILE child IS NOT NULL
    LET path = os.Path.join(parentDir, child.getAttribute("name"))
    LET tag = child.getTagName()
    IF tag == "File" THEN
      IF os.Path.exists(path) THEN
        IF NOT os.Path.delete(path) THEN
          DISPLAY "couldn't delete:", path
        ELSE
          IF _opt_verbose THEN
            DISPLAY "deleted file:", path
          END IF
        END IF
      END IF
    END IF
    CALL undo(child, path)
    IF tag == "Dir" THEN
      IF os.Path.exists(path) AND os.Path.isDirectory(path) THEN
        IF NOT os.Path.delete(path) THEN
          DISPLAY "Could not delete dir:", path, ",probably not empty"
        ELSE
          IF _opt_verbose THEN
            DISPLAY "deleted dir:", path, "/"
          END IF
        END IF
      END IF
    END IF
    LET child = child.getNext()
  END WHILE
END FUNCTION

#+check if the unzip command did work
PRIVATE FUNCTION verify(parent, parentDir)
  DEFINE parent, child om.DomNode
  DEFINE parentDir, path, tag STRING
  LET child = parent.getFirstChild()
  WHILE child IS NOT NULL
    LET path = os.Path.join(parentDir, child.getAttribute("name"))
    LET tag = child.getTagName()
    CASE
      WHEN tag == "Dir"
        MYASSERT(os.Path.isDirectory(path))
      WHEN tag == "File"
        MYASSERT(os.Path.isFile(path))
      OTHERWISE
        CALL myErr(SFMT("unexpected tagName:%1", tag))
    END CASE
    IF _opt_verbose THEN
      DISPLAY "verified:", path
    END IF
    CALL verify(child, path)
    LET child = child.getNext()
  END WHILE
END FUNCTION

#+check if the unzip command did work
PRIVATE FUNCTION simulate(parent, parentDir)
  DEFINE parent, child om.DomNode
  DEFINE parentDir, path, tag, marker STRING
  LET child = parent.getFirstChild()
  WHILE child IS NOT NULL
    LET path = os.Path.join(parentDir, child.getAttribute("name"))
    LET tag = child.getTagName()
    CASE
      WHEN tag == "Dir"
        IF os.Path.exists(path) THEN
          LET marker = IIF(os.Path.isFile(path), "C", "D")
        ELSE
          LET marker = "N"
        END IF
        DISPLAY SFMT("%1 %2%3", marker, path, os.Path.separator())
        IF marker == "C" THEN
          DISPLAY "  expected: directory, actual: file"
        END IF
      WHEN tag == "File"
        IF os.Path.exists(path) THEN
          LET marker = IIF(os.Path.isDirectory(path), "C", "F")
        ELSE
          LET marker = "N"
        END IF
        DISPLAY SFMT("%1 %2", marker, path)
        IF marker == "C" THEN
          DISPLAY "  expected: file, actual: directory"
        END IF
      OTHERWISE
        CALL myErr(SFMT("unexpected tagName:%1", tag))
    END CASE
    CALL simulate(child, path)
    LET child = child.getNext()
  END WHILE
END FUNCTION

PRIVATE FUNCTION checkFilesExisting(root)
  DEFINE root om.DomNode
  DEFINE dh INT
  DEFINE fname, ans STRING
  DEFINE foundEntries INT
  LET dh = os.Path.dirOpen(os.Path.pwd())
  IF dh == 0 THEN
    CALL userError(SFMT("Can't open directory '%1'", os.Path.pwd()))
  END IF
  WHILE (fname := os.Path.dirNext(dh)) IS NOT NULL
    IF fname == "." OR fname == ".." THEN
      CONTINUE WHILE
    END IF
    LET foundEntries = foundEntries + 1
  END WHILE
  CALL os.Path.dirClose(dh)
  IF foundEntries > 0 THEN
    LET ans =
        yesno(
            SFMT("There are already %1 files, folders or links in this directory...\nContinue extracting %2 files ?",
                foundEntries, getNumChildren(root)))
    --DISPLAY "ans:",ans
    IF NOT ans.equals("yes") THEN
      EXIT PROGRAM 1
    END IF
  END IF
END FUNCTION

PRIVATE FUNCTION isGBC(root)
  DEFINE root om.DomNode
  RETURN fileExists(root, "VERSION")
      AND fileExists(root, "PRODUCTINFO")
      AND fileExists(root, "index.html")
      AND fileExists(root, "js/gbc.js")
END FUNCTION

PRIVATE FUNCTION myChdir(path)
  DEFINE path STRING
  IF NOT os.Path.chDir(path) THEN
    CALL myErr(SFMT("Can't chdir to:%1", path))
  END IF
END FUNCTION

PRIVATE FUNCTION getFglDir()
  RETURN base.Application.getFglDir()
END FUNCTION

PRIVATE FUNCTION unzipGBCoverFGLDIR(root)
  DEFINE root om.DomNode
  DEFINE gbcDir STRING
  LET gbcDir = SFMT("%1/web_utilities/gbc/gbc", getFglDir())
  CALL mkdirp(gbcDir)
  CALL myChdir(gbcDir)
  CALL unzip(root)
END FUNCTION

PRIVATE FUNCTION unzipOverFGLDIR(root)
  DEFINE root om.DomNode
  IF isGBC(root) THEN
    CALL unzipGBCoverFGLDIR(root)
  ELSE
    CALL myChdir(getFglDir())
    CALL unzip(root)
  END IF
END FUNCTION

PRIVATE FUNCTION generateUndoScript(root)
  DEFINE root om.DomNode
  DEFINE ch_sh, ch_bat base.Channel
  DEFINE name_sh, name_bat STRING
  LET ch_sh = base.Channel.create()
  LET name_sh = SFMT("rm-%1.sh", computeDefName())
  CALL ch_sh.openFile(name_sh, "w")
  CALL ch_sh.writeLine("#!/bin/bash")
  IF isWin() THEN
    LET ch_bat = base.Channel.create()
    LET name_bat = SFMT("rm-%1.bat", computeDefName())
    CALL ch_bat.openFile(name_bat, "w")
    CALL ch_bat.writeLine("@echo off")
  END IF
  CALL add_rm(root, ".", ch_sh, ch_bat)
  CALL ch_sh.writeLine(SFMT("rm -f %1", quote(name_sh)))
  CALL ch_sh.close()
  MYASSERT(os.Path.chRwx(name_sh, 484) == TRUE) --u+x
END FUNCTION

PRIVATE FUNCTION add_rm(parent, parentDir, ch_sh, ch_bat)
  DEFINE parent, child om.DomNode
  DEFINE parentDir, path, winpath STRING
  DEFINE ch_sh, ch_bat base.Channel
  LET child = parent.getFirstChild()
  WHILE child IS NOT NULL
    LET path = SFMT("%1/%2", parentDir, child.getAttribute("name"))
    LET winpath = replace(path, "/", "\\")
    IF child.getTagName() == "File" THEN
      CALL ch_sh.writeLine(SFMT("rm -f %1", quote(path)))
      IF isWin() THEN
        CALL ch_bat.writeLine(SFMT("del /Q %1", quote(winpath)))
      END IF
    END IF
    CALL add_rm(child, path, ch_sh, ch_bat)
    IF child.getTagName() == "Dir" THEN
      CALL ch_sh.writeLine(SFMT("rmdir %1", quote(path)));
      IF isWin() THEN
        CALL ch_bat.writeLine(SFMT("rmdir %1", quote(winpath)));
      END IF
    END IF
    LET child = child.getNext()
  END WHILE
END FUNCTION

#+ for zip archives not having a single root we create a root dir named after the product file name (similar to what desktop extraction tools do)
PRIVATE FUNCTION computeDefName()
  DEFINE def, b STRING
  DEFINE idx1, idx2 INT
  LET b = os.Path.baseName(_product_zip)
  IF b.getIndexOf("fjs-", 1) == 1 THEN
    LET idx1 = b.getIndexOf("-", 5)
    MYASSERT(idx1 != 0)
    LET idx2 = b.getIndexOf("-", idx1 + 1)
    LET def = b.subString(5, idx2 - 1)
  ELSE
    LET def = removeExtension(b)
  END IF
  --DISPLAY "defname:", def
  RETURN def
END FUNCTION

--utils

PRIVATE FUNCTION isWin() RETURNS BOOLEAN
  RETURN os.Path.separator().equals("\\")
END FUNCTION

PRIVATE FUNCTION printStderr(errstr STRING)
  DEFINE ch base.Channel
  LET ch = base.Channel.create()
  CALL ch.openFile("<stderr>", "w")
  CALL ch.writeLine(errstr)
  CALL ch.close()
END FUNCTION
{
PRIVATE FUNCTION printStdout(str STRING, noNewLine BOOLEAN)
  IF noNewLine THEN
    LET _stdoutNONL = _stdoutNONL, str
  ELSE
    LET str = _stdoutNONL, str
    LET _stdoutNONL = ""
    DISPLAY str
  END IF
END FUNCTION
}

PRIVATE FUNCTION myErr(errstr STRING)
  CALL printStderr(
      SFMT("ERROR:%1 stack:\n%2", errstr, base.Application.getStackTrace()))
  EXIT PROGRAM 1
END FUNCTION
{
PRIVATE FUNCTION myWarning(errstr STRING)
  CALL printStderr(SFMT("Warning %1:%2", progName(), errstr))
END FUNCTION

PRIVATE FUNCTION log(msg STRING)
  IF fgl_getenv("VERBOSE") IS NOT NULL THEN
    DISPLAY "log:", msg
  END IF
END FUNCTION

--for dev: replace log() with dlog() for simply write to stdout
PRIVATE FUNCTION dlog(s STRING)
  DISPLAY s
END FUNCTION
}

PRIVATE FUNCTION already_quoted(path) RETURNS BOOLEAN
  DEFINE path, first, last STRING
  LET first = NVL(path.getCharAt(1), "NULL")
  LET last = NVL(path.getCharAt(path.getLength()), "NULL")
  IF isWin() THEN
    RETURN (first == '"' AND last == '"')
  END IF
  RETURN (first == "'" AND last == "'") OR (first == '"' AND last == '"')
END FUNCTION

PRIVATE FUNCTION quote(path STRING) RETURNS STRING
  RETURN quoteInt(path, FALSE)
END FUNCTION

PRIVATE FUNCTION quoteForce(path STRING) RETURNS STRING
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

PRIVATE FUNCTION replace(
    src STRING, oldStr STRING, newString STRING)
    RETURNS STRING
  DEFINE b base.StringBuffer
  LET b = base.StringBuffer.create()
  CALL b.append(src)
  CALL b.replace(oldStr, newString, 0)
  RETURN b.toString()
END FUNCTION

PRIVATE FUNCTION backslash2slash(src STRING) RETURNS STRING
  RETURN replace(src, "\\", "/")
END FUNCTION

#+case insensitive variant of getIndexOf
PRIVATE FUNCTION getIndexOfI(src, pattern, idx) RETURNS INT
  DEFINE src, pattern STRING
  DEFINE idx INTEGER
  LET src = src.toLowerCase()
  RETURN src.getIndexOf(pattern.toLowerCase(), idx)
END FUNCTION

PRIVATE FUNCTION getProgramOutputWithErr(cmd STRING) RETURNS(STRING, STRING)
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
    LET errStr = ",\noutput:", ret
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

PRIVATE FUNCTION getProgramOutput(cmd STRING) RETURNS STRING
  DEFINE result, err STRING
  CALL getProgramOutputWithErr(cmd) RETURNING result, err
  IF err IS NOT NULL THEN
    CALL userError(SFMT("failed to RUN:%1%2", cmd, err))
  END IF
  RETURN result
END FUNCTION

#+computes a temporary file name
PRIVATE FUNCTION makeTempName() RETURNS STRING
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

-- returns
-- -<count>-g<SHA> for a non release
-- -<SHA> for a release (GIT_COMMIT_COUNT==0)
PRIVATE FUNCTION adjustGitCountAndRev(cnt INT, rev STRING)
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

PRIVATE FUNCTION removeExtension(fname STRING) RETURNS STRING
  VAR ext = os.Path.extension(fname)
  IF ext.getLength() > 0 THEN
    LET fname = fname.subString(1, fname.getLength() - (ext.getLength() + 1))
  END IF
  RETURN fname
END FUNCTION

PRIVATE FUNCTION printVersion()
  VAR prog = removeExtension(os.Path.baseName(arg_val(0)))
  DISPLAY SFMT("%1 %2 rev%3",
      prog, GIT_VERSION, adjustGitCountAndRev(GIT_COMMIT_COUNT, GIT_REV))
  EXIT PROGRAM 0
END FUNCTION

PRIVATE FUNCTION isLetter(c STRING)
  VAR letters = "abcdefghijklmnopqrstuvwxyz"
  RETURN getIndexOfI(src: letters, pattern: c, idx: 1) > 0
END FUNCTION

PRIVATE FUNCTION isWinDriveInt(path STRING)
  RETURN isWin()
      AND path.getCharAt(2) == ":"
      AND (path.getCharAt(3) == "\\" OR path.getCharAt(3) == "/")
      AND isLetter(path.getCharAt(1))
END FUNCTION
{
PRIVATE FUNCTION isWinDriveRoot(path STRING)
  RETURN path.getLength() == 3 AND isWinDriveInt(path)
END FUNCTION
}

PRIVATE FUNCTION pathStartsWithWinDrive(path STRING)
  RETURN path.getLength() >= 3 AND isWinDriveInt(path)
END FUNCTION

#creates a directory path recursively like mkdir -p
PRIVATE FUNCTION mkdirp(path STRING)
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

PRIVATE FUNCTION progName() RETURNS STRING
  VAR ret = os.Path.baseName(arg_val(0))
  VAR ext = os.Path.extension(ret)
  IF ext.getLength() > 0 THEN
    LET ret = ret.subString(1, ret.getLength() - ext.getLength() - 1)
  END IF
  RETURN ret
END FUNCTION

PRIVATE FUNCTION userError(err STRING)
  CALL printStderr(SFMT("Error %1:%2", progName(), err))
  EXIT PROGRAM 1
END FUNCTION

PRIVATE FUNCTION whichExe(prog STRING) RETURNS STRING
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

PRIVATE FUNCTION fullfglrunEXE()
  RETURN os.Path.join(
      os.Path.join(base.Application.getFglDir(), "bin"),
      SFMT("fglrun%1", IIF(isWin(), ".exe", "")))
END FUNCTION

PRIVATE FUNCTION fullProgName()
  RETURN os.Path.join(
      base.Application.getProgramDir(), os.Path.baseName(arg_val(0)))
END FUNCTION

PRIVATE FUNCTION writeStringToFile(file STRING, content STRING)
  DEFINE ch base.Channel
  LET ch = base.Channel.create()
  CALL ch.openFile(file, "w")
  CALL ch.writeNoNL(content)
  CALL ch.close()
END FUNCTION

FUNCTION readTextFile(filename) RETURNS STRING
  DEFINE filename STRING
  DEFINE content STRING
  DEFINE t TEXT
  LOCATE t IN FILE filename
  LET content = t
  RETURN content
END FUNCTION

FUNCTION yesno_mode()
  DEFINE yesno_msg STRING
  LET yesno_msg = fgl_getenv("__FGL_UNZIP_YESNO_MESSAGE__")
  IF yesno_msg IS NOT NULL THEN --we did invoke us for yesno
    CALL yesno(yesno_msg) RETURNING status
    RETURN TRUE
  END IF
  RETURN FALSE
END FUNCTION

PRIVATE FUNCTION yesno_cmd(message)
  DEFINE message STRING
  DEFINE cmd, ans, resfile STRING
  DEFINE code INT
  CALL fgl_setenv("FGLGUI", "0")
  CALL fgl_setenv("__FGL_UNZIP_YESNO_MESSAGE__", message)
  LET resfile = makeTempName()
  CALL fgl_setenv("__FGL_UNZIP_RESULT_FILE__", resfile)
  LET cmd = SFMT("%1 %2", quote(fullfglrunEXE()), quote(fullProgName()))
  --DISPLAY "cmd:",cmd
  RUN cmd RETURNING code
  IF code THEN
    LET ans = "failed"
  ELSE
    LET ans = readTextFile(resfile)
  END IF
  CALL os.Path.delete(resfile) RETURNING status
  RETURN ans
END FUNCTION

--displays a multiline message in a temp .42f
--and calls a MENU with yes no
PRIVATE FUNCTION yesno(message)
  DEFINE message STRING
  DEFINE fglgui, ans, resfile, frmfile STRING
  LET fglgui = fgl_getenv("FGLGUI")
  IF NOT fglgui.equals("0") THEN --run sub process
    RETURN yesno_cmd(message)
  END IF
  LET resfile = fgl_getenv("__FGL_UNZIP_RESULT_FILE__")
  LET frmfile = makeTempName(), ".42f"
  CALL writeStringToFile(frmfile, fglunzip_42f())
  OPTIONS MENU LINE 5
  OPTIONS COMMENT LINE 6
  OPEN FORM f FROM frmfile
  DISPLAY FORM f
  CALL os.Path.delete(frmfile) RETURNING status
  DISPLAY message TO msg
  MENU " Please answer"
    COMMAND "yes"
      LET ans = "yes"
      EXIT MENU
    COMMAND "no"
      LET ans = "no"
      EXIT MENU
  END MENU
  IF resfile IS NOT NULL THEN
    CALL writeStringToFile(resfile, content: ans)
  END IF
  RETURN ans
END FUNCTION

--returns fglunzip.per ready compiled
FUNCTION fglunzip_42f()
  RETURN '<?xml version=\'1.0\' encoding=\'UTF-8\'?>\n<Form name="fglunzip" build="5.01.02" width="80" height="2" delimiters="">\n<Screen width="80" height="2">\n<FormField name="formonly.msg" colName="msg" fieldId="0" sqlTabName="formonly" tabIndex="1">\n<TextEdit wantReturns="1" scrollBars="none" scroll="0" height="2" width="78" posY="0" posX="1" gridWidth="78" gridHeight="2"/>\n</FormField>\n</Screen>\n<RecordView tabName="formonly">\n<Link colName="msg" fieldIdRef="0"/>\n</RecordView>\n</Form>'
END FUNCTION
--SCREEN
--{
--[msg                                                                           ]
--[msg                                                                           ]
--}
--END
--ATTRIBUTES

--msg=FORMONLY.msg,WORDWRAP;
--INSTRUCTIONS
--DELIMITERS "";
