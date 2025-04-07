--IMPORT util
OPTIONS
SHORT CIRCUIT
IMPORT os
IMPORT FGL futils
IMPORT FGL mygetopt
&include "myassert.inc"
DEFINE _product_zip STRING --the zip file to process
DEFINE _opt_verbose BOOLEAN
--DEFINE _opt_quiet BOOLEAN
--DEFINE _opt_logfile STRING
DEFINE _opt_in_FGLDIR BOOLEAN
--DEFINE _opt_ext_dir STRING
DEFINE _opt_simulate BOOLEAN
DEFINE _opt_undo BOOLEAN
MAIN
  --DEFINE fglgui, fglrunExe, cmd STRING
  --DEFINE code INT
  DEFINE argsarr DYNAMIC ARRAY OF STRING
  DEFINE root om.DomNode
  DEFINE numChildren INT
  {
  LET fglgui = fgl_getenv("FGLGUI")
  --DISPLAY "fglgui:",fglgui
  IF NOT fglgui.equals("0") THEN
    CALL fgl_setenv("FGLGUI", "0")
    --don't mess with Unix and windows shell quoting, just use the env
    --to pass the args
    CALL passArgsViaEnv()
    LET fglrunExe = futils.fglrunEXE()
    LET cmd = SFMT("%1 %2", quote(fglrunExe), quote(arg_val(0)))
    --DISPLAY "RUN cmd:",cmd
    RUN cmd RETURNING code
    --DISPLAY "code:",code
    EXIT PROGRAM code
  END IF
  }
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
  --DEFINE cmd,res,err STRING
  DEFINE cmd STRING
  LET cmd = SFMT("%1 tf %2", tarExe(), quote(_product_zip))
  --DISPLAY "unzipList cmd:", cmd
  RETURN getProgramOutput(cmd)
END FUNCTION

FUNCTION passArgsViaEnv()
  DEFINE i INT
  CALL fgl_setenv("_FGLUNZIP_NUMARGS", num_args())
  FOR i = 1 TO num_args()
    CALL fgl_setenv(SFMT("_FGLUNZIP_ARG_NUMARGS%1", i), arg_val(i))
  END FOR
END FUNCTION

FUNCTION setupArgs()
  DEFINE i, mynumargs INT
  DEFINE argsarr DYNAMIC ARRAY OF STRING
  LET mynumargs = fgl_getenv("_FGLUNZIP_NUMARGS")
  IF mynumargs IS NOT NULL THEN
    --receive args via env
    FOR i = 1 TO mynumargs
      LET argsarr[i] = fgl_getenv(SFMT("_FGLUNZIP_ARG_NUMARGS%1", i))
    END FOR
  ELSE
    FOR i = 1 TO num_args()
      LET argsarr[i] = arg_val(i)
    END FOR
  END IF
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
  LET o[i].description = "simulates what would be extracted"
  LET o[i].opt_char = "s"
  LET o[i].arg_type = mygetopt.NONE

  {
  LET i = o.getLength() + 1
  LET o[i].name = "quiet"
  LET o[i].description = "Does install quietly without asking yes/no"
  LET o[i].opt_char = "q"
  LET o[i].arg_type = mygetopt.NONE
  }

  LET i = o.getLength() + 1
  LET o[i].name = "list"
  LET o[i].description = "Lists the archive content"
  LET o[i].opt_char = "l"
  LET o[i].arg_type = mygetopt.NONE
  {
  LET i = o.getLength() + 1
  LET o[i].name = "logfile"
  LET o[i].description = "File written for logs and success"
  LET o[i].opt_char = "L"
  LET o[i].arg_type = mygetopt.REQUIRED
  }

  LET i = o.getLength() + 1
  LET o[i].name = "use-FGLDIR"
  LET o[i].description =
      "installs over FGLDIR to make the product avaiable without further env settings"
  LET o[i].opt_char = "F"
  LET o[i].arg_type = mygetopt.NONE

  {
  LET i = o.getLength() + 1
  LET o[i].name = "destination-dir"
  LET o[i].description =
      "choose another extraction directory than the current dir"
  LET o[i].opt_char = "d"
  LET o[i].arg_type = mygetopt.NONE
  }

  LET i = o.getLength() + 1
  LET o[i].name = "undo"
  LET o[i].description = "Reverts the install"
  LET o[i].opt_char = "u"
  LET o[i].arg_type = mygetopt.NONE

  CALL mygetopt.initialize(gr, "fglunzip", argsarr, o)
  WHILE mygetopt.getopt(gr) == mygetopt.SUCCESS
    LET opt_arg = mygetopt.opt_arg(gr)
    CASE mygetopt.opt_char(gr)
      WHEN 'V'
        CALL futils.printVersion()
        EXIT PROGRAM 0
      WHEN 'v'
        LET _opt_verbose = TRUE
        {
        WHEN 'q'
          LET _opt_quiet = TRUE
        }
      WHEN 'h'
        CALL mygetopt.displayUsage(gr, "fjs-<product>.zip")
        EXIT PROGRAM 0
      WHEN 'l'
        LET listSeen = TRUE
        {
        WHEN 'L'
          LET _opt_logfile = opt_arg
        }
      WHEN 'F'
        LET _opt_in_FGLDIR = TRUE
      WHEN 's'
        LET _opt_simulate = TRUE
      WHEN 'u'
        LET _opt_undo = TRUE
        {
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
  {
  IF _opt_in_FGLDIR AND _opt_ext_dir IS NOT NULL THEN
    CALL userError(
        "option --use-FGLDIR(-F) and --destination-dir(-d) are mutually exclusive")
  END IF
  }
END FUNCTION

FUNCTION yesno(message)
  DEFINE message STRING
  CALL fgl_setenv("FGLGUI", "0")
  --RETURN fgl_winButton(title: "fglunzip",message: message,ans: "no",items: "yes|no",icon: "",dang: 0)
  MENU message
    COMMAND "yes"
      RETURN "yes"
    COMMAND "no"
      RETURN "no"
  END MENU
  RETURN "no"
END FUNCTION

FUNCTION readFiles()
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
FUNCTION findFileNode(parent, path, createIfNotFound)
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

FUNCTION addFile(parent, path)
  DEFINE parent, node om.DomNode
  DEFINE path STRING
  --DISPLAY "addFile:",path
  LET node = findFileNode(parent, path, TRUE)
  --DISPLAY "added:",node.toString()
END FUNCTION

FUNCTION fileExists(root, name)
  DEFINE root, node om.DomNode
  DEFINE name STRING
  LET node = findFileNode(root, name, FALSE)
  RETURN node IS NOT NULL
END FUNCTION

FUNCTION isDir(root, name)
  DEFINE root, node om.DomNode
  DEFINE name STRING
  LET node = findFileNode(root, name, FALSE)
  RETURN node IS NOT NULL AND node.getTagName() == "Dir"
END FUNCTION

FUNCTION analyze(root)
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
  RETURN numChildren
END FUNCTION

FUNCTION doit(root, numChildren)
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
      LET defRoot = computeDefName()
      IF NOT os.Path.exists(defRoot) THEN
        CALL mkdirp(defRoot)
        IF _opt_verbose THEN
          DISPLAY "created extraction root:", os.Path.fullPath(defRoot)
        END IF
      END IF
      MYASSERT(os.Path.chDir(defRoot) == TRUE)
      CALL unzip(root)
    END IF
  END IF
END FUNCTION

FUNCTION unzip(root)
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

FUNCTION undo(parent, parentDir)
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
FUNCTION verify(parent, parentDir)
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
FUNCTION simulate(parent, parentDir)
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

FUNCTION isGBC(root)
  DEFINE root om.DomNode
  RETURN fileExists(root, "VERSION")
      AND fileExists(root, "PRODUCTINFO")
      AND fileExists(root, "index.html")
      AND fileExists(root, "js/gbc.js")
END FUNCTION

FUNCTION myChdir(path)
  DEFINE path STRING
  IF NOT os.Path.chDir(path) THEN
    CALL myErr(SFMT("Can't chdir to:%1", path))
  END IF
END FUNCTION

FUNCTION getFglDir()
  RETURN base.Application.getFglDir()
END FUNCTION

FUNCTION unzipGBCoverFGLDIR(root)
  DEFINE root om.DomNode
  DEFINE gbcDir STRING
  LET gbcDir = SFMT("%1/web_utilities/gbc/gbc", getFglDir())
  CALL mkdirp(gbcDir)
  CALL myChdir(gbcDir)
  CALL unzip(root)
END FUNCTION

FUNCTION unzipOverFGLDIR(root)
  DEFINE root om.DomNode
  IF isGBC(root) THEN
    CALL unzipGBCoverFGLDIR(root)
  ELSE
    CALL myChdir(getFglDir())
    CALL unzip(root)
  END IF
END FUNCTION

FUNCTION generateUndoScript(root)
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

FUNCTION add_rm(parent, parentDir, ch_sh, ch_bat)
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
FUNCTION computeDefName()
  DEFINE def, b STRING
  DEFINE idx1, idx2 INT
  LET b = os.Path.baseName(_product_zip)
  IF b.getIndexOf("fjs-", 1) == 1 THEN
    LET idx1 = b.getIndexOf("-", 5)
    MYASSERT(idx1 != 0)
    LET idx2 = b.getIndexOf("-", idx1 + 1)
    LET def = b.subString(5, idx2 - 1)
  ELSE
    LET def = cutExtension(b)
  END IF
  DISPLAY "defname:", def
  RETURN def
END FUNCTION
