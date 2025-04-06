--IMPORT util
OPTIONS SHORT CIRCUIT
IMPORT os
IMPORT FGL futils
IMPORT FGL mygetopt
&include "myassert.inc"
--IMPORT FGL fgldialog
{
TYPE TFileEntry RECORD
  name STRING,
  children DYNAMIC ARRAY OF STRING
END RECORD
}
DEFINE _product_zip STRING
DEFINE _opt_verbose BOOLEAN
DEFINE _opt_quiet BOOLEAN
DEFINE _opt_logfile STRING
DEFINE _opt_in_FGLDIR BOOLEAN
DEFINE _opt_ext_dir STRING
--DEFINE _root om.DomNode --holds the file list from the zip as a DOM tree
MAIN
  DEFINE fglgui,fglrunExe,cmd STRING
  DEFINE code INT
  DEFINE argsarr DYNAMIC ARRAY OF STRING
  DEFINE root om.DomNode
  LET fglgui=fgl_getenv("FGLGUI")
  --DISPLAY "fglgui:",fglgui
  IF NOT fglgui.equals("0") THEN
    CALL fgl_setenv("FGLGUI","0")
    --don't mess with Unix and windows shell quoting, just use the env
    --to pass the args
    CALL passArgsViaEnv()
    LET fglrunExe=futils.fglrunEXE()
    LET cmd=sfmt("%1 %2",quote(fglrunExe),quote(arg_val(0)))
    --DISPLAY "RUN cmd:",cmd
    RUN cmd RETURNING code
    --DISPLAY "code:",code
    EXIT PROGRAM code
  END IF
  LET argsarr=setupArgs()
  --DISPLAY "argsarr:",util.JSON.stringify(argsarr)
  CALL parseArgs(argsarr)
  LET root=readFiles()
  --DISPLAY yesno("testyesno")
  DISPLAY 'isDir "bin":"',isDir(root,"bin")
  DISPLAY 'fileExists "doc/gwa/location.html":"',fileExists(root,"doc/gwa/location.html")
  DISPLAY 'fileExists "foo":"',fileExists(root,"foo")
  CALL analyze(root)
END MAIN

FUNCTION unzipList()
  --DEFINE cmd,res,err STRING
  DEFINE cmd STRING
  LET cmd=sfmt("tar tf %1",quote(_product_zip))
  DISPLAY "unzipList cmd:",cmd
  RETURN getProgramOutput(cmd)
  {
  IF err THEN
    CALL printStderr(err)
  ELSE
    DISPLAY res
  END IF
  }
END FUNCTION

FUNCTION passArgsViaEnv()
  DEFINE i INT
  CALL fgl_setenv("_FGLUNZIP_NUMARGS",num_args())
  FOR i=1 TO num_args()
    CALL fgl_setenv(sfmt("_FGLUNZIP_ARG_NUMARGS%1",i),arg_val(i))
  END FOR
END FUNCTION

FUNCTION setupArgs()
  DEFINE i,mynumargs INT
  DEFINE argsarr DYNAMIC ARRAY OF STRING
  LET mynumargs=fgl_getenv("_FGLUNZIP_NUMARGS")
  IF mynumargs IS NOT NULL THEN
    --receive args via env
    FOR i=1 TO mynumargs
      LET argsarr[i]=fgl_getenv(sfmt("_FGLUNZIP_ARG_NUMARGS%1",i))
    END FOR
  ELSE
    FOR i=1 TO num_args()
      LET argsarr[i]=arg_val(i)
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
  LET o[i].name = "quiet"
  LET o[i].description = "Does install quietly without asking yes/no"
  LET o[i].opt_char = "q"
  LET o[i].arg_type = mygetopt.NONE

  LET i = o.getLength() + 1
  LET o[i].name = "list"
  LET o[i].description = "Lists the archive content"
  LET o[i].opt_char = "l"
  LET o[i].arg_type = mygetopt.NONE

  LET i = o.getLength() + 1
  LET o[i].name = "logfile"
  LET o[i].description = "File written for logs and success"
  LET o[i].opt_char = "L"
  LET o[i].arg_type = mygetopt.REQUIRED

  LET i = o.getLength() + 1
  LET o[i].name = "use-FGLDIR"
  LET o[i].description =
      "installs over FGLDIR to make the product avaiable without further env settings"
  LET o[i].opt_char = "F"
  LET o[i].arg_type = mygetopt.NONE

  LET i = o.getLength() + 1
  LET o[i].name = "destination-dir"
  LET o[i].description =
      "choose another extraction directory than the current dir"
  LET o[i].opt_char = "d"
  LET o[i].arg_type = mygetopt.NONE

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
      WHEN 'q'
        LET _opt_quiet = TRUE
      WHEN 'h'
        CALL mygetopt.displayUsage(gr, "fjs-<product>.zip")
        EXIT PROGRAM 0
      WHEN 'l'
        LET listSeen=TRUE
      WHEN 'L'
        LET _opt_logfile = opt_arg
      WHEN 'F'
        LET _opt_in_FGLDIR = TRUE
      WHEN 'd'
        LET _opt_ext_dir = opt_arg
    END CASE
  END WHILE
  IF (cnt := mygetopt.getMoreArgumentCount(gr)) <> 1 THEN
    CALL mygetopt.displayUsage(gr, "fjs-<product>.zip")
    EXIT PROGRAM 1
  END IF
  DISPLAY "cnt:",cnt
  LET _product_zip = mygetopt.getMoreArgument(gr, 1)
  LET _product_zip = os.Path.fullPath(_product_zip)
  IF listSeen THEN
    DISPLAY unzipList()
    EXIT PROGRAM 0
  END IF
  IF _opt_in_FGLDIR AND _opt_ext_dir IS NOT NULL THEN
    CALL userError("option --use-FGLDIR(-F) and --destination-dir(-d) are mutually exclusive")
  END IF
END FUNCTION

FUNCTION yesno(message)
  DEFINE message STRING
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
  LET doc=om.DomDocument.create("Files")
  LET root=doc.getDocumentElement()
  LET raw=unzipList()
  LET raw=replace(raw,"\r\n","\n") --windows
  LET tok=base.StringTokenizer.create(raw,"\n")
  WHILE tok.hasMoreTokens()
    LET path=tok.nextToken() 
    IF path.getLength()>0 THEN
      CALL addFile(root,path)
    END IF
  END WHILE
  --DISPLAY "readFiles did get:",root.toString()
  RETURN root
END FUNCTION


--loop thru the path parts
FUNCTION findFileNode(parent,path,createIfNotFound)
  DEFINE path,part,tName,full STRING
  DEFINE parent,child,newchild om.DomNode
  DEFINE createIfNotFound,found BOOLEAN
  DEFINE tok base.StringTokenizer
  LET tok=base.StringTokenizer.create(path,"/")
  WHILE tok.hasMoreTokens()
    LET found=FALSE
    LET part=tok.nextToken()
    IF createIfNotFound THEN
      LET full=IIF(full IS NULL,part,sfmt("%1/%2",full,part))
    END IF
    --DISPLAY "begin handle part:",part," for parent:",parent.getTagName(),",full:",full
    LET child=parent.getFirstChild()
    WHILE child IS NOT NULL
      IF child.getAttribute("name")==part THEN
        LET found=TRUE
        --DISPLAY sfmt("found childTag:%1 for parent:%2",part,parent.getTagName())
        LET parent=child
        EXIT WHILE
      ELSE
        LET child=child.getNext()
      END IF
    END WHILE
    IF NOT found THEN
      IF NOT createIfNotFound THEN
         RETURN NULL
      END IF
      IF path.getCharAt(full.getLength()+1)=="/" THEN --this full part ends with a slash
        LET tName="Dir"
      ELSE
        LET tName="File"
      END IF
      LET newchild=parent.createChild(tName)
      CALL newchild.setAttribute("name",part)
      CALL parent.setAttribute("isDir","1")
      --DISPLAY sfmt("created newchild for tag:%1,parentName:%2",part,parent.getAttribute("name"))
      LET parent=newchild
    END IF
  END WHILE
  RETURN parent
END FUNCTION

FUNCTION addFile(parent,path)
  DEFINE parent,node om.DomNode
  DEFINE path STRING
  --DISPLAY "addFile:",path
  LET node=findFileNode(parent,path,TRUE)
  --DISPLAY "added:",node.toString()
END FUNCTION

FUNCTION fileExists(root,name)
  DEFINE root,node om.DomNode
  DEFINE name STRING
  LET node=findFileNode(root,name,FALSE)
  RETURN node IS NOT NULL
END FUNCTION

FUNCTION isDir(root,name)
  DEFINE root,node om.DomNode
  DEFINE name STRING
  LET node=findFileNode(root,name,FALSE)
  RETURN node IS NOT NULL AND node.getTagName()=="Dir"
END FUNCTION

FUNCTION analyze(root)
  DEFINE root,child,lastChild om.DomNode
  DEFINE numChildren INT
  --DEFINE children DYNAMIC ARRAY OF om.DomNode
  DEFINE defRoot STRING
  LET child=root.getFirstChild()
  WHILE child IS NOT NULL
    LET numChildren=numChildren+1
    --LET children[children.getLength()+1]=child.getAttribute("name")
    LET lastChild=child
    LET child=child.getNext()
  END WHILE
  IF _opt_ext_dir IS NOT NULL THEN
    CALL mkdirp(_opt_ext_dir)
    MYASSERT(os.Path.chDir(_opt_ext_dir)==TRUE)
  END IF
  IF numChildren==1 THEN --single root , no need to compute one
    LET defRoot=lastChild.getAttribute("name")
    IF _opt_in_FGLDIR THEN
      CALL userError("This package is not prepared to be installed over FGLDIR(yet).")
    END IF
    CALL unzip()
  ELSE
    IF _opt_in_FGLDIR THEN
      CALL unzipOverFGLDIR(root)
    ELSE
      LET defRoot=computeDefName()
      MYASSERT(os.Path.mkdir(defRoot)==1)
      MYASSERT(os.Path.chDir(defRoot)==TRUE)
      CALL unzip()
    END IF
  END IF
  DISPLAY "defRoot:",defRoot
END FUNCTION

FUNCTION unzip()
  DEFINE cmd STRING
  DEFINE code INT
  LET cmd=sfmt("tar xf %1",quote(_product_zip))
  RUN cmd RETURNING code
  IF code THEN
    EXIT PROGRAM code
  END IF
END FUNCTION

FUNCTION isGBC(root)
  DEFINE root om.DomNode
  RETURN fileExists(root,"VERSION") AND fileExists(root,"PRODUCTINFO") AND fileExists(root,"index.html") AND fileExists(root,"js/gbc.js")
END FUNCTION

FUNCTION myChdir(path)
  DEFINE path STRING
  IF NOT os.Path.chDir(path) THEN
    CALL myErr(sfmt("Can't chdir to:%1",path))
  END IF
END FUNCTION

FUNCTION getFglDir()
  RETURN base.Application.getFglDir()
END FUNCTION

FUNCTION unzipGBCoverFGLDIR()
  DEFINE gbcDir STRING
  LET gbcDir=sfmt("%1/web_utilities/gbc/gbc",getFglDir())
  CALL mkdirp(gbcDir)
  CALL myChdir(gbcDir)
  CALL unzip()
END FUNCTION

FUNCTION unzipOverFGLDIR(root)
  DEFINE root om.DomNode
  IF isGBC(root) THEN
    CALL unzipGBCoverFGLDIR()
  ELSE
    CALL myChdir(getFglDir())
    CALL generateUndoScript(root) 
    CALL unzip()
  END IF
END FUNCTION

FUNCTION generateUndoScript(root)
  DEFINE root om.DomNode
  DEFINE ch_sh,ch_bat base.Channel
  LET ch_sh=base.Channel.create()
  LET ch_bat=base.Channel.create()
  CALL ch_sh.openFile("rm-%1.sh",computeDefName())
  CALL ch_sh.writeLine("#!/bin/bash")
  CALL ch_bat.openFile("rm-%1.bat",computeDefName())
  CALL ch_bat.writeLine("@echo off")
  CALL add_rm(root,".",ch_sh,ch_bat)
END FUNCTION

FUNCTION add_rm(parent,parentDir,ch_sh,ch_bat)
  DEFINE parent,child om.DomNode
  DEFINE parentDir,path,winpath STRING
  DEFINE ch_sh,ch_bat base.Channel
  LET child=parent.getFirstChild()
  WHILE child IS NOT NULL
    LET path=sfmt("%1/%2",parentDir,child.getAttribute("name"))
    LET winpath=replace(path,"/","\\")
    IF child.getTagName()=="File" THEN
      CALL ch_sh.writeLine(sfmt("rm -f %1",quote(path)))
      CALL ch_bat.writeLine(sfmt("del /Q %1",quote(winpath)))
    END IF
    CALL add_rm(child,path,ch_sh,ch_bat)
    IF child.getTagName()=="Dir" THEN
      CALL ch_sh.writeLine(sfmt("rmdir %1",quote(path)));
      CALL ch_bat.writeLine(sfmt("rmdir %1",quote(winpath)));
    END IF
    LET child=child.getNext()
  END WHILE
END FUNCTION

#+ for zip archives not having a single root we create a root dir named after the product file name (similar to what desktop extraction tools do)
FUNCTION computeDefName()
  DEFINE def,b STRING
  DEFINE idx1,idx2 INT
  LET b=os.Path.baseName(_product_zip)
  IF b.getIndexOf("fjs-",1)==1 THEN
    LET idx1=b.getIndexOf("-",5)
    MYASSERT(idx1!=0)
    LET idx2=b.getIndexOf("-",idx1+1)
    LET def=b.subString(5,idx2-1)
  ELSE
    LET def=cutExtension(b)
  END IF
  DISPLAY "defname:",def
  RETURN def
END FUNCTION
