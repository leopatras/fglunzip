IMPORT os
CONSTANT FGLUTILS_VERSION_INC = "fglunzip_version.inc"
DEFINE git_version STRING
DEFINE git_commit_count STRING
DEFINE git_rev STRING
DEFINE git_fgl_version STRING
DEFINE git_fgl_commit_count STRING
DEFINE git_fgl_rev STRING
MAIN
  CALL parseVersion(arg_val(1), FALSE)
  {
  VAR fglver=arg_val(2)
  VAR firstChar=fglver.getCharAt(1)
  IF firstChar<>"v" OR NOT isNumber(firstChar) THEN
    LET fglver=arg_val(3)
  END IF
  CALL parseVersion(fglver, TRUE)
  }
  VAR versionContent = SFMT("%1", formatContent())
  IF NOT os.Path.exists(FGLUTILS_VERSION_INC) THEN
    DISPLAY FGLUTILS_VERSION_INC, " not found: create."
    CALL writeStringToFile(FGLUTILS_VERSION_INC, versionContent)
  ELSE
    VAR prevContent = readTextFile(FGLUTILS_VERSION_INC)
    IF NOT versionContent.equals(prevContent) THEN
      DISPLAY FGLUTILS_VERSION_INC, " overwrite with new version:\n", versionContent
      CALL writeStringToFile(FGLUTILS_VERSION_INC, versionContent)
    ELSE
      DISPLAY FGLUTILS_VERSION_INC, " good version."
    END IF
  END IF
END MAIN

FUNCTION isNumber(c STRING)
  VAR numbers="0123456789"
  RETURN numbers.getIndexOf(c,1)<>0
END FUNCTION

FUNCTION parseVersion(longver STRING, fgl BOOLEAN)
  VAR i = 1
  VAR tok = base.StringTokenizer.create(longver, "-")
  WHILE tok.hasMoreTokens()
    VAR nextToken = tok.nextToken()
    CASE i
      WHEN 1
        IF nextToken.getIndexOf("v", 1) == 1 THEN
          LET nextToken = nextToken.subString(2, nextToken.getLength())
        END IF
        IF fgl THEN
          LET git_fgl_version = nextToken
        ELSE
          LET git_version = nextToken
        END IF
      WHEN 2
        IF fgl THEN
          LET git_fgl_commit_count = nextToken
        ELSE
          LET git_commit_count = nextToken
        END IF
      WHEN 3
        IF fgl THEN
          LET git_fgl_rev = nextToken
        ELSE
          LET git_rev = nextToken
        END IF
    END CASE
    LET i = i + 1
  END WHILE
END FUNCTION

FUNCTION readTextFile(filename) RETURNS STRING
  DEFINE filename STRING
  DEFINE content STRING
  DEFINE t TEXT
  LOCATE t IN FILE filename
  LET content = t
  RETURN content
END FUNCTION

FUNCTION writeStringToFile(file STRING, content STRING)
  DEFINE ch base.Channel
  LET ch = base.Channel.create()
  CALL ch.openFile(file, "w")
  CALL ch.writeNoNL(content)
  CALL ch.close()
END FUNCTION

FUNCTION formatContent() RETURNS STRING
  RETURN SFMT(`public CONSTANT GIT_VERSION="%1"
public CONSTANT GIT_COMMIT_COUNT=%2
public CONSTANT GIT_REV="%3"
`,
      git_version, git_commit_count, git_rev)
END FUNCTION

FUNCTION formatFGLContent() RETURNS STRING
  RETURN SFMT(`public CONSTANT GIT_FGL_VERSION="%1"
public CONSTANT GIT_FGL_COMMIT_COUNT=%2
public CONSTANT GIT_FGL_REV="%3"
`,
      git_fgl_version, git_fgl_commit_count, git_fgl_rev)
END FUNCTION
