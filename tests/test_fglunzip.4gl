OPTIONS
SHORT CIRCUIT
IMPORT os
IMPORT FGL fglunzip

DEFINE _pass INT
DEFINE _fail INT
DEFINE _fglunzip_path STRING -- path to the fglunzip shell script
DEFINE _work_dir STRING
DEFINE _current_test STRING
DEFINE _test_ok BOOLEAN

MAIN
  CALL setup()
  CALL run_all_tests()
  CALL teardown()
  CALL report()
END MAIN

-- ============================================================
-- Test framework
-- ============================================================

FUNCTION setup()
  LET _fglunzip_path =
      os.Path.fullPath(
          os.Path.join(
              os.Path.join(base.Application.getProgramDir(), ".."), "fglunzip"))
  IF fglunzip.isWin() THEN
    LET _work_dir =
        os.Path.join(
            fgl_getenv("TEMP"), SFMT("fglunzip_tests_%1", fgl_getpid()))
  ELSE
    LET _work_dir = SFMT("/tmp/fglunzip_tests_%1", fgl_getpid())
  END IF
  DISPLAY "run tests in:", _work_dir
  CALL fglunzip.mkdirp(_work_dir)
  IF NOT isWin() THEN
    CALL fgl_setenv("LC_ALL", "en_US.UTF-8")
  END IF
  CALL fgl_setenv("FGLGUI", "0")
  DISPLAY SFMT("fglunzip: %1", _fglunzip_path)
  DISPLAY ""
END FUNCTION

FUNCTION teardown()
  IF fglunzip.isWin() THEN
    RUN SFMT("rmdir /s /q \"%1\"", _work_dir)
  ELSE
    RUN SFMT("rm -rf '%1'", _work_dir)
  END IF
END FUNCTION

FUNCTION report()
  DISPLAY ""
  DISPLAY SFMT("Results: %1 passed, %2 failed", _pass, _fail)
  IF _fail > 0 THEN
    EXIT PROGRAM 1
  END IF
END FUNCTION

FUNCTION begin_test(name STRING)
  LET _current_test = name
  LET _test_ok = TRUE
END FUNCTION

FUNCTION end_test()
  IF _test_ok THEN
    DISPLAY SFMT("PASS  %1", _current_test)
    LET _pass = _pass + 1
  ELSE
    LET _fail = _fail + 1
  END IF
END FUNCTION

-- Assert condition is true; on failure mark the test failed and print msg.
FUNCTION check(cond BOOLEAN, msg STRING)
  IF NOT NVL(cond, FALSE) THEN
    IF _test_ok THEN
      DISPLAY SFMT("FAIL  %1", _current_test)
    END IF
    DISPLAY SFMT("      %1", msg)
    LET _test_ok = FALSE
  END IF
END FUNCTION

-- ============================================================
-- Helpers
-- ============================================================

-- Quote a value for shell inclusion.
-- cmd.exe uses double quotes; Unix shells use single quotes.
FUNCTION qt(s STRING) RETURNS STRING
  IF fglunzip.isWin() THEN
    RETURN SFMT("\"%1\"", s)
  ELSE
    RETURN SFMT("'%1'", s)
  END IF
END FUNCTION

-- Return TRUE if output contains needle.
FUNCTION has(out STRING, needle STRING) RETURNS BOOLEAN
  RETURN out.getIndexOf(needle, 1) > 0
END FUNCTION

-- Run the fglunzip script from workdir with the given argument string.
-- Returns ok=TRUE when exit code is 0, and the captured stdout+stderr.
FUNCTION run_fglunzip(workdir STRING, args STRING) RETURNS(BOOLEAN, STRING)
  DEFINE output, err STRING
  DEFINE cmd STRING
  -- "cd /d" changes both directory and drive on Windows cmd.exe.
  IF fglunzip.isWin() THEN
    LET cmd = SFMT("cd /d %1 && %2 %3", qt(workdir), qt(_fglunzip_path), args)
  ELSE
    LET cmd = SFMT("cd %1 && %2 %3", qt(workdir), qt(_fglunzip_path), args)
  END IF
  CALL fglunzip.getProgramOutputWithErr(cmd) RETURNING output, err
  RETURN (err IS NULL), NVL(output, "")
END FUNCTION

-- Create a zip with multiple top-level entries: a.txt, b.txt, subdir/c.txt.
-- Returns the path to the created .zip file.
FUNCTION make_rootless_zip(td STRING, zipname STRING) RETURNS STRING
  DEFINE zipfile STRING
  DEFINE srcdir STRING
  LET zipfile = os.Path.join(td, zipname || ".zip")
  LET srcdir = os.Path.join(td, "src_" || zipname)
  CALL fglunzip.mkdirp(os.Path.join(srcdir, "subdir"))
  CALL fglunzip.writeStringToFile(os.Path.join(srcdir, "a.txt"), "hello")
  CALL fglunzip.writeStringToFile(os.Path.join(srcdir, "b.txt"), "world")
  CALL fglunzip.writeStringToFile(
      os.Path.join(os.Path.join(srcdir, "subdir"), "c.txt"), "nested")
  -- Windows 10+ tar supports zip creation via -a (auto-format from extension).
  IF fglunzip.isWin() THEN
    RUN SFMT("cd /d %1 && tar -a -c -f %2 a.txt b.txt subdir >NUL 2>&1",
        qt(srcdir), qt(zipfile))
  ELSE
    RUN SFMT("cd %1 && zip -r %2 a.txt b.txt subdir/ >/dev/null 2>&1",
        qt(srcdir), qt(zipfile))
  END IF
  RETURN zipfile
END FUNCTION

-- Create a zip whose only top-level entry is a directory named rootname.
-- Returns the path to the created .zip file.
FUNCTION make_singleroot_zip(
    td STRING, zipname STRING, rootname STRING)
    RETURNS STRING
  DEFINE zipfile STRING
  DEFINE srcdir, rootdir STRING
  LET zipfile = os.Path.join(td, zipname || ".zip")
  LET srcdir = os.Path.join(td, "src_" || zipname)
  LET rootdir = os.Path.join(srcdir, rootname)
  CALL fglunzip.mkdirp(os.Path.join(rootdir, "bin"))
  CALL fglunzip.writeStringToFile(os.Path.join(rootdir, "README.txt"), "readme")
  CALL fglunzip.writeStringToFile(
      os.Path.join(os.Path.join(rootdir, "bin"), "tool"), "exe")
  IF fglunzip.isWin() THEN
    RUN SFMT("cd /d %1 && tar -a -c -f %2 %3 >NUL 2>&1",
        qt(srcdir), qt(zipfile), rootname)
  ELSE
    RUN SFMT("cd %1 && zip -r %2 %3/ >/dev/null 2>&1",
        qt(srcdir), qt(zipfile), rootname)
  END IF
  RETURN zipfile
END FUNCTION

-- ============================================================
-- Test dispatcher
-- ============================================================

FUNCTION run_all_tests()
  -- help / version / argument validation
  CALL t_help_exits_zero()
  CALL t_help_shows_usage()
  CALL t_version_exits_zero()
  CALL t_version_shows_program_name()
  CALL t_no_args_exits_nonzero()
  CALL t_nonexistent_zip_exits_nonzero()
  -- --list (-l)
  CALL t_list_exits_zero()
  CALL t_list_shows_filenames()
  CALL t_list_does_not_extract()
  -- default extraction: rootless zip
  CALL t_rootless_creates_root_dir()
  CALL t_rootless_files_inside_root()
  -- single-root zip
  CALL t_singleroot_no_extra_dir()
  CALL t_singleroot_files_correct()
  -- fjs- prefix naming
  CALL t_fjs_prefix_naming()
  -- --simulate (-s)
  CALL t_simulate_exits_zero()
  CALL t_simulate_shows_would_extract()
  CALL t_simulate_no_files_extracted()
  -- --undo (-u)
  CALL t_undo_removes_extracted_root()
  CALL t_undo_nothing_to_undo()
  CALL t_undo_simulate_shows_would_remove()
  -- --like-unzip (-i)
  CALL t_like_unzip_files_in_cwd()
  CALL t_like_unzip_no_root_dir_added()
  -- --verbose (-v) / --quiet (-q)
  CALL t_verbose_shows_unzip_cmd()
  CALL t_quiet_suppresses_verified()
END FUNCTION

-- ============================================================
-- Individual tests
-- ============================================================

FUNCTION t_help_exits_zero()
  DEFINE ok BOOLEAN
  DEFINE output STRING
  CALL begin_test("help exits zero")
  CALL run_fglunzip(_work_dir, "--help") RETURNING ok, output
  CALL check(ok, "expected exit 0")
  CALL end_test()
END FUNCTION

FUNCTION t_help_shows_usage()
  DEFINE ok BOOLEAN
  DEFINE output STRING
  CALL begin_test("help shows 'Usage:'")
  CALL run_fglunzip(_work_dir, "--help") RETURNING ok, output
  CALL check(has(output, "Usage:"), "output does not contain 'Usage:'")
  CALL end_test()
END FUNCTION

FUNCTION t_version_exits_zero()
  DEFINE ok BOOLEAN
  DEFINE output STRING
  CALL begin_test("version exits zero")
  CALL run_fglunzip(_work_dir, "--version") RETURNING ok, output
  CALL check(ok, "expected exit 0")
  CALL end_test()
END FUNCTION

FUNCTION t_version_shows_program_name()
  DEFINE ok BOOLEAN
  DEFINE output STRING
  CALL begin_test("version shows program name")
  CALL run_fglunzip(_work_dir, "--version") RETURNING ok, output
  CALL check(has(output, "fglunzip"), "output does not contain 'fglunzip'")
  CALL end_test()
END FUNCTION

FUNCTION t_no_args_exits_nonzero()
  DEFINE ok BOOLEAN
  DEFINE output STRING
  CALL begin_test("no args exits nonzero")
  CALL run_fglunzip(_work_dir, "") RETURNING ok, output
  CALL check(NOT ok, "expected nonzero exit for missing zip argument")
  CALL end_test()
END FUNCTION

FUNCTION t_nonexistent_zip_exits_nonzero()
  DEFINE ok BOOLEAN
  DEFINE output STRING
  CALL begin_test("nonexistent zip exits nonzero")
  CALL run_fglunzip(_work_dir, "/nonexistent/file.zip") RETURNING ok, output
  CALL check(NOT ok, "expected nonzero exit for nonexistent zip")
  CALL end_test()
END FUNCTION

FUNCTION t_list_exits_zero()
  DEFINE td STRING
  DEFINE zipfile STRING
  DEFINE ok BOOLEAN
  DEFINE output STRING
  CALL begin_test("list exits zero")
  LET td = os.Path.join(_work_dir, "list_exit")
  CALL fglunzip.mkdirp(td)
  LET zipfile = make_rootless_zip(td, "test")
  CALL run_fglunzip(td, SFMT("-l %1", qt(zipfile))) RETURNING ok, output
  CALL check(ok, "expected exit 0")
  CALL end_test()
END FUNCTION

FUNCTION t_list_shows_filenames()
  DEFINE td STRING
  DEFINE zipfile STRING
  DEFINE ok BOOLEAN
  DEFINE output STRING
  CALL begin_test("list shows filenames")
  LET td = os.Path.join(_work_dir, "list_names")
  CALL fglunzip.mkdirp(td)
  LET zipfile = make_rootless_zip(td, "test")
  CALL run_fglunzip(td, SFMT("-l %1", qt(zipfile))) RETURNING ok, output
  CALL check(has(output, "a.txt"), "output does not contain 'a.txt'")
  CALL end_test()
END FUNCTION

FUNCTION t_list_does_not_extract()
  DEFINE td, out STRING
  DEFINE zipfile STRING
  DEFINE ok BOOLEAN
  DEFINE output STRING
  CALL begin_test("list does not extract")
  LET td = os.Path.join(_work_dir, "list_noextract")
  LET out = os.Path.join(td, "out")
  CALL fglunzip.mkdirp(out)
  LET zipfile = make_rootless_zip(td, "test")
  CALL run_fglunzip(out, SFMT("-l %1", qt(zipfile))) RETURNING ok, output
  CALL check(
      NOT os.Path.exists(os.Path.join(out, "test")),
      "extraction dir was created during list")
  CALL end_test()
END FUNCTION

FUNCTION t_rootless_creates_root_dir()
  DEFINE td, out STRING
  DEFINE zipfile STRING
  DEFINE ok BOOLEAN
  DEFINE output STRING
  CALL begin_test("rootless zip: creates root dir named after zip")
  LET td = os.Path.join(_work_dir, "rootless_dir")
  LET out = os.Path.join(td, "out")
  CALL fglunzip.mkdirp(out)
  LET zipfile = make_rootless_zip(td, "mypackage")
  CALL run_fglunzip(out, SFMT("-q %1", qt(zipfile))) RETURNING ok, output
  CALL check(ok, "fglunzip failed")
  CALL check(
      os.Path.isDirectory(os.Path.join(out, "mypackage")),
      "root dir 'mypackage' was not created")
  CALL end_test()
END FUNCTION

FUNCTION t_rootless_files_inside_root()
  DEFINE td, out, pkg STRING
  DEFINE zipfile STRING
  DEFINE ok BOOLEAN
  DEFINE output STRING
  CALL begin_test("rootless zip: files inside root dir")
  LET td = os.Path.join(_work_dir, "rootless_files")
  LET out = os.Path.join(td, "out")
  CALL fglunzip.mkdirp(out)
  LET zipfile = make_rootless_zip(td, "mypackage")
  CALL run_fglunzip(out, SFMT("-q %1", qt(zipfile))) RETURNING ok, output
  LET pkg = os.Path.join(out, "mypackage")
  CALL check(
      os.Path.isFile(os.Path.join(pkg, "a.txt")), "mypackage/a.txt not found")
  CALL check(
      os.Path.isFile(os.Path.join(pkg, "b.txt")), "mypackage/b.txt not found")
  CALL check(
      os.Path.isFile(os.Path.join(os.Path.join(pkg, "subdir"), "c.txt")),
      "mypackage/subdir/c.txt not found")
  CALL end_test()
END FUNCTION

FUNCTION t_singleroot_no_extra_dir()
  DEFINE td, out STRING
  DEFINE zipfile STRING
  DEFINE ok BOOLEAN
  DEFINE output STRING
  CALL begin_test("single-root zip: no extra dir added")
  LET td = os.Path.join(_work_dir, "singleroot_noextra")
  LET out = os.Path.join(td, "out")
  CALL fglunzip.mkdirp(out)
  LET zipfile = make_singleroot_zip(td, "product", "myroot")
  CALL run_fglunzip(out, SFMT("-q %1", qt(zipfile))) RETURNING ok, output
  CALL check(ok, "fglunzip failed")
  CALL check(
      os.Path.isDirectory(os.Path.join(out, "myroot")),
      "root dir 'myroot' not found")
  CALL check(
      NOT os.Path.exists(os.Path.join(out, "product")),
      "extra dir 'product' was added but should not have been")
  CALL end_test()
END FUNCTION

FUNCTION t_singleroot_files_correct()
  DEFINE td, out, root STRING
  DEFINE zipfile STRING
  DEFINE ok BOOLEAN
  DEFINE output STRING
  CALL begin_test("single-root zip: files extracted correctly")
  LET td = os.Path.join(_work_dir, "singleroot_files")
  LET out = os.Path.join(td, "out")
  CALL fglunzip.mkdirp(out)
  LET zipfile = make_singleroot_zip(td, "product", "myroot")
  CALL run_fglunzip(out, SFMT("-q %1", qt(zipfile))) RETURNING ok, output
  LET root = os.Path.join(out, "myroot")
  CALL check(
      os.Path.isFile(os.Path.join(root, "README.txt")),
      "myroot/README.txt not found")
  CALL check(
      os.Path.isFile(os.Path.join(os.Path.join(root, "bin"), "tool")),
      "myroot/bin/tool not found")
  CALL end_test()
END FUNCTION

FUNCTION t_fjs_prefix_naming()
  DEFINE td, out STRING
  DEFINE zipfile STRING
  DEFINE ok BOOLEAN
  DEFINE output STRING
  CALL begin_test("fjs- prefix: root dir derived from product name")
  LET td = os.Path.join(_work_dir, "fjs_naming")
  LET out = os.Path.join(td, "out")
  CALL fglunzip.mkdirp(out)
  LET zipfile = make_rootless_zip(td, "fjs-gmi-4.01.05-build_c9a2caf-m64x1014")
  CALL run_fglunzip(out, SFMT("-q %1", qt(zipfile))) RETURNING ok, output
  CALL check(ok, "fglunzip failed")
  CALL check(
      os.Path.isDirectory(os.Path.join(out, "gmi-4.01.05")),
      "expected root dir 'gmi-4.01.05' was not created")
  CALL end_test()
END FUNCTION

FUNCTION t_simulate_exits_zero()
  DEFINE td, out STRING
  DEFINE zipfile STRING
  DEFINE ok BOOLEAN
  DEFINE output STRING
  CALL begin_test("simulate exits zero")
  LET td = os.Path.join(_work_dir, "sim_exit")
  LET out = os.Path.join(td, "out")
  CALL fglunzip.mkdirp(out)
  LET zipfile = make_rootless_zip(td, "mypackage")
  CALL run_fglunzip(out, SFMT("-qs %1", qt(zipfile))) RETURNING ok, output
  CALL check(ok, "expected exit 0")
  CALL end_test()
END FUNCTION

FUNCTION t_simulate_shows_would_extract()
  DEFINE td, out STRING
  DEFINE zipfile STRING
  DEFINE ok BOOLEAN
  DEFINE output STRING
  CALL begin_test("simulate: output contains 'Would extract'")
  LET td = os.Path.join(_work_dir, "sim_output")
  LET out = os.Path.join(td, "out")
  CALL fglunzip.mkdirp(out)
  LET zipfile = make_rootless_zip(td, "mypackage")
  CALL run_fglunzip(out, SFMT("-s %1", qt(zipfile))) RETURNING ok, output
  CALL check(
      has(output, "Would extract"), "output does not contain 'Would extract'")
  CALL end_test()
END FUNCTION

FUNCTION t_simulate_no_files_extracted()
  DEFINE td, out STRING
  DEFINE zipfile STRING
  DEFINE ok BOOLEAN
  DEFINE output STRING
  CALL begin_test("simulate: no files written to disk")
  LET td = os.Path.join(_work_dir, "sim_nofiles")
  LET out = os.Path.join(td, "out")
  CALL fglunzip.mkdirp(out)
  LET zipfile = make_rootless_zip(td, "mypackage")
  CALL run_fglunzip(out, SFMT("-qs %1", qt(zipfile))) RETURNING ok, output
  CALL check(
      NOT os.Path.isFile(os.Path.join(os.Path.join(out, "mypackage"), "a.txt")),
      "a.txt was written to disk during simulate")
  CALL end_test()
END FUNCTION

FUNCTION t_undo_removes_extracted_root()
  DEFINE td, out STRING
  DEFINE zipfile STRING
  DEFINE ok BOOLEAN
  DEFINE output STRING
  CALL begin_test("undo: removes extracted root dir")
  LET td = os.Path.join(_work_dir, "undo_removes")
  LET out = os.Path.join(td, "out")
  CALL fglunzip.mkdirp(out)
  LET zipfile = make_rootless_zip(td, "mypackage")
  CALL run_fglunzip(out, SFMT("-q %1", qt(zipfile))) RETURNING ok, output
  CALL check(ok, "extract failed")
  CALL check(
      os.Path.isDirectory(os.Path.join(out, "mypackage")),
      "extract did not create 'mypackage'")
  CALL run_fglunzip(out, SFMT("-qu %1", qt(zipfile))) RETURNING ok, output
  CALL check(ok, "undo failed")
  CALL check(
      NOT os.Path.exists(os.Path.join(out, "mypackage")),
      "undo did not remove 'mypackage'")
  CALL end_test()
END FUNCTION

FUNCTION t_undo_nothing_to_undo()
  DEFINE td, out STRING
  DEFINE zipfile STRING
  DEFINE ok BOOLEAN
  DEFINE output STRING
  CALL begin_test("undo: 'Nothing to undo' when not extracted")
  LET td = os.Path.join(_work_dir, "undo_nothing")
  LET out = os.Path.join(td, "out")
  CALL fglunzip.mkdirp(out)
  LET zipfile = make_rootless_zip(td, "mypackage")
  CALL run_fglunzip(out, SFMT("-u %1", qt(zipfile))) RETURNING ok, output
  CALL check(ok, "expected exit 0")
  CALL check(
      has(output, "Nothing to undo"),
      "output does not contain 'Nothing to undo'")
  CALL end_test()
END FUNCTION

FUNCTION t_undo_simulate_shows_would_remove()
  DEFINE td, out STRING
  DEFINE zipfile STRING
  DEFINE ok BOOLEAN
  DEFINE output STRING
  CALL begin_test("undo simulate: output contains 'Would remove'")
  LET td = os.Path.join(_work_dir, "undo_sim")
  LET out = os.Path.join(td, "out")
  CALL fglunzip.mkdirp(out)
  LET zipfile = make_rootless_zip(td, "mypackage")
  CALL run_fglunzip(out, SFMT("-q %1", qt(zipfile))) RETURNING ok, output
  CALL check(ok, "extract failed")
  CALL run_fglunzip(out, SFMT("-su %1", qt(zipfile))) RETURNING ok, output
  CALL check(
      has(output, "Would remove"), "output does not contain 'Would remove'")
  CALL end_test()
END FUNCTION

FUNCTION t_like_unzip_files_in_cwd()
  DEFINE td, out STRING
  DEFINE zipfile STRING
  DEFINE ok BOOLEAN
  DEFINE output STRING
  CALL begin_test("-i (like-unzip): files extracted directly into cwd")
  LET td = os.Path.join(_work_dir, "like_unzip_files")
  LET out = os.Path.join(td, "out")
  CALL fglunzip.mkdirp(out)
  LET zipfile = make_rootless_zip(td, "mypackage")
  CALL run_fglunzip(out, SFMT("-q -o -i %1", qt(zipfile))) RETURNING ok, output
  CALL check(ok, "fglunzip failed")
  CALL check(
      os.Path.isFile(os.Path.join(out, "a.txt")),
      "a.txt not found directly in extraction dir")
  CALL end_test()
END FUNCTION

FUNCTION t_like_unzip_no_root_dir_added()
  DEFINE td, out STRING
  DEFINE zipfile STRING
  DEFINE ok BOOLEAN
  DEFINE output STRING
  CALL begin_test("-i (like-unzip): no auto root dir created")
  LET td = os.Path.join(_work_dir, "like_unzip_noroot")
  LET out = os.Path.join(td, "out")
  CALL fglunzip.mkdirp(out)
  LET zipfile = make_rootless_zip(td, "mypackage")
  CALL run_fglunzip(out, SFMT("-q -o -i %1", qt(zipfile))) RETURNING ok, output
  CALL check(
      NOT os.Path.exists(os.Path.join(out, "mypackage")),
      "auto root dir 'mypackage' was created but should not have been")
  CALL end_test()
END FUNCTION

FUNCTION t_verbose_shows_unzip_cmd()
  DEFINE td, out STRING
  DEFINE zipfile STRING
  DEFINE ok BOOLEAN
  DEFINE output STRING
  CALL begin_test("verbose: output contains 'unzip cmd:'")
  LET td = os.Path.join(_work_dir, "verbose")
  LET out = os.Path.join(td, "out")
  CALL fglunzip.mkdirp(out)
  LET zipfile = make_rootless_zip(td, "mypackage")
  CALL run_fglunzip(out, SFMT("-v %1", qt(zipfile))) RETURNING ok, output
  CALL check(has(output, "unzip cmd:"), "output does not contain 'unzip cmd:'")
  CALL end_test()
END FUNCTION

FUNCTION t_quiet_suppresses_verified()
  DEFINE td, out STRING
  DEFINE zipfile STRING
  DEFINE ok BOOLEAN
  DEFINE output STRING
  CALL begin_test("quiet: 'verified:' lines suppressed")
  LET td = os.Path.join(_work_dir, "quiet")
  LET out = os.Path.join(td, "out")
  CALL fglunzip.mkdirp(out)
  LET zipfile = make_rootless_zip(td, "mypackage")
  CALL run_fglunzip(out, SFMT("-q %1", qt(zipfile))) RETURNING ok, output
  CALL check(
      NOT has(output, "verified:"),
      "output contains 'verified:' even in quiet mode")
  CALL end_test()
END FUNCTION
