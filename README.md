# fglunzip: 
unzips Genero zip packages

# Motivation
standard unzip does clutter the current directory with several directories/files if the Genero zip package doesn't contain a single root directory.
For the novice/unexperienced command line user it is hard to clean up, there is no undo command in standard unzip for that.

fglunzip has the following features
1. creates by default a single root directory in the current directory if there is no single root in the zip. The single root directory is based on the zip file name. This is similar to what windows explorer does but in a smarter way: 
If there is already a single root directory in the zip file: no additional root directory is created by fglunzip.
2. has a `--undo` (`-u`) command line switch to revert the extraction
3. has a `--simulate` (`-s`) command line switch to dry run the extraction/undo
4. has a `--use-FGLDIR`(`-F`) switch to install packages *without* a single root directory and which are intended/reported to work inside FGLDIR (such as GMI and GWA and the GBC runtime package)

# Installation

You don't need to install fglunzip.
If you did check out this repository you can call
```
$ <path_to_this_repository>/fglunzip ?options? fjs-<product>.zip
```

Windows
```
C:> <path_to_this_repository>\fglunzip ?options? fjs-<product>.zip
```

or extend `PATH` to include this repository.

Furthermore you can download fglunzip as a single self extracting/self compiling shell script: [fglunzip](dist/fglunzip?raw=1)(Download Linked file As...)
or download as self extracting/self compiling batch file for Windows:[fglunzip.bat](dist/fglunzip.bat)(Download Linked file As...). You should place it in your `$HOME/bin` or `$FGLDIR/bin` (somewhere in your `PATH`).

Both files were produced using the [fglscriptify](https://github.com/leopatras/fglscriptify) Genero tool.



# Options
```
./fglunzip --help
Usage: fglunzip [options] fjs-<product>.zip

Options:
    -V, --version             Version information
    -h, --help                program help
    -v, --verbose             detailed log
    -s, --simulate            simulates what would be extracted/deleted
    -l, --list                Lists the archive content
    -F, --use-FGLDIR          installs over FGLDIR to make the product avaiable without further env settings
    -i, --like-unzip          extracts multiple files/directories in the root directory of the archive like unzip 'plain' in the current directory
    -u, --undo                Reverts the install
```

# Examples
```
fglunzip fjs-gmi-4.01.05-build_c9a2caf-m64x1014.zip
```
extracts gmi in the sub directory "gmi-4.01.05" (omitting `fjs-` and the build suffix from the .zip)
```
zip somezip.zip a.txt b.txt
fglunzip somezip.zip
```
extracts `a.txt` and `b.txt` in the sub directory `somezip` (if the zip has no `fjs-` prefix the whole base name is taken as root directory name)
```
zip -r somezip.zip subdir/
fglunzip somezip.zip
```
extracts with `subdir` as root directory and does *not* add a `somezip` directory like windows explorer would do.
```
fglunzip -F fjs-gmi-4.01.05-build_c9a2caf-m64x1014.zip
```
extracts GMI over `FGLDIR` in `$FGLDIR/bin` , `$FGLDIR/lib` etc. After this operation the gmibuildtool command is immediately available (because FGLDIR/bin is already in the PATH)
```
fglunzip -Fs fjs-gmi-4.01.05-build_c9a2caf-m64x1014.zip
```
would only print which files would be extracted in $FGLDIR and indicate which files would be written newly and which files would be overwritten.
```
fglunzip -ui fjs-gmi-4.01.05-build_c9a2caf-m64x1014.zip
```
removes all content added from unzipping with `unzip fjs-gmi-4.01.05-build_c9a2caf-m64x1014.zip` in the current directory. This avoids a lot of headache after unzipping a rootless package!

```
fglunzip -Fuv fjs-gmi-4.01.05-build_c9a2caf-m64x1014.zip
```
removes all content added from unzipping with `fglunzip -F fjs-gmi-4.01.05-build_c9a2caf-m64x1014.zip` in FGLDIR and print which files were actually deleted.
```
fglunzip -Fus fjs-gmi-4.01.05-build_c9a2caf-m64x1014.zip
```
prints which files would be removed from FGLDIR without actually removing them.
