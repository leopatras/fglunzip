# fglunzip: 
unzips Genero zip packages

# Motivation
standard unzip does clutter the current directory with several directories/files if the Genero zip package doesn't contain a single root directory.
For the novice/unexperienced command line user it is hard to clean up, there is no undo command in standard unzip for that.

fglunzip has the following features
1. creates a single root directory in the current directory by default if there is no single root in the zip. The single root directory is based on the zip file name.
If there is already a single root directory in the zip file: no additional root directory is created by fglunzip.
2. has a --undo(-u) command line switch to revert the extraction
3. has a --simulate(-s) command line switch to dry run the extraction/undo
4. has a --use-FGLDIR(-F) switch to install packages *without* a single root directory and are intended/reported to work inside FGLDIR (such as GMI and GWA and the GBC runtime package)

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
