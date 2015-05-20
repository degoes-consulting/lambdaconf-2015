# Preparing for the "Introduction to Constraint Logic Programming" workshop

## Install ECLiPSe CLP

### For Linux and MS Windows

Install version 6.1 #201 from http://eclipseclp.org/Distribution/6.1_201/

Use i386_linux or x86_64_linux/ for 32-bit or 64-bit Linux binaries, and i386_nt/ or x86_64_nt/ for Windows binaries.
Or, if you adventurous, install from source.

### For MacOS X

There are MacOS binaries for version 6.1 #164 at http://eclipseclp.org/Distribution/6.1_164/x86_64_macosx/

If you adventurous, you can try to install newer version from source.

## Verify installation

Using the command line run the following command in the directory with this README.md and verify-installation.ecl files:

    $ eclipse -b verify-installation.ecl -e main

The output should be:

    Looks good!

## Learn some Prolog

Prolog experience isn't assumed for the workshop, but would be helpful.
For a quick introduction look through "Beginning Prolog: Basics" section of http://en.wikibooks.org/wiki/Prolog
