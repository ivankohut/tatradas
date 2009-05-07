#!/bin/bash

# Free Pascal/Lazarus generated files
find ./ -name "*.ppu" -print | xargs /bin/rm -f
find ./ -name "*.o" -print | xargs /bin/rm -f
find ./ -name "*.s" -print | xargs /bin/rm -f
find ./ -name "*.bak" -print | xargs /bin/rm -f
find ./ -name "*.a" -print | xargs /bin/rm -f
find ./ -name "*.rst" -print | xargs /bin/rm -f

# Delphi generated files
find ./ -name "*.dcu" -print | xargs /bin/rm -f
find ./ -name "*.ddp" -print | xargs /bin/rm -f
find ./ -name "*.~*" -print | xargs /bin/rm -f

find ./ -name "*.exe" -print | xargs /bin/rm -f
find ./ -name "*.EXE" -print | xargs /bin/rm -f

find ./ -name "*.*~" -print | xargs /bin/rm -f
find ./ -name "*.das" -print | xargs /bin/rm -f
find ./ -name "*.DAS" -print | xargs /bin/rm -f

