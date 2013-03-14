#!/bin/bash
for i in `find ~/.wine/drive_c/ghc/ghc-7.0.4/ -iname *.dll`; do ln -s "$i" "/home/andrew/.wine/drive_c/windows/system32/`basename $i`"; done
for i in /home/andrew/.wine/drive_c/pf/Haskell/*/*/*.dll; do ln -s "$i" "/home/andrew/.wine/drive_c/windows/system32/`basename $i`"; done
