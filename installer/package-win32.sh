#!/bin/bash

WINROOT=/tmp/lanthaps_package/win32_root
rm -fr $WINROOT
mkdir -p $WINROOT
cp -R icons $WINROOT
cp -R sounds $WINROOT
cp ./COPYING $WINROOT/COPYING
./testscripts/install-head-win32-devbuild.sh
echo "ProdTimer will run - browse UI and then exit it to build file list"
../standalone_builder/standalone_builder "./prodtimer.exe" $WINROOT
rm $WINROOT/*.so*
for i in advapi32.dll gdiplus.dll oleaut32.dll usp10.dll uxtheme.dll comctl32.dll   imm32.dll localspl.dll rpcrt4.dll version.dll comdlg32.dll msimg32.dll shell32.dll windowscodecs.dll winmm.dll msacm32.dll mmdevapi.dll dnsapi.dll iphlpapi.dll msvcrt.dll shlwapi.dll ws2_32.dll netapi32.dll spoolss.dll wsock32.dll gdi32.dll ole32.dll user32.dll OpenAL32.dll; do
  rm $WINROOT/$i
done
cp ./dist/build/prodtimer/prodtimer.exe $WINROOT/prodtimer.exe
i586-mingw32msvc-strip $WINROOT/prodtimer.exe
#for i in $WINROOT/*.dll; do
#  i586-mingw32msvc-strip $i;
#done
echo -n "SUBSTSA78901234567890123456789012345678901234567" >/tmp/subst1.txt
echo -n "SUBSTSB7890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123" >/tmp/subst2.txt

rm /tmp/installfiles.nsi /tmp/uninstallfiles.nsi
for v in $(find /tmp/lanthaps_package/win32_root -type f | sed -e 's/\//\\/g'); do
  short=$(echo $v | sed -e 's/\\tmp\\lanthaps_package\\win32_root\\//g')
  echo "File /oname=\$INSTDIR\\$short \"z:$v\"" >>/tmp/installfiles.nsi
  echo "Delete \"\$INSTDIR\\$short\"" >>/tmp/uninstallfiles.nsi
done

wine c:/Program\ Files/NSIS/makensis installer\\win32.nsi
