#!/bin/bash

rm -fr /tmp/lanthaps_package/linux_root
mkdir -p /tmp/lanthaps_package/linux_root
mkdir /tmp/lanthaps_package/linux_root/lib
mkdir /tmp/lanthaps_package/linux_root/docs
cp ./COPYING /tmp/lanthaps_package/linux_root/
cp ./src/prodtimer-main-script-linux.sh /tmp/lanthaps_package/linux_root/prodtimer
cp ./installer/simplified-gconv /tmp/lanthaps_package/linux_root/lib/gconv-modules
chmod +x /tmp/lanthaps_package/linux_root/prodtimer
cp -R icons /tmp/lanthaps_package/linux_root
cp -R sounds /tmp/lanthaps_package/linux_root/sounds
echo "ProdTimer will run - browse UI and then exit it to build file list"
../standalone_builder/standalone_builder ./dist/build/prodtimer/prodtimer /tmp/lanthaps_package/linux_root/lib
mkdir /tmp/lanthaps_package/linux_root/bin
cp ./dist/build/prodtimer/prodtimer /tmp/lanthaps_package/linux_root/bin/prodtimer-bin
strip -R .comment --strip-unneeded /tmp/lanthaps_package/linux_root/bin/prodtimer-bin
chrpath -d /tmp/lanthaps_package/linux_root/bin/prodtimer-bin
if [[ `uname -m` == "x86_64" ]];
  then cp /lib64/ld-linux-x86-64.so.2 /tmp/lanthaps_package/linux_root/lib/dynamic-loader;
  else cp /lib/ld-linux.so.2 /tmp/lanthaps_package/linux_root/lib/dynamic-loader;
fi
# for i in libFLAC.so.8 libvorbisenc.so.2 libvorbis.so.0 libvorbisfile.so.3 libogg.so.0; do rm /tmp/lanthaps_package/linux_root/lib/$i; done
for i in /tmp/lanthaps_package/linux_root/lib/*.so.*; do strip -R .comment --strip-unneeded $i; chrpath -d $i; done
CFLAGS='-DFINAL_EXECVE={\"./prodtimer\",\"--setup\",NULL} -DPRODUCT=\"ProdTimer\"' make -C ../standalone_builder/selfinst
../standalone_builder/packer/packer -d /tmp/lanthaps_package/linux_root --selfinst=../standalone_builder/selfinst/selfinst --installerout=/tmp/lanthaps_package/lanthaps-prodtimer-linux-installer.bin
