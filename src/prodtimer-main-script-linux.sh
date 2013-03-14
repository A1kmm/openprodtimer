#!/bin/sh

export LC_ALL=C
SCRIPTDIR=`dirname $0`
PRODTIMER_HOME=`readlink -f $SCRIPTDIR`

if test "$1" = "--setup"; then
  cat >~/.local/share/applications/lanthaps-prodtimer.desktop <<EOF
[Desktop Entry]
Name=Lanthaps ProdTimer 2012
Exec=$PRODTIMER_HOME/prodtimer
Type=Application
GenericName=Productivity Timer
Terminal=false
Icon=$PRODTIMER_HOME/icons/work-time.png
Caption=Productivity Timer
Categories=Office;Utility;Education;Development
EOF
fi

export PANGO_PATH=$PRODTIMER_HOME/lib/
cat >$PRODTIMER_HOME/lib/gdkpixbuf-loaders.cache <<EOF
"$PRODTIMER_HOME/lib/libpixbufloader-png.so"
"png" 5 "gdk-pixbuf" "The PNG image format" "LGPL"
"image/png" ""
"png" ""
"\211PNG\r\n\032\n" "" 100

EOF
export GDK_PIXBUF_MODULE_FILE="$PRODTIMER_HOME/lib/gdkpixbuf-loaders.cache"
cat >$PRODTIMER_HOME/lib/pangorc <<EOF
[Pango]
ModuleFiles = $PRODTIMER_HOME/lib/pango.modules
EOF
cat >$PRODTIMER_HOME/lib/pango.modules <<EOF
$PRODTIMER_HOME/lib/pango-basic-fc.so BasicScriptEngineFc PangoEngineShape PangoRenderFc latin:* cyrillic:* greek:* armenian:* georgian:* runic:* ogham:* bopomofo:* cherokee:* coptic:* deseret:* ethiopic:* gothic:* han:* hiragana:* katakana:* old-italic:* canadian-aboriginal:* yi:* braille:* cypriot:* limbu:* osmanya:* shavian:* linear-b:* ugaritic:* glagolitic:* cuneiform:* phoenician:* common:
$PRODTIMER_HOME/lib/pango-basic-x.so BasicScriptEngineX PangoEngineShape PangoRenderX common:
EOF
export PATH="$PRODTIMER_HOME:$PATH"
export PANGO_RC_FILE="$PRODTIMER_HOME/lib/pangorc"
export GCONV_PATH="$PRODTIMER_HOME/lib"
export LD_LIBRARY_PATH=$PRODTIMER_HOME/lib/
"$PRODTIMER_HOME/lib/dynamic-loader" "$PRODTIMER_HOME/bin/prodtimer-bin" "$*"
