; Use the Modern UI...
!include "MUI2.nsh"

Name "Lanthaps ProdTimer 2012"
CRCCheck off
OutFile "z:\tmp\lanthaps_package\install-prodtimer.exe"
SetCompressor lzma

InstallDir "$PROGRAMFILES\Lanthaps\ProdTimer2012"
InstallDirRegKey HKCU "Software\Lanthaps\ProdTimer\2012" "InstallDir"

RequestExecutionLevel highest

Var StartMenuFolder

!define MUI_ABORTWARNING
!insertmacro MUI_PAGE_LICENSE "z:\tmp\lanthaps_package\win32_root\COPYING"
!insertmacro MUI_PAGE_DIRECTORY

!define MUI_STARTMENUPAGE_REGISTRY_ROOT "HKCU"
!define MUI_STARTMENUPAGE_REGISTRY_KEY "Software\Lanthaps\ProdTimer\2012"
!define MUI_STARTMENUPAGE_REGISTRY_VALUENAME "Start Menu Folder"

!insertmacro MUI_PAGE_STARTMENU Application $StartMenuFolder

!insertmacro MUI_PAGE_INSTFILES
 
!insertmacro MUI_UNPAGE_CONFIRM
!insertmacro MUI_UNPAGE_INSTFILES

!insertmacro MUI_LANGUAGE "English"

Section "Lanthaps ProdTimer" SecPT
  SetCompress auto

  SetOutPath "$INSTDIR"
  CreateDirectory "$INSTDIR\sounds"
  CreateDirectory "$INSTDIR\icons"
  !include "z:\tmp\installfiles.nsi"

  ; Exec '"$INSTDIR\oalinst.exe" /s'
  ; Delete "$INSTDIR\oalinst.exe"
  WriteRegStr HKCU "Software\Lanthaps\ProdTimer\2012" "InstallDir" $INSTDIR

  WriteUninstaller "$INSTDIR\Uninstall.exe"
  !insertmacro MUI_STARTMENU_WRITE_BEGIN Application
    
  ;Create shortcuts
  CreateDirectory "$SMPROGRAMS\$StartMenuFolder"
  CreateShortCut "$SMPROGRAMS\$StartMenuFolder\ProdTimer.lnk" "$INSTDIR\prodtimer.exe" "" "$INSTDIR\icons\prodtimer.ico" 0 SW_SHOWNORMAL ALT|CONTROL|SHIFT|p "Lanthaps ProdTimer 2012"
  CreateShortCut "$SMPROGRAMS\$StartMenuFolder\Uninstall.lnk" "$INSTDIR\Uninstall.exe"

  !insertmacro MUI_STARTMENU_WRITE_END

  Exec '"$INSTDIR\prodtimer.exe"'
SectionEnd

Section "Uninstall"
  !include "z:\tmp\uninstallfiles.nsi"
  RMDir "$INSTDIR\sounds"
  RMDir "$INSTDIR\icons"
  RMDir "$INSTDIR\Uninstall.exe"
  RMDir "$INSTDIR"
  
  !insertmacro MUI_STARTMENU_GETFOLDER Application $StartMenuFolder
    
  Delete "$SMPROGRAMS\$StartMenuFolder\Uninstall.lnk"
  Delete "$SMPROGRAMS\$StartMenuFolder\ProdTimer.lnk"
  RMDir "$SMPROGRAMS\$StartMenuFolder"
  
  DeleteRegKey /ifempty HKCU "Software\Lanthaps\ProdTimer\2012"
SectionEnd
