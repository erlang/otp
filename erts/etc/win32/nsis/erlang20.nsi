; NSIS Modern User Interface version 1.63
; Erlang OTP installation script based on "Start Menu Folder Selection 
; Example Script"
; Original example written by Joost Verburg
; Modified for Erlang by Patrik

;
; %CopyrightBegin%
;
; Copyright Ericsson AB 2012-2016. All Rights Reserved.
;
; Licensed under the Apache License, Version 2.0 (the "License");
; you may not use this file except in compliance with the License.
; You may obtain a copy of the License at
;
;     http://www.apache.org/licenses/LICENSE-2.0
;
; Unless required by applicable law or agreed to in writing, software
; distributed under the License is distributed on an "AS IS" BASIS,
; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
; See the License for the specific language governing permissions and
; limitations under the License.
;
; %CopyrightEnd%
;

; Verbosity does not come naturally with MUI, have to set it back now and then.
	!verbose 1
	!define MUI_MANUALVERBOSE 1

	!define OTP_PRODUCT "Erlang OTP"

	!include "erlang.nsh" ; All release specific parameters come from this

	Name "${OTP_PRODUCT} ${OTP_VERSION}"

	!include "MUI.nsh"
	!include "WordFunc.nsh"
	!include "WinVer.nsh"
;--------------------------------
;Configuration

	SetCompressor bzip2

Var MYTEMP
;Var MUI_TEMP
Var STARTMENU_FOLDER


!define  MY_STARTMENUPAGE_REGISTRY_ROOT HKLM
!define  MY_STARTMENUPAGE_REGISTRY_KEY "SOFTWARE\Ericsson\Erlang\${ERTS_VERSION}"
!define MY_STARTMENUPAGE_REGISTRY_VALUENAME "Start Menu Folder"

;General
	OutFile "${OUTFILEDIR}\otp_${WINTYPE}_${OTP_VERSION}.exe"

;Folder selection page
!if ${WINTYPE} == "win64"
  	InstallDir "$PROGRAMFILES64\erl${ERTS_VERSION}"
!else
  	InstallDir "$PROGRAMFILES\erl${ERTS_VERSION}"
!endif  
;Remember install folder
  	InstallDirRegKey HKLM "SOFTWARE\Ericsson\Erlang\${ERTS_VERSION}" ""
  
; Set the default start menu folder

!if ${WINTYPE} == "win64"
	!define MUI_STARTMENUPAGE_DEFAULTFOLDER "${OTP_PRODUCT} ${OTP_VERSION} (x64)"
!else
	!define MUI_STARTMENUPAGE_DEFAULTFOLDER "${OTP_PRODUCT} ${OTP_VERSION}"
!endif  

;--------------------------------
;Modern UI Configuration
!ifdef HAVE_CUSTOM_MODERN
	!define MUI_UI "custom_modern.exe"
!endif
        !define MUI_ICON "erlang_inst.ico"
        !define MUI_UNICON "erlang_uninst.ico"

  	!insertmacro MUI_PAGE_COMPONENTS
  	!insertmacro MUI_PAGE_DIRECTORY
; Registry keys where start menu folder is stored

  	!define MUI_STARTMENUPAGE_REGISTRY_ROOT ${MY_STARTMENUPAGE_REGISTRY_ROOT} 
  	!define MUI_STARTMENUPAGE_REGISTRY_KEY "${MY_STARTMENUPAGE_REGISTRY_KEY}"
  	!define MUI_STARTMENUPAGE_REGISTRY_VALUENAME "${MY_STARTMENUPAGE_REGISTRY_VALUENAME}"

        !insertmacro MUI_PAGE_STARTMENU Application $STARTMENU_FOLDER

	!insertmacro MUI_PAGE_INSTFILES
  
        !insertmacro MUI_UNPAGE_CONFIRM
        !insertmacro MUI_UNPAGE_INSTFILES

;--------------------------------
;Languages
 
  	!insertmacro MUI_LANGUAGE "English"
  
;--------------------------------
;Language Strings

;Description
  	LangString DESC_SecErlang ${LANG_ENGLISH} "Erlang OTP."
  	LangString DESC_SecErlangDev ${LANG_ENGLISH} \
		"Erlang OTP development environment (required)."
  	LangString DESC_SecErlangAssoc ${LANG_ENGLISH} \
		"Erlang filetype associations (.erl, .hrl, .beam)."
!ifdef HAVE_DOCS
  	LangString DESC_SecErlangDoc ${LANG_ENGLISH} "Documentation."
!endif
!ifdef HAVE_REDIST_FILE
  	LangString DESC_SecMSRedist ${LANG_ENGLISH} "Microsoft redistributable C runtime libraries, these are mandatory for Erlang runtime and development. Always installed if not already present."
!endif
;--------------------------------
; WordFunc
!ifdef HAVE_REDIST_FILE
	!insertmacro VersionCompare
!endif
;--------------------------------
;Installer Sections

!ifdef HAVE_REDIST_FILE
Section "Microsoft redistributable libraries." SecMSRedist

  	SetOutPath "$INSTDIR"
	File "${TESTROOT}\${REDIST_EXECUTABLE}"
  
; Set back verbosity...
  	!verbose 1
; Run the setup program
	IfSilent +3
	    ExecWait '"$INSTDIR\${REDIST_EXECUTABLE}"'
	Goto +2
	    ExecWait '"$INSTDIR\${REDIST_EXECUTABLE}" /q'

  	!verbose 1
SectionEnd ; MSRedist
!endif

SubSection /e "Erlang" SecErlang
Section "Development" SecErlangDev
SectionIn 1 RO

  	SetOutPath "$INSTDIR"
  	File "${TESTROOT}\Install.ini"
  	File "${TESTROOT}\Install.exe"
	SetOutPath "$INSTDIR\releases"
  	File /r "${TESTROOT}\releases\"
	SetOutPath "$INSTDIR\lib"
  	File /r "${TESTROOT}\lib\"
	SetOutPath "$INSTDIR\erts-${ERTS_VERSION}"
  	File /r "${TESTROOT}\erts-${ERTS_VERSION}\"
	SetOutPath "$INSTDIR\usr"
  	File /r "${TESTROOT}\usr\"
  
;Store install folder
  	WriteRegStr HKLM "SOFTWARE\Ericsson\Erlang\${ERTS_VERSION}" "" $INSTDIR

; Run the setup program  
  	Exec '"$INSTDIR\Install.exe" -s'

; The startmenu stuff
  	!insertmacro MUI_STARTMENU_WRITE_BEGIN Application
; Set back verbosity...
  	!verbose 1
; Try to use the Common startmenu...
  	SetShellVarContext All
  	ClearErrors
  	CreateDirectory "$SMPROGRAMS\$STARTMENU_FOLDER"
  	IfErrors 0 continue_create
    	;MessageBox MB_OK "Error creating file"
    	SetShellVarContext current
    	CreateDirectory "$SMPROGRAMS\$STARTMENU_FOLDER"
continue_create:
  	CreateShortCut "$SMPROGRAMS\$STARTMENU_FOLDER\Erlang.lnk" \
		"$INSTDIR\bin\werl.exe"
  
  	!insertmacro MUI_STARTMENU_WRITE_END
; And once again, the verbosity...
  	!verbose 1
; Check that the registry could be written, we only check one key,
; but it should be sufficient...
  	ReadRegStr $MYTEMP ${MY_STARTMENUPAGE_REGISTRY_ROOT}  "${MY_STARTMENUPAGE_REGISTRY_KEY}" "${MY_STARTMENUPAGE_REGISTRY_VALUENAME}"

  	StrCmp $MYTEMP "" 0 done_startmenu

; If startmenu was skipped, this might be unnecessary, but wont hurt...	
  	WriteRegStr HKCU "Software\Ericsson\Erlang\${ERTS_VERSION}" \
		"" $INSTDIR
  	WriteRegStr HKCU "${MY_STARTMENUPAGE_REGISTRY_KEY}" \
		"${MY_STARTMENUPAGE_REGISTRY_VALUENAME}" \
		"$STARTMENU_FOLDER"


done_startmenu:
;Create uninstaller
  	WriteUninstaller "$INSTDIR\Uninstall.exe"

  	WriteRegStr HKLM \
		"SOFTWARE\Microsoft\Windows\CurrentVersion\Uninstall\Erlang OTP ${OTP_VERSION} (${ERTS_VERSION})" \
		"DisplayName" "Erlang OTP ${OTP_VERSION} (${ERTS_VERSION})"
  	WriteRegStr HKLM \
		"SOFTWARE\Microsoft\Windows\CurrentVersion\Uninstall\Erlang OTP ${OTP_VERSION} (${ERTS_VERSION})" \
		"UninstallString" "$INSTDIR\Uninstall.exe"
  	WriteRegDWORD HKLM \
		"SOFTWARE\Microsoft\Windows\CurrentVersion\Uninstall\Erlang OTP ${OTP_VERSION} (${ERTS_VERSION})" \
		"NoModify" 1
  	WriteRegDWORD HKLM \
		"SOFTWARE\Microsoft\Windows\CurrentVersion\Uninstall\Erlang OTP ${OTP_VERSION} (${ERTS_VERSION})" \
		"NoRepair" 1

; Check that the registry could be written, we only check one key,
; but it should be sufficient...
  	ReadRegStr $MYTEMP HKLM \
	"SOFTWARE\Microsoft\Windows\CurrentVersion\Uninstall\Erlang OTP ${OTP_VERSION} (${ERTS_VERSION})" \
	"NoRepair"

  	StrCmp $MYTEMP "" 0 done

; Now we're done if we are a superuser. If the registry stuff failed, we 
; do the things below...

  	WriteRegStr HKCU \
		"Software\Microsoft\Windows\CurrentVersion\Uninstall\Erlang OTP ${OTP_VERSION} (${ERTS_VERSION})" \
		"DisplayName" "Erlang OTP ${OTP_VERSION} (${ERTS_VERSION})"
  	WriteRegStr HKCU \
		"Software\Microsoft\Windows\CurrentVersion\Uninstall\Erlang OTP ${OTP_VERSION} (${ERTS_VERSION})" \
		"UninstallString" "$INSTDIR\Uninstall.exe"
  	WriteRegDWORD HKCU \
		"Software\Microsoft\Windows\CurrentVersion\Uninstall\Erlang OTP ${OTP_VERSION} (${ERTS_VERSION})" \
		"NoModify" 1
  	WriteRegDWORD HKCU \
		"Software\Microsoft\Windows\CurrentVersion\Uninstall\Erlang OTP ${OTP_VERSION} (${ERTS_VERSION})" \
		"NoRepair" 1

done:
SectionEnd ; SecErlangDev

Section "Associations" SecErlangAssoc

  	;File /r "${TESTROOT}\usr\lib\icons"

; .erl
  	DeleteRegKey HKCR ".erl"
  	DeleteRegKey HKCR "ErlangSource"
  	WriteRegStr HKCR ".erl" "" "ErlangSource"
	WriteRegStr HKCR "ErlangSource" "" "Erlang source file"
	WriteRegStr HKCR "ErlangSource\shell\compile" "" "Compile"
	WriteRegStr HKCR "ErlangSource\shell\compile\command" "" \
		'"$INSTDIR\bin\erlc.exe" "%1"'
	WriteRegStr HKCR "ErlangSource\DefaultIcon" "" \
		$INSTDIR\usr\lib\icons\erl_icon.ico
; .hrl
  	DeleteRegKey HKCR ".hrl"
  	DeleteRegKey HKCR "ErlangHeader"
  	WriteRegStr HKCR ".hrl" "" "ErlangHeader"
	WriteRegStr HKCR "ErlangHeader" "" "Erlang header file"
	WriteRegStr HKCR "ErlangHeader\DefaultIcon" "" \
		$INSTDIR\usr\lib\icons\hrl_icon.ico

; .beam
  	DeleteRegKey HKCR ".beam"
  	DeleteRegKey HKCR "ErlangBeam"
  	WriteRegStr HKCR ".beam" "" "ErlangBeam"
	WriteRegStr HKCR "ErlangBeam" "" "Erlang beam code"
	WriteRegStr HKCR "ErlangBeam\DefaultIcon" "" \
		$INSTDIR\usr\lib\icons\beam_icon.ico


	SearchPath $1 "write.exe"
	StrCmp $1 "" writeNotFound
	WriteRegStr HKCR "ErlangSource\shell" "" "Open"
  	WriteRegStr HKCR "ErlangSource\shell\open\command" "" \
		'"$1" "%1"'
	WriteRegStr HKCR "ErlangHeader\shell" "" "Open"
  	WriteRegStr HKCR "ErlangHeader\shell\open\command" "" \
		'"$1" "%1"'


writeNotFound:
SectionEnd ; SecErlangAssoc
SubSectionEnd

!ifdef HAVE_DOCS
Section "Erlang Documentation" SecErlangDoc

  	SetOutPath "$INSTDIR"
  	File /r "${TESTROOT}\docs\*"
  
; The startmenu stuff
  	!insertmacro MUI_STARTMENU_WRITE_BEGIN Application
; Set back verbosity...
  	!verbose 1
; Try to use the Common startmenu...
  	SetShellVarContext All
  	ClearErrors
  	CreateShortCut "$SMPROGRAMS\$STARTMENU_FOLDER\Erlang Documentation.lnk" \
		"$INSTDIR\doc\index.html"
  	IfErrors 0 continue_create
    	;MessageBox MB_OK "Error creating file"
    	SetShellVarContext current
  	CreateShortCut \
		"$SMPROGRAMS\$STARTMENU_FOLDER\Erlang Documentation.lnk" \
		"$INSTDIR\doc\index.html"
continue_create:
  
  	!insertmacro MUI_STARTMENU_WRITE_END
; And once again, the verbosity...
  	!verbose 1
SectionEnd ; ErlangDoc
!endif

!ifdef HAVE_REDIST_FILE
Function DllVersionGoodEnough
    IntCmp 0 $R0 normal0 normal0 negative0
    normal0: 
        IntOp $R2 $R0 >> 16
	Goto continue0
    negative0:
	IntOp $R2 $R0 & 0x7FFF0000
	IntOp $R2 $R2 >> 16
	IntOp $R2 $R2 | 0x8000
    continue0:		
    IntOp $R3 $R0 & 0x0000FFFF
    IntCmp 0 $R1 normal1 normal1 negative1
    normal1: 
        IntOp $R4 $R1 >> 16
	Goto continue1
    negative1:
	IntOp $R4 $R1 & 0x7FFF0000
	IntOp $R4 $R4 >> 16
	IntOp $R4 $R4 | 0x8000
    continue1:		
    IntOp $R5 $R1 & 0x0000FFFF
    StrCpy $2 "$R2.$R3.$R4.$R5"
    ${VersionCompare} $2 ${REDIST_DLL_VERSION} $R0
    Return
FunctionEnd

Function .onInit
   Var /GLOBAL archprefix
   Var /GLOBAL sysnativedir
   Var /GLOBAL winvermajor
   Var /GLOBAL winverminor

   SectionGetFlags 0 $MYTEMP
   StrCmpS ${WINTYPE} "win64" +1 +4
	StrCpy $archprefix "amd64"
	StrCpy $sysnativedir "$WINDIR\sysnative"
   Goto +3
	StrCpy $archprefix "x86"
	StrCpy $sysnativedir $SYSDIR
   ${WinVerGetMajor} $0
   ${WinVerGetMinor} $1
   StrCpy $winvermajor $0
   StrCpy $winverminor $1
   IfFileExists $sysnativedir\${REDIST_DLL_NAME} MaybeFoundInSystemLbl
   SearchSxSLbl:	
        IntCmp $winvermajor 6 WVCheckMinorLbl WVCheckDoneLbl NotFoundLbl
   WVCheckMinorLbl:
	IntCmp $winverminor 1 WVCheckDoneLbl WVCheckDoneLbl NotFoundLbl
   WVCheckDoneLbl:
        FindFirst $0 $1 $WINDIR\WinSxS\$archprefix*
        LoopLbl:
	    StrCmp $1 "" NotFoundLbl
	    IfFileExists $WINDIR\WinSxS\$1\${REDIST_DLL_NAME} MaybeFoundInSxSLbl
	    FindNext $0 $1
	    Goto LoopLbl
        MaybeFoundInSxSLbl:
	    GetDllVersion $WINDIR\WinSxS\$1\${REDIST_DLL_NAME} $R0 $R1
	    Call DllVersionGoodEnough
	    FindNext $0 $1
	    IntCmp 2 $R0 LoopLbl
	    Goto FoundLbl  
   MaybeFoundInSystemLbl:
	GetDllVersion $sysnativedir\${REDIST_DLL_NAME} $R0 $R1
	Call DllVersionGoodEnough
	IntCmp 2 $R0 SearchSxSLbl  
   FoundLbl:
	IntOp $MYTEMP $MYTEMP & 4294967294
	SectionSetFlags 0 $MYTEMP
	SectionSetText 0 "Microsoft DLL's (present)"
	Return
   NotFoundLbl:
        IntOp $MYTEMP $MYTEMP | 16
	SectionSetFlags 0 $MYTEMP
	SectionSetText 0 "Microsoft DLL's (needed)"
	Return
FunctionEnd
!endif


;Display the Finish header
;Insert this macro after the sections if you are not using a finish page
;	!insertmacro MUI_SECTIONS_FINISHHEADER

;--------------------------------
;Descriptions

	!insertmacro MUI_FUNCTION_DESCRIPTION_BEGIN
  	!insertmacro MUI_DESCRIPTION_TEXT ${SecErlang} $(DESC_SecErlang)
  	!insertmacro MUI_DESCRIPTION_TEXT ${SecErlangDev} $(DESC_SecErlangDev)
  	!insertmacro MUI_DESCRIPTION_TEXT ${SecErlangAssoc} \
		$(DESC_SecErlangAssoc)
!ifdef HAVE_DOCS
  	!insertmacro MUI_DESCRIPTION_TEXT ${SecErlangDoc} $(DESC_SecErlangDoc)
!endif
!ifdef HAVE_REDIST_FILE
  	!insertmacro MUI_DESCRIPTION_TEXT ${SecMSRedist} $(DESC_SecMSRedist)
!endif
	!insertmacro MUI_FUNCTION_DESCRIPTION_END
 
;--------------------------------
;Uninstaller Section

Section "Uninstall"

  	RMDir /r "$INSTDIR"  
  
;Remove shortcut
  	ReadRegStr $MYTEMP "${MY_STARTMENUPAGE_REGISTRY_ROOT}" \
		"${MY_STARTMENUPAGE_REGISTRY_KEY}" \
		"${MY_STARTMENUPAGE_REGISTRY_VALUENAME}"
	StrCmp $MYTEMP "" 0 end_try
; Try HKCU instead...
  	ReadRegStr $MYTEMP "${MY_STARTMENUPAGE_REGISTRY_ROOT}" \
		"${MY_STARTMENUPAGE_REGISTRY_KEY}" \
		"${MY_STARTMENUPAGE_REGISTRY_VALUENAME}"
; If this failed to, we have no shortcuts (eh?)
  	StrCmp $MYTEMP "" noshortcuts
end_try:
  	SetShellVarContext All
  	ClearErrors
; If we cannot find the shortcut, switch to current user context
  	GetFileTime "$SMPROGRAMS\$MYTEMP\Erlang.lnk" $R1 $R2
  	IfErrors 0 continue_delete
    	;MessageBox MB_OK "Error removing file"
    	SetShellVarContext current
continue_delete:
  	Delete "$SMPROGRAMS\$MYTEMP\Erlang.lnk"
  	Delete "$SMPROGRAMS\$MYTEMP\Uninstall.lnk"
  	Delete "$SMPROGRAMS\$MYTEMP\Erlang Documentation.lnk"
  	RMDir "$SMPROGRAMS\$MYTEMP" ;Only if empty

noshortcuts:
; We delete both in HKCU and HKLM, we don't really know were they might be...
  	DeleteRegKey /ifempty HKLM "SOFTWARE\Ericsson\Erlang\${ERTS_VERSION}"
  	DeleteRegKey /ifempty HKCU "SOFTWARE\Ericsson\Erlang\${ERTS_VERSION}"
  	DeleteRegKey HKLM \
		"SOFTWARE\Microsoft\Windows\CurrentVersion\Uninstall\Erlang OTP ${OTP_VERSION} (${ERTS_VERSION})"
  	DeleteRegKey HKCU \
		"SOFTWARE\Microsoft\Windows\CurrentVersion\Uninstall\Erlang OTP ${OTP_VERSION} (${ERTS_VERSION})"


; Now remove shell/file associations we'we made...
; .erl
  	ReadRegStr $1 HKCR ".erl" ""
  	StrCmp $1 "ErlangSource" 0 NoOwnSource 
  	ReadRegStr $1 HKCR "ErlangSource\DefaultIcon" ""
	StrCmp $1 "$INSTDIR\usr\lib\icons\erl_icon.ico" 0 NoOwnSource 
  	DeleteRegKey HKCR ".erl"
  	DeleteRegKey HKCR "ErlangSource"
NoOwnSource:
; .hrl
  	ReadRegStr $1 HKCR ".hrl" ""
  	StrCmp $1 "ErlangHeader" 0 NoOwnHeader 
  	ReadRegStr $1 HKCR "ErlangHeader\DefaultIcon" ""
	StrCmp $1 "$INSTDIR\usr\lib\icons\hrl_icon.ico" 0 NoOwnHeader 
  	DeleteRegKey HKCR ".hrl"
  	DeleteRegKey HKCR "ErlangHeader"
NoOwnHeader:

; .beam
  	ReadRegStr $1 HKCR ".beam" ""
  	StrCmp $1 "ErlangBeam" 0 NoOwnBeam 
  	ReadRegStr $1 HKCR "ErlangBeam\DefaultIcon" ""
	StrCmp $1 "$INSTDIR\usr\lib\icons\beam_icon.ico" 0 NoOwnBeam 
  	DeleteRegKey HKCR ".beam"
  	DeleteRegKey HKCR "ErlangBeam"
NoOwnBeam:

;Display the Finish header
;  	!insertmacro MUI_UNFINISHHEADER

SectionEnd
	!verbose 3
