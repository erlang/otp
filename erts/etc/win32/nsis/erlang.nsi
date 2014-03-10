; NSIS Modern User Interface version 1.63
; Erlang OTP installation script based on "Start Menu Folder Selection 
; Example Script"
; Original example written by Joost Verburg
; Modified for Erlang by Patrik

; Verbosity does not come naturally with MUI, have to set it back now and then.
	!verbose 1
	!define MUI_MANUALVERBOSE 1

	!define MUI_PRODUCT "Erlang OTP"

	!include "erlang.nsh" ; All release specific parameters come from this


	!include "MUI.nsh"

;--------------------------------
;Configuration

	;SetCompressor bzip2

;General
	OutFile "${OUTFILEDIR}\otp_win32_${MUI_VERSION}.exe"

;Folder selection page
  	InstallDir "$PROGRAMFILES\erl${ERTS_VERSION}"
  
;Remember install folder
  	InstallDirRegKey HKLM "SOFTWARE\Ericsson\Erlang\${ERTS_VERSION}" ""
  
;$9 is being used to store the Start Menu Folder.
;Do not use this variable in your script (or Push/Pop it)!

;To change this variable, use MUI_STARTMENUPAGE_VARIABLE.
;Have a look at the Readme for info about other options (default folder,
;registry).

; Set the default start menu folder

	!define MUI_STARTMENUPAGE_DEFAULTFOLDER "${MUI_PRODUCT} ${MUI_VERSION}"

; Registry keys where start menu folder is stored
  	!define MUI_STARTMENUPAGE_REGISTRY_ROOT "HKLM" 
  	!define MUI_STARTMENUPAGE_REGISTRY_KEY \
		"SOFTWARE\Ericsson\Erlang\${ERTS_VERSION}"
  	!define MUI_STARTMENUPAGE_REGISTRY_VALUENAME "Start Menu Folder"

; Temporary variable used here and there...
  	!define TEMP $R0
  
;--------------------------------
;Modern UI Configuration
  	!define MUI_ICON "erlang_inst.ico"
  	!define MUI_UNICON "erlang_uninst.ico"
	!define MUI_WELCOMEPAGE
  	!define MUI_COMPONENTSPAGE
  	!define MUI_DIRECTORYPAGE
  	!define MUI_STARTMENUPAGE
  
  	!define MUI_ABORTWARNING
  
  	!define MUI_UNINSTALLER
  	!define MUI_UNCONFIRMPAGE

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
;--------------------------------
;Installer Sections

SubSection /e "Erlang" SecErlang
Section "Development" SecErlangDev
SectionIn 1 RO

  	StrCmp ${MUI_STARTMENUPAGE_VARIABLE} "" 0 skip_silent_mode
	StrCpy ${MUI_STARTMENUPAGE_VARIABLE} \
		"${MUI_STARTMENUPAGE_DEFAULTFOLDER}"
skip_silent_mode:

  	SetOutPath "$INSTDIR"
  	File "${TESTROOT}\Install.ini"
  	File "${TESTROOT}\Install.exe"
  	File /r "${TESTROOT}\releases"
  	File /r "${TESTROOT}\lib"
  	File /r "${TESTROOT}\erts-${ERTS_VERSION}"
  	File /r "${TESTROOT}\usr"
  
;Store install folder
  	WriteRegStr HKLM "SOFTWARE\Ericsson\Erlang\${ERTS_VERSION}" "" $INSTDIR

; Run the setup program  
  	ExecWait '"$INSTDIR\Install.exe" -s'

; The startmenu stuff
  	!insertmacro MUI_STARTMENU_WRITE_BEGIN
; Set back verbosity...
  	!verbose 1
; Try to use the Common startmenu...
  	SetShellVarContext All
  	ClearErrors
  	CreateDirectory "$SMPROGRAMS\${MUI_STARTMENUPAGE_VARIABLE}"
  	IfErrors 0 continue_create
    	;MessageBox MB_OK "Error creating file"
    	SetShellVarContext current
    	CreateDirectory "$SMPROGRAMS\${MUI_STARTMENUPAGE_VARIABLE}"
continue_create:
  	WriteUninstaller "$INSTDIR\Uninstall.exe"
  	CreateShortCut "$SMPROGRAMS\${MUI_STARTMENUPAGE_VARIABLE}\Erlang.lnk" \
		"$INSTDIR\bin\werl.exe"
  	CreateShortCut \
		"$SMPROGRAMS\${MUI_STARTMENUPAGE_VARIABLE}\Uninstall.lnk" \
		"$INSTDIR\Uninstall.exe" "" "$INSTDIR\Uninstall.exe" 0 
  
  	!insertmacro MUI_STARTMENU_WRITE_END
; And once again, the verbosity...
  	!verbose 1
;Create uninstaller
;  	WriteUninstaller "$INSTDIR\Uninstall.exe"

  	WriteRegStr HKLM \
		"SOFTWARE\Microsoft\Windows\CurrentVersion\Uninstall\Erlang OTP ${MUI_VERSION} (${ERTS_VERSION})" \
		"DisplayName" "Erlang OTP ${MUI_VERSION} (${ERTS_VERSION})"
  	WriteRegStr HKLM \
		"SOFTWARE\Microsoft\Windows\CurrentVersion\Uninstall\Erlang OTP ${MUI_VERSION} (${ERTS_VERSION})" \
		"UninstallString" "$INSTDIR\Uninstall.exe"
  	WriteRegDWORD HKLM \
		"SOFTWARE\Microsoft\Windows\CurrentVersion\Uninstall\Erlang OTP ${MUI_VERSION} (${ERTS_VERSION})" \
		"NoModify" 1
  	WriteRegDWORD HKLM \
		"SOFTWARE\Microsoft\Windows\CurrentVersion\Uninstall\Erlang OTP ${MUI_VERSION} (${ERTS_VERSION})" \
		"NoRepair" 1

; Check that the registry could be written, we only check one key,
; but it should be sufficient...
  	ReadRegStr ${TEMP} "${MUI_STARTMENUPAGE_REGISTRY_ROOT}" \
		"${MUI_STARTMENUPAGE_REGISTRY_KEY}" \
		"${MUI_STARTMENUPAGE_REGISTRY_VALUENAME}"

  	StrCmp ${TEMP} "" 0 done

; Now we're done if we are a superuser. If the registry stuff failed, we 
; do the things below...

  	WriteRegStr HKCU "Software\Ericsson\Erlang\${ERTS_VERSION}" \
		"" $INSTDIR
  	WriteRegStr HKCU "${MUI_STARTMENUPAGE_REGISTRY_KEY}" \
		"${MUI_STARTMENUPAGE_REGISTRY_VALUENAME}" \
		"${MUI_STARTMENUPAGE_VARIABLE}"
  	WriteRegStr HKCU \
		"Software\Microsoft\Windows\CurrentVersion\Uninstall\Erlang OTP ${MUI_VERSION} (${ERTS_VERSION})" \
		"DisplayName" "Erlang OTP ${MUI_VERSION} (${ERTS_VERSION})"
  	WriteRegStr HKCU \
		"Software\Microsoft\Windows\CurrentVersion\Uninstall\Erlang OTP ${MUI_VERSION} (${ERTS_VERSION})" \
		"UninstallString" "$INSTDIR\Uninstall.exe"
  	WriteRegDWORD HKCU \
		"Software\Microsoft\Windows\CurrentVersion\Uninstall\Erlang OTP ${MUI_VERSION} (${ERTS_VERSION})" \
		"NoModify" 1
  	WriteRegDWORD HKCU \
		"Software\Microsoft\Windows\CurrentVersion\Uninstall\Erlang OTP ${MUI_VERSION} (${ERTS_VERSION})" \
		"NoRepair" 1

done:
SectionEnd ; SecErlangDev

Section "Associations" SecErlangAssoc

  	;File /r "${TESTROOT}\usr\lib\icons"

; .erl
  	; back up old value of .erl
  	ReadRegStr $1 HKCR ".erl" ""
  	StrCmp $1 "" OwnErl
    	StrCmp $1 "ErlangSource" OwnErl
    	WriteRegStr HKCR ".erl" "backup_val" $1
OwnErl:
  	WriteRegStr HKCR ".erl" "" "ErlangSource"
  	ReadRegStr $0 HKCR "ErlangSource" ""
  	StrCmp $0 "" 0 skipErlAssoc
	WriteRegStr HKCR "ErlangSource" "" "Erlang source file"
	WriteRegStr HKCR "ErlangSource\shell" "" "open"
	WriteRegStr HKCR "ErlangSource\shell\compile" "" "Compile"
	WriteRegStr HKCR "ErlangSource\shell\compile\command" "" \
		'"$INSTDIR\bin\erlc.exe" "%1"'
	WriteRegStr HKCR "ErlangSource\DefaultIcon" "" \
		$INSTDIR\usr\lib\icons\erl_icon.ico
  	WriteRegStr HKCR "ErlangSource\shell\open\command" \
		"" 'write.exe "%1"'
skipErlAssoc:

; .hrl
  	; back up old value of .hrl
  	ReadRegStr $1 HKCR ".hrl" ""
  	StrCmp $1 "" OwnHrl
    	StrCmp $1 "ErlangHeader" OwnHrl
    	WriteRegStr HKCR ".hrl" "backup_val" $1
OwnHrl:
  	WriteRegStr HKCR ".hrl" "" "ErlangHeader"
  	ReadRegStr $0 HKCR "ErlangHeader" ""
  	StrCmp $0 "" 0 skipHrlAssoc
	WriteRegStr HKCR "ErlangHeader" "" "Erlang header file"
	WriteRegStr HKCR "ErlangHeader\shell" "" "open"
	WriteRegStr HKCR "ErlangHeader\DefaultIcon" "" \
		$INSTDIR\usr\lib\icons\hrl_icon.ico
  	WriteRegStr HKCR "ErlangHeader\shell\open\command" \
		"" 'write.exe "%1"'
skipHrlAssoc:

; .beam
  	; back up old value of .beam
  	ReadRegStr $1 HKCR ".beam" ""
  	StrCmp $1 "" OwnBeam
    	StrCmp $1 "ErlangBeam" OwnBeam
    	WriteRegStr HKCR ".beam" "backup_val" $1
OwnBeam:
  	WriteRegStr HKCR ".beam" "" "ErlangBeam"
  	ReadRegStr $0 HKCR "ErlangBeam" ""
  	StrCmp $0 "" 0 skipBeamAssoc
	WriteRegStr HKCR "ErlangBeam" "" "Erlang beam code"
	WriteRegStr HKCR "ErlangBeam\DefaultIcon" "" \
		$INSTDIR\usr\lib\icons\beam_icon.ico
skipBeamAssoc:

SectionEnd ; SecErlangAssoc
SubSectionEnd

!ifdef HAVE_DOCS
Section "Erlang Documentation" SecErlangDoc

  	SetOutPath "$INSTDIR"
  	File /r "${TESTROOT}\docs\*"
  
; The startmenu stuff
  	!insertmacro MUI_STARTMENU_WRITE_BEGIN
; Set back verbosity...
  	!verbose 1
; Try to use the Common startmenu...
  	SetShellVarContext All
  	ClearErrors
  	CreateShortCut "$SMPROGRAMS\${MUI_STARTMENUPAGE_VARIABLE}\Erlang Documentation.lnk" \
		"$INSTDIR\doc\index.html"
  	IfErrors 0 continue_create
    	;MessageBox MB_OK "Error creating file"
    	SetShellVarContext current
  	CreateShortCut \
		"$SMPROGRAMS\${MUI_STARTMENUPAGE_VARIABLE}\Erlang Documentation.lnk" \
		"$INSTDIR\doc\index.html"
continue_create:
  
  	!insertmacro MUI_STARTMENU_WRITE_END
; And once again, the verbosity...
  	!verbose 1
SectionEnd ; ErlangDoc
!endif


;Display the Finish header
;Insert this macro after the sections if you are not using a finish page
	!insertmacro MUI_SECTIONS_FINISHHEADER

;--------------------------------
;Descriptions

	!insertmacro MUI_FUNCTIONS_DESCRIPTION_BEGIN
  	!insertmacro MUI_DESCRIPTION_TEXT ${SecErlang} $(DESC_SecErlang)
  	!insertmacro MUI_DESCRIPTION_TEXT ${SecErlangDev} $(DESC_SecErlangDev)
  	!insertmacro MUI_DESCRIPTION_TEXT ${SecErlangAssoc} \
		$(DESC_SecErlangAssoc)
!ifdef HAVE_DOCS
  	!insertmacro MUI_DESCRIPTION_TEXT ${SecErlangDoc} $(DESC_SecErlangDoc)
!endif
	!insertmacro MUI_FUNCTIONS_DESCRIPTION_END
 
;--------------------------------
;Uninstaller Section

Section "Uninstall"

  	RMDir /r "$INSTDIR"  
  
;Remove shortcut
  	ReadRegStr ${TEMP} "${MUI_STARTMENUPAGE_REGISTRY_ROOT}" \
		"${MUI_STARTMENUPAGE_REGISTRY_KEY}" \
		"${MUI_STARTMENUPAGE_REGISTRY_VALUENAME}"
	StrCmp ${TEMP} "" 0 end_try
; Try HKCU instead...
  	ReadRegStr ${TEMP} HKCU \
		"${MUI_STARTMENUPAGE_REGISTRY_KEY}" \
		"${MUI_STARTMENUPAGE_REGISTRY_VALUENAME}"
; If this failed to, we have no shortcuts (eh?)
  	StrCmp ${TEMP} "" noshortcuts
end_try:
  	SetShellVarContext All
  	ClearErrors
; If we cannot find the shortcut, switch to current user context
  	GetFileTime "$SMPROGRAMS\${TEMP}\Erlang.lnk" $R1 $R2
  	IfErrors 0 continue_delete
    	;MessageBox MB_OK "Error removing file"
    	SetShellVarContext current
continue_delete:
  	Delete "$SMPROGRAMS\${TEMP}\Erlang.lnk"
  	Delete "$SMPROGRAMS\${TEMP}\Uninstall.lnk"
  	Delete "$SMPROGRAMS\${TEMP}\Erlang Documentation.lnk"
  	RMDir "$SMPROGRAMS\${TEMP}" ;Only if empty

noshortcuts:
; We delete both in HKCU and HKLM, we don't really know were they might be...
  	DeleteRegKey /ifempty HKLM "SOFTWARE\Ericsson\Erlang\${ERTS_VERSION}"
  	DeleteRegKey /ifempty HKCU "SOFTWARE\Ericsson\Erlang\${ERTS_VERSION}"
  	DeleteRegKey HKLM \
		"SOFTWARE\Microsoft\Windows\CurrentVersion\Uninstall\Erlang OTP ${MUI_VERSION} (${ERTS_VERSION})"
  	DeleteRegKey HKCU \
		"SOFTWARE\Microsoft\Windows\CurrentVersion\Uninstall\Erlang OTP ${MUI_VERSION} (${ERTS_VERSION})"


; Now remove shell/file associations we'we made...
; .erl
  	ReadRegStr $1 HKCR ".erl" ""
  	StrCmp $1 "ErlangSource" 0 NoOwnSourceExt 
    	ReadRegStr $1 HKCR ".erl" "backup_val"
    	StrCmp $1 "" 0 RestoreBackupSource 
      	DeleteRegKey HKCR ".erl"
    	Goto NoOwnSourceExt
RestoreBackupSource:
      	WriteRegStr HKCR ".erl" "" $1
      	DeleteRegValue HKCR ".erl" "backup_val"
NoOwnSourceExt:
	
  	ReadRegStr $1 HKCR "ErlangSource\DefaultIcon" ""
	StrCmp $1 "$INSTDIR\usr\lib\icons\erl_icon.ico" 0 NoOwnSource 
  	DeleteRegKey HKCR "ErlangSource"
NoOwnSource:

;.hrl
  	ReadRegStr $1 HKCR ".hrl" ""
  	StrCmp $1 "ErlangHeader" 0 NoOwnHeaderExt 
    	ReadRegStr $1 HKCR ".hrl" "backup_val"
    	StrCmp $1 "" 0 RestoreBackupHeader 
      	DeleteRegKey HKCR ".hrl"
    	Goto NoOwnHeaderExt
RestoreBackupHeader:
      	WriteRegStr HKCR ".hrl" "" $1
      	DeleteRegValue HKCR ".hrl" "backup_val"
NoOwnHeaderExt:
	
  	ReadRegStr $1 HKCR "ErlangHeader\DefaultIcon" ""
	StrCmp $1 "$INSTDIR\usr\lib\icons\hrl_icon.ico" 0 NoOwnHeader 
  	DeleteRegKey HKCR "ErlangHeader"
NoOwnHeader:

;.beam
  	ReadRegStr $1 HKCR ".beam" ""
  	StrCmp $1 "ErlangBeam" 0 NoOwnBeamExt 
    	ReadRegStr $1 HKCR ".beam" "backup_val"
    	StrCmp $1 "" 0 RestoreBackupBeam 
      	DeleteRegKey HKCR ".beam"
    	Goto NoOwnBeamExt
RestoreBackupBeam:
      	WriteRegStr HKCR ".beam" "" $1
      	DeleteRegValue HKCR ".beam" "backup_val"
NoOwnBeamExt:
	
  	ReadRegStr $1 HKCR "ErlangBeam\DefaultIcon" ""
	StrCmp $1 "$INSTDIR\usr\lib\icons\beam_icon.ico" 0 NoOwnBeam 
  	DeleteRegKey HKCR "ErlangBeam"
NoOwnBeam:

;Display the Finish header
  	!insertmacro MUI_UNFINISHHEADER

SectionEnd
	!verbose 3
