#Requires AutoHotkey v2.0

#SingleInstance Force

/*
 Reminder:
   #    Windows Key
   ^    Ctrl
   !    Alt
   +    Shift
   <^>! AltGr (since the OS implements it as a combination of LCtrl and RAlt.
*/

; Define variables
;;;;;;;;;;;;;;;;;;

MsgBox "Ini file is: " . GetIniFile() . ", and its attributes are: " . FileExist(GetIniFile()) . "."

myFirstName   := GetIni("personal", "firstName")
myLastName    := GetIni("personal", "lastName")
myEmail       := GetIni("personal", "email")
myPhoneNumber := GetIni("personal", "phoneNumber")
myAddress     := GetIni("personal", "address")
myCity        := GetIni("personal", "city")
myPostalCode  := GetIni("personal", "postalCode")

myWorkEmail       := GetIni("work", "email")
myWorkPhoneNumber := GetIni("work", "phoneNumber")
myLinkedInProfile := GetIni("work", "linkedInProfile")


MsgBox ("Path used is: " . A_ScriptDir . "\privateSettings.ini")
MsgBox ("First name is: " . myFirstName)

; Send texts
;;;;;;;;;;;;

; Send variable text
^#!i::Send  GetCurrentDayString()

; Send pre-defined personal texts
^#!f::Send  myFirstName
^#!l::Send  myLastName
^#!e::Send  myEmail
^#!p::Send  myPhoneNumber
^#!a::Send  myAddress
^#!c::Send  myCity
^#!z::Send  myPostalCode

; Send pre-defined work text
^#!+e::Send myWorkEmail
^#!+l::Send myLinkedInProfile
^#!+p::Send myWorkPhoneNumber

; Interecept shortcuts
;;;;;;;;;;;;;;;;;;;;;;;;

; Remove the Windows + Space shortcut that switches language/keyboard layout
#space::InterceptedShortcut("Windows + Space")

; Same for Alt + Shift (in case it has been turned on)
!+::InterceptedShortcut("Alt + Shift")

; Do stuffs with selected text
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Conjuguate verb in French
^#!+v::Run "https://conjugaison.lemonde.fr/conjugaison/search?verb=" . GetSelectedText()

; Define current selection
^#!d::Run "https://dictionary.cambridge.org/fr/dictionnaire/anglais/" . GetSelectedText()

; Google current selection
^#!g::Run "https://www.google.com/search?q=" . GetSelectedText()

; Translate current selection into English
^#!t::Run "https://dictionary.cambridge.org/fr/dictionnaire/francais-anglais/" . GetSelectedText()

; Helpers functions
;;;;;;;;;;;;;;;;;;;

GetInifile()  {
  return A_ScriptDir . "\privateSettings.ini"
}


GetIni(section, key) {
  return IniRead(GetIniFile(), section, key)
}

GetSelectedText() {
	save := ClipboardAll()      ; Save current Clipboard state
	Send "^c"                   ; Overwrite clipboard with current selection
	Sleep 500                   ; Give it a chance to finish properly (?)
	selectedText := A_Clipboard ; Fetch the clipboard content as text
	A_Clipboard := save         ; Restore original Clipbboard
	save := ""                  ; Free the used variable (in case of a alrge clipboard)
	return selectedText         ; Return what we've got
}

GetCurrentDayString() {
	return FormatTime(A_Now, "yyyy-MM-dd")
}

InterceptedShortcut(message) {
	MsgBox("I caught the shortcut: " . message . "`n`nTry to NOT repeat that shortcut again...", "Wrong shortcut!", 64+262144)
}
