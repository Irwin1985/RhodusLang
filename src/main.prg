=AddProperty(_Screen, "cPath", Addbs(Justpath(Sys(16))))
=AddProperty(_Screen, "oScanner", .Null.)

Set Century On
Set Date Dmy
Set Hours To 24
Set Path To (_Screen.cPath) Additive
Set Path To "src" Additive
Set Procedure To "RhodusLexer" Additive

_Screen.oScanner = Createobject("RhodusLexer")

Do Form Shell
Read Events
_Screen.oScanner = .Null.
=Removeproperty(_Screen, "cPath")
=Removeproperty(_Screen, "oScanner")
