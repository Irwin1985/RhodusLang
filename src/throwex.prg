&& ======================================================================== &&
&& Default code snippet when user issue: do ThrowEx.prg
&& ======================================================================== &&
If Type("_Screen.oException") = "U"
	=AddProperty(_Screen, "oException", .Null.)
EndIf
_Screen.oException = Createobject("CustomException")
&& ======================================================================== &&
&& ThrowException.
&& ======================================================================== &&
Function ThrowException As Void
	Lparameters tcMessage As Message, tnErrorNo As Integer, tcDetails As Memo
	If Type("_Screen.oException") = "O"
		_Screen.oException.ThrowException(tcMessage, tnErrorNo, tcDetails)
	EndIf
Endfunc
&& ======================================================================== &&
&& NewException.
&& ======================================================================== &&
Function NewException As Void
	Lparameters tcMessage As Message, tnErrorNo As Integer, tcDetails As Memo
	ThrowException(tcMessage, tnErrorNo, tcDetails)
Endfunc
&& ======================================================================== &&
&& ReleaseException
&& ======================================================================== &&
Function ReleaseException As Void
	Try
		Clear Class CustomException
	Catch
	Endtry
Endfunc
&& ======================================================================== &&
&& Class ThrowException
&& ======================================================================== &&
Define Class CustomException As Custom
&& ======================================================================== &&
&& Function ThrowException
&& ======================================================================== &&
	Function ThrowException As Void
		Lparameters tcMessage As Message, tnErrorNo As Integer, tcDetails As Memo
		Local oExp, lnLevel, laStack[1]
		Try
			Throw
		Catch To oExp
			lnLevel				= Astackinfo(laStack) - 1
			oExp.Message		= Evl(tcMessage, "")
			oExp.ErrorNo		= Evl(tnErrorNo, oExp.ErrorNo)
			oExp.Details		= Evl(tcDetails, "")
			oExp.Procedure		= laStack[lnLevel, 3]
			oExp.Lineno			= laStack[lnLevel, 5]
			oExp.LineContents	= laStack[lnLevel, 6]
			oExp.StackLevel		= lnLevel
			Throw
		Endtry
		Return
	Endfunc
Enddefine