&& ======================================================================== &&
&& Class Scanner
&& ======================================================================== &&
Define Class RhodusLexer As Session
	#Define UNDER_SCORE 	"_"
	#Define C_DOT 			"."
	#Define C_SPACE			Space(1)
	#Define C_TAB 			Chr(9)
	#Define C_SLASH			"/"
	#Define DOUBLE_QUOTE 	Chr(34)
	#Define EOF_CHAR		Chr(255)
	#Define CR 				Chr(13)
	#Define LF				Chr(10)
	#Define MAX_INT			2147483647
	#Define MAX_EXPONENT	309

	oTokenCode 		= .Null.
	oTokenRecord	= .Null.
	Token 			= ""
	tokenString 	= ""
	tokenInteger 	= 0
	tokenFloat 		= 0.00

	Hidden Fch
	Hidden nColumnNumber
	Hidden nLineNumber
	Hidden oReader
	Hidden lInMultiLineComment
	Hidden nKeywordCounter
	Hidden cVersion

	Dimension KeyWordList(1, 2)
&& ======================================================================== &&
&& Function Init
&& ======================================================================== &&
	Function Init
		Set Procedure To "StreamReader" Additive
		Set Procedure To "eTokenCode"	Additive
		With This
			.cVersion		 = "0.1"
			.Fch 			 = ""
			.nColumnNumber   = 0
			.nLineNumber	 = 0
			.oReader         = Createobject("StreamReader")
			.oTokenCode 	 = Createobject("eTokenCode")
			.nKeywordCounter = 0
			.CreateTokenRecord()
		Endwith
	Endfunc
&& ======================================================================== &&
&& Function StartScanner
&& ======================================================================== &&
	Hidden Function StartScanner As Void
		With This
			.nLineNumber	 = 1
			.nColumnNumber   = 0
			.nKeywordCounter = 0
			.AddKeyWords()
			.Fch = This.NextChar()
		Endwith
	Endfunc
&& ======================================================================== &&
&& Function ScanString
&& ======================================================================== &&
	Function ScanString As Void
		Lparameters tcString As String
		If !Empty(tcString)
			This.oReader.SetString(tcString)
			This.StartScanner()
		Else
			Error "Empty String"
		Endif
	Endfunc
&& ======================================================================== &&
&& Function ScanFile
&& ======================================================================== &&
	Function ScanFile As Void
		Lparameters tcFileName As String
		If File(tcFileName)
			This.oReader.SetString(Filetostr(tcFileName))
			This.StartScanner()
		Else
			Error "File not found: " + tcFileName
		Endif
	Endfunc
&& ======================================================================== &&
&& Function NextToken
&& ======================================================================== &&
	Function NextToken As Void
		This.SkipBlanksAndComments()
* Record the position of the token that we are about to identify.
		This.oTokenRecord.LineNumber 	= This.nLineNumber
		This.oTokenRecord.ColumnNumber 	= This.nColumnNumber

		If This.Fch != EOF_CHAR
			Do Case
			Case This.IsLetter(This.Fch)
				This.Getword()

			Case Isdigit(This.Fch) Or This.Fch == C_DOT
				This.GetNumber()

			Case This.Fch == DOUBLE_QUOTE
				This.GetString()

			Otherwise
				This.GetSpecial()
			Endcase
		Else
			This.oTokenRecord.Token = This.oTokenCode.tEndOfStream
			If This.lInMultiLineComment
				Error 'Detecting unterminated comment, expecting "*/"'
			Endif
		Endif
	Endfunc
&& ======================================================================== &&
&& Function GetSpecial
&& ======================================================================== &&
	Function GetSpecial As Character
		Do Case
		Case This.Fch == "+"
			This.oTokenRecord.Token = This.oTokenCode.tPlus
		Case This.Fch == "^"
			This.oTokenRecord.Token = This.oTokenCode.tPower
		Case This.Fch == "("
			This.oTokenRecord.Token = This.oTokenCode.tLeftParenthesis
		Case This.Fch == ")"
			This.oTokenRecord.Token = This.oTokenCode.tRightParenthesis
		Case This.Fch == "["
			This.oTokenRecord.Token = This.oTokenCode.tLeftCurleyBracket
		Case This.Fch == "]"
			This.oTokenRecord.Token = This.oTokenCode.tRightCurleyBracket
		Case This.Fch == "!"
			If This.oReader.Peek() == "="
				This.Fch = This.NextChar()
				This.oTokenRecord.Token = This.oTokenCode.tNotEqual
			Else
				Error "unexpecting '=' character after explanation point: " + This.Fch
			Endif
		Case This.Fch == ">"
			If This.oReader.Peek() == "="
				This.Fch = This.NextChar()
				This.oTokenRecord.Token = This.oTokenCode.tMoreThanOrEqual
			Else
				This.oTokenRecord.Token = This.oTokenCode.tMoreThan
			Endif
		Case This.Fch == "<"
			If This.oReader.Peek() == "="
				This.Fch = This.NextChar()
				This.oTokenRecord.Token = This.oTokenCode.tLessThanOrEqual
			Else
				This.oTokenRecord.Token = This.oTokenCode.tLessThan
			Endif
		Case This.Fch == "="
			If This.oReader.Peek() == "="
				This.Fch = This.NextChar()
				This.oTokenRecord.Token = This.oTokenCode.tEquivalence
			Else
				This.oTokenRecord.Token = This.oTokenCode.tEquals
			Endif
		Case This.Fch == ";"
			This.oTokenRecord.Token = This.oTokenCode.tSemicolon

		Case This.Fch == ":"
			This.oTokenRecord.Token = This.oTokenCode.tColon

		Case This.Fch == ","
			This.oTokenRecord.Token = This.oTokenCode.tComma

		Case This.Fch == "''"
			This.oTokenRecord.Token = This.oTokenCode.tApostrophy

		Case This.Fch == "-"
			This.oTokenRecord.Token = This.oTokenCode.tMinus

		Case This.Fch == "/"
			This.oTokenRecord.Token = This.oTokenCode.tDivide

		Case This.Fch == "*"
			This.oTokenRecord.Token = This.oTokenCode.tMult

		Otherwise
			Error "unrecongnised character in source code: " + Transform(This.Fch)
		Endcase
		This.Fch = This.NextChar()
	Endfunc
&& ======================================================================== &&
&& Function AddKeyWords
&& Add the Rhodus supported keywords here
&& Could be refactored to use a TDictionary
&& ======================================================================== &&
	Function AddKeyWords
		Local loTokenCode As Object
		loTokenCode = This.oTokenCode
		With This
			.AddKeyWordToArray("if", loTokenCode.tIf)
			.AddKeyWordToArray("do", loTokenCode.tDo)
			.AddKeyWordToArray("to", loTokenCode.tTo)
			.AddKeyWordToArray("or", loTokenCode.tOr)
			.AddKeyWordToArray("of", loTokenCode.tOf)
			.AddKeyWordToArray("end", loTokenCode.tEnd)
			.AddKeyWordToArray("for", loTokenCode.tFor)
			.AddKeyWordToArray("and", loTokenCode.tAnd)
			.AddKeyWordToArray("xor", loTokenCode.tXor)
			.AddKeyWordToArray("not", loTokenCode.tNot)
			.AddKeyWordToArray("div", loTokenCode.tDiv)
			.AddKeyWordToArray("mod", loTokenCode.tMod)
			.AddKeyWordToArray("then", loTokenCode.tThen)
			.AddKeyWordToArray("else", loTokenCode.tElse)
			.AddKeyWordToArray("True", loTokenCode.tTrue)
			.AddKeyWordToArray("False", loTokenCode.tFalse)
			.AddKeyWordToArray("while", loTokenCode.tWhile)
			.AddKeyWordToArray("until", loTokenCode.tUntil)
			.AddKeyWordToArray("break", loTokenCode.tBreak)
			.AddKeyWordToArray("repeat", loTokenCode.tRepeat)
			.AddKeyWordToArray("downto", loTokenCode.tDownTo)
			.AddKeyWordToArray("function", loTokenCode.tFunction)
			.AddKeyWordToArray("print", loTokenCode.tPrint)
		Endwith
	Endfunc
&& ======================================================================== &&
&& Function AddKeyWordToArray
&& ======================================================================== &&
	Function AddKeyWordToArray
		Lparameters tcKeyWord As String, tnTokenCode As Integer
		This.nKeywordCounter = This.nKeywordCounter + 1
		Dimension This.KeyWordList(This.nKeywordCounter, 2)
		This.KeyWordList[this.nKeywordCounter, 1] = tcKeyWord
		This.KeyWordList[this.nKeywordCounter, 2] = tnTokenCode
	Endfunc
&& ======================================================================== &&
&& Function IsKeyWord
&& ======================================================================== &&
	Function IsKeyWord As Boolean
		Lparameters tcTokenString As String
		Local lResult As Boolean
		For i = 1 To Alen(This.KeyWordList, 1)
			If This.KeyWordList[i, 1] == tcTokenString
				This.oTokenRecord.Token = This.KeyWordList[i, 2]
				lResult = .T.
				Exit
			Endif
		Endfor
		Return lResult
	Endfunc
&& ======================================================================== &&
&& Function SkipBlanksAndComments
&& ======================================================================== &&
	Function SkipBlanksAndComments As Void
		Do While Inlist(This.Fch, C_SPACE, C_TAB, C_SLASH)
			If Inlist(This.Fch, C_SPACE, C_TAB)
				This.Fch = This.NextChar()
			Else
				lcChar = This.oReader.Peek()
				If lcChar $ "/*"
					This.Fch = This.GetOS_IndependentChar()
					Do Case
					Case This.Fch == "/"
						This.SkipSingleLineComment()
					Case This.Fch == "*"
						This.SkipMultiLineComment()
					Endcase
				Else
					Exit
				Endif
			Endif
		Enddo
	Endfunc
&& ======================================================================== &&
&& Function SkipSingleLineComment
&& ======================================================================== &&
	Function SkipSingleLineComment As Void
		Do While (This.Fch != LF) And (This.Fch != EOF_CHAR)
			This.Fch = This.GetOS_IndependentChar()
		Enddo
		If This.Fch == LF
			This.Fch = This.NextChar()
			This.nLineNumber = This.nLineNumber + 1
		Endif
	Endfunc
&& ======================================================================== &&
&& Function SkipMultiLineComment
&& Deal with this kind of comment /* ... */
&& ======================================================================== &&
	Function SkipMultiLineComment As Void
		This.lInMultiLineComment = .T.
* Move past '*'
		This.Fch = This.NextChar()
		Do While .T.
			Do While (This.Fch != "*") And (This.Fch != EOF_CHAR)
				This.Fch = This.NextChar()
			Enddo
			If This.Fch == EOF_CHAR
				Exit
			Endif
			This.Fch = This.NextChar()
			If This.Fch == "/"
				This.Fch = This.NextChar()
				This.lInMultiLineComment = .F.
				Exit
			Endif
		Enddo
	Endfunc
&& ======================================================================== &&
&& Function GetString
&& Get a token of the form "A string"
&& ======================================================================== &&
	Function GetString As Void
		This.oTokenRecord.tokenString = ""
		This.oTokenRecord.Token = This.oTokenCode.tString
* Move past '"'
		This.Fch = This.NextChar()
		Do While This.Fch != EOF_CHAR
			If This.Fch == "\"
				This.Fch = This.NextChar()
				Do Case
				Case This.Fch == "\"
					This.oTokenRecord.tokenString = This.oTokenRecord.tokenString + "\"
				Case This.Fch == "n"
					This.oTokenRecord.tokenString = This.oTokenRecord.tokenString + LF
				Case This.Fch == "r"
					This.oTokenRecord.tokenString = This.oTokenRecord.tokenString + CR
				Case This.Fch == "t"
					This.oTokenRecord.tokenString = This.oTokenRecord.tokenString + C_TAB
				Otherwise
					This.oTokenRecord.tokenString = This.oTokenRecord.tokenString + "\" + This.Fch
				Endcase
				This.Fch = This.NextChar()
			Else
				If This.Fch == DOUBLE_QUOTE && Empty string or terminated string.
					This.Fch = This.NextChar()
					Exit
				Else
					This.oTokenRecord.tokenString = This.oTokenRecord.tokenString + This.Fch
					This.Fch = This.NextChar()
				Endif
			Endif
		Enddo
	Endfunc
&& ======================================================================== &&
&& Function GetWord
&& Scan in an Identifier token
&& ======================================================================== &&
	Function Getword As Void
		This.oTokenRecord.tokenString = ""
		Do While This.IsLetter(This.Fch) Or Isdigit(This.Fch)
			This.oTokenRecord.tokenString = This.oTokenRecord.tokenString + This.Fch
			This.Fch = This.NextChar()
		Enddo
		If !This.IsKeyWord(This.oTokenRecord.tokenString)
			This.oTokenRecord.Token = This.oTokenCode.tIdentifier
		Endif
	Endfunc
&& ======================================================================== &&
&& Function GetNumber
&& ======================================================================== &&
	Function GetNumber
		Local lnSingleDigit As Integer, lnScale As Double, lneValue As Integer, ;
			lnExponentSign As Integer, lHasLeftHandSide As Boolean, lHasRightHandSize As Boolean
		This.oTokenRecord.tokenInteger 	= 0
		This.oTokenRecord.tokenFloat 	= 0.0
		Store .F. To lHasLeftHandSide, lhasRightHandSide
* Assume it's an integer
		This.oTokenRecord.Token = This.oTokenCode.tInteger
* check for decimal point just in case user has typed something like .5
		If This.Fch != "."
			lHasLeftHandSide = .T.
			Do While .T.
				lnSingleDigit = Val(This.Fch)
				If This.oTokenRecord.tokenInteger <= Int((MAX_INT - lnSingleDigit) / 10)
					This.oTokenRecord.tokenInteger = 10 * This.oTokenRecord.tokenInteger + lnSingleDigit
					This.Fch = This.NextChar()
				Else
					Error "integer overflow, constant value too large to read"
					Exit
				Endif
				If !Isdigit(This.Fch)
					Exit
				Endif
			Enddo
		Endif
		lnScale = 1
		If This.Fch == "."
* Then it's a float. Start collecting fractional part
			This.oTokenRecord.Token 	 = This.oTokenCode.tFLOAT
			This.oTokenRecord.tokenFloat = This.oTokenRecord.tokenInteger
			This.Fch = This.NextChar()
			If Isdigit(This.Fch)
				lhasRightHandSide = .T.
			Endif
			Do While Isdigit(This.Fch)
				lnScale = lnScale * 0.1
				lnSingleDigit = Val(This.Fch)
				This.oTokenRecord.tokenFloat = This.oTokenRecord.tokenFloat + (lnSingleDigit * lnScale)
				This.Fch = This.NextChar()
			Enddo
		Endif
* Check there is actually a number
		If !lHasLeftHandSide And !lhasRightHandSide
			Error "single period on its own is not a valid number"
		Endif
		lnExponentSign = 1
*Next check for scientific notation
		If Inlist(This.Fch, "e", "E")
* Then it's a float. Start collecting exponent part
			If This.oTokenRecord.Token = This.oTokenCode.tInteger
				This.oTokenRecord.Token = This.oTokenCode.tFLOAT
				This.oTokenRecord.tokenFloat = This.oTokenRecord.tokenInteger
			Endif
			This.Fch = This.NextChar()
			If Inlist(This.Fch, "-", "+")
				If This.Fch == "-"
					lnExponentSign = -1
				Endif
				This.Fch = This.NextChar()
			Endif
* accumulate exponent, check that first ch is a digit
			If !Isdigit(This.Fch)
				Error "syntax error: number expected in exponent"
			Endif
			lneValue = 0
			Do While .T.
				lnSingleDigit = Val(This.Fch)
				If lneValue <= Int((MAX_EXPONENT - lnSingleDigit) / 10)
					lneValue = 10 * lneValue + lnSingleDigit
					This.Fch = This.NextChar()
				Else
					Error "exponent overflow, maximum value for exponent is " + Alltrim(Str(MAX_EXPONENT))
					Exit
				Endif
				If !Isdigit(This.Fch)
					Exit
				Endif
			Enddo
			lneValue = lneValue * lnExponentSign
			If This.Token = This.oTokenCode.tInteger
				This.oTokenRecord.tokenFloat = This.oTokenRecord.tokenInteger ** lneValue
			Else
				This.oTokenRecord.tokenFloat = This.oTokenRecord.tokenFloat ** lneValue
			Endif
		Endif
	Endfunc
&& ======================================================================== &&
&& Function ReadRawChar
&& Get a single char from the input stream.
&& ======================================================================== &&
	Hidden Function ReadRawChar As Character
		Local lcResult As String
		lcResult = ""
		If This.oReader.EndOfStream
			lcResult = EOF_CHAR
		Else
			This.nColumnNumber = This.nColumnNumber + 1
			lcResult = This.oReader.Read()
		Endif
		Return lcResult
	Endfunc
&& ======================================================================== &&
&& Function GetOS_IndependentChar
&& ======================================================================== &&
	Hidden Function GetOS_IndependentChar As Character
		This.Fch = This.ReadRawChar()
		If This.Fch = CR Or This.Fch = LF
			If This.Fch = CR
				This.Fch = This.ReadRawChar()
				If This.Fch != LF
					Error "Expecting line feed character."
				Endif
			Endif
		Endif
		Return This.Fch
	Endfunc
&& ======================================================================== &&
&& Function NextChar
&& return the next char in the input stream
&& filter out the LF and increment the line number.
&& ======================================================================== &&
	Hidden Function NextChar As Character
		Local lcChar As Character
		lcChar = This.GetOS_IndependentChar()
* Ignore LF and return the next character.
		If lcChar == LF
			This.nLineNumber 	= This.nLineNumber + 1
			This.nColumnNumber  = 0
			lcChar = " "
		Endif
		Return lcChar
	Endfunc
&& ======================================================================== &&
&& Function ToString
&& ======================================================================== &&
	Function ToString As String
		Lparameters tnTokenCode As Integer
		Local lcTokenStr As String
		lcTokenStr = ""
		Do Case
		Case tnTokenCode = This.oTokenCode.tIdentifier
			lcTokenStr = "identifier: <'" + This.oTokenRecord.tokenString + "'>"
		Case tnTokenCode = This.oTokenCode.tInteger
			lcTokenStr = "integer: <'" + Alltrim(Str(This.oTokenRecord.tokenInteger)) + "'>"
		Case tnTokenCode = This.oTokenCode.tFLOAT
			lcTokenStr = "float: <'" + Transform(This.oTokenRecord.tokenFloat) + "'>"
		Case tnTokenCode = This.oTokenCode.tString
			lcTokenStr = "string: <'" + Transform(This.oTokenRecord.tokenString) + "'>"
		Case tnTokenCode = This.oTokenCode.tMinus
			lcTokenStr = "special: <'-'>"
		Case tnTokenCode = This.oTokenCode.tPlus
			lcTokenStr = "special: <'+'>"
		Case tnTokenCode = This.oTokenCode.tMult
			lcTokenStr = "special: <'*'>"
		Case tnTokenCode = This.oTokenCode.tDivide
			lcTokenStr = "special: <'/'>"
		Case tnTokenCode = This.oTokenCode.tPower
			lcTokenStr = "special: <'*'>"
		Case tnTokenCode = This.oTokenCode.tRightParenthesis
			lcTokenStr = "special: <')'>"
		Case tnTokenCode = This.oTokenCode.tLeftParenthesis
			lcTokenStr = "special: <'('>"
		Case tnTokenCode = This.oTokenCode.tRightBracket
			lcTokenStr = "special: <']'>"
		Case tnTokenCode = This.oTokenCode.tLeftBracket
			lcTokenStr = "special: <'['>"
		Case tnTokenCode = This.oTokenCode.tLeftCurleyBracket
			lcTokenStr = "special: <'}'>"
		Case tnTokenCode = This.oTokenCode.tRightCurleyBracket
			lcTokenStr = "special: <'{'>"
		Case tnTokenCode = This.oTokenCode.tEquals
			lcTokenStr = "special: <'='>"
		Case tnTokenCode = This.oTokenCode.tEquivalence
			lcTokenStr = "special: <'=='>"
		Case tnTokenCode = This.oTokenCode.tMoreThan
			lcTokenStr = "special: <'>'>"
		Case tnTokenCode = This.oTokenCode.tLessThan
			lcTokenStr = "special: <'<'>"
		Case tnTokenCode = This.oTokenCode.tMoreThanOrEqual
			lcTokenStr = "special: <'>='>"
		Case tnTokenCode = This.oTokenCode.tLessThanOrEqual
			lcTokenStr = "special: <'<='>"
		Case tnTokenCode = This.oTokenCode.tApostrophy
			lcTokenStr = "Apostrophy: <'>"
		Case tnTokenCode = This.oTokenCode.tSemicolon
			lcTokenStr = "special: <';'>"
		Case tnTokenCode = This.oTokenCode.tColon
			lcTokenStr = "special: <':'>"
		Case tnTokenCode = This.oTokenCode.tComma
			lcTokenStr = "special: <','>"
		Case tnTokenCode = This.oTokenCode.tDollar
			lcTokenStr = "special: <'$'>"
		Case tnTokenCode = This.oTokenCode.tEnd
			lcTokenStr = "key word: <'end'>"
		Case tnTokenCode = This.oTokenCode.tIf
			lcTokenStr = "key word: <'if'>"
		Case tnTokenCode = This.oTokenCode.tThen
			lcTokenStr = "key word: <'then'>"
		Case tnTokenCode = This.oTokenCode.tFor
			lcTokenStr = "key word: <'for'>"
		Case tnTokenCode = This.oTokenCode.tTo
			lcTokenStr = "key word: <'to'>"
		Case tnTokenCode = This.oTokenCode.tWhile
			lcTokenStr = "key word: <'while'>"
		Case tnTokenCode = This.oTokenCode.tDo
			lcTokenStr = "key word: <'do'>"
		Case tnTokenCode = This.oTokenCode.tElse
			lcTokenStr = "key word: <'else'>"
		Case tnTokenCode = This.oTokenCode.tRepeat
			lcTokenStr = "key word: <'repeat'>"
		Case tnTokenCode = This.oTokenCode.tUntil
			lcTokenStr = "key word: <'until'>"
		Case tnTokenCode = This.oTokenCode.tOf
			lcTokenStr = "key word: <'of'>"
		Case tnTokenCode = This.oTokenCode.tBreak
			lcTokenStr = "key word: <'break'>"
		Case tnTokenCode = This.oTokenCode.tFunction
			lcTokenStr = "key word: <'function'>"
		Case tnTokenCode = This.oTokenCode.tPrint
			lcTokenStr = "key word: <'print'>"
		Case tnTokenCode = This.oTokenCode.tEndOfStream
			lcTokenStr = "End of stream"
		Otherwise
			lcTokenStr = "unrecognized token " + Alltrim(Str(tnTokenCode))
		Endcase
		Return lcTokenStr
	Endfunc
&& ======================================================================== &&
&& Function IsLetter
&& ======================================================================== &&
	Function IsLetter As Boolean
		Lparameters tcChar As Character
		Return Between(Asc(tcChar), Asc("a"), Asc("z")) Or ;
			Between(Asc(tcChar), Asc("A"), Asc("Z")) Or ;
			Asc(tcChar) = Asc("_")
	Endfunc
&& ======================================================================== &&
&& Function Destroy
&& ======================================================================== &&
	Function Destroy
		This.oReader = .Null.
		Try
			Clear Class StreamReader
		Catch
		Endtry
		Try
			Release Procedure StreamReader
		Catch
		Endtry
		This.oTokenCode = .Null.
		Try
			Clear Class eTokenCode
		Catch
		Endtry
		Try
			Release Procedure eTokenCode
		Catch
		EndTry
		try
			=ReleaseException()
		Catch
		EndTry
		This.oTokenRecord = .Null.
	Endfunc
&& ======================================================================== &&
&& Function Token_Access
&& ======================================================================== &&
	Function Token_Access
		Return This.GetTokenCode()
	Endfunc
	Function tokenString_Access
		Return This.GetTokenString()
	Endfunc
	Function tokenInteger_Access
		Return This.GetTokenInteger()
	Endfunc
	Function tokenFloat_Access
		Return This.GetTokenDouble()
	Endfunc
&& ======================================================================== &&
&& Function GetTokenCode
&& ======================================================================== &&
	Function GetTokenCode
		Return This.oTokenRecord.Token
	Endfunc
&& ======================================================================== &&
&& Function GetTokenString
&& ======================================================================== &&
	Function GetTokenString As String
		Return This.oTokenRecord.tokenString
	Endfunc
&& ======================================================================== &&
&& Function GetTokenInteger
&& ======================================================================== &&
	Function GetTokenInteger As Integer
		Return This.oTokenRecord.tokenInteger
	Endfunc
&& ======================================================================== &&
&& Function GetTokenDouble
&& ======================================================================== &&
	Function GetTokenDouble As Double
		Return This.oTokenRecord.TokenDouble
	Endfunc
&& ======================================================================== &&
&& Hidden Function CreateTokenRecord
&& ======================================================================== &&
	Hidden Function CreateTokenRecord
		This.oTokenRecord = Createobject("Empty")
		loTokenRecord = This.oTokenRecord
		AddProperty(loTokenRecord, "LineNumber", 0)
		AddProperty(loTokenRecord, "ColumnNumber", 0)
		AddProperty(loTokenRecord, "Token", 0)
		AddProperty(loTokenRecord, "TokenString", "")
		AddProperty(loTokenRecord, "TokenFloat", 0.00)
		AddProperty(loTokenRecord, "TokenInteger", 0)
	Endfunc
&& ======================================================================== &&
&& Function GetVersion
&& ======================================================================== &&
	Function GetVersion
		Return This.cVersion
	EndFunc
Enddefine
