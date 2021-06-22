&& ******************************************************************************************* &&
&&  PROGRAMA:        SYNTAXANALIZER.PRG
&&  AUTOR:           IRWIN
&&  FECHA:           22 July 2020, 16:33:10
&&  RESUMEN:         Analizador sintactico para Rohdus
&&  RENDIMIENTO: 	 Big-O(n²)
&& ******************************************************************************************* &&
Define Class SyntaxAnalysis As Custom
	sc = .Null.
	Hidden Code
&& ======================================================================== &&
&& Function init
&& ======================================================================== &&
	Function init
		Lparameters toSC As Object
		Set Procedure To "eTokenCode"	Additive
		this.sc 	= toSC
		this.Code 	= Createobject("eTokenCode")
	EndFunc
&& ======================================================================== &&
&& Function Program
&& program = assignment | outputstatement
&& ======================================================================== &&
	Function Program
		this.sc.NextToken()
		Do case
		Case this.sc.Token = This.Code.tIdentifier
			This.Assignment()
		Case this.sc.Token = This.Code.tPrint
			This.OutputStatement()
		Otherwise
			Error "Expecting assignment or print statement"
		EndCase
	EndFunc
&& ======================================================================== &&
&& Function Expression
&& expression = term | { ( '+' | '-' ) term}
&& ======================================================================== &&
	Function Expression
		This.Term()
		Do while InList(this.sc.Token, this.Code.tPlus, this.code.tMinus)
			this.sc.NextToken()
			This.Term()
		EndDo
	EndFunc
&& ======================================================================== &&
&& Function Term
&& term = factor | { ('*' | '/') factor}
&& ======================================================================== &&
	Function Term
		This.Factor()
		Do While InList(this.sc.Token, This.Code.tMult, This.Code.tDivide)
			this.sc.NextToken()
			This.Factor()
		EndDo
	EndFunc
&& ======================================================================== &&
&& Function Factor
&& factor = '(' expression ')' | number | variable
&& ======================================================================== &&
	Function Factor
		Do case
		Case this.sc.Token = this.Code.tInteger
			this.sc.NextToken()
		Case this.sc.Token = this.Code.tFloat
			this.sc.NextToken()
		Case this.sc.Token = this.Code.tIdentifier
			this.sc.NextToken()
		Case this.sc.Token = this.Code.tLeftParenthesis
			this.sc.NextToken()
			This.Expression()
			This.Expect(this.Code.tLeftParenthesis)
		Otherwise
			Error "expecting identifier, scalar or left parenthesis"
		EndCase
	EndFunc
&& ======================================================================== &&
&& Function Assignment
&& assignment = variable '=' expression
&& ======================================================================== &&
	Function Assignment
		This.Expect(this.Code.tIdentifier)
		This.Expect(this.Code.tEquals)
		This.Expression()
	EndFunc
&& ======================================================================== &&
&& Function OutputStatement
&& ======================================================================== &&
	Function OutputStatement
		This.Expect(this.Code.tPrint)
		This.Expression()
	EndFunc
&& ======================================================================== &&
&& Function Expect
&& Espera que el token pasado por parámetro sea igual al del lexer.
&& ======================================================================== &&
	Function Expect
		Lparameters tnTokenExpected As Integer
		If this.sc.Token = tnTokenExpected
			this.sc.NextToken()
		Else
			Error "Unexpected token"
		EndIf
	EndFunc
&& ======================================================================== &&
&& Function Destroy
&& ======================================================================== &&
	Function Destroy
		this.sc = .Null.
		Clear Class RhodusLexer
		Release Procedure RhodusLexer
	EndFunc
EndDefine
