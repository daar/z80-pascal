UNIT Scanner;
(*<Implements the compiler scanner.

   This scanner is much a translation of Nikclaus Wirth's one described in his
   book "Compiler Construction" (ISBN 0-201-40353-6). *)
(*
  Copyright (c) 2017 Darius Blaszyk & Guillermo Martínez J.

  This software is provided 'as-is', without any express or implied
  warranty. In no event will the authors be held liable for any damages
  arising from the use of this software.

  Permission is granted to anyone to use this software for any purpose,
  including commercial applications, and to alter it and redistribute it
  freely, subject to the following restrictions:

    1. The origin of this software must not be misrepresented; you must not
    claim that you wrote the original software. If you use this software
    in a product, an acknowledgment in the product documentation would be
    appreciated but is not required.

    2. Altered source versions must be plainly marked as such, and must not be
    misrepresented as being the original software.

    3. This notice may not be removed or altered from any source
    distribution.
*)

INTERFACE

  USES
    Classes;

  TYPE
  (* Terminators.  Used to identify symbols.  See file minipas.atg. *)
    TpasTerminators = (
      ptNull = 0,
      ptComment,
      ptProgram,
      ptSemicolon,
      ptEnd,
      ptPeriod,
      ptConst,
      ptEql,
      ptPlus,
      ptMinus,
      ptType,
      ptCaret, { "^" }
      ptArray,
      ptLBrak,
      ptComma,
      ptRBrak,
      ptOf,
      ptVar,
      ptColon,
      ptProcedure,
      ptFunction,
      ptForward,
      ptLParen,
      ptRParen,
      ptBegin,
      ptBecomes,
      ptWhile,
      ptDo,
      ptRepeat,
      ptUntil,
      ptIf,
      ptThen,
      ptElse,
      ptFor,
      ptTo,
      ptDownTo,
      ptLess,
      ptGreat,
      ptLEql,
      ptGEql,
      ptNotEql,
      ptOr,
      ptTimes,
      ptDivide,
      ptDiv,
      ptMod,
      ptAnd,
      ptShr,
      ptShl,
      ptNot,
      ptNil,
      ptIdentifier,
      ptInteger,
      ptHexInteger,
      ptString,
      ptEof
    );



  (* Pascal scanner.

     It takes the source code from a @code(TStream). *)
    TPascalScanner = CLASS (TObject)
    PRIVATE
      fOrigin: TStream;
      fSymbolId: TpasTerminators;
      fSymbol: STRING;
      fForwardCharacter, fCharacter: CHAR;

      FUNCTION GetForwardCharacter: CHAR;
      PROCEDURE GetNextCharacter;
      PROCEDURE LookKeyword;
    PUBLIC
    (* Initializes the scanner, getting the first symbol from the stream.
       Should be called before to start the scan and after assigning a
       @link(Source) stream. *)
      PROCEDURE Initialize;
    (* Gets the next symbol. *)
      PROCEDURE GetNext;
    (* Checks if it is at the end of the source.

       Note that the end of the source may not be the end of the program. *)
      FUNCTION CheckEOF: BOOLEAN;
    (* Returns the name of the given symbol. *)
      FUNCTION GetSymbolName (CONST aSymbolId: TpasTerminators): STRING;

    (* Access to the origin stream. *)
      PROPERTY Source: TStream READ fOrigin WRITE fOrigin;
    (* Symbol identifier. *)
      PROPERTY SymbolId: TpasTerminators READ fSymbolId;
    (* Symbol. *)
      PROPERTY Symbol: STRING READ fSymbol;
    END;

IMPLEMENTATION

  USES
    sysutils;

  CONST
  (* Some lists to help identifying characters. *)
    lLetter     = ['A' .. 'Z', 'a' ..'z'];
    lDigit      = ['0' .. '9'];
    lHexDigit   = lDigit + ['A' .. 'F', 'a' .. 'f'];
    lAlfanum    = lLetter + lDigit;
  { Not actually used.  See "Keywords and identifiers.".
    lIdentFirst = lLetter + ['_']; }
    lIdentChar  = lAlfanum + ['_'];

(*
 * TPascalScanner
 ***************************************************************************)

  FUNCTION TPascalScanner.GetForwardCharacter: CHAR;
  BEGIN
    IF fForwardCharacter = #0 THEN fOrigin.Read (fForwardCharacter, 1);
    RESULT := fForwardCharacter
  END;



  PROCEDURE TPascalScanner.GetNextCharacter;
  BEGIN
    IF fForwardCharacter <> #0 THEN
    BEGIN
      fCharacter := fForwardCharacter;
      fForwardCharacter := #0
    END
    ELSE
      fOrigin.Read (fCharacter, 1)
  END;



  PROCEDURE TPascalScanner.LookKeyword;
  CONST
  { Keyword list in same order than in the EBNF file description. }
    Keywords: ARRAY [0..29] OF STRING = (
      'PROGRAM', 'CONST',     'TYPE',      'ARRAY',    'OF',
      'VAR',     'PROCEDURE', 'PROCEDURE', 'FUNCTION', 'FORWARD',
      'BEGIN',   'END',       'WHILE',     'DO',       'REPEAT',
      'UNTIL',   'IF',        'THEN',      'ELSE',     'FOR',
      'TO',      'DOWNTO',    'OR',        'DIV',      'MOD',
      'AND',     'SHR',       'SHL',       'NOT',      'NIL'
    );
  { Identifier of the keyword. }
    KeywordId: ARRAY [0..29] OF TpasTerminators = (
      ptProgram, ptConst,     ptType,      ptArray,    ptOf,
      ptVar,     ptProcedure, ptProcedure, ptFunction, ptForward,
      ptBegin,   ptEnd,       ptWhile,     ptDo,       ptRepeat,
      ptUntil,   ptIf,        ptThen,      ptElse,     ptFor,
      ptTo,      ptDownTo,    ptOr,        ptDiv,      ptMod,
      ptAnd,     ptShr,       ptShl,       ptNot,      ptNil
    );
  VAR
    Ndx: INTEGER;
  BEGIN
    fSymbol := UpperCase (fSymbol);
    FOR Ndx := LOW (Keywords) TO HIGH (Keywords) DO
      IF fSymbol = Keywords[Ndx] THEN
      BEGIN
        fSymbolId := KeywordId[Ndx];
        EXIT
      END
  END;



(* Initializes the scanner. *)
  PROCEDURE TPascalScanner.Initialize;
  BEGIN
    fForwardCharacter := #0; fCharacter := #0;
    SELF.GetNext
  END;



(* Gets next symbol. *)
  PROCEDURE TPascalScanner.GetNext;
  VAR
    Ndx: INTEGER;

  { Helper procedure to get comments. }
    PROCEDURE GetComment;
    BEGIN
      fSymbolId := ptComment;
      Ndx := 0;
      fSymbol := '';
      IF fCharacter = '{' THEN
      BEGIN
	SELF.GetNextCharacter;
	WHILE fCharacter <> '}' DO
	BEGIN
	  fSymbol := fSymbol + fCharacter;
	  INC (Ndx);
	  SELF.GetNextCharacter;
	END;
	SELF.GetNextCharacter;
      END
      ELSE BEGIN
	SELF.GetNextCharacter;
	REPEAT
	  IF fCharacter = '*' THEN
	  BEGIN
	    IF SELF.GetForwardCharacter = ')' THEN
	    BEGIN
	    { Needs two calls because "end comment" is two characters wide. }
	      SELF.GetNextCharacter; SELF.GetNextCharacter;
	      EXIT
	    END
	  END;
	  fSymbol := fSymbol + fCharacter;
	  INC (Ndx);
	  SELF.GetNextCharacter
	UNTIL FALSE
      END
    END;

  { Helper procedure to get identifiers and keywords. }
    PROCEDURE GetIdentifier;
    BEGIN
      fSymbolId := ptIdentifier;
      Ndx := 0;
      fSymbol := '';
      WHILE fCharacter IN lIdentChar DO
      BEGIN
	fSymbol := fSymbol + fCharacter;
	INC (Ndx);
	SELF.GetNextCharacter
      END;
      SELF.LookKeyword
    END;

  { Helper procedure to get numeric values. }
    PROCEDURE GetNumber;
    BEGIN
      fSymbolId := ptInteger;
      Ndx := 0;
      fSymbol := '';
      WHILE fCharacter IN lDigit DO
      BEGIN
	fSymbol := fSymbol + fCharacter;
	INC (Ndx);
	SELF.GetNextCharacter
      END
    END;

  { Helper procedure to get hexagesimal values. }
    PROCEDURE GetHexNumber;
    BEGIN
      fSymbolId := ptHexInteger;
      Ndx := 0;
      fSymbol := '';
      WHILE fCharacter IN lHexDigit DO
      BEGIN
	fSymbol := fSymbol + fCharacter;
	INC (Ndx);
	SELF.GetNextCharacter
      END
    END;

  { Helper procedure to get string values. }
    PROCEDURE GetString;
    BEGIN
      fSymbolId := ptString;
      Ndx := 0;
      fSymbol := '';
      SELF.GetNextCharacter;
      REPEAT
	IF fCharacter = '''' THEN
	BEGIN
	  IF SELF.GetForwardCharacter <> '''' THEN
	  BEGIN
	    SELF.GetNextCharacter;
	    EXIT
	  END;
	{ The "quote" character is two characters wide ("''"), so we need to
	  skip one of them. }
	  SELF.GetNextCharacter;
	END;
	fSymbol := fSymbol + fCharacter;
	INC (Ndx);
	SELF.GetNextCharacter
      UNTIL FALSE
    END;

  BEGIN
  { Ignore spaces. }
    WHILE (fOrigin.Position < fOrigin.Size) AND (fCharacter <= ' ') DO
      SELF.GetNextCharacter;
  { End of the file. }
    IF fOrigin.Position >= fOrigin.Size THEN
    BEGIN
      fSymbolId := ptEof;
      EXIT
    END;
  { Detect and extract the token. }
    fSymbol := fCharacter;
    CASE fCharacter OF
  { Keywords and identifiers. }
    'A' .. 'Z', 'a' .. 'z', '_' :
      GetIdentifier;
  { Constants. }
    '0' .. '9':
      GetNumber;
    '$':
      BEGIN
	SELF.GetNextCharacter;
	GetHexNumber
      END;
    '''':
      GetString;
  { Operators. }
    '=' :
      BEGIN
	fSymbolId := ptEql; SELF.GetNextCharacter
      END;
    '<' :
      BEGIN
	fSymbolId := ptLess; SELF.GetNextCharacter;
	IF fCharacter = '=' THEN
	BEGIN
	  fSymbol := fSymbol + fCharacter;
	  fSymbolId := ptLEql; SELF.GetNextCharacter
	END
      END;
    '>' :
      BEGIN
	fSymbolId := ptGreat; SELF.GetNextCharacter;
	IF fCharacter = '=' THEN
	BEGIN
	  fSymbol := fSymbol + fCharacter;
	  fSymbolId := ptGEql; SELF.GetNextCharacter
	END
      END;
    '+' :
      BEGIN
	fSymbolId := ptPlus; SELF.GetNextCharacter
      END;
    '-' :
      BEGIN
	fSymbolId := ptMinus; SELF.GetNextCharacter
      END;
    '*' :
      BEGIN
	fSymbolId := ptTimes; SELF.GetNextCharacter
      END;
    '/' :
      BEGIN
	fSymbolId := ptDivide; SELF.GetNextCharacter
      END;
  { Separators and other symbols. }
    ';':
      BEGIN
	fSymbolId := ptSemiColon; SELF.GetNextCharacter
      END;
    '.':
      BEGIN
	fSymbolId := ptPeriod; SELF.GetNextCharacter
      END;
    '{':
      GetComment;
    '(':
      BEGIN
	IF SELF.GetForwardCharacter = '*' THEN
	BEGIN
	  SELF.GetNextCharacter;
	  GetComment
	END
	ELSE BEGIN
	  fSymbolId := ptLParen; SELF.GetNextCharacter;
	END
      END;
    ')':
      BEGIN
	fSymbolId := ptRParen; SELF.GetNextCharacter
      END;
    ':' :
      BEGIN
	fSymbolId := ptColon; SELF.GetNextCharacter;
	IF fCharacter = '=' THEN
	BEGIN
	  fSymbol := fSymbol + fCharacter;
	  fSymbolId := ptBecomes; SELF.GetNextCharacter
        END
      END;
    ',':
      BEGIN
	fSymbolId := ptComma; SELF.GetNextCharacter
      END;
    '[':
      BEGIN
	fSymbolId := ptLBrak; SELF.GetNextCharacter
      END;
    ']':
      BEGIN
	fSymbolId := ptRBrak; SELF.GetNextCharacter
      END;
    '^':
      BEGIN
	fSymbolId := ptCaret; SELF.GetNextCharacter
      END;
    ELSE
      BEGIN
	fSymbolId := ptNull; SELF.GetNextCharacter;
      END
    END
  END;



(* Checks if it is at the end of the program. *)
  FUNCTION TPascalScanner.CheckEOF: BOOLEAN;
  BEGIN
    RESULT := SELF.fSymbolId = ptEof
  END;



(* Returns the symbol name. *)
  FUNCTION TPascalScanner.GetSymbolName
    (CONST aSymbolId: TpasTerminators): STRING;
  CONST
    Names: ARRAY [0..55] OF STRING = (
      '<null>',
      'comment',
      'PROGRAM',
      ';',
      'END',
      '.',
      'CONST',
      '=',
      '+',
      '-',
      'TYPE',
      'caret ("^")',
      'ARRAY',
      '[',
      ',',
      ']',
      'OF',
      'VAR',
      ':',
      'PROCEDURE',
      'FUNCTION',
      'FORWARD',
      '(',
      ')',
      'BEGIN',
      ':=',
      'WHILE',
      'DO',
      'REPEAT',
      'UNTIL',
      'IF',
      'THEN',
      'ELSE',
      'FOR',
      'TO',
      'DOWNTO',
      '<',
      '>',
      '<=',
      '>=',
      '<>',
      'OR',
      '*',
      '/',
      'DIV',
      'MOD',
      'AND',
      'SHR',
      'SHL',
      'NOT',
      'NIL',
      'identifier',
      'integer',
      'hexagesimal integer',
      'string',
      '<eof>'
    );
  BEGIN
    RESULT := Names[ORD (aSymbolId)]
  END;

END.
