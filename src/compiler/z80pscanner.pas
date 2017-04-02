UNIT z80pScanner;
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
      zptNull = 0,
      zptComment,
      zptProgram,
      zptSemicolon,
      zptEnd,
      zptPeriod,
      zptConst,
      zptEql,
      zptPlus,
      zptMinus,
      zptType,
      zptCaret, { "^" }
      zptArray,
      zptLBrak,
      zptComma,
      zptRBrak,
      zptOf,
      zptVar,
      zptColon,
      zptProcedure,
      zptFunction,
      zptForward,
      zptLParen,
      zptRParen,
      zptBegin,
      zptBecomes,
      zptWhile,
      zptDo,
      zptRepeat,
      zptUntil,
      zptIf,
      zptThen,
      zptElse,
      zptFor,
      zptTo,
      zptDownTo,
      zptLess,
      zptGreat,
      zptLEql,
      zptGEql,
      zptNotEql,
      zptOr,
      zptTimes,
      zptDivide,
      zptDiv,
      zptMod,
      zptAnd,
      zptShr,
      zptShl,
      zptNot,
      zptNil,
      zptIdentifier,
      zptInteger,
      zptHexInteger,
      zptString,
      zptEof
    );



  (* Pascal scanner.

     It takes the source code from a @code(TStream). *)
    Tz80PascalScanner = CLASS (TObject)
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
    (* Checks if it is at the end of the program. *)
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
 * Tz80PascalScanner
 ***************************************************************************)

  FUNCTION Tz80PascalScanner.GetForwardCharacter: CHAR;
  BEGIN
    IF fForwardCharacter = #0 THEN fOrigin.Read (fForwardCharacter, 1);
    RESULT := fForwardCharacter
  END;



  PROCEDURE Tz80PascalScanner.GetNextCharacter;
  BEGIN
    IF fForwardCharacter <> #0 THEN
    BEGIN
      fCharacter := fForwardCharacter;
      fForwardCharacter := #0
    END
    ELSE
      fOrigin.Read (fCharacter, 1)
  END;



  PROCEDURE Tz80PascalScanner.LookKeyword;
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
      zptProgram, zptConst,     zptType,      zptArray,    zptOf,
      zptVar,     zptProcedure, zptProcedure, zptFunction, zptForward,
      zptBegin,   zptEnd,       zptWhile,     zptDo,       zptRepeat,
      zptUntil,   zptIf,        zptThen,      zptElse,     zptFor,
      zptTo,      zptDownTo,    zptOr,        zptDiv,      zptMod,
      zptAnd,     zptShr,       zptShl,       zptNot,      zptNil
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
  PROCEDURE Tz80PascalScanner.Initialize;
  BEGIN
    fForwardCharacter := #0; fCharacter := #0;
    SELF.GetNext
  END;



(* Gets next symbol. *)
  PROCEDURE Tz80PascalScanner.GetNext;
  VAR
    Ndx: INTEGER;

  { Helper procedure to get comments. }
    PROCEDURE GetComment;
    BEGIN
      fSymbolId := zptComment;
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
      fSymbolId := zptIdentifier;
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
      fSymbolId := zptInteger;
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
      fSymbolId := zptHexInteger;
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
      fSymbolId := zptString;
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
      fSymbolId := zptEof;
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
	fSymbolId := zptEql; SELF.GetNextCharacter
      END;
    '<' :
      BEGIN
	fSymbolId := zptLess; SELF.GetNextCharacter;
	IF fCharacter = '=' THEN
	BEGIN
	  fSymbol := fSymbol + fCharacter;
	  fSymbolId := zptLEql; SELF.GetNextCharacter
	END
      END;
    '>' :
      BEGIN
	fSymbolId := zptGreat; SELF.GetNextCharacter;
	IF fCharacter = '=' THEN
	BEGIN
	  fSymbol := fSymbol + fCharacter;
	  fSymbolId := zptGEql; SELF.GetNextCharacter
	END
      END;
    '+' :
      BEGIN
	fSymbolId := zptPlus; SELF.GetNextCharacter
      END;
    '-' :
      BEGIN
	fSymbolId := zptMinus; SELF.GetNextCharacter
      END;
    '*' :
      BEGIN
	fSymbolId := zptTimes; SELF.GetNextCharacter
      END;
    '/' :
      BEGIN
	fSymbolId := zptDivide; SELF.GetNextCharacter
      END;
  { Separators and other symbols. }
    ';':
      BEGIN
	fSymbolId := zptSemiColon; SELF.GetNextCharacter
      END;
    '.':
      BEGIN
	fSymbolId := zptPeriod; SELF.GetNextCharacter
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
	  fSymbolId := zptLParen; SELF.GetNextCharacter;
	END
      END;
    ')':
      BEGIN
	fSymbolId := zptRParen; SELF.GetNextCharacter
      END;
    ':' :
      BEGIN
	fSymbolId := zptColon; SELF.GetNextCharacter;
	IF fCharacter = '=' THEN
	BEGIN
	  fSymbol := fSymbol + fCharacter;
	  fSymbolId := zptBecomes; SELF.GetNextCharacter
        END
      END;
    ',':
      BEGIN
	fSymbolId := zptComma; SELF.GetNextCharacter
      END;
    '[':
      BEGIN
	fSymbolId := zptLBrak; SELF.GetNextCharacter
      END;
    ']':
      BEGIN
	fSymbolId := zptRBrak; SELF.GetNextCharacter
      END;
    '^':
      BEGIN
	fSymbolId := zptCaret; SELF.GetNextCharacter
      END;
    ELSE
      BEGIN
	fSymbolId := zptNull; SELF.GetNextCharacter;
      END
    END
  END;



(* Checks if it is at the end of the program. *)
  FUNCTION Tz80PascalScanner.CheckEOF: BOOLEAN;
  BEGIN
    RESULT := SELF.fSymbolId = zptEof
  END;



(* Returns the symbol name. *)
  FUNCTION Tz80PascalScanner.GetSymbolName
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
