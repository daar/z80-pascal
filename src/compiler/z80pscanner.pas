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
      zptProgram,
      zptSemicolon,
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
      zptEnd,
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
      fCharacter: CHAR;

      FUNCTION GetSymbolName: STRING;
      PROCEDURE LookKeyword;
    PUBLIC
    (* Initializes the scanner.  Should be called before to start the scan and
       after assigning a @link(Source) stream. *)
      PROCEDURE Initialize;
    (* Gets the next symbol. *)
      PROCEDURE GetNext;
    (* Check if it is at the end of the program. *)
      FUNCTION CheckEOF: BOOLEAN;

    (* Access to the origin stream. *)
      PROPERTY Source: TStream READ fOrigin WRITE fOrigin;
    (* Symbol identifier. *)
      PROPERTY SymbolId: TpasTerminators READ fSymbolId;
    (* Symbol name. *)
      PROPERTY SymbolName: STRING READ GetSymbolName;
    (* Symbol. *)
      PROPERTY Symbol: STRING READ fSymbol;
    END;

IMPLEMENTATION

  USES
    sysutils;

  CONST
  (* Some lists to help identifying characters. *)
    lLetter     = ['A' .. 'Z', 'a' ..'z'];
    lDigit      = ['0' .. '0'];
    lHexDigit   = lDigit + ['A' .. 'F', 'a' .. 'f'];
    lAlfanum    = lLetter + lDigit;
  { Not actually used.  See "Keywords and identifiers.".
    lIdentFirst = lLetter + ['_']; }
    lIdentChar  = lAlfanum + ['_'];

(*
 * Tz80PascalScanner
 ***************************************************************************)

  FUNCTION Tz80PascalScanner.GetSymbolName: STRING;
  CONST
    Names: ARRAY [0..53] OF STRING = (
      '<null>',
      'PROGRAM',
      ';',
      '.',
      'CONST',
      '=',
      '+',
      '-',
      'TYPE',
      'Caret ("^")',
      'ARRAY',
      '[',
      ',',
      ']',
      'VAR',
      ':',
      'PROCEDURE',
      'FUNCTION',
      'FORWARD',
      '(',
      ')',
      'BEGIN',
      'END',
      'BECOMES',
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
      'integer',
      'string',
      '<eof>'
    );
  BEGIN
    RESULT := Names[ORD (fSymbolId)]
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
    fCharacter := #0
  END;



(* Gets next symbol. *)
  PROCEDURE Tz80PascalScanner.GetNext;
  VAR
    Ndx: INTEGER;

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
	fOrigin.Read (fCharacter, 1)
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
	fOrigin.Read (fCharacter, 1)
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
	fOrigin.Read (fCharacter, 1)
      END
    END;

  { Helper procedure to ignore comments. }
    PROCEDURE JumpComment;
    BEGIN
      IF fCharacter  = '{' THEN
      BEGIN
	REPEAT
	  fOrigin.Read (fCharacter, 1);
	UNTIL fCharacter = '}';
	EXIT
      END;
    { This algorithm is adapted from Mr. Wirth's book. }
      WHILE TRUE DO
      BEGIN
	WHILE TRUE DO
	BEGIN
	  WHILE fCharacter = '(' DO
	  BEGIN
	    fOrigin.Read (fCharacter, 1);
	    IF fCharacter = '*' THEN JumpComment
	  END;
	  IF fCharacter = '*' THEN BREAK;
	  IF SELF.CheckEOF THEN BREAK;
	  fOrigin.Read (fCharacter, 1)
	END;
	IF fCharacter = ')' THEN
	BEGIN
	  fOrigin.Read (fCharacter, 1);
	  BREAK
	END;
	IF SELF.CheckEOF THEN RAISE Exception.Create ('Unexpected end of file.');
      END
    END;

  BEGIN
  { Ignore spaces. }
    WHILE (fOrigin.Position < fOrigin.Size) AND (fCharacter <= ' ') DO
      fOrigin.Read (fCharacter, 1);
  { Detect and extract the token. }
    fSymbol := fCharacter;
    CASE fCharacter OF
  { Keywords and identifiers. }
    'A' .. 'Z', 'a' .. 'z', '_' :
      GetIdentifier;
  { Numbers. }
    '0' .. '9':
      GetNumber;
  { Hexagesimal. }
    '$':
      BEGIN
	fOrigin.Read (fCharacter, 1);
	GetHexNumber
      END;
  { Operators. }
    '=' :
      BEGIN
	fSymbolId := zptEql; fOrigin.Read (fCharacter, 1)
      END;
    '<' :
      BEGIN
	fSymbolId := zptLess; fOrigin.Read (fCharacter, 1);
	IF fCharacter = '=' THEN
	BEGIN
	  fSymbolId := zptLEql; fOrigin.Read (fCharacter, 1)
	END
      END;
    '>' :
      BEGIN
	fSymbolId := zptGreat; fOrigin.Read (fCharacter, 1);
	IF fCharacter = '=' THEN
	BEGIN
	  fSymbolId := zptGEql; fOrigin.Read (fCharacter, 1)
	END
      END;
    '+' :
      BEGIN
	fSymbolId := zptPlus; fOrigin.Read (fCharacter, 1)
      END;
    '-' :
      BEGIN
	fSymbolId := zptMinus; fOrigin.Read (fCharacter, 1)
      END;
    '*' :
      BEGIN
	fSymbolId := zptTimes; fOrigin.Read (fCharacter, 1)
      END;
    '/' :
      BEGIN
	fSymbolId := zptDivide; fOrigin.Read (fCharacter, 1)
      END;
  { Separators and other symbols. }
    ';':
      BEGIN
	fSymbolId := zptSemiColon; fOrigin.Read (fCharacter, 1)
      END;
    '.':
      BEGIN
	fSymbolId := zptPeriod; fOrigin.Read (fCharacter, 1)
      END;
    '{':
        JumpComment;
    '(':
      BEGIN
	fSymbolId := zptLParen; fOrigin.Read (fCharacter, 1);
	IF fCharacter = '*' THEN JumpComment
      END;
    ')':
      BEGIN
	fSymbolId := zptRParen; fOrigin.Read (fCharacter, 1)
      END;
    ':' :
      BEGIN
	fSymbolId := zptColon; fOrigin.Read (fCharacter, 1);
        IF fCharacter = '=' THEN
        BEGIN
         fSymbolId := zptBecomes; fOrigin.Read (fCharacter, 1)
        END
      END;
    ',':
      BEGIN
	fSymbolId := zptComma; fOrigin.Read (fCharacter, 1)
      END;
    '[':
      BEGIN
	fSymbolId := zptLBrak; fOrigin.Read (fCharacter, 1)
      END;
    ']':
      BEGIN
	fSymbolId := zptRBrak; fOrigin.Read (fCharacter, 1)
      END;
    '^':
      BEGIN
	fSymbolId := zptCaret; fOrigin.Read (fCharacter, 1)
      END;
    ELSE
      BEGIN
	fSymbolId := zptNull; fOrigin.Read (fCharacter, 1);
      END
    END;
  {
    WriteLn (Format (
      'Siguiente => "%s" = [%d] %s.', [
      fSimbolo, fSymbolId, TomaNombreIdSimbolo
    ]));
  }
  END;



(* Checks if it is at the end of the program. *)
  FUNCTION Tz80PascalScanner.CheckEOF: BOOLEAN;
  BEGIN
    RESULT := fOrigin.Position >= fOrigin.Size
  END;


END.
