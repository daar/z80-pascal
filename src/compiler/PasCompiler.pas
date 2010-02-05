UNIT PasCompiler;
(*<Defines a basic reusable Pascal compiler. *)
(*
  Copyright (c) 2009, 2010 Guillermo Martínez J.

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
(* Token types. *)
  TPascalTokenType = ( pttIdentifier, pttInteger, pttHexInteger, pttReal,
			pttString, pttOther );



(* Lexical scanner.  It's prepared to parse Pascal source files. *)
  TPascalLexicalScanner = CLASS (TObject)
  PRIVATE
    fInputStream: TStream;
  (* Lookahead character. *)
    fLookahead: CHAR;
  (* Advance next character. *)
    fNext: STRING;
  (* Last token read. *)
    fLastTokenString: STRING;
    fLastTokenType: TPascalTokenType;
  (* To store comments. *)
    fComments: TStringList;

  (* The "StoreComments" property. *)
    FUNCTION GetStoreComments: BOOLEAN;
    PROCEDURE PutStoreComments (Value: BOOLEAN);
  (* To get the next character. *)
    FUNCTION GetNextCharacter: CHAR;
  PUBLIC
  (* Constructor.  Uses the given stream as input.
     @param(aStoreComments sets the StoreComments property.
       Default to @false). *)
    CONSTRUCTOR Create (aStream: TStream; aStoreComments: BOOLEAN = FALSE);
  (* Destructor. *)
    DESTRUCTOR Destroy; OVERRIDE;

  (* Gets next character from the stream and stores it in the LookAhead
     property.  Ignores control characters. *)
    PROCEDURE GetChar;

  (* Checks if the given character is a leter. *)
    FUNCTION isAlpha (aChar: CHAR): BOOLEAN;
  (* Checks if the given character is a digit. *)
    FUNCTION isDigit (aChar: CHAR): BOOLEAN;
  (* Checks if the given character is an hexagesimal digit. *)
    FUNCTION isHexDigit (aChar: CHAR): BOOLEAN;
  (* Checks if the given character is alphanumeric. *)
    FUNCTION isAlphaNum (aChar: CHAR): BOOLEAN;
  (* Checks if the given character is a string delimiter. *)
    FUNCTION isStringDelimiter (aChar: CHAR): BOOLEAN;
  (* Checks if the given character is a white space. *)
    FUNCTION isWhite (aChar: CHAR): BOOLEAN;

  (* Skip over leading white spaces and comments. *)
    PROCEDURE SkipWhite;
  (* Skips CR/CF. *)
    PROCEDURE SkipLineBreak;
  (* Skips a separator (i.e.: ',', '+', '(', etc.).  It will skip also the
     following white spaces. *)
    PROCEDURE SkipCharacter;

  (* Retrieves and returns the next token from the stream. *)
    FUNCTION GetToken: STRING;
  (* Returns an identifier. *)
    FUNCTION GetIdentifier: STRING;
  (* Returns a number constant. *)
    FUNCTION GetNumber: STRING;
  (* Returns a string constant. *)
    FUNCTION GetString: STRING;

  (* Returns the last character read. *)
    PROPERTY Lookahead: CHAR READ fLookahead;
  (* Returns the next character to read but don't extracts it from the stream.
     Needed to identify some tokens (i.e. := and comments) *)
    PROPERTY NextCharacter: CHAR READ GetNextCharacter;
  (* Returns the last parsed token. *)
    PROPERTY LastToken: STRING READ fLastTokenString;
  (* Returns the last parsed token type. *)
    PROPERTY LastTokenType: TPascalTokenType READ fLastTokenType;
  (* Tells the scanner if should store the comments it finds.
     @seealso(Comments) *)
    PROPERTY StoreComments: BOOLEAN READ GetStoreComments WRITE PutStoreComments;
  (* If @link(StoreComments) is @true, it stores the comments it finds here. *)
    PROPERTY Comments: TStringList READ fComments;
  END;



(* Abstract class that generalizes the encoding.  It's used by the compiler to
   create the code.  Actually is a P-Code encoder:  The compiler "translates"
   the Pascal sources to P-Code and "send" it to the encoder, and the encoder
   "translates" the P-Code to the final code. *)
  TPascalEncoder = CLASS (TObject)
  PRIVATE
    fRefScanner: TPascalLexicalScanner;
  PROTECTED
  (* Reference to the scanner.  It should be the same than the used by the
     compler.  Assigned by the constructor. *)
    PROPERTY RefScanner: TPascalLexicalScanner READ fRefScanner;
  PUBLIC
  (* Constructor.
     @param(aScanner Reference to the scanner used by the compiler.  The
      encoder may need it to parse inline assembler code (see
      @link(ParseAssembler).  It's assigned to @link(RefScanner) and souldn't
      be destroyed) *)
    CONSTRUCTOR Create (aScanner: TPascalLexicalScanner); VIRTUAL;
  (* Add a coment line to the output. *)
    PROCEDURE AddComment (Comment: STRING); VIRTUAL;
  (* It's called by the compiler when it finds an "ASM .. END" block.  It
     should use the RefScanner, and the first token should be "ASM" and should
     parse until the first END token.

     By default it raises an exception because there are no target. *)
    PROCEDURE ParseASM; VIRTUAL;


  (* Adds an assembler line.  This is only temporal. *)
    PROCEDURE Emit (aLine: STRING); VIRTUAL; ABSTRACT;
  END;



IMPLEMENTATION

USES
  sysutils;



CONST
(* Fool constants for control characters. *)
  TAB = ^I;
  CR = ^M;
  LF = ^J;



(*************************
 * TPascalLexicalScanner *
 *************************)

(* Returns TRUE if it stores comments. *)
  FUNCTION TPascalLexicalScanner.GetStoreComments: BOOLEAN;
  BEGIN
    RESULT := fComments <> NIL;
  END;



(* Activates or deactivates the comment storage. *)
  PROCEDURE TPascalLexicalScanner.PutStoreComments (Value: BOOLEAN);
  BEGIN
    IF Value THEN
    BEGIN
      IF fComments = NIL THEN
	fComments := TStringList.Create;
    END
    ELSE IF fComments <> NIL THEN
    BEGIN
      fComments.Free;
      fComments := NIL;
    END;
  END;



(* To get the next character. *)
  FUNCTION TPascalLexicalScanner.GetNextCharacter: CHAR;
  VAR
    Character: BYTE;
  BEGIN
    IF fNext = '' THEN
    BEGIN
      Character := fInputStream.ReadByte;
      fNext := ' ';
      fNext[1] := CHAR (Character);
    END;
    RESULT := fNext[1];
  END;



(* Constructor.  Uses the given stream as input. *)
  CONSTRUCTOR TPascalLexicalScanner.Create (aStream: TStream; aStoreComments: BOOLEAN);
  BEGIN
    PutStoreComments (aStoreComments);
    fInputStream := aStream;
    SELF.GetChar;
    SkipWhite;
  END;



(* Destructor. *)
  DESTRUCTOR TPascalLexicalScanner.Destroy;
  BEGIN
    IF fComments <> NIL THEN
      fComments.Free;
    INHERITED;
  END;



(* Gets a simple character from the stream. *)
  PROCEDURE TPascalLexicalScanner.GetChar;
  VAR
    Character: BYTE;
  BEGIN
  { Checks if it has read the next character yet. }
    IF fNext = '' THEN
      Character := fInputStream.ReadByte
    ELSE BEGIN
      Character := BYTE (fNext[1]);
      fNext := '';
    END;
    fLookahead := CHAR (Character);
  // To help debug.  Should be deleted on final release.
  //WriteLn ('Lh := ''', fLookahead, '''');
  END;



(* Checks if the given character is a leter. *)
  FUNCTION TPascalLexicalScanner.isAlpha (aChar: CHAR): BOOLEAN;
  BEGIN
    RESULT := aChar IN ['A'..'Z', 'a'..'z'];
  END;



(* Checks if the given character is a digit. *)
  FUNCTION TPascalLexicalScanner.isDigit (aChar: CHAR): BOOLEAN;
  BEGIN
    RESULT := aChar IN ['0'..'9'];
  END;



(* Checks if the given character is an hexagesimal digit. *)
  FUNCTION TPascalLexicalScanner.isHexDigit (aChar: CHAR): BOOLEAN;
  BEGIN
    RESULT := SELF.isDigit (aChar) OR (aChar IN ['A'..'F', 'a'..'f']);
  END;



(* Checks if the given character is alphanumeric. *)
  FUNCTION TPascalLexicalScanner.isAlphaNum (aChar: CHAR): BOOLEAN;
  BEGIN
    RESULT := SELF.isAlpha (aChar) OR SELF.isDigit (aChar);
  END;



(* Checks if the given character is a string delimiter. *)
  FUNCTION TPascalLexicalScanner.isStringDelimiter (aChar: CHAR): BOOLEAN;
  BEGIN
    RESULT := aChar = '''';
  END;



(* Checks if the given character is a white space. *)
  FUNCTION TPascalLexicalScanner.isWhite (aChar: CHAR): BOOLEAN;
  BEGIN
    RESULT := aChar IN [' ', TAB];
  END;



(* Skip over leading white spaces. *)
  PROCEDURE TPascalLexicalScanner.SkipWhite;

  (* Checks and skips a comment. *)
    PROCEDURE SkipComment;

      FUNCTION IsBeginComment: BOOLEAN;
      BEGIN
	RESULT := (fLookahead = '{')
		OR ((fLookahead = '(') AND (SELF.NextCharacter = '*'));
	IF RESULT THEN
	BEGIN
	  SELF.GetChar;
	  IF fLookahead = '*' THEN
	    SELF.GetChar;
	END;
      END;

      FUNCTION IsEndComment: BOOLEAN;
      BEGIN
	RESULT := (fLookahead = '}')
		OR ((fLookahead = '*') AND (SELF.NextCharacter = ')'));
	IF RESULT THEN
	BEGIN
	  SELF.GetChar;
	  IF fLookahead = ')' THEN
	    SELF.GetChar;
	END;
      END;

    VAR
      Comment, Tmp: STRING;
    BEGIN
      Comment := '';
      Tmp := ' ';
      WHILE IsBeginComment DO
	REPEAT
	  IF fComments <> NIL THEN
	  BEGIN
	    Tmp[1] := fLookahead;
	    Comment := Comment + Tmp;
	  END;
	  SELF.GetChar;
	UNTIL IsEndComment;
      IF (fComments <> NIL) AND (Comment <> '') THEN
	fComments.Text := Comment;
    END;

  BEGIN
    SkipComment;
    WHILE SELF.isWhite (fLookahead) DO
    BEGIN
      SELF.GetChar;
      SkipComment;
    END;
  END;



(* Skips CR/CF. *)
  PROCEDURE TPascalLexicalScanner.SkipLineBreak;
  BEGIN
    WHILE fLookahead IN [CR, LF] DO
      SELF.GetChar;
    SELF.SkipWhite;
  END;



(* Skips a separator (i.e.: ',', '+', '(', etc. *)
  PROCEDURE TPascalLexicalScanner.SkipCharacter;
  BEGIN
    SELF.GetChar;
    SELF.SkipWhite;
  END;



(* Returns any token. *)
  FUNCTION TPascalLexicalScanner.GetToken: STRING;
  BEGIN
    WHILE (fLookahead = CR) OR (fLookahead = LF) DO SkipLineBreak;
    IF SELF.isAlpha (Lookahead) THEN
      RESULT := SELF.GetIdentifier
    ELSE IF SELF.isDigit (Lookahead) OR (Lookahead = '$') THEN
      RESULT := SELF.GetNumber
    ELSE IF SELF.isStringDelimiter (Lookahead) THEN
      RESULT := SELF.GetString
    ELSE BEGIN
      fLastTokenString := fLookahead;
      fLastTokenType := pttOther;
      RESULT := fLastTokenString;
      SELF.GetChar;
      SELF.SkipWhite;
    // To help debug.  Should be deleted on final release.
    //WriteLn ('Last Token : ''', fLastTokenString, '''');
    END;
  END;



(* Returns an identifier. *)
  FUNCTION TPascalLexicalScanner.GetIdentifier: STRING;
  BEGIN
    WHILE (fLookahead = CR) OR (fLookahead = LF) DO SkipLineBreak;
    fLastTokenString := '';
    IF NOT SELF.isAlpha (fLookahead) THEN
      RAISE Exception.Create ('Name expected!');
    fLastTokenType := pttIdentifier;
    REPEAT
      fLastTokenString := fLastTokenString + UpCase (fLookahead);
      SELF.GetChar;
    UNTIL NOT SELF.isAlphaNum (fLookahead);
    RESULT := fLastTokenString;
    SkipWhite;
  // To help debug.  Should be deleted on final release.
  //WriteLn ('Last Token : ''', fLastTokenString, '''');
  END;



(* Returns a number. *)
  FUNCTION TPascalLexicalScanner.GetNumber: STRING;

    PROCEDURE HexNum;
    BEGIN
      REPEAT
	fLastTokenString := fLastTokenString + fLookahead;
	SELF.GetChar;
      UNTIL NOT SELF.isHexDigit (fLookahead);
    END;


    PROCEDURE IntegerNum;
    BEGIN
      REPEAT
	fLastTokenString := fLastTokenString + fLookahead;
	SELF.GetChar;
      UNTIL NOT SELF.isDigit (fLookahead);
    END;

  BEGIN
    WHILE (fLookahead = CR) OR (fLookahead = LF) DO SkipLineBreak;
    fLastTokenString := '';
    IF NOT SELF.isDigit (fLookahead) AND (fLookahead <> '$') THEN
      RAISE Exception.Create ('Number expected!');
    IF fLookahead = '$' THEN
    BEGIN
      fLastTokenType := pttHexInteger;
      HexNum;
    END
    ELSE BEGIN
      fLastTokenType := pttInteger;
      IntegerNum;
    END;
    RESULT := fLastTokenString;
    SkipWhite;
  // To help debug.  Should be deleted on final release.
  //WriteLn ('Last Token : ''', fLastTokenString, '''');
  END;

(* Returns a string constant. *)
  FUNCTION TPascalLexicalScanner.GetString: STRING;

  (* Helper function to detect the delimiter. *)
    FUNCTION EndOfString: BOOLEAN;
    BEGIN
      RESULT := SELF.isStringDelimiter (fLookahead)
		AND NOT SELF.isStringDelimiter (NextCharacter);
    END;

  BEGIN
    IF NOT SELF.isStringDelimiter (fLookahead) THEN
      RAISE Exception.Create ('String constant delimiter expected!');
    SELF.GetChar; { Skips first delimiter. }
    fLastTokenString := '';
  { Get's the string. }
    WHILE NOT EndOfString DO
    BEGIN
    { Test for line breaking. }
      IF (fLookahead = CR) OR (fLookahead = LF) THEN
	RAISE Exception.Create ('String exceeds line');
      fLastTokenString := fLastTokenString + fLookahead;
    { Test for double quote. }
      IF SELF.isStringDelimiter (fLookahead)
	 AND SELF.isStringDelimiter (NextCharacter) THEN
    { Skips one of them.  The other will be skipped later. }
	SELF.GetChar;
    { Skips current character. }
      SELF.GetChar;
    END;
    RESULT := fLastTokenString;
  END;



(******************
 * TPascalEncoder *
 ******************)

(* Constructor. *)
  CONSTRUCTOR TPascalEncoder.Create (aScanner: TPascalLexicalScanner);
  BEGIN
    fRefScanner := aScanner;
  END;



(* Add a coment line to the output. *)
  PROCEDURE TPascalEncoder.AddComment (Comment: STRING);
  BEGIN
    ;
  END;



(* By default it raises an exception because there are no target. *)
  PROCEDURE TPascalEncoder.ParseASM;
  BEGIN
    RAISE Exception.Create ('No ASM allowed.');
  END;

END.
