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
  Classes, sysutils, Configuration;



TYPE
(* To manage compilation exceptions. *)
  CompilationException = CLASS (Exception)
  PUBLIC
  (* Something expected but anotherthing found. *)
    CONSTRUCTOR Expected (Line, Column: INTEGER; fExpected, fFound: STRING);
  END;



(* Token types. *)
  TPascalTokenType = ( pttIdentifier, pttInteger, pttHexInteger, pttReal,
			pttString, pttOther );



(* @exclude
   Forward declaration needed by TPascalLexicalScanner. *)
  TPascalCompiler = CLASS;



(* Lexical scanner.  It's prepared to parse Pascal source files. *)
  TPascalLexicalScanner = CLASS (TObject)
  PRIVATE
  (* Reference to the owner. *)
    fOwner: TPascalCompiler;
  (* The sream to read from. *)
    fInputStream: TStream;
  (* Position in the source file. *)
    fLine, fColumn: INTEGER;
  (* Lookahead character. *)
    fLookahead: CHAR;
  (* Advance next character. *)
    fNext: STRING;
  (* Last token read. *)
    fLastTokenString: STRING;
    fLastTokenType: TPascalTokenType;
  (* The input stream property. *)
    PROCEDURE PutInputStream (aStream: TStream);
  (* To get the next character. *)
    FUNCTION GetNextCharacter: CHAR;
  PUBLIC
  (* Constructor.
     @param(Owner The compiler, used to fire some events.  Can be @nil.) *)
    CONSTRUCTOR Create (Owner: TPascalCompiler);

  (* Gets next character from the stream and stores it in the LookAhead
     property.  Ignores comments and control characters. *)
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

  (* Returns the next token from the stream. *)
    FUNCTION GetToken: STRING;
  (* Returns an identifier. *)
    FUNCTION GetIdentifier: STRING;
  (* Returns a number constant. *)
    FUNCTION GetNumber: STRING;
  (* Returns a string constant. *)
    FUNCTION GetString: STRING;

  (* The input stream.  It will not be destroyed by the scanner. *)
    PROPERTY InputStream: TStream READ fInputStream WRITE PutInputStream;
  (* Current line. *)
    PROPERTY Line: INTEGER READ fLine;
  (* Current column. *)
    PROPERTY Column: INTEGER READ fColumn;
  (* Returns the last character read. *)
    PROPERTY Lookahead: CHAR READ fLookahead;
  (* Returns the next character to read but don't extracts it from the stream.
     Needed to identify some tokens (i.e. := and comments) *)
    PROPERTY NextCharacter: CHAR READ GetNextCharacter;
  (* Returns the last parsed token. *)
    PROPERTY LastToken: STRING READ fLastTokenString;
  (* Returns the last parsed token type. *)
    PROPERTY LastTokenType: TPascalTokenType READ fLastTokenType;
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
  (* Add a coment line to the output. *)
    PROCEDURE AddComment (Comment: STRING); VIRTUAL;
  (* It's called by the compiler when it finds an "ASM .. END" block.  It
     should use the RefScanner, and the first token should be "ASM" and should
     parse until the first END token.

     By default it raises an exception because there are no target. *)
    PROCEDURE ParseASM; VIRTUAL;
  (* Emits program prologue.  That is, initialization code needed.  By default
     it does nothing. *)
    PROCEDURE ProgramProlog (ProgramName: STRING); VIRTUAL;
  (* Emits program Epilogue.  That is, Finalization code needed.  By default it
     does nothing. *)
    PROCEDURE ProgramEpilog; VIRTUAL;
  END;



(* Implements a P-Code compiler of Pascal.

   It raises a @link(CompilationException) if finds a compilation error. *)
  TPascalCompiler = CLASS (TObject)
  PRIVATE
  (* Lexical Scanner used. *)
    fScanner: TPascalLexicalScanner;
  (* Encoder used. *)
    fEncoder: TPascalEncoder;
  PROTECTED
  (* This procedure is called by when the compiler needs to inform the user for
     someting that is not an error (i.e.: Warnings, notes,... ).  By default it
     does nothing. *)
    PROCEDURE Verbose (Level: VERBOSITY_LEVELS; aMessage: STRING); VIRTUAL;
  PUBLIC
  (* Constructor.
     @param(aEncoder The encoder to be used by the compiler.  It will be freed
       by the compiler's destructor.) *)
    CONSTRUCTOR Create (aEncoder: TPascalEncoder);
  (* Destructor.  It will free the encoder too. *)
    DESTRUCTOR Destroy; OVERRIDE;
  (* Entry point for compilation. *)
    PROCEDURE Compile; VIRTUAL;
  (* It's called by the Encoder when it finds a comment line.  It's useful to
     parse compiler directives (i.e.: {$ ...) or to add the comments to the
     Assembler output.  By default it ignores the comment. *)
    PROCEDURE AddComment (Comment: STRING); VIRTUAL;
  (* Lexical scanner used by the compiler. *)
    PROPERTY Scanner: TPascalLexicalScanner READ fScanner;
  (* Encoder used by the compiler. *)
    PROPERTY Encoder: TPascalEncoder READ fEncoder;
  PRIVATE
  (* Recursive compiler.  See the BNF description in the atg file at internal
     documentation directory. *)
    PROCEDURE PascalProgram;
    PROCEDURE Block;
    PROCEDURE DeclarationPart;
    PROCEDURE StatementPart;
    PROCEDURE CompoundStatement;
  END;



IMPLEMENTATION

CONST
(* Fool constants for control characters. *)
  CR = ^M;
  LF = ^J;
  QT = '''';
  SPC = ' ';
  TAB = ^I;



(************************
 * CompilationException *
 ************************)

(* Something expected but anotherthing found. *)
  CONSTRUCTOR CompilationException.Expected (Line, Column: INTEGER; fExpected, fFound: STRING);
  BEGIN
    INHERITED Create (' ('+IntToStr (Line)+', '+IntToStr (Column)+') Fatal syntax error, "'+fExpected+'" expected but "'+fFound+'" found.');
  END;



(*************************
 * TPascalLexicalScanner *
 *************************)

(* The input stream property. *)
  PROCEDURE TPascalLexicalScanner.PutInputStream (aStream: TStream);
  BEGIN
    fInputStream := aStream;
  { Initializes the scanner. }
    fLine := 1; fColumn := 1;
    SELF.GetChar;
    SkipWhite;
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



(* Constructor. *)
  CONSTRUCTOR TPascalLexicalScanner.Create (Owner: TPascalCompiler);
  BEGIN
    INHERITED Create;
    fOwner := Owner;
    fInputStream := NIL;
  END;



(* Gets a simple character from the stream. *)
  PROCEDURE TPascalLexicalScanner.GetChar;
  BEGIN
  { Checks if it has read the next character yet. }
    IF fNext = '' THEN
      fLookahead := CHAR (fInputStream.ReadByte)
    ELSE BEGIN
      fLookahead := fNext[1];
      fNext := '';
    END;
  { Position. }
    IF fLookahead IN [CR, LF] THEN
    BEGIN
      IF ((DefaultTextLineBreakStyle = tlbsLF) AND (fLookahead = LF))
      OR (fLookahead = CR) THEN
      BEGIN
	INC (fLine);
	fColumn := 1
      END;
    END
    ELSE
      INC (fColumn);
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
    RESULT := aChar = QT;
  END;



(* Checks if the given character is a white space. *)
  FUNCTION TPascalLexicalScanner.isWhite (aChar: CHAR): BOOLEAN;
  BEGIN
    RESULT := aChar IN [SPC, TAB];
  END;



(* Skip over leading white spaces. *)
  PROCEDURE TPascalLexicalScanner.SkipWhite;

  (* Checks and skips a comment. *)
    PROCEDURE SkipComment;

      FUNCTION IsBeginComment: BOOLEAN;
      BEGIN
	RESULT := (fLookahead = '{')
		OR ((fLookahead = '(') AND (NextCharacter = '*'));
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
		OR ((fLookahead = '*') AND (NextCharacter = ')'));
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
	  Tmp[1] := fLookahead;
	  Comment := Comment + Tmp;
	  SELF.GetChar;
	UNTIL IsEndComment;
	IF fOwner <> NIL THEN
	  fOwner.AddComment (Comment);
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
    SkipWhite;
  END;



(* Skips a separator (i.e.: ',', '+', '(', etc. *)
  PROCEDURE TPascalLexicalScanner.SkipCharacter;
  BEGIN
    SELF.GetChar;
    SkipWhite;
  END;



(* Returns any token. *)
  FUNCTION TPascalLexicalScanner.GetToken: STRING;
  BEGIN
    WHILE (fLookahead = CR) OR (fLookahead = LF) DO SkipLineBreak;
    IF SELF.isAlpha (Lookahead) THEN
      RESULT := GetIdentifier
    ELSE IF SELF.isDigit (Lookahead) OR (Lookahead = '$') THEN
      RESULT := GetNumber
    ELSE IF isStringDelimiter (Lookahead) THEN
      RESULT := GetString
    ELSE BEGIN
      fLastTokenString := fLookahead;
      fLastTokenType := pttOther;
      RESULT := fLastTokenString;
      SELF.GetChar;
      SkipWhite;
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
      RESULT := isStringDelimiter (fLookahead)
		AND NOT isStringDelimiter (NextCharacter);
    END;

  BEGIN
    IF NOT isStringDelimiter (fLookahead) THEN
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
      IF isStringDelimiter (fLookahead)
      AND isStringDelimiter (NextCharacter) THEN
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



(* Emits program prologue.  That is, initialization code needed.  By default
   it does nothing. *)
  PROCEDURE TPascalEncoder.ProgramProlog (ProgramName: STRING);
  BEGIN
    ;
  END;



(* Emits program Epilogue.  That is, Finalization code needed.  By default it
   does nothing. *)
  PROCEDURE TPascalEncoder.ProgramEpilog;
  BEGIN
    ;
  END;



(*******************
 * TPascalCompiler *
 *******************)

(* This procedure is called by when the compiler needs to inform the user for
   someting that is not an error (i.e.: Warnings, notes,... ).  By default it
   does nothing. *)
  PROCEDURE TPascalCompiler.Verbose (Level: VERBOSITY_LEVELS; aMessage: STRING);
  BEGIN
    ;
  END;



(* Constructor.
   @param(aEncoder The encoder to be used by the compiler.  It will be freed by
     the compiler's destructor.) *)
  CONSTRUCTOR TPascalCompiler.Create (aEncoder: TPascalEncoder);
  BEGIN
    INHERITED Create;
    fScanner := TPascalLexicalScanner.Create (SELF);
    fEncoder := aEncoder;
    fEncoder.fRefScanner := fScanner;
  END;



(* Destructor.  It will free the encoder too. *)
  DESTRUCTOR TPascalCompiler.Destroy;
  BEGIN
    fScanner.Free;
    fEncoder.Free;
    INHERITED Destroy;
  END;



(* Entry point for compilation. *)
  PROCEDURE TPascalCompiler.Compile;
  VAR
    Token: STRING;
  BEGIN
  { Z80pas ::= PascalProgram .
    PascalProgram ::= PascalProgram . }
    Token := fScanner.GetToken;
    IF Token = 'PROGRAM' THEN
      PascalProgram
    ELSE
      RAISE CompilationException.Expected (
	fScanner.Line, fScanner.Column, 'PROGRAM', Token);
  END;



(* It's called by the Encoder when it finds a comment line.  It's useful to
   parse compiler directives (i.e.: {$ ...) or to add the comments to the
   Assembler output.
   By default it ignores the comment. *)
  PROCEDURE TPascalCompiler.AddComment (Comment: STRING);
  BEGIN
    ;
  END;



(**********************
 * Recursive compiler *
 **********************)

(* PascalProgram ::= "PROGRAM" NewIdent ";" Block "." . *)
  PROCEDURE TPascalCompiler.PascalProgram;
  VAR
    ProgramIdentifier: STRING;
  BEGIN
    IF fScanner.LastToken <> 'PROGRAM' THEN
      RAISE CompilationException.Expected (
	fScanner.Line, fScanner.Column, 'PROGRAM', fScanner.LastToken);
    ProgramIdentifier := fScanner.GetIdentifier; { NewIdent ::= identifier . }
  { TODO: May be it should store the program identifier in a symbol list to
    prevent duplicate definitions. }
    Verbose (vblWarnings, 'Program name ''' + ProgramIdentifier + '''');
    IF fScanner.GetToken <> ';' THEN
      RAISE CompilationException.Expected (
	fScanner.Line, fScanner.Column, ';', fScanner.LastToken);
  { Program prolog. }
    fEncoder.ProgramProlog (ProgramIdentifier);
    Block;
    IF fScanner.GetToken <> '.' THEN
      RAISE CompilationException.Expected (
	fScanner.Line, fScanner.Column, '.', fScanner.LastToken);
    fEncoder.ProgramEpilog;
  END;



(* Block ::= DeclarationPart StatementPart . *)
  PROCEDURE TPascalCompiler.Block;
  BEGIN
    DeclarationPart;
    fEncoder.AddComment ('>');
    fEncoder.AddComment ('> Program starts here');
    fEncoder.AddComment ('>');
    StatementPart;
  END;



(* At the moment, DeclarationPart is empty. *)
  PROCEDURE TPascalCompiler.DeclarationPart;
  BEGIN
    ;
  END;



(* StatementPart ::= CompoundStatement | ASMCompoundStatement . *)
  PROCEDURE TPascalCompiler.StatementPart;
  BEGIN
    IF fScanner.GetToken = 'ASM' THEN
      fEncoder.ParseASM
    ELSE
      CompoundStatement;
  END;



(* CompoundStatement ::= "BEGIN" StatementSequence "END" .
   StatementSequence ::= Statement [ ";" Statement ]* . *)
  PROCEDURE TPascalCompiler.CompoundStatement;
  BEGIN
    IF fScanner.LastToken <> 'BEGIN' THEN
      RAISE CompilationException.Expected (
	fScanner.Line, fScanner.Column, 'BEGIN', fScanner.LastToken);
    WHILE fScanner.GetToken <> 'END' DO
    BEGIN
      IF fScanner.LastToken = 'ASM' THEN
	fEncoder.ParseASM
      ELSE IF fScanner.LastToken = ';' THEN
      { This allows empty statements. }
	CONTINUE
      ELSE
	RAISE CompilationException.Create ('Synax error.');
    END;
  END;

END.
