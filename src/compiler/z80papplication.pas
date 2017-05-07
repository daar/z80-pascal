UNIT z80pApplication;
(*<Implements an application class. *)
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
    PasCompiler,
    CustApp,
    Classes, sysutils;

  CONST
  (* Major version number. *)
    MAJOR_V = 0;
  (* Minor version number. *)
    MINOR_V = 2;
  (* Revision version number.  Negative means work in progress (alpha, beta,
     pre-release, etc.). *)
    REVISION_V = -1;
  (* Version string. *)
    VERSION_STR = '0.2.a1';
  (* Copyright legend. *)
    COPY_STR = '(c) Darius Blaszyk & Guillermo Martínez J. 2017';

  (* Default verbosity of compiler. *)
    DEFAULT_VERBOSITY = [etError, etWarning];
  (* Default value of "Stop on warning". *)
    STOP_ON_WARNING = FALSE;
  (* Default value of "Stop on hint". *)
    STOP_ON_HINT = FALSE;

  TYPE
  (* Contains the compiler configuration. *)
    Tz80Configuration = CLASS (TObject)
    PRIVATE
      fInputFilename, fOutputFilename: STRING;
      fVerbosity: TEventTypes;
      fStopOnWarning, fStopOnHint: BOOLEAN;

      PROCEDURE ShowCommandLineHelp;
    PUBLIC
    (* Initializes the object.

       This loads the configuration file and it gets the command line options. *)
      PROCEDURE Initialize;

    (* Input filename. *)
      PROPERTY InputFilename: STRING READ fInputFilename;
    (* Output filename. *)
      PROPERTY OutputFilename: STRING READ fOutputFilename;
    (* How much verbose the compiler should be. *)
      PROPERTY Verbosity: TEventTypes READ fVerbosity;
    (* Tells the compiler to stop when warning is found.
       @seealso(StopOnHint) *)
      PROPERTY StopOnWarning: BOOLEAN READ fStopOnWarning;
    (* Tells the compiler to stop when a hint is found.
       @seealso(StopOnWarning) *)
      PROPERTY StopOnHint: BOOLEAN READ fStopOnHint;
    END;



  (* The application object. *)
    Tz80PascalApplication = CLASS (TCustomApplication)
    PRIVATE
      fConfiguration: Tz80Configuration;

      fInputFile: TStream;
      fScanner: TPascalScanner;
    PROTECTED
    (* Application execution. *)
      PROCEDURE DoRun; OVERRIDE;
    PUBLIC
    (* Constructor. *)
      CONSTRUCTOR Create (aOwner: TComponent); OVERRIDE;
    (* Destructor. *)
      DESTRUCTOR Destroy; OVERRIDE;
    (* Shows an exception to the user. *)
      PROCEDURE ShowException (E: Exception); OVERRIDE;
    (* Write a message to the standard output (verbose). *)
      PROCEDURE Trace (EventType: TEventType; CONST Msg: STRING);
    (* Initializes application. *)
      PROCEDURE Initialize; OVERRIDE;

    (* Access to configuration. *)
      PROPERTY Configuration: Tz80Configuration READ fConfiguration;
    END;

  VAR
  (* Global reference to the application. *)
    Application: Tz80PascalApplication;

IMPLEMENTATION

(*
 * Tz80Configuration
 ****************************************************************************)

  PROCEDURE Tz80Configuration.ShowCommandLineHelp;
  BEGIN
    WriteLn ('Usage:');
    WriteLn (
      '  ',
      ExtractFileName (Application.ExeName),
      ' <pasfile> [options]'
    );
    WriteLn ('Options:');
    WriteLn ('  --output-file=<x>, -o<x>');
    WriteLn ('         Change the name of the output file to <x>.');
    WriteLn ('  -Se<x> Error options. <x> is a combination of the following:');
    WriteLn ('     w : Compiler also halts after warnings.');
    WriteLn ('     h : Compiler also halts after hints.');
    WriteLn ('  -v<x>  Be verbose. <x> is a combination of the following letters:');
    WriteLn ('      e : Show errors (default)       w : Show warnings');
    WriteLn ('      h : Show hints                  d : Show debug info');
    WriteLn ('      a : Show everything');
    WriteLn ('  --help, -h, -?');
    WriteLn ('         Shows this help.');
  END;



(* Initializes configuration. *)
  PROCEDURE Tz80Configuration.Initialize;

  { Returns TRUE if command is parsed. }
    FUNCTION ParseCommand (aCommand: STRING): BOOLEAN;
    VAR
      Ndx: INTEGER;
    BEGIN
      IF Length (aCommand) < 2 THEN EXIT (FALSE);
      IF (aCommand = '--help') OR (aCommand = '-h') OR (aCommand='-?') THEN
      BEGIN
	SELF.ShowCommandLineHelp;
	Application.Terminate;
	EXIT (TRUE)
      END;
      IF LeftStr (aCommand, 2) = '-o' THEN
      BEGIN
	fOutputFilename := RightStr (aCommand, Length (aCommand) - 2);
	EXIT (TRUE)
      END
      ELSE IF LeftStr (aCommand, 14) = '--output-file=' THEN
      BEGIN
	fOutputFilename := RightStr (aCommand, Length (aCommand) - 14);
	EXIT (TRUE)
      END;
      IF aCommand[2] = 'S' THEN
      BEGIN
	IF Length (aCommand) <> 4 THEN EXIT (FALSE);
	IF aCommand[3] <> 'e' THEN EXIT (FALSE);
	CASE aCommand[4] OF
	  'w': fStopOnWarning := TRUE;
	  'h': fStopOnHint := TRUE;
	  ELSE EXIT (FALSE);
	END;
	EXIT (TRUE)
      END;
      IF aCommand[2] = 'v' THEN
      BEGIN
	IF Length (aCommand) < 3 THEN EXIT (FALSE);
	fVerbosity := [];
	FOR Ndx := 3 TO Length (aCommand) DO
	  CASE aCommand[Ndx] OF
	    'e': fVerbosity := fVerbosity + [etError];
	    'w': fVerbosity := fVerbosity + [etWarning];
	    'h': fVerbosity := fVerbosity + [etInfo, etCustom];
	    'd': fVerbosity := fVerbosity + [etDebug];
	    'a':
	      fVerbosity := fVerbosity
	                  + [etError, etWarning, etInfo, etCustom, etDebug];
	    ELSE EXIT (FALSE);
	  END;
	EXIT (TRUE)
      END;
    { If here, then command was unknown. }
      RESULT := FALSE
    END;

  VAR
    Ndx: INTEGER;
  BEGIN
  { Default values. }
    fInputFilename := '';
    fOutputFilename := '';
    fVerbosity := DEFAULT_VERBOSITY;
    fStopOnWarning := STOP_ON_WARNING;
    fStopOnHint    := STOP_ON_HINT;
  { Configuration file. }
    Application.Trace (etWarning, 'Configuration file not implemented (yet).');
  { Check command line options. }
    IF Application.ParamCount > 0 THEN
    BEGIN
    { I don't like the way TCustomApplication manages command line options, so
      I'll do it "the way of C". }
    { Start with the input file. }
      IF NOT (Application.Params[1][1] IN ['a'..'z', 'A'..'Z', '0'..'9']) THEN
      BEGIN
	IF (Application.Params[1] = '--help') OR (Application.Params[1] = '-h')
	OR (Application.Params[1] ='-?')
	THEN BEGIN
	  SELF.ShowCommandLineHelp;
	  Application.Terminate;
	  EXIT
	END;
      { No asking for help, so... }
	Application.Trace (etError, 'No input file.');
	WriteLn ('Use command "--help" to see how to use the compiler.');
	Application.Terminate;
	EXIT
      END;
      fInputFilename := Application.Params[1];
    { Parse all parameters. }
      IF Application.ParamCount > 1 THEN
	FOR Ndx := 2 TO Application.ParamCount DO
	BEGIN
	  IF NOT ParseCommand (Application.Params[Ndx]) THEN
	  BEGIN
	    Application.Trace (
	      etError,
	      Format ('Illegal parameter: %s', [Application.Params[Ndx]])
	    );
	    WriteLn ('Use command "--help" to see the list of commands.');
	  END;
	{ If application is terminated (i.e. an error), do not continue. }
	  IF Application.Terminated THEN EXIT
	END;
    END
  END;



(*
 * Tz80PascalApplication
 ****************************************************************************)

(* Application execution. *)
  PROCEDURE Tz80PascalApplication.DoRun;

    PROCEDURE WriteSymbolDescription;
    BEGIN
      WriteLn (Format (
	'"%s" - [%d] %s',
	[
	  fScanner.Symbol,
	  fScanner.SymbolId,
	  fScanner.GetSymbolName (fScanner.SymbolId)
	]
      ))
    END;

  BEGIN
    fScanner.Initialize;
    WHILE NOT SELF.Terminated DO
    BEGIN
      WriteSymbolDescription;
      fScanner.GetNext;
      IF fScanner.CheckEOF THEN SELF.Terminate
    END
  END;



(* Constructor. *)
  CONSTRUCTOR Tz80PascalApplication.Create (aOwner: TComponent);
  BEGIN
    INHERITED Create (aOwner);
    fConfiguration := Tz80Configuration.Create;
    fScanner := TPascalScanner.Create
  END;



(* Destructor. *)
  DESTRUCTOR Tz80PascalApplication.Destroy;
  BEGIN
    fScanner.Free;
    fInputFile.Free;
    fConfiguration.Free;
    INHERITED Destroy
  END;



(* Shows an exception to the user. *)
  PROCEDURE Tz80PascalApplication.ShowException (E: Exception);
  BEGIN
    WriteLn (StringOfChar ('=', 75));
    WriteLn ('An exception raised:');
    WriteLn (E.Message);
    WriteLn (StringOfChar ('=', 75))
  END;



(* Write a message to the standard output (verbose). *)
  PROCEDURE Tz80PascalApplication.Trace (EventType: TEventType; CONST Msg: STRING);
  VAR
    FinalMsg: STRING;
  BEGIN
  { Only verbose when it is verbose. }
    IF EventType IN fConfiguration.Verbosity THEN
    BEGIN
      CASE EventType OF
      etCustom, etInfo:
	BEGIN
	  FinalMsg := 'Hint: ' + Msg;
	  IF fConfiguration.StopOnHint THEN SELF.Terminate
	END;
      etWarning:
	BEGIN
	  FinalMsg := 'Warning: ' + Msg;
	  IF fConfiguration.StopOnWarning THEN SELF.Terminate
	END;
      etError:
	BEGIN
	  FinalMsg := 'Error: ' + Msg;
	  SELF.Terminate { Errors stops compiling always. }
	END;
      etDebug:
	FinalMsg := '[dbg] ' + Msg;
      END;
      WriteLn (stdout, FinalMsg)
    END
  END;



(* Initializes application. *)
  PROCEDURE Tz80PascalApplication.Initialize;
  BEGIN
    INHERITED Initialize;
    TRY
      SELF.Title := 'z80-Pascal';
      SELF.StopOnException := TRUE;
      fConfiguration.Initialize;
      IF fConfiguration.InputFilename = '' THEN
      BEGIN
	SELF.Trace (etError, 'No input file.');
	WriteLn ('Use command "--help" to see how to use the compiler.');
	SELF.Terminate;
	EXIT
      END;
      IF NOT FileExists (fConfiguration.InputFilename) THEN
      BEGIN
	SELF.Trace (
	  etError,
	  Format ('Input file "%s" doesn''t exist.', [fConfiguration.InputFilename])
	);
	WriteLn ('Use command "--help" to see how to use the compiler.');
	SELF.Terminate;
	EXIT
      END;
    { Opens input file. }
      fInputFile := TFileStream.Create (fConfiguration.InputFilename, fmOpenRead);
      fScanner.Source := fInputFile
    EXCEPT
      ON lException: Exception DO
      BEGIN
	SELF.ShowException (lException);
	SELF.Terminate
      END
    END
  END;



INITIALIZATION
  Application := Tz80PascalApplication.Create (NIL)
FINALIZATION
  Application.Free
END.
