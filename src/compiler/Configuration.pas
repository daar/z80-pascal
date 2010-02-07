(* Obtains and stores the configuration.
 * It should be used as an object, i. e.: Configuration.Verbose. *)
UNIT Configuration;

INTERFACE

TYPE
(* Defines the level of "verbosity". *)
  VERBOSITY_LEVELS = (
    vblErrors,		(*< Shows compilation errors. *)
    vblWarnings		(*< Shows warnings. *)
  );
  VERBOSITY = SET OF VERBOSITY_LEVELS;



VAR
(* The source and the output files. *)
  InputFileName, OutputFileName: STRING;
(* Lists sourcecodelines in assembler file. *)
  ListsComments: BOOLEAN;
(* Verbosity level. *)
  Verbose: VERBOSITY;



(* Gets the configuration. *)
  PROCEDURE Load;

(* Releases resources if any. *)
  PROCEDURE Unload;



IMPLEMENTATION

USES
  Classes, sysutils;



(* Shows a help message with description of available options. *)
  PROCEDURE Help;
  BEGIN
    WriteLn ('Usage:');
    WriteLn ('  z80pas [options] <inputfile> [options]');
    WriteLn ('Options:');
    WriteLn ('  -a     List source comments in assembler file');
    WriteLn ('  -cfg   Returns the full path to the config file');
  {
    WriteLn ('  -i     Information');
    WriteLn ('      -iD        Return compiler date');
    WriteLn ('      -iW        Return full compiler version');
  }
    WriteLn ('  -o<x>  Change the name of the assembler file produced to <x>');
    WriteLn ('  -v<x>  Be verbose. <x> is a combination of the following letters:');
    WriteLn ('     e : Show errors (default)');
    WriteLn ('     w : Show warnings');
  END;



(* Sets the default configuration. *)
  PROCEDURE Default;
  BEGIN
    OutputFileName := '';
    ListsComments := FALSE;
    Verbose := [vblErrors];
  END;



(* Parses a command. *)
  PROCEDURE ParseCommand (Command: STRING);
  VAR
    Ndx: INTEGER;
  BEGIN
    IF Command[1] <> '-' THEN
      RAISE Exception.Create ('Unknown parameter ''' + Command + '''');
    CASE Command[2] OF
    'a':
      BEGIN
	ListsComments := TRUE;
      END;
    'c':
      BEGIN
	IF Command <> '-cfg' THEN
	  RAISE Exception.Create ('Unknown parameter ''' + Command + '''');
	WriteLn ('Config file path: ', GetAppConfigFile (FALSE));
      END;
    'o':
      OutputFileName := RightStr (Command, Length (Command) - 2);
    'v':
      BEGIN
	Verbose := []; { To cancel previous configuration. }
	FOR Ndx := 3 TO Length (Command) DO
	  CASE Command[Ndx] OF
	  'e':
	    Include (Verbose, vblErrors);
	  'w':
	    Include (Verbose, vblWarnings);
	  ELSE
	    RAISE Exception.Create ('Unknown parameter ''' + Command + '''');
	  END;
      END;
    ELSE
      RAISE Exception.Create ('Unknown parameter ''' + Command + '''');
    END;
  END;



(* Loads and parses the configuration files. *)
  PROCEDURE LoadFile;
  VAR
    ConfigFile: TStringList;
    Ndx: INTEGER;
  BEGIN
    IF FileExists (GetAppConfigFile (FALSE)) THEN
    BEGIN
      ConfigFile := TStringList.Create;
      ConfigFile.LoadFromFile (GetAppConfigFile (FALSE));
      TRY
        FOR Ndx := 0 TO ConfigFile.Count - 1 DO
	  IF (ConfigFile[Ndx] <> '') AND (ConfigFile[Ndx][1] <> '#') THEN
	    ParseCommand (ConfigFile[Ndx]);
      FINALLY
	ConfigFile.Free;
      END;
    END;
  END;



(* Checks the parameter list and sets the configuration values. *)
  PROCEDURE CheckParameterList;
  VAR
    Cnt: INTEGER;
  BEGIN
    IF ParamCount < 1 THEN
      Help;
    FOR Cnt := 1 TO ParamCount DO
    BEGIN
      IF ParamStr (Cnt)[1] = '-' THEN
	ParseCommand (ParamStr (Cnt))
      ELSE
	InputFileName := ParamStr (Cnt);
    END;
  END;



(* Checks configuration to avoid mistakes. *)
  PROCEDURE CheckConfiguration;
  VAR
    Tmp: STRING;
  BEGIN
  { Check verbose. }
    IF vblWarnings IN Verbose THEN
      Include (Verbose, vblErrors);
  { Must have output file name. }
    IF OutputFileName = '' THEN
    BEGIN
      Tmp := ExtractFileName (InputFileName);
      OutputFileName := LeftStr (Tmp, Length (Tmp) - Length (ExtractFileExt (Tmp))) + '.asm';
    END;
  END;



(* Gets the configuration. *)
  PROCEDURE Load;
  BEGIN
    Default;
    LoadFile;
    CheckParameterList;
    CheckConfiguration;
  END;



(* Releases resources if any. *)
  PROCEDURE Unload;
  BEGIN
  END;

END.
