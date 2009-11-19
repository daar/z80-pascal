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
  ListsComments, ListsSourcecode: BOOLEAN;
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
    WriteLn ('  -a     Lists comments in assembler file');
    WriteLn ('      -al    List sourcecode lines in assembler file');
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
    OutputFileName := 'out.asm';
    ListsComments := FALSE;
    ListsSourcecode := FALSE;
    Verbose := [vblErrors];
  END;



(* Loads and parses the configuration files. *)
  PROCEDURE LoadFile;
  VAR
    ConfigFile: TStringList;
  BEGIN
    IF FileExists (GetAppConfigFile (FALSE)) THEN
    BEGIN
      ConfigFile := TStringList.Create;
      TRY
	ConfigFile.LoadFromFile (GetAppConfigFile (FALSE));
      FINALLY
	ConfigFile.Free;
      END;
    END;
  END;



(* Checks the parameter list and sets the configuration values. *)
  PROCEDURE CheckParameterList;
  VAR
    Cnt, Ndx: INTEGER;
  BEGIN
    IF ParamCount < 1 THEN
      Help;
    FOR Cnt := 1 TO ParamCount DO
    BEGIN
      IF ParamStr (Cnt)[1] = '-' THEN
      BEGIN
	CASE ParamStr (Cnt)[2] OF
	'a':
	  BEGIN
	    ListsComments := TRUE;
	    IF (Length (ParamStr (Cnt)) > 2) AND (ParamStr (Cnt)[3] = 'l') THEN
	      ListsSourcecode := TRUE;
	  END;
	'c':
	  BEGIN
	    IF ParamStr (Cnt) <> '-cfg' THEN
	      RAISE Exception.Create ('Unknown parameter ''' + ParamStr (Cnt) + '''');
	    WriteLn ('Config file path: ', GetAppConfigFile (FALSE));
	  END;
	'o':
	  OutputFileName := RightStr (ParamStr (Cnt), Length (ParamStr (Cnt)) - 2);
	'v':
	  BEGIN
	    Verbose := [];
	    FOR Ndx := 3 TO Length (ParamStr (Cnt)) DO
	      CASE ParamStr (Cnt)[Ndx] OF
	      'e':
		Include (Verbose, vblErrors);
	      'w':
		Include (Verbose, vblWarnings);
	      ELSE
		RAISE Exception.Create ('Unknown parameter ''' + ParamStr (Cnt) + '''');
	      END;
	  END;
	ELSE
	  RAISE Exception.Create ('Unknown parameter ''' + ParamStr (Cnt) + '''');
	END;
      END
      ELSE
	InputFileName := ParamStr (Cnt);
    END;
  END;



(* Gets the configuration. *)
  PROCEDURE Load;
  BEGIN
    Configuration.Default;
    LoadFile;
    CheckParameterList;
  END;



(* Releases resources if any. *)
  PROCEDURE Unload;
  BEGIN
  END;

END.
