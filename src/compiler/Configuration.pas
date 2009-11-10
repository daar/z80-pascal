(* Obtains and stores the configuration.
 * It should be used as an object, i. e.: Configuration.Verbose. *)
UNIT Configuration;

INTERFACE

TYPE
(* Defines the level of "verbosity". *)
  VERBOSITY_LEVELS = (
    vblNone,		(*< Used to make it "quiet".  Shouldn be combined. *)
    vblErrors,		(*< Shows compilation errors. *)
    vblWarnings		(*< Shows warnings. *)
  );
  VERBOSITY = SET OF VERBOSITY_LEVELS;



VAR
(* The source and the output files. *)
  InputFileName, OutputFileName: STRING;
(* Lists sourcecodelines in assembler file. *)
  ListsSourcecode: BOOLEAN;
(* Verbosity level. *)
  Verbose: VERBOSITY;



(* Gets the configuration. *)
  PROCEDURE Load;

(* Releases resources if any. *)
  PROCEDURE Unload;



IMPLEMENTATION

VAR
(* Config only options. *)
  SaveConfigAt: STRING; (*< If set, saves the configuration here. *)
  ParseConfigFile: BOOLEAN;



(* Shows a help message with description of available options. *)
  PROCEDURE Help;
  BEGIN
    WriteLn ('Usage:');
    WriteLn ('  z80pas [options] <inputfile> [options]');
    WriteLn ('Options:');
    {    WriteLn ('  -al    List sourcecode lines in assembler file'); }
    WriteLn ('  -c<x>  Save the configuration options to <x> file');
    WriteLn ('  -n     Do not read the default config files');
    WriteLn ('  -o<x>  Change the name of the executable produced to <x>');
    WriteLn ('  -v<x>  Be verbose. <x> is a combination of the following letters:');
    WriteLn ('     e : Show errors (default)');
    WriteLn ('     w : Show warnings');
  END;



(* Sets the default configuration. *)
  PROCEDURE Default;
  BEGIN
    SaveConfigAt := '';
    ParseConfigFile := TRUE;
    OutputFileName := 'out.asm';
    ListsSourcecode := FALSE;
    Verbose := [vblErrors];
  END;



(* Avoids configuration options that are uncompatible. *)
  PROCEDURE CheckOptions;
  BEGIN
  END;



(* Loads and parses the configuration files. *)
  PROCEDURE LoadFiles;
  BEGIN
    IF NOT ParseConfigFile THEN
      EXIT;
  END;



(* Checks the parameter list and sets the configuration values. *)
  PROCEDURE CheckParameterList;
  BEGIN
    IF ParamCount < 1 THEN
      Help;
  { TODO: Options. }
    InputFileName := ParamStr(1);
    OutputFileName := ParamStr(2);
  END;



(* Gets the configuration. *)
  PROCEDURE Load;
  BEGIN
    Configuration.Default;
    CheckParameterList;
    CheckOptions
  END;



(* Releases resources if any. *)
  PROCEDURE Unload;
  BEGIN
  END;

END.
