(* Error handling module. *)
UNIT Error;

{$mode objfpc} {$H+}

INTERFACE

VAR
  CurrentFile: STRING;

PROCEDURE InitModule;

PROCEDURE PrintErrors;



IMPLEMENTATION

USES
  Z80PasS;



TYPE
(* Linked list to store errors, warnings and notes. *)
  PEvent = ^EventDesc;
  EventDesc = RECORD
    nr, line, col: INTEGER;
    FileName: STRING;
    next: PEvent;
  END;
  CHARSET = SET OF CHAR;



CONST
  TAB  = #09;
  _LF  = #10;
  _CR  = #13;
  _EF  = #0;



VAR
  FirstEvt, LastEvt: PEvent;



(* Stores a compilation event at the list. *)
  PROCEDURE StoreEvent (nr, line, col: SMALLINT; pos: LONGINT);
  VAR
    NextEvt: PEvent;
  BEGIN
    NEW (NextEvt);
    NextEvt^.nr := nr; NextEvt^.line := line; NextEvt^.col := col;
    NextEvt^.FileName := CurrentFile;
    NextEvt^.next := NIL;
    IF FirstEvt = NIL THEN
      FirstEvt := NextEvt
    ELSE
      LastEvt^.next := NextEvt;
    LastEvt := NextEvt;
    INC (errors);
  END;



  PROCEDURE InitModule;
  BEGIN
  { Initialices. }
    FirstEvt := NIL;
    Z80PasS.Error := @StoreEvent;
  END;



(* Prints all messages stored. *)
  PROCEDURE PrintErrors;
  VAR
    NumErrors: INTEGER;

  (* Print an error message *)
    PROCEDURE PrintMessage (FileName: STRING; nr, line, col: INTEGER);

      PROCEDURE PrintErrorLine (Message: STRING);
      BEGIN
	WriteLn (FileName, '(', line, ', ', col, ') ERROR: ', Message);
	INC (NumErrors);
      END;

    BEGIN
      CASE nr OF
         0 : PrintErrorLine('EOF expected');
         1 : PrintErrorLine('identifier expected');
         2 : PrintErrorLine('integer expected');
         3 : PrintErrorLine('hexinteger expected');
         4 : PrintErrorLine('real expected');
         5 : PrintErrorLine('string expected');
         6 : PrintErrorLine('"PROGRAM" expected');
         7 : PrintErrorLine('";" expected');
         8 : PrintErrorLine('"." expected');
         9 : PrintErrorLine('"CONST" expected');
        10 : PrintErrorLine('"=" expected');
        11 : PrintErrorLine('"+" expected');
        12 : PrintErrorLine('"-" expected');
        13 : PrintErrorLine('"TYPE" expected');
        14 : PrintErrorLine('"PACKED" expected');
        15 : PrintErrorLine('"^" expected');
        16 : PrintErrorLine('"(" expected');
        17 : PrintErrorLine('")" expected');
        18 : PrintErrorLine('".." expected');
        19 : PrintErrorLine('"ARRAY" expected');
        20 : PrintErrorLine('"[" expected');
        21 : PrintErrorLine('"]" expected');
        22 : PrintErrorLine('"OF" expected');
        23 : PrintErrorLine('"," expected');
        24 : PrintErrorLine('"RECORD" expected');
        25 : PrintErrorLine('"END" expected');
        26 : PrintErrorLine('"SET" expected');
        27 : PrintErrorLine('"FILE" expected');
        28 : PrintErrorLine('":" expected');
        29 : PrintErrorLine('"CASE" expected');
        30 : PrintErrorLine('"VAR" expected');
        31 : PrintErrorLine('"PROCEDURE" expected');
        32 : PrintErrorLine('"FUNCTION" expected');
        33 : PrintErrorLine('"FORWARD" expected');
        34 : PrintErrorLine('"BEGIN" expected');
        35 : PrintErrorLine('":=" expected');
        36 : PrintErrorLine('"@" expected');
        37 : PrintErrorLine('"WHILE" expected');
        38 : PrintErrorLine('"DO" expected');
        39 : PrintErrorLine('"REPEAT" expected');
        40 : PrintErrorLine('"UNTIL" expected');
        41 : PrintErrorLine('"IF" expected');
        42 : PrintErrorLine('"THEN" expected');
        43 : PrintErrorLine('"ELSE" expected');
        44 : PrintErrorLine('"FOR" expected');
        45 : PrintErrorLine('"TO" expected');
        46 : PrintErrorLine('"DOWNTO" expected');
        47 : PrintErrorLine('"WITH" expected');
        48 : PrintErrorLine('"<" expected');
        49 : PrintErrorLine('">" expected');
        50 : PrintErrorLine('"<=" expected');
        51 : PrintErrorLine('">=" expected');
        52 : PrintErrorLine('"<>" expected');
        53 : PrintErrorLine('"IN" expected');
        54 : PrintErrorLine('"OR" expected');
        55 : PrintErrorLine('"*" expected');
        56 : PrintErrorLine('"/" expected');
        57 : PrintErrorLine('"DIV" expected');
        58 : PrintErrorLine('"MOD" expected');
        59 : PrintErrorLine('"AND" expected');
        60 : PrintErrorLine('"NOT" expected');
        61 : PrintErrorLine('"NIL" expected');
        62 : PrintErrorLine('"ASM" expected');
        63 : PrintErrorLine('"ADC" expected');
        64 : PrintErrorLine('"ADD" expected');
        65 : PrintErrorLine('"XOR" expected');
        66 : PrintErrorLine('"BIT" expected');
        67 : PrintErrorLine('"RES" expected');
        68 : PrintErrorLine('"CALL" expected');
        69 : PrintErrorLine('"CP" expected');
        70 : PrintErrorLine('"DEC" expected');
        71 : PrintErrorLine('"INC" expected');
        72 : PrintErrorLine('"CCF" expected');
        73 : PrintErrorLine('"CPD" expected');
        74 : PrintErrorLine('"CPDR" expected');
        75 : PrintErrorLine('"CPI" expected');
        76 : PrintErrorLine('"CPIR" expected');
        77 : PrintErrorLine('"CPL" expected');
        78 : PrintErrorLine('"DAA" expected');
        79 : PrintErrorLine('"DI" expected');
        80 : PrintErrorLine('"EI" expected');
        81 : PrintErrorLine('"EXX" expected');
        82 : PrintErrorLine('"HALT" expected');
        83 : PrintErrorLine('"IND" expected');
        84 : PrintErrorLine('"INDR" expected');
        85 : PrintErrorLine('"INI" expected');
        86 : PrintErrorLine('"INIR" expected');
        87 : PrintErrorLine('"LDD" expected');
        88 : PrintErrorLine('"LDDR" expected');
        89 : PrintErrorLine('"LDI" expected');
        90 : PrintErrorLine('"LDIR" expected');
        91 : PrintErrorLine('"NEG" expected');
        92 : PrintErrorLine('"NOP" expected');
        93 : PrintErrorLine('"OTDR" expected');
        94 : PrintErrorLine('"OTIR" expected');
        95 : PrintErrorLine('"OUTD" expected');
        96 : PrintErrorLine('"OUTI" expected');
        97 : PrintErrorLine('"RETI" expected');
        98 : PrintErrorLine('"RETN" expected');
        99 : PrintErrorLine('"RLA" expected');
       100 : PrintErrorLine('"RLCA" expected');
       101 : PrintErrorLine('"RLD" expected');
       102 : PrintErrorLine('"RRA" expected');
       103 : PrintErrorLine('"RRD" expected');
       104 : PrintErrorLine('"SCF" expected');
       105 : PrintErrorLine('"DJNZ," expected');
       106 : PrintErrorLine('"EX" expected');
       107 : PrintErrorLine('"[SP]" expected');
       108 : PrintErrorLine('"HL" expected');
       109 : PrintErrorLine('"IX" expected');
       110 : PrintErrorLine('"IY" expected');
       111 : PrintErrorLine('"AF" expected');
       112 : PrintErrorLine('"AF"" expected');
       113 : PrintErrorLine('"DE" expected');
       114 : PrintErrorLine('"IM" expected');
       115 : PrintErrorLine('"OUT" expected');
       116 : PrintErrorLine('"C" expected');
       117 : PrintErrorLine('"JP" expected');
       118 : PrintErrorLine('"JR" expected');
       119 : PrintErrorLine('"LD" expected');
       120 : PrintErrorLine('"PUSH" expected');
       121 : PrintErrorLine('"POP" expected');
       122 : PrintErrorLine('"RET" expected');
       123 : PrintErrorLine('"RL" expected');
       124 : PrintErrorLine('"RLC" expected');
       125 : PrintErrorLine('"RR" expected');
       126 : PrintErrorLine('"RRC" expected');
       127 : PrintErrorLine('"SLA" expected');
       128 : PrintErrorLine('"SRA" expected');
       129 : PrintErrorLine('"RST" expected');
       130 : PrintErrorLine('"SBC" expected');
       131 : PrintErrorLine('"A" expected');
       132 : PrintErrorLine('"SUB" expected');
       133 : PrintErrorLine('"Z" expected');
       134 : PrintErrorLine('"NZ" expected');
       135 : PrintErrorLine('"NC" expected');
       136 : PrintErrorLine('"H" expected');
       137 : PrintErrorLine('"L" expected');
       138 : PrintErrorLine('"B" expected');
       139 : PrintErrorLine('"D" expected');
       140 : PrintErrorLine('"E" expected');
       141 : PrintErrorLine('"BC" expected');
       142 : PrintErrorLine('"SP" expected');
       143 : PrintErrorLine('"PC" expected');
       144 : PrintErrorLine('not expected');
       145 : PrintErrorLine('invalid Z80Register16bit');
       146 : PrintErrorLine('invalid Z80Integer');
       147 : PrintErrorLine('invalid Z80Condition');
       148 : PrintErrorLine('invalid Z80Register8bit');
       149 : PrintErrorLine('invalid Z80Indirection');
       150 : PrintErrorLine('invalid Z80Parameter8bit');
       151 : PrintErrorLine('invalid Z80Parameter');
       152 : PrintErrorLine('invalid Z80Register');
       153 : PrintErrorLine('invalid Z80Sub');
       154 : PrintErrorLine('invalid Z80Sub');
       155 : PrintErrorLine('invalid Z80Rotate');
       156 : PrintErrorLine('invalid Z80Stack');
       157 : PrintErrorLine('invalid Z80Port');
       158 : PrintErrorLine('invalid Z80Port');
       159 : PrintErrorLine('invalid Z80Exchange');
       160 : PrintErrorLine('invalid Z80Exchange');
       161 : PrintErrorLine('invalid Z80Operation');
       162 : PrintErrorLine('invalid Z80Inc');
       163 : PrintErrorLine('invalid Z80Inc');
       164 : PrintErrorLine('invalid Z80Bit');
       165 : PrintErrorLine('invalid Z80Bit');
       166 : PrintErrorLine('invalid Z80Bin');
       167 : PrintErrorLine('invalid Z80Sum');
       168 : PrintErrorLine('invalid UnsignedLiteral');
       169 : PrintErrorLine('invalid MulOp');
       170 : PrintErrorLine('invalid Factor');
       171 : PrintErrorLine('invalid AddOp');
       172 : PrintErrorLine('invalid RelOp');
       173 : PrintErrorLine('invalid SimpleExpression');
       174 : PrintErrorLine('invalid Value');
       175 : PrintErrorLine('invalid ForStatement');
       176 : PrintErrorLine('invalid AssignmentOrCall');
       177 : PrintErrorLine('invalid ParamType');
       178 : PrintErrorLine('invalid FormalSection');
       179 : PrintErrorLine('invalid Body');
       180 : PrintErrorLine('invalid StructType');
       181 : PrintErrorLine('invalid SimpleType');
       182 : PrintErrorLine('invalid Type');
       183 : PrintErrorLine('invalid UnsignedInt');
       184 : PrintErrorLine('invalid UnsignedNumber');
       185 : PrintErrorLine('invalid Constant');
       186 : PrintErrorLine('invalid Constant');
       187 : PrintErrorLine('invalid ProcDeclarations');
       188 : PrintErrorLine('invalid StatementPart');
     (* add your customized cases here *)
       ELSE PrintErrorLine ('Unknown');
     END;
    END;

  { PROCEDURE PrintErrors (InputFilename: STRING); }
  VAR
    Event: PEvent;
  BEGIN
    NumErrors := 0;
    Event := FirstEvt;
    WHILE Event <> NIL DO
    BEGIN
      PrintMessage (Event^.FileName, Event^.nr, Event^.line, Event^.col);
      Event := Event^.next;
    END;
    WriteLn ('Found ', NumErrors,' error(s).');
  END;

END.
