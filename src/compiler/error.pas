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
         0 : PrintErrorLine ('EOF expected');
         1 : PrintErrorLine ('identifier expected');
         2 : PrintErrorLine ('integer expected');
         3 : PrintErrorLine ('hexinteger expected');
         4 : PrintErrorLine ('real expected');
         5 : PrintErrorLine ('string expected');
         6 : PrintErrorLine ('"PROGRAM" expected');
         7 : PrintErrorLine ('";" expected');
         8 : PrintErrorLine ('"." expected');
         9 : PrintErrorLine ('"CONST" expected');
        10 : PrintErrorLine ('"=" expected');
        11 : PrintErrorLine ('"+" expected');
        12 : PrintErrorLine ('"-" expected');
        13 : PrintErrorLine ('"TYPE" expected');
        14 : PrintErrorLine ('"PACKED" expected');
        15 : PrintErrorLine ('"^" expected');
        16 : PrintErrorLine ('"(" expected');
        17 : PrintErrorLine ('")" expected');
        18 : PrintErrorLine ('".." expected');
        19 : PrintErrorLine ('"ARRAY" expected');
        20 : PrintErrorLine ('"[" expected');
        21 : PrintErrorLine ('"]" expected');
        22 : PrintErrorLine ('"OF" expected');
        23 : PrintErrorLine ('"," expected');
        24 : PrintErrorLine ('"RECORD" expected');
        25 : PrintErrorLine ('"END" expected');
        26 : PrintErrorLine ('"SET" expected');
        27 : PrintErrorLine ('"FILE" expected');
        28 : PrintErrorLine ('":" expected');
        29 : PrintErrorLine ('"CASE" expected');
        30 : PrintErrorLine ('"VAR" expected');
        31 : PrintErrorLine ('"PROCEDURE" expected');
        32 : PrintErrorLine ('"FUNCTION" expected');
        33 : PrintErrorLine ('"FORWARD" expected');
        34 : PrintErrorLine ('"BEGIN" expected');
        35 : PrintErrorLine ('":=" expected');
        36 : PrintErrorLine ('"@" expected');
        37 : PrintErrorLine ('"WHILE" expected');
        38 : PrintErrorLine ('"DO" expected');
        39 : PrintErrorLine ('"REPEAT" expected');
        40 : PrintErrorLine ('"UNTIL" expected');
        41 : PrintErrorLine ('"IF" expected');
        42 : PrintErrorLine ('"THEN" expected');
        43 : PrintErrorLine ('"ELSE" expected');
        44 : PrintErrorLine ('"FOR" expected');
        45 : PrintErrorLine ('"TO" expected');
        46 : PrintErrorLine ('"DOWNTO" expected');
        47 : PrintErrorLine ('"WITH" expected');
        48 : PrintErrorLine ('"<" expected');
        49 : PrintErrorLine ('">" expected');
        50 : PrintErrorLine ('"<=" expected');
        51 : PrintErrorLine ('">=" expected');
        52 : PrintErrorLine ('"<>" expected');
        53 : PrintErrorLine ('"IN" expected');
        54 : PrintErrorLine ('"OR" expected');
        55 : PrintErrorLine ('"*" expected');
        56 : PrintErrorLine ('"/" expected');
        57 : PrintErrorLine ('"DIV" expected');
        58 : PrintErrorLine ('"MOD" expected');
        59 : PrintErrorLine ('"AND" expected');
        60 : PrintErrorLine ('"NOT" expected');
        61 : PrintErrorLine ('"NIL" expected');
        62 : PrintErrorLine ('"ASM" expected');
        63 : PrintErrorLine ('"ADC" expected');
        64 : PrintErrorLine ('"ADD" expected');
        65 : PrintErrorLine ('"BIT" expected');
        66 : PrintErrorLine ('"CALL" expected');
        67 : PrintErrorLine ('"CCF" expected');
        68 : PrintErrorLine ('"CP" expected');
        69 : PrintErrorLine ('"CPD" expected');
        70 : PrintErrorLine ('"CPDR" expected');
        71 : PrintErrorLine ('"CPI" expected');
        72 : PrintErrorLine ('"CPIR" expected');
        73 : PrintErrorLine ('"CPL" expected');
        74 : PrintErrorLine ('"DAA" expected');
        75 : PrintErrorLine ('"DEC" expected');
        76 : PrintErrorLine ('"DI" expected');
        77 : PrintErrorLine ('"DJNZ," expected');
        78 : PrintErrorLine ('"EI" expected');
        79 : PrintErrorLine ('"EX" expected');
        80 : PrintErrorLine ('"EXX" expected');
        81 : PrintErrorLine ('"HALT" expected');
        82 : PrintErrorLine ('"IM" expected');
        83 : PrintErrorLine ('"INC" expected');
        84 : PrintErrorLine ('"IND" expected');
        85 : PrintErrorLine ('"INDR" expected');
        86 : PrintErrorLine ('"INI" expected');
        87 : PrintErrorLine ('"INIR" expected');
        88 : PrintErrorLine ('"JP" expected');
        89 : PrintErrorLine ('"JR" expected');
        90 : PrintErrorLine ('"LD" expected');
        91 : PrintErrorLine ('"LDD" expected');
        92 : PrintErrorLine ('"LDDR" expected');
        93 : PrintErrorLine ('"LDI" expected');
        94 : PrintErrorLine ('"LDIR" expected');
        95 : PrintErrorLine ('"NEG" expected');
        96 : PrintErrorLine ('"NOP" expected');
        97 : PrintErrorLine ('"OTDR" expected');
        98 : PrintErrorLine ('"OTIR" expected');
        99 : PrintErrorLine ('"OUT" expected');
       100 : PrintErrorLine ('"OUTD" expected');
       101 : PrintErrorLine ('"OUTI" expected');
       102 : PrintErrorLine ('"POP" expected');
       103 : PrintErrorLine ('"PUSH" expected');
       104 : PrintErrorLine ('"RES" expected');
       105 : PrintErrorLine ('"RET" expected');
       106 : PrintErrorLine ('"RETI" expected');
       107 : PrintErrorLine ('"RETN" expected');
       108 : PrintErrorLine ('"RL" expected');
       109 : PrintErrorLine ('"RLA" expected');
       110 : PrintErrorLine ('"RLC" expected');
       111 : PrintErrorLine ('"RLCA" expected');
       112 : PrintErrorLine ('"RLD" expected');
       113 : PrintErrorLine ('"RR" expected');
       114 : PrintErrorLine ('"RRA" expected');
       115 : PrintErrorLine ('"RRC" expected');
       116 : PrintErrorLine ('"RRD" expected');
       117 : PrintErrorLine ('"RST" expected');
       118 : PrintErrorLine ('"SCF" expected');
       119 : PrintErrorLine ('"SLA" expected');
       120 : PrintErrorLine ('"SRA" expected');
       121 : PrintErrorLine ('"SBC" expected');
       122 : PrintErrorLine ('"SUB" expected');
       123 : PrintErrorLine ('"XOR" expected');
       124 : PrintErrorLine ('"M" expected');
       125 : PrintErrorLine ('"NC" expected');
       126 : PrintErrorLine ('"NP" expected');
       127 : PrintErrorLine ('"NZ" expected');
       128 : PrintErrorLine ('"P" expected');
       129 : PrintErrorLine ('"PE" expected');
       130 : PrintErrorLine ('"PO" expected');
       131 : PrintErrorLine ('"Z" expected');
       132 : PrintErrorLine ('"BC" expected');
       133 : PrintErrorLine ('"DE" expected');
       134 : PrintErrorLine ('"HL" expected');
       135 : PrintErrorLine ('"IX" expected');
       136 : PrintErrorLine ('"IY" expected');
       137 : PrintErrorLine ('"A" expected');
       138 : PrintErrorLine ('"H" expected');
       139 : PrintErrorLine ('"L" expected');
       140 : PrintErrorLine ('"B" expected');
       141 : PrintErrorLine ('"C" expected');
       142 : PrintErrorLine ('"D" expected');
       143 : PrintErrorLine ('"E" expected');
       144 : PrintErrorLine ('"AF" expected');
       145 : PrintErrorLine ('"SP" expected');
       146 : PrintErrorLine ('"PC" expected');
       147 : PrintErrorLine ('not expected');
       148 : PrintErrorLine ('invalid Z80Register16bit');
       149 : PrintErrorLine ('invalid Z80Register8bit');
       150 : PrintErrorLine ('invalid Z80Register');
       151 : PrintErrorLine ('invalid Z80Indirection');
       152 : PrintErrorLine ('invalid Z80Condition');
       153 : PrintErrorLine ('invalid Z80Integer');
       154 : PrintErrorLine ('invalid Z80Parameter');
       155 : PrintErrorLine ('invalid Z80Instruction');
       156 : PrintErrorLine ('invalid UnsignedLiteral');
       157 : PrintErrorLine ('invalid MulOp');
       158 : PrintErrorLine ('invalid Factor');
       159 : PrintErrorLine ('invalid AddOp');
       160 : PrintErrorLine ('invalid RelOp');
       161 : PrintErrorLine ('invalid SimpleExpression');
       162 : PrintErrorLine ('invalid Value');
       163 : PrintErrorLine ('invalid ForStatement');
       164 : PrintErrorLine ('invalid AssignmentOrCall');
       165 : PrintErrorLine ('invalid ParamType');
       166 : PrintErrorLine ('invalid FormalSection');
       167 : PrintErrorLine ('invalid Body');
       168 : PrintErrorLine ('invalid StructType');
       169 : PrintErrorLine ('invalid SimpleType');
       170 : PrintErrorLine ('invalid Type');
       171 : PrintErrorLine ('invalid UnsignedInt');
       172 : PrintErrorLine ('invalid UnsignedNumber');
       173 : PrintErrorLine ('invalid Constant');
       174 : PrintErrorLine ('invalid Constant');
       175 : PrintErrorLine ('invalid ProcDeclarations');
       176 : PrintErrorLine ('invalid StatementPart');
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
