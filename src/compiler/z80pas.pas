PROGRAM Z80Pas;
(* This is an example of a rudimentary main module for use with COCO/R (Pascal)
   The auxiliary modules <Grammar>S (scanner) and <Grammar>P (parser)
   are assumed to have been constructed with COCO/R compiler generator. *)

USES Z80PasS, (* lst, src, errors, Error, CharAt *)
     Z80PasP;  (* Parse, Successful *)

PROCEDURE AppendExtension (OldName, Ext : STRING; VAR NewName : STRING);
  VAR
    i : INTEGER;
  BEGIN
    i := System.Length(OldName);
    WHILE (i > 0) AND (OldName[i] <> '.') AND (OldName[i] <> '\') DO DEC(i);
    IF (i > 0) AND (OldName[i] = '.') THEN System.Delete(OldName, i, 255);
    IF System.Pos('.', Ext) = 1 THEN System.Delete(Ext, 1, 1);
    NewName := OldName + '.' + Ext
  END;

(* ------------------- Source Listing and Error handler -------------- *)

  TYPE
    CHARSET = SET OF CHAR;
    Err = ^ErrDesc;
    ErrDesc = RECORD
      nr, line, col: INTEGER;
      next: Err
    END;

  CONST
    TAB  = #09;
    _LF  = #10;
    _CR  = #13;
    _EF  = #0;
    LineEnds : CHARSET = [_CR, _LF, _EF];

  VAR
    firstErr, lastErr: Err;
    Extra : INTEGER;

  PROCEDURE StoreError (nr, line, col: INTEGER; pos: LONGINT); FAR;
  (* Store an error message for later printing *)
    VAR
      nextErr: Err;
    BEGIN
      NEW(nextErr);
      nextErr^.nr := nr; nextErr^.line := line; nextErr^.col := col;
      nextErr^.next := NIL;
      IF firstErr = NIL
        THEN firstErr := nextErr
        ELSE lastErr^.next := nextErr;
      lastErr := nextErr;
      INC(errors)
    END;

  PROCEDURE GetLine (VAR pos  : LONGINT;
                     VAR line : STRING;
                     VAR eof  : BOOLEAN);
  (* Read a source line. Return empty line if eof *)
    VAR
      ch: CHAR;
      i:  INTEGER;
    BEGIN
      i := 1; eof := FALSE; ch := CharAt(pos); INC(pos);
      WHILE NOT (ch IN LineEnds) DO BEGIN
        line[i] := ch; INC(i); ch := CharAt(pos); INC(pos);
      END;
      line[0] := Chr(i-1);
      eof := (i = 1) AND (ch = _EF);
      IF ch = _CR THEN BEGIN (* check for MsDos *)
        ch := CharAt(pos);
        IF ch = _LF THEN BEGIN INC(pos); Extra := 0 END
      END
    END;

  PROCEDURE PrintErr (line : STRING; nr, col: INTEGER);
  (* Print an error message *)

    PROCEDURE Msg (s: STRING);
      BEGIN
        Write(lst, s)
      END;

    PROCEDURE Pointer;
      VAR
        i : INTEGER;
      BEGIN
        Write(lst, '*****  ');
        i := 0;
        WHILE i < col + Extra - 2 DO BEGIN
          IF line[i] = TAB
            THEN Write(lst, TAB)
            ELSE Write(lst, ' ');
          INC(i)
        END;
        Write(lst, '^ ')
      END;

    BEGIN
      Pointer;
      CASE nr OF
         0 : Msg('EOF expected');
         1 : Msg('identifier expected');
         2 : Msg('integer expected');
         3 : Msg('hexinteger expected');
         4 : Msg('real expected');
         5 : Msg('string expected');
         6 : Msg('"PROGRAM" expected');
         7 : Msg('";" expected');
         8 : Msg('"." expected');
         9 : Msg('"CONST" expected');
        10 : Msg('"=" expected');
        11 : Msg('"+" expected');
        12 : Msg('"-" expected');
        13 : Msg('"TYPE" expected');
        14 : Msg('"PACKED" expected');
        15 : Msg('"^" expected');
        16 : Msg('"(" expected');
        17 : Msg('")" expected');
        18 : Msg('".." expected');
        19 : Msg('"ARRAY" expected');
        20 : Msg('"[" expected');
        21 : Msg('"]" expected');
        22 : Msg('"OF" expected');
        23 : Msg('"," expected');
        24 : Msg('"RECORD" expected');
        25 : Msg('"END" expected');
        26 : Msg('"SET" expected');
        27 : Msg('"FILE" expected');
        28 : Msg('":" expected');
        29 : Msg('"CASE" expected');
        30 : Msg('"VAR" expected');
        31 : Msg('"PROCEDURE" expected');
        32 : Msg('"FUNCTION" expected');
        33 : Msg('"FORWARD" expected');
        34 : Msg('"BEGIN" expected');
        35 : Msg('":=" expected');
        36 : Msg('"@" expected');
        37 : Msg('"WHILE" expected');
        38 : Msg('"DO" expected');
        39 : Msg('"REPEAT" expected');
        40 : Msg('"UNTIL" expected');
        41 : Msg('"IF" expected');
        42 : Msg('"THEN" expected');
        43 : Msg('"ELSE" expected');
        44 : Msg('"FOR" expected');
        45 : Msg('"TO" expected');
        46 : Msg('"DOWNTO" expected');
        47 : Msg('"WITH" expected');
        48 : Msg('"<" expected');
        49 : Msg('">" expected');
        50 : Msg('"<=" expected');
        51 : Msg('">=" expected');
        52 : Msg('"<>" expected');
        53 : Msg('"IN" expected');
        54 : Msg('"OR" expected');
        55 : Msg('"*" expected');
        56 : Msg('"/" expected');
        57 : Msg('"DIV" expected');
        58 : Msg('"MOD" expected');
        59 : Msg('"AND" expected');
        60 : Msg('"NOT" expected');
        61 : Msg('"NIL" expected');
        62 : Msg('"ASM" expected');
        63 : Msg('"ADC" expected');
        64 : Msg('"ADD" expected');
        65 : Msg('"XOR" expected');
        66 : Msg('"BIT" expected');
        67 : Msg('"RES" expected');
        68 : Msg('"CALL" expected');
        69 : Msg('"CP" expected');
        70 : Msg('"DEC" expected');
        71 : Msg('"INC" expected');
        72 : Msg('"CCF" expected');
        73 : Msg('"CPD" expected');
        74 : Msg('"CPDR" expected');
        75 : Msg('"CPI" expected');
        76 : Msg('"CPIR" expected');
        77 : Msg('"CPL" expected');
        78 : Msg('"DAA" expected');
        79 : Msg('"DI" expected');
        80 : Msg('"EI" expected');
        81 : Msg('"EXX" expected');
        82 : Msg('"HALT" expected');
        83 : Msg('"IND" expected');
        84 : Msg('"INDR" expected');
        85 : Msg('"INI" expected');
        86 : Msg('"INIR" expected');
        87 : Msg('"LDD" expected');
        88 : Msg('"LDDR" expected');
        89 : Msg('"LDI" expected');
        90 : Msg('"LDIR" expected');
        91 : Msg('"NEG" expected');
        92 : Msg('"NOP" expected');
        93 : Msg('"OTDR" expected');
        94 : Msg('"OTIR" expected');
        95 : Msg('"OUTD" expected');
        96 : Msg('"OUTI" expected');
        97 : Msg('"RETI" expected');
        98 : Msg('"RETN" expected');
        99 : Msg('"RLA" expected');
       100 : Msg('"RLCA" expected');
       101 : Msg('"RLD" expected');
       102 : Msg('"RRA" expected');
       103 : Msg('"RRD" expected');
       104 : Msg('"SCF" expected');
       105 : Msg('"DJNZ," expected');
       106 : Msg('"EX" expected');
       107 : Msg('"[SP]" expected');
       108 : Msg('"HL" expected');
       109 : Msg('"IX" expected');
       110 : Msg('"IY" expected');
       111 : Msg('"AF" expected');
       112 : Msg('"AF"" expected');
       113 : Msg('"DE" expected');
       114 : Msg('"IM" expected');
       115 : Msg('"OUT" expected');
       116 : Msg('"C" expected');
       117 : Msg('"JP" expected');
       118 : Msg('"JR" expected');
       119 : Msg('"LD" expected');
       120 : Msg('"PUSH" expected');
       121 : Msg('"POP" expected');
       122 : Msg('"RET" expected');
       123 : Msg('"RL" expected');
       124 : Msg('"RLC" expected');
       125 : Msg('"RR" expected');
       126 : Msg('"RRC" expected');
       127 : Msg('"SLA" expected');
       128 : Msg('"SRA" expected');
       129 : Msg('"RST" expected');
       130 : Msg('"SBC" expected');
       131 : Msg('"A" expected');
       132 : Msg('"SUB" expected');
       133 : Msg('"Z" expected');
       134 : Msg('"NZ" expected');
       135 : Msg('"NC" expected');
       136 : Msg('"H" expected');
       137 : Msg('"L" expected');
       138 : Msg('"B" expected');
       139 : Msg('"D" expected');
       140 : Msg('"E" expected');
       141 : Msg('"BC" expected');
       142 : Msg('"SP" expected');
       143 : Msg('"PC" expected');
       144 : Msg('not expected');
       145 : Msg('invalid Z80Register16bit');
       146 : Msg('invalid Z80Integer');
       147 : Msg('invalid Z80Condition');
       148 : Msg('invalid Z80Register8bit');
       149 : Msg('invalid Z80Indirection');
       150 : Msg('invalid Z80Parameter8bit');
       151 : Msg('invalid Z80Parameter');
       152 : Msg('invalid Z80Register');
       153 : Msg('invalid Z80Sub');
       154 : Msg('invalid Z80Sub');
       155 : Msg('invalid Z80Rotate');
       156 : Msg('invalid Z80Stack');
       157 : Msg('invalid Z80Port');
       158 : Msg('invalid Z80Port');
       159 : Msg('invalid Z80Exchange');
       160 : Msg('invalid Z80Exchange');
       161 : Msg('invalid Z80Operation');
       162 : Msg('invalid Z80Inc');
       163 : Msg('invalid Z80Inc');
       164 : Msg('invalid Z80Bit');
       165 : Msg('invalid Z80Bit');
       166 : Msg('invalid Z80Bin');
       167 : Msg('invalid Z80Sum');
       168 : Msg('invalid UnsignedLiteral');
       169 : Msg('invalid MulOp');
       170 : Msg('invalid Factor');
       171 : Msg('invalid AddOp');
       172 : Msg('invalid RelOp');
       173 : Msg('invalid SimpleExpression');
       174 : Msg('invalid Value');
       175 : Msg('invalid ForStatement');
       176 : Msg('invalid AssignmentOrCall');
       177 : Msg('invalid ParamType');
       178 : Msg('invalid FormalSection');
       179 : Msg('invalid Body');
       180 : Msg('invalid StructType');
       181 : Msg('invalid SimpleType');
       182 : Msg('invalid Type');
       183 : Msg('invalid UnsignedInt');
       184 : Msg('invalid UnsignedNumber');
       185 : Msg('invalid Constant');
       186 : Msg('invalid Constant');
       187 : Msg('invalid ProcDeclarations');
       188 : Msg('invalid StatementPart');
      
      (* add your customized cases here *)
      ELSE BEGIN Msg('Error: '); Write(lst, nr); END
      END;
      WriteLn(lst)
    END;

  PROCEDURE PrintListing;
  (* Print a source listing with error messages *)
    VAR
      nextErr:   Err;
      eof:       BOOLEAN;
      lnr, errC: INTEGER;
      srcPos:    LONGINT;
      line:      STRING;
    BEGIN
      WriteLn(lst, 'Listing:');
      WriteLn(lst);
      srcPos := 0; nextErr := firstErr;
      GetLine(srcPos, line, eof); lnr := 1; errC := 0;
      WHILE NOT eof DO BEGIN
        WriteLn(lst, lnr:5, '  ', line);
        WHILE (nextErr <> NIL) AND (nextErr^.line = lnr) DO BEGIN
          PrintErr(line, nextErr^.nr, nextErr^.col); INC(errC);
          nextErr := nextErr^.next
        END;
        GetLine(srcPos, line, eof); INC(lnr);
      END;
      IF nextErr <> NIL THEN BEGIN
        WriteLn(lst, lnr:5);
        WHILE nextErr <> NIL DO BEGIN
          PrintErr(line, nextErr^.nr, nextErr^.col); INC(errC);
          nextErr := nextErr^.next
        END
      END;
      WriteLn(lst);
      Write(lst, errC:5, ' error');
      IF errC <> 1 THEN Write(lst, 's');
      WriteLn(lst); WriteLn(lst); WriteLn(lst);
    END;

(* --------------------------- main module ------------------------------- *)

VAR
  sourceName, listName : STRING;

BEGIN
   firstErr := NIL; Extra := 1;

  (* check on correct parameter usage *)
   IF ParamCount < 1 THEN BEGIN
     WriteLn('No input file specified');
     HALT;
   END;
   sourceName := ParamStr(1);

  (* open the source file Scanner.src *)
  Assign(src, sourceName);
  {$I-} Reset(src, 1); {$I+}
  IF IOResult <> 0 THEN BEGIN
    WriteLn('Could not open input file');
    HALT;
  END;

  (* open the output file for the source listing Scanner.lst *)
  AppendExtension(sourceName, 'lst', listName);
  Assign(lst, listName);
  {$I-} Rewrite(lst); {$I+}
  IF IOResult <> 0 THEN BEGIN
    Close(lst);
    WriteLn('Could not open listing file');
    (* default Scanner.lst to screen *)
    Assign(lst, ''); Rewrite(lst);
  END;

  (* install error reporting procedure *)
  Z80PasS.Error := StoreError;

  (* instigate the compilation *)
  WriteLn('Parsing');
  Parse; 

  (* generate the source listing on Scanner.lst *)
  PrintListing; Close(lst);

  (* examine the outcome from Scanner.errors *)
  IF NOT Successful
    THEN
      Write('Incorrect source - see ', listName)
    ELSE BEGIN
      Write('Parsed correctly');
      (* ++++++++ Add further activities if required ++++++++++ *)
  END;
END. (* Z80Pas *)
