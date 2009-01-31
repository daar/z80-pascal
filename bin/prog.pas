(* Programa mínimo para probar compilador Z80 Pascal. *)
PROGRAM prog;

PROCEDure WriteLn (String: STRING);
ASM
  RST 08
END;

BEGIN
  WriteLn ('Hola, mundo!');
END.
