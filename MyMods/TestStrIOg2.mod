MODULE TestStrIOg2;
(*
  REVISION HISTORY
  ----------------
  10 Oct 13 -- Testing terminal read char if it does what it should under gm2.
*)

IMPORT MiscStdInOutg;
FROM MiscStdInOutg IMPORT WriteString, ReadAllChars, WriteLn, WriteCard;
FROM ASCII IMPORT EOL, nul, bs;

VAR 
  CH             : CHAR;
  string         : ARRAY [1..127] OF CHAR;


PROCEDURE WriteOutDecValues(s: ARRAY OF CHAR);
VAR
  len,c : CARDINAL;

  
BEGIN
  len := LENGTH(s);
  FOR c := 0 TO len DO
    WriteCard(ORD(s[c]));
    WriteString('   ');
  END;
END WriteOutDecValues;

BEGIN
  LOOP
      WriteString(' Enter something: ');
      ReadAllChars(string);
      WriteString(string); 
      WriteLn;
      WriteString(' Decimal value of entered chars: ');
      WriteOutDecValues(string);
      WriteLn;
      IF CAP(string[1]) = 'Q' THEN EXIT; END(*IF*);
      
  END(*LOOP*);
END TestStrIOg2.
