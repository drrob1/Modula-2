IMPLEMENTATION MODULE RTJUST;

IMPORT SYSTEM;
FROM SYSTEM IMPORT ADR, MAKEADR;
IMPORT Terminal, BasicDialogs, DlgShell;
FROM BasicDialogs IMPORT MessageTypes;
IMPORT Strings,MemUtils;
IMPORT WholeStr,LongStr, LongConv;
IMPORT LongMath;
IMPORT ASCII;
FROM UTILLIB IMPORT BLANK,NULL,STRTYP,BUFSIZ,BUFTYP,STR10TYP,TRIM,STRLENFNT,STRCMPFNT,
    SCANBACK,SCANFWD,COPYLEFT,COPYRIGHT,FILLCHAR,ASSIGN2BUF;


PROCEDURE RtJust(VAR INOUT str : ARRAY OF CHAR; width : CARDINAL);
VAR diff : INTEGER;
    len : CARDINAL;
BEGIN
  len := LENGTH(str);
  diff := INT(width) - INT(len);
  IF diff >=1 THEN       (* must include terminating null *)
    COPYRIGHT(ADR(str),ADR(str[diff]),len+1);
    FILLCHAR(ADR(str),diff,' ');
  END;
END RtJust;

(*
or

len
diff
while diff < 1 do
  strings.insert(' ',0,str);
  dec(diff);
end;

or

len
diff
if diff >=1 then
  MemUtils.MoveMem
  MemUtils.FillChar
end

*)

END RTJUST.
