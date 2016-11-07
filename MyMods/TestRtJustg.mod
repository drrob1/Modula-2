MODULE TestRtJustg;
(*
  REVISION HISTORY
  ----------------
  10 Oct 13 -- converted to gm2.  Unknown how old the original version of this routine is.
*)
IMPORT SYSTEM;
FROM SYSTEM IMPORT ADR;
IMPORT Strings;
IMPORT WholeStr,LongStr, LongConv;
IMPORT ASCII;
FROM UTILLIBg IMPORT BLANK,NULL,STRTYP,BUFSIZ,BUFTYP,STR10TYP,TRIM,STRLENFNT,STRCMPFNT,
    SCANBACK,SCANFWD,COPYLEFT,COPYRIGHT,ASSIGN2BUF;
(* FROM RTJUST IMPORT RtJust; *)
IMPORT MiscStdInOutg;

VAR s   : ARRAY[1..255] OF CHAR;
    w,c : CARDINAL;

BEGIN
        MiscStdInOutg.WriteString(' Enter string to justify: ');
        MiscStdInOutg.ReadString(s);
        MiscStdInOutg.WriteLn;
        MiscStdInOutg.WriteString(' Enter width: ');
        MiscStdInOutg.ReadCard(w);
        MiscStdInOutg.RtJust(s,w);
        MiscStdInOutg.WriteLn;
        MiscStdInOutg.WriteString(s);
        MiscStdInOutg.WriteLn;
        MiscStdInOutg.WriteString(' Enter CARDINAL to justify: ');
        MiscStdInOutg.ReadCard(c);
        MiscStdInOutg.WriteLn;
        MiscStdInOutg.WriteRJCard(c,w);
        MiscStdInOutg.WriteLn;
        MiscStdInOutg.PressAnyKey;

END TestRtJustg.
