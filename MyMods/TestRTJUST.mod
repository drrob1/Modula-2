MODULE TestRTJUST;

IMPORT SYSTEM;
FROM SYSTEM IMPORT ADR, MAKEADR;
IMPORT Terminal, BasicDialogs, DlgShell;
FROM BasicDialogs IMPORT MessageTypes;
IMPORT Strings,MemUtils;
IMPORT WholeStr,LongStr, LongConv;
IMPORT LongMath;
IMPORT ASCII;
FROM UTILLIB IMPORT BLANK,NULL,STRTYP,BUFSIZ,BUFTYP,STR10TYP,TRIM,STRLENFNT,STRCMPFNT,
    SCANBACK,SCANFWD,COPYLEFT,COPYRIGHT,ASSIGN2BUF;
(* FROM RTJUST IMPORT RtJust; *)
IMPORT MiscM2;

VAR s   : ARRAY[1..255] OF CHAR;
    w,c : CARDINAL;

BEGIN
        MiscM2.WriteString(' Enter string to justify: ');
        MiscM2.ReadString(s);
        MiscM2.WriteLn;
        MiscM2.WriteString(' Enter width: ');
        MiscM2.ReadCard(w);
        MiscM2.RtJust(s,w);
        MiscM2.WriteLn;
        MiscM2.WriteString(s);
        MiscM2.WriteLn;
        MiscM2.WriteString(' Enter CARDINAL to justify: ');
        MiscM2.ReadCard(c);
        MiscM2.WriteLn;
        MiscM2.WriteRJCard(c,w);
        MiscM2.WriteLn;
        MiscM2.PressAnyKey;

END TestRTJUST.
