<*/NOWARN:F*>
MODULE cmdlineTest;

FROM UTILLIB IMPORT CR,LF,NULL,BUFSIZ,CTRLCOD,STRTYP,STR10TYP,BUFTYP,
    MAXCARDFNT, COPYLEFT,COPYRIGHT,FILLCHAR,SCANFWD,SCANBACK,
    STRLENFNT,STRCMPFNT,LCHINBUFFNT,MRGBUFS,TRIMFNT,TRIM,RMVCHR,
    APPENDA2B,CONCATAB2C,INSERTAin2B,ASSIGN2BUF;
IMPORT MiscM2;
FROM MiscM2 IMPORT CLS, PressAnyKey, Error, WriteString, WriteLn, WriteCard, ReadString;
(* FROM Terminal IMPORT Read, WriteString, WriteLn, ReadChar, Write, Reset; *)
FROM LongStr IMPORT StrToReal, RealToFloat, RealToEng, RealToFixed, RealToStr;
FROM SysClock IMPORT DateTime,GetClock,CanGetClock,CanSetClock,IsValidDateTime,SetClock;
IMPORT SYSTEM;
FROM SYSTEM IMPORT ADR;
IMPORT WINUSER, WIN32, WINGDI, WINX;
FROM Storage IMPORT ALLOCATE, DEALLOCATE;
IMPORT Terminal, BasicDialogs;
FROM BasicDialogs IMPORT MessageTypes;
IMPORT SysMets;
IMPORT Strings,MemUtils;
IMPORT WholeStr,LongStr, LongConv;
IMPORT LongMath;
IMPORT ASCII;
FROM Environment IMPORT GetCommandLine;
FROM REALLIB IMPORT AINT,AMOD,ROUND,PWRI,PWRR;
FROM TOKENPTR IMPORT FSATYP,DELIMCH,INI1TKN,INI3TKN,GETCHR,
    UNGETCHR,GETTKN,GETTKNSTR,GETTKNEOL,
    UNGETTKN,GETTKNREAL,TOKENTYP,TKNPTRTYP;
FROM TIMLIB IMPORT JULIAN,GREGORIAN,TIME2MDY;

VAR cmdline : ARRAY [1..256] OF CHAR;
	  b1, b2, b3, token : BUFTYP;
	  tpv        : TKNPTRTYP;
	  retcod     : CARDINAL;
	  ignoreme   : INTEGER;
	  tknstate   : FSATYP;
	  
BEGIN
	GetCommandLine (cmdline);
	WriteString(' After GetCommandLine call.  cmdline is:');
	WriteLn;
	WriteString(cmdline);
	PressAnyKey;
	
	ASSIGN2BUF(cmdline,b1);
	WriteString(' Now a buftyp.  Count=');
	WriteCard(b1.COUNT);
	WriteLn;
	WriteString(b1.CHARS);
	WriteLn;
	PressAnyKey;
	
	INI1TKN(tpv,b1);
(*
PROCEDURE GETTKN(VAR tpv : TKNPTRTYP; VAR TOKEN:BUFTYP; VAR TKNSTATE:FSATYP;
                 VAR SUM:INTEGER; VAR RETCOD2:CARDINAL);
*)
  LOOP
  	GETTKN(tpv,token,tknstate,ignoreme,retcod);
    IF retcod > 0 THEN EXIT END;
  	WriteString(' Count=');
	  WriteCard(token.COUNT);
  	WriteString('.  ');
	  WriteString(token.CHARS);
    WriteLn;
    PressAnyKey;
    
  	
  END; (* LOOP *)
  PressAnyKey;
  IF tpv <> NIL THEN DISPOSE(tpv); END;
  
	
END cmdlineTest.
	