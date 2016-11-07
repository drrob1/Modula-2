MODULE TestTimLibg;
(*
REVISION HISTORY
----------------
13 Oct 13 -- Testing gm2 routines.
*)
  FROM SYSTEM IMPORT ADR;
  FROM MiscStdInOutg IMPORT WriteString,WriteLn,PressAnyKey,WriteCard,WriteInt,ReadString,ReadCard,
                     WriteCard,WriteLongCard;
  FROM Storage IMPORT ALLOCATE, DEALLOCATE;
  FROM UTILLIBg IMPORT BUFSIZ,CTRLCOD,STRTYP,STR10TYP,BUFTYP,
    MAXCARDFNT,MINCARDFNT,NULL, COPYLEFT,COPYRIGHT,FILLCHAR,SCANFWD,SCANBACK,
    STRLENFNT,STRCMPFNT,LCHINBUFFNT,MRGBUFS,TRIMFNT,TRIM,RMVCHR,
    APPENDA2B,CONCATAB2C,INSERTAin2B,ASSIGN2BUF;
(*
  FROM TOKENPTRg IMPORT FSATYP,TKNPTRTYP,INI1TKN,GETCHR,
    UNGETCHR,GETTKN,UNGETTKN,GETTKNREAL,GETTKNSTR,DELIMCH,DELIMSTATE;
*)
(*
  FROM TKNRTNSg IMPORT FSATYP,CHARSETTYP,DELIMCH,DELIMSTATE,INI1TKN,
    INI3TKN,GETCHR,UNGETCHR,GETTKN,NEWDELIMSET,NEWOPSET,NEWDGTSET,
    GETTKNSTR,GETTKNEOL,UNGETTKN,GETTKNREAL;
  FROM RealStr IMPORT StrToReal, RealToFloat, RealToEng, RealToFixed, RealToStr;
  FROM SLWholeIO IMPORT ReadLongInt,WriteLongInt,ReadLongCard,WriteLongCard;
  IMPORT RConversions, LongStr, LongConv;
*)
FROM Environg IMPORT GetCommandLine;
IMPORT TIMLIBg;
FROM TIMLIBg IMPORT TIME2MDY, JULIAN, GREGORIAN;
(*
  PROCEDURE TIME2MDY(VAR M,D,Y : CARDINAL);
  PROCEDURE JULIAN(M,D,Y : CARDINAL) : LONGCARD;
  PROCEDURE GREGORIAN(Juldate : LONGCARD; VAR M,D,Y : CARDINAL);
*)
FROM SysClock IMPORT GetClock,DateTime;
(* IMPORT NumberIO, CardinalIO, StdIO, StrIO; *)
(* FROM NumberIO IMPORT WriteCard,WriteInt; *)
(* FROM StrIO IMPORT WriteString,WriteLn; *)

  VAR INBUF,TOKEN : BUFTYP;
(*
    TKNSTATE       : FSATYP;
    tpv1,tpv2,tpv3 : TKNPTRTYP;
*)
    RETCOD,C,posn,c1,c2,
    M,D,Y                  : CARDINAL;
    I                      : INTEGER;
    L                      : LONGINT;
    R,r1,r2,r3,r4,r5,r6    : LONGREAL;
    CH                     : CHAR;
    LC                     : LONGCARD;
    dt                     : DateTime;

BEGIN
  GetClock(dt);
  WriteString(' From system clock: m d y hr min sec frac zone, DST ');
  WriteCard(dt.month);
  WriteString('  ');
  WriteCard(dt.day);
  WriteString('  ');
  WriteCard(dt.year);
  WriteString('  ');
  WriteCard(dt.hour);
  WriteString('  ');
  WriteCard(dt.minute);
  WriteString('  ');
  WriteCard(dt.second);
  WriteString('  ');
  WriteCard(dt.fractions);
  WriteString('  ');
  WriteInt(dt.zone);
  IF dt.summerTimeFlag THEN
    WriteString(' DST');
  ELSE
    WriteString(' std time');
  END;
  WriteLn;
  WriteLn;
  
  
  

  TIME2MDY(M,D,Y);
  WriteString(' From TIME2MDY:');
  WriteCard(M);
  WriteString(' / ');
  WriteCard(D);
  WriteString(' / ');
  WriteCard(Y);
  WriteLn;

  LC := JULIAN(M,D,Y);
  WriteString('Julian date number: ');
  WriteLongCard(LC);
  WriteLn;
  GREGORIAN(LC,M,D,Y);
  WriteString(' after gregorian: ');
  WriteCard(M);
  WriteString('/');
  WriteCard(D);
  WriteString('/');
  WriteCard(Y);
  WriteLn;

(*  
  LOOP
    WriteString(' Input line : ');
    ReadString(INBUF.CHARS);
    WriteLn;
(*
    WriteString(' length of input string is: ');
    WriteCard(STRLENFNT(INBUF.CHARS));
    PressAnyKey;
*)
    TRIM(INBUF);
    WriteString(' INBUF.COUNT is ');
    WriteCard(INBUF.COUNT);
    PressAnyKey;
    IF STRCMPFNT(INBUF.CHARS,'quit') = 0 THEN EXIT; END(*IF*);
    IF INBUF.COUNT = 0 THEN EXIT; END(*IF*);
    posn := LCHINBUFFNT(INBUF,"e");
    WHILE posn > 0 DO
      WriteString(' posn=');
      WriteCard(posn);
      WriteString(', count=');
      WriteCard(INBUF.COUNT);
      WriteLn;
      
      RMVCHR(INBUF,posn,1);
      WriteString(' after rmvchr call:  ');
      WriteString(INBUF.CHARS);
      WriteString(', count=');
      WriteCard(INBUF.COUNT);
      WriteLn;
      posn := LCHINBUFFNT(INBUF,"'");
      PressAnyKey;
    END;
  END (* loop *);
  FILLCHAR(ADR(TOKEN.CHARS),20,'0');
  WriteString(' FillChar test with 0: ');
  WriteString(TOKEN.CHARS);
  WriteLn;

  WriteString(' MinCard and MaxCard tests.  Enter card 1: ');
  ReadCard(c1);
  WriteString(', Enter Card 2: ');
  ReadCard(c2);
  WriteLn;
  WriteString(' Max: ');
  WriteCard(MAXCARDFNT(c1,c2));
  WriteString(', Min: ');
  WriteCard(MINCARDFNT(c1,c2));
  WriteLn;
*)
END TestTimLibg.
