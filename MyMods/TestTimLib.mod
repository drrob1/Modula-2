MODULE TestTimLib;
(*
REVISION HISTORY
----------------
13 Oct 13 -- Testing gm2 routines.
11 Oct 16 -- gm2 was a bust.  I'm testing StonyBrook Modula-2 addition of GetDayTime, backporting C++ and go code I recently wrote.
*)
  FROM SYSTEM IMPORT ADR;
  FROM MiscStdInOut IMPORT WriteString,WriteLn,PressAnyKey,WriteCard,WriteInt,ReadString,ReadCard,
                     WriteLongCard;
  FROM Storage IMPORT ALLOCATE, DEALLOCATE;
  FROM UTILLIB IMPORT BUFSIZ,CTRLCOD,STRTYP,STR10TYP,BUFTYP,
    MAXCARDFNT,NULL, COPYLEFT,COPYRIGHT,FILLCHAR,SCANFWD,SCANBACK,
    STRLENFNT,STRCMPFNT,LCHINBUFFNT,MRGBUFS,TRIMFNT,TRIM,RMVCHR,
    APPENDA2B,CONCATAB2C,INSERTAin2B,ASSIGN2BUF;
(*
  FROM TOKENPTR IMPORT FSATYP,TKNPTRTYP,INI1TKN,GETCHR,
    UNGETCHR,GETTKN,UNGETTKN,GETTKNREAL,GETTKNSTR,DELIMCH,DELIMSTATE;
*)
(*
  FROM TKNRTNS IMPORT FSATYP,CHARSETTYP,DELIMCH,DELIMSTATE,INI1TKN,
    INI3TKN,GETCHR,UNGETCHR,GETTKN,NEWDELIMSET,NEWOPSET,NEWDGTSET,
    GETTKNSTR,GETTKNEOL,UNGETTKN,GETTKNREAL;
  FROM RealStr IMPORT StrToReal, RealToFloat, RealToEng, RealToFixed, RealToStr;
  FROM SLWholeIO IMPORT ReadLongInt,WriteLongInt,ReadLongCard,WriteLongCard;
  IMPORT RConversions, LongStr, LongConv;
*)
FROM Environment IMPORT GetCommandLine;
IMPORT TIMLIB;
FROM TIMLIB IMPORT TIME2MDY, JULIAN, GREGORIAN, DateTimeType, GetDateTime;
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
    dt1,dt2                : DateTimeType;

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

  dt2 := GetDateTime(dt1);

  WriteString(' dt2: m d y hr min sec frac zone, DST ');
  WriteCard(dt2.M);
  WriteString('  ');
  WriteCard(dt2.D);
  WriteString('  ');
  WriteCard(dt2.Yr);
  WriteString('  ');
  WriteCard(dt2.Hr);
  WriteString('  ');
  WriteCard(dt2.Minutes);
  WriteString('  ');
  WriteCard(dt2.Seconds);
  WriteString('  ');
  WriteString(dt2.MonthStr);
  WriteString('  ');
  WriteString(dt2.DayOfWeekStr);
  WriteLn;
  WriteLn;

  WriteString(' dt1: m d y hr min sec frac zone, DST ');
  WriteCard(dt1.M);
  WriteString('  ');
  WriteCard(dt1.D);
  WriteString('  ');
  WriteCard(dt1.Yr);
  WriteString('  ');
  WriteCard(dt1.Hr);
  WriteString('  ');
  WriteCard(dt1.Minutes);
  WriteString('  ');
  WriteCard(dt1.Seconds);
  WriteString('  ');
  WriteString(dt1.MonthStr);
  WriteString('  ');
  WriteString(dt1.DayOfWeekStr);
  WriteLn;
  WriteLn;


END TestTimLib.
