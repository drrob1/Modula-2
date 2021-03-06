MODULE TestTimLibRevised;
(*
REVISION HISTORY
----------------
13 Oct 13 -- Testing gm2 routines.
11 Oct 16 -- gm2 was a bust.  I'm testing StonyBrook Modula-2 addition of GetDateTime, backporting C++ and go code I recently wrote.
25 Apr 19 -- Testing enhancements to GetDateTime in TIMLIBrevised.
 6 Aug 19 -- Testing Now() and NOW()
13 Aug 19 -- Found that writing DateStr to a file from MyFIO2 also writes a null char.
 5 Dec 20 -- Testing sleepsec and sleepmsec.
*)
  FROM SYSTEM IMPORT ADR;
  FROM MiscStdInOut IMPORT WriteString,WriteLn,PressAnyKey,WriteCard,WriteInt,ReadString,ReadCard,
                     WriteLongCard;
  FROM Storage IMPORT ALLOCATE, DEALLOCATE;
  FROM UTILLIB IMPORT BUFSIZ, CTRLCOD, STRTYP, STR10TYP, BUFTYP, MAXCARDFNT, NULL, COPYLEFT, COPYRIGHT, FILLCHAR, SCANFWD, SCANBACK,
    STRLENFNT, STRCMPFNT, LCHINBUFFNT, MRGBUFS, TRIMFNT, TRIM, RMVCHR, APPENDA2B, CONCATAB2C, INSERTAin2B, ASSIGN2BUF;
(*
{{{
  FROM TOKENPTR IMPORT FSATYP,TKNPTRTYP,INI1TKN,GETCHR, UNGETCHR,GETTKN,UNGETTKN,GETTKNREAL,GETTKNSTR,DELIMCH,DELIMSTATE;

  FROM TKNRTNS IMPORT FSATYP,CHARSETTYP,DELIMCH,DELIMSTATE,INI1TKN, INI3TKN,GETCHR,UNGETCHR,GETTKN,NEWDELIMSET,NEWOPSET,NEWDGTSET, GETTKNSTR,GETTKNEOL,UNGETTKN,GETTKNREAL;
  FROM RealStr IMPORT StrToReal, RealToFloat, RealToEng, RealToFixed, RealToStr;
  FROM SLWholeIO IMPORT ReadLongInt,WriteLongInt,ReadLongCard,WriteLongCard;
  IMPORT RConversions, LongStr, LongConv;
  IMPORT NumberIO, CardinalIO, StdIO, StrIO;
  FROM NumberIO IMPORT WriteCard,WriteInt;
  FROM StrIO IMPORT WriteString,WriteLn;
reminders
  PROCEDURE TIME2MDY(VAR M,D,Y : CARDINAL);
  PROCEDURE JULIAN(M,D,Y : CARDINAL) : LONGCARD;
  PROCEDURE GREGORIAN(Juldate : LONGCARD; VAR M,D,Y : CARDINAL);
}}}
*)
FROM Environment IMPORT GetCommandLine;
IMPORT TIMLIBrevised,FormatString;
FROM TIMLIBrevised IMPORT TIME2MDY, JULIAN, GREGORIAN, DateTimeType, GetDateTime;
FROM SysClock IMPORT GetClock,DateTime;

CONST
  LastAltered="August 13, 2019";

VAR
  INBUF,TOKEN : BUFTYP;
    RETCOD,C,posn,c1,c2,
    M,D,Y                  : CARDINAL;
    I                      : INTEGER;
    L                      : LONGINT;
    R,r1,r2,r3,r4,r5,r6    : LONGREAL;
    CH                     : CHAR;
    LC                     : LONGCARD;
    dt                     : DateTime;
    dt1,dt2,d1,d2          : DateTimeType;
    str,s0,s1,s2           : STRTYP;

(*
{{{
    TKNSTATE       : FSATYP;
    tpv1,tpv2,tpv3 : TKNPTRTYP;
}}}
*)

BEGIN
  WriteString(" Test TimLibRevised program.  Last Altered ");
  WriteString(LastAltered);
  WriteLn;
  WriteLn;
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

  WriteString(" After GetDateTime call, dt2: m d y hr min sec frac zone, DST ");
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


(*  DateTimetype RECORD
{{{
  TYPE
    DateTimeType = RECORD
      M,D,Yr,Hr,Minutes,Seconds,Millisecs,DOW : CARDINAL;
      MonthStr,DayOfWeekStr : STR10TYP;
      DateStr,TimeStr,TimeWithSecondsStr : STRTYP;
      Julian : CARDINAL;
    END;
}}}
*)

  FormatString.FormatString(" and dt1: %c/%c/%c %c:%c:%c.%c  %s, %s, %s, %s and %s. \n ",str,dt1.M,dt1.D,dt1.Yr,dt1.Hr,dt1.Minutes,dt1.Seconds,dt1.Millisecs,
                              dt1.MonthStr, dt1.DayOfWeekStr,dt1.DateStr,dt1.TimeStr,dt1.TimeWithSecondsStr);
  WriteString(str);
  WriteLn;

  d1 := TIMLIBrevised.Now();
  d2 := TIMLIBrevised.NOW();

  FormatString.FormatString("After Now routine D1.  String: %s, string %s and %s.  DOW: %c, Cardinals: %c/%c/%c_%c:%c:%c.%c, %c \n",s1,d1.DateStr,d1.TimeStr,d1.TimeWithSecondsStr,d1.DOW,
                             d1.M,d1.D,d1.Yr,d1.Hr,d1.Minutes,d1.Seconds,d1.Millisecs,d1.Julian);
  WriteString(s1);
  WriteLn;
  FormatString.FormatString("After Now routine D2.  String: %s, string %s and %s.  DOW: %c, Cardinals: %c/%c/%c_%c:%c:%c.%c, %c \n",s2,d2.DateStr,d2.TimeStr,d2.TimeWithSecondsStr,d2.DOW,
                             d2.M,d2.D,d2.Yr,d2.Hr,d2.Minutes,d2.Seconds,d2.Millisecs,d2.Julian);
  WriteString(s2);
  WriteLn;
  WriteString(" DateStr, TimeStr: ");
  WriteString(d1.DateStr);
  WriteString(", ");
  WriteString(d1.TimeStr);
  WriteString(".  Len DateStr, TimeStr= ");
  c1 := STRLENFNT(d1.DateStr);
  c2 := STRLENFNT(d1.TimeStr);
  WriteCard(c1);
  WriteString(", ");
  WriteCard(c2);


  WriteLn;
  WriteString(" Will now sleep for 5 seconds");
  TIMLIBrevised.Sleepsec(5);
  WriteString(" ... Time to wake up.");
  WriteLn;

  WriteString(" Will now sleep for 500 msec.");
  TIMLIBrevised.Sleepmsec(500);
  WriteString(" ... Time to wake up.");
  WriteLn;

  WriteString(" Will now sleep for 10 sec");
  TIMLIBrevised.Sleepsec(10);
  WriteString(" ... Time to wake up.");
  WriteLn;
  WriteLn;

  PressAnyKey;

END TestTimLibRevised.
