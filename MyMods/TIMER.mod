(*V0=M2_V_3*)
(*V1=M2_V_4*)

IMPLEMENTATION MODULE TIMER;
(*
  This is a very simple module that implements an event timer by reading
  the system time and comparing to that.  This routine will not work
  properly when the timer goes past midnight.  Perhaps I'll add this at
  some other point in time.

  REVISION HISTORY
  ================
  23 Dec 91 -- Converted to M2 V 4.00.  No longer need to do contortions for
                LongInts.
   9 Mar 05 -- First Win 32 version SBM2 build 31.
*)

FROM SysClock IMPORT DateTime,GetClock,CanGetClock,CanSetClock,IsValidDateTime,SetClock;
(*CONST
    maxSecondParts      = 999;/*implementation dependent value*/

TYPE
    Month       = [1..12];
    Day         = [1..31];
    Hour        = [0..23];
    Min         = [0..59];
    Sec         = [0..59];
    Fraction    = [0..maxSecondParts];
    UTCDiff     = [-780..720];

    DateTime    =
        RECORD
        year            : CARDINAL;
        month           : Month;
        day             : Day;
        hour            : Hour;
        minute          : Min;
        second          : Sec;
        fractions       : Fraction; /* parts of a second */
        zone            : UTCDiff;  /* Time zone differential factor
                                       which is the number of minutes
                                       to add to local time to obtain
                                       UTC. */
        summerTimeFlag  : BOOLEAN;  /* interpretation depends on local
                                       usage */
        END;
*)
CONST L60000 = 60000;  (* Number of msecs per min *)
      L60    = 60;
      L1000  = 1000;

VAR
  T0      : DateTime;

PROCEDURE STARTTIMER;
BEGIN
  GetClock(T0);
END STARTTIMER;

PROCEDURE READTIMER(VAR HRS,MINS,SECS,TOTALSECS : CARDINAL);

VAR
  CurTime          : DateTime;
  EHRS,EMINS,ESECS : CARDINAL; (* Elapsed time in the respective units *)

BEGIN
  GetClock(CurTime);
  TOTALSECS := CurTime.hour*3600 + CurTime.minute*60 + CurTime.second -
                                                              (T0.hour*3600 + T0.minute*60 + T0.second);
(*  IF TOTALSECS < VAL(LONGINT,0) THEN TOTALMSECS := -TOTALMSECS; END; *)
  EHRS  := TOTALSECS DIV 3600;
  EMINS := (TOTALSECS - EHRS*3600) DIV 60;
  ESECS := (TOTALSECS - EHRS*3600 - EMINS*60);
  HRS   := EHRS;
  MINS  := EMINS;
  SECS  := ESECS;
END READTIMER;

BEGIN (* Initialize the timer call *)
  STARTTIMER;
END TIMER.
