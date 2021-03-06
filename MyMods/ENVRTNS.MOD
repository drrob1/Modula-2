IMPLEMENTATION MODULE ENVRTNS;

(*  Copyright (C) 1987  Robert Solomon MD.  All rights reserved. *)
(*
  REVISION HISTORY
  ----------------
  31 Mar 88 -- Converted to M2 V3.03 by changing DOS3 import to RTSMain.
  21 Dec 91 -- Converted to M2 V4.00 by changing way PSP is received.
  10 Oct 02 -- Converted to M2 win32 DOS mode
*)
  FROM SYSTEM IMPORT ADDRESS (*,PSP*);   (* New for M2 V 4.00 *)
(* from obsolete modules
  FROM InOut IMPORT WriteString,WriteLn,WriteCard,WriteInt;
  FROM FileSystem IMPORT Lookup, Response;
  FROM DOS3 IMPORT GetProgramSegmentPrefix;
*)
(*  FROM RTSMain IMPORT PSPAddress; Not good in M2 V 4.00 *)
  FROM UTILLIB IMPORT BUFSIZ,NULL,BUFTYP,STRTYP,STR10TYP,STRLENFNT,
    GETFNM;
  FROM TOKENIZE IMPORT FSATYP,DELIMCH,INI3TKN,INI1TKN,STOTKNPOSN,
    RCLTKNPOSN,GETTKN;
  FROM MyFIO IMPORT MYFILTYP,IOSTATE,FRESET,FCLOSE,FRDTXLN,FWRTXLN,FWRTX,
    RETBLKBUF,FWRSTR,FWRLN;

TYPE ENVADRPTR = RECORD CASE :CARDINAL OF
                   0 : ADDR      : ADDRESS;
                 | 1 : NXTCHRPTR : POINTER TO CHAR;
                 END(*CASE*);
     END(*RECORD*);

VAR
(*
  THIS FLAG IS USED IF ENVIRONMENT STRING IS LARGER THAN BUFSIZ, AND IT IS
  USED BY THE RDENV AND GETPATH ROUTINES.
*)
  MORE      : BOOLEAN;
(*
  GLOBAL STATIC DECLARATIONS
*)
  ENDOFENV,HAVEPATH  : BOOLEAN;
  ENVBUF             : BUFTYP;
  ENVADRINPSP        : ADDRESS;
  ENVADR             : ENVADRPTR;
  PATH               : STR10TYP;
  TKNSTATE           : FSATYP;

PROCEDURE RDENV;
(*
****************************** RDENV ****************************************
READ ENVIRONMENT STRING.
THE PURPOSE OF THIS ROUTINE IS TO READ THE ENVIRONMENT STRING AND PLACE IT INTO
ENVBUF.  RETCOD IS 0 IF OPERATION IS DONE SUCCESSFULLY.

*)

VAR C      : CARDINAL;
    NULFLG : BOOLEAN;
    CH     : CHAR;
    TMPADR : ENVADRPTR;

BEGIN
  IF ENDOFENV THEN
    WriteString(' *ERROR* Already read environment.  Cannot read again.');
    WriteLn;
    RETURN;
  END(*IF*);
  IF MORE THEN
    C := 0;
    MORE := FALSE;
    NULFLG := FALSE;
  ELSE
    NULFLG := FALSE;
(*    ENVADRINPSP := PSP;*)
(*    ENVADRINPSP := PSPAddress; Not good for V 4.00 *)
    GetProgramSegmentPrefix(ENVADRINPSP.SEGMENT);
    ENVADRINPSP.OFFSET := 2CH;
    ENVADR.ADDR.SEGMENT := CARDINAL(ENVADRINPSP^);
    ENVADR.ADDR.OFFSET := 0;
    C := 0;
  END(*IF*);
  LOOP   (* UNTIL HAVE DOUBLE NULL OR BUFFER IS FULL *)
    WHILE (C < BUFSIZ) AND (ENVADR.NXTCHRPTR^ <> NULL) DO
      INC(C);
      ENVBUF.CHARS[C] := ENVADR.NXTCHRPTR^;
      INC(ENVADR.ADDR);
      NULFLG := FALSE;
    END(*WHILE*);
    IF (ENVADR.NXTCHRPTR^ = NULL) AND NULFLG THEN
(*
  HAVE TWO CONSEC NULLS SO HAVE END OF ENVIRONMENT.
*)
      ENVBUF.COUNT := C-1; (* NOTE THAT THIS COUNT WILL NOT INCLUDE ANY
                              NULLS AT END OF BUFFER *)
      ENDOFENV := TRUE;
      RETURN;
    ELSIF (C < BUFSIZ) AND (ENVADR.NXTCHRPTR^ = NULL) THEN
      INC(C);
      ENVBUF.CHARS[C] := NULL;
      NULFLG := TRUE;
      INC(ENVADR.ADDR);
    ELSIF C >= BUFSIZ THEN (* C SHOULD NEVER > BUFSIZ, BUT
                                         TEST IS >= FOR CLARITY OF EXPRESSION *)
      MORE := TRUE;
      IF ENVADR.NXTCHRPTR^ = NULL THEN
        TMPADR := ENVADR;
        INC(TMPADR.ADDR);
        IF TMPADR.NXTCHRPTR^ = NULL THEN
(* NEXT TWO CHARS ARE NULL, SO HAVE DOUBLE NULL.  CAN RETURN NORMALLY. *)
          MORE := FALSE;
          ENDOFENV := TRUE;
        END(*IF*);
(*
  NO NEED TO BACK UP TO NULL SINCE NEXT CHAR IS A NULL.
*)
        ENVBUF.COUNT := C;
        RETURN;
      ELSE  (* MUST BACK UP TO NEXT OCCURENCE OF NULL SO A COMPLETE STRING
               IS IN ENVBUF *)
        WHILE (C >= 1) AND (ENVBUF.CHARS[C] <> NULL) DO
          DEC(C);
          DEC(ENVADR.ADDR);
        END(*WHILE*);
        IF C = 0 THEN
          WriteString(' *ERROR*  In RDENV and temp ptr, C, = 0.  Part of ');
          WriteString('environment is :');
          WriteLn;
          WriteString(ENVBUF.CHARS);
          WriteLn;
          ENVBUF.COUNT := C;
        ELSE
          ENVBUF.COUNT := C-1;  (* DON'T INCLUDE TERMINATING NULL *)
        END(*IF*);
        RETURN;
      END(*IF*);
    END(*IF*);
  END(*LOOP*);
END RDENV;

PROCEDURE GETPATH(VAR RETCOD:CARDINAL);
(*
****************************** GETPATH *************************************
THIS ROUTINE EXAMINES THE ENVIRONMENT STRING UNTIL IT FINDS THE PATH STRING.
IF NO SUCH PATH IS FOUND, RETCOD = 1.

*)

VAR C,RETCOD2 : CARDINAL;
    TOKEN     : BUFTYP;
    TKNSTATE  : FSATYP;
    IGNORE    : INTEGER;

BEGIN
  RETCOD := 0;
  RDENV;
  INI1TKN(ENVBUF);
  LOOP
    GETTKN(TOKEN,TKNSTATE,IGNORE,RETCOD2);
(*
  IF TKNSTATE IS AN OPERATOR, RETCOD2 MAY = 6 BECAUSE AN ERRONEOUS INVALID
  OPERATOR IS DETECTED.  THIS HAPPENS BECAUSE THE ENVIRONMENT VARIABLES FOR
  BRIEF REQUIRE THE UNIX STYLE OF FLAG USAGE, USING A HYPHEN INSTEAD OF A
  SLASH.  THIS CREATES AN INVALID OPERATOR CONDITION FROM TOKENIZE BECAUSE
  GETTKN WILL GET THE '=-' AS AN OPERATOR, AND SIGNAL THIS AS INVALID.  THE
  FULL STATEMENT IS 'BFLAGS=-t-r', SO ONE CAN SEE HOW THE '=-' IS DETECTED
  AS AN ERRONEOUSLY INVALID OPCODE.  THIS PROBLEM IS EXCLUDED HERE BY THE
  TEST FOR THE OP TKNSTATE.
*)
    IF (RETCOD2 <> 0) AND (TKNSTATE <> OP) THEN
      IF MORE THEN
        RDENV;
        INI1TKN(ENVBUF);
      ELSE
        RETCOD := 1;
        RETURN;
      END(*IF*);
    END(*IF*);
    IF (TKNSTATE = ALLELSE) AND (DELIMCH = '=') AND (TOKEN.COUNT = 4) THEN
      C := 0;
      WHILE (C < 4) AND (PATH[C] = TOKEN.CHARS[C+1]) DO INC(C); END(*WHILE*);
      IF C > 3 THEN  (* TOKEN = PATH, THAT IS, FOUND PATH *)
        GETTKN(TOKEN,TKNSTATE,IGNORE,RETCOD2);
        IF (TOKEN.CHARS[1] <> '=') OR (RETCOD2 <> 0) THEN
          RETCOD := 1;
          WriteString(' *ERROR* In GETPATH and TOKEN following PATH is not ');
          WriteString('equal sign.  It is ');
          WriteString(TOKEN.CHARS);
          WriteLn;
          RETURN;
        END(*IF*);
        STOTKNPOSN; (* STORE POINTER INTO ENVBUF SO IT REMAINS ON
                       THE PATH STR'S *)
        HAVEPATH := TRUE;
        RETURN;
      END(*IF*);
    END(*IF*);
  END(*LOOP*);
END GETPATH;

PROCEDURE OPNFIL(VAR F:MYFILTYP; VAR FILNAM:BUFTYP; VAR RETCOD:CARDINAL);
(*
********************************* OPNFIL *************************************
OPEN FILE
THIS ROUTINE WILL SEARCH THE ENVIRONMENT'S PATH STRING FOR A GIVEN FILE NAME.
THE CURRENT DIRECTORY IS FIRST SEARCHED, AND IF UNSUCCESSFUL, THEN THE PATH
DIRECTORIES ARE SEARCHED IN ORDER OF OCCURRENCE.  IF THE INPUT FILNAM HAS
EITHER A DRIVE OR A SUBDIRECTORY SPECIFICATION OF ITS OWN, THEN THE PATH IS
NOT SEARCHED.  IF THE FILE IS NOT FOUND IN THE SPECIFIED DRIVE AND/OR
SUBDIRECTORY, THIS ROUTINE WILL EXIT WITH THE FILE NOT FOUND RETURN CODE.

RETCOD : 0--MEANS NORMAL RETURN.  FILE WAS FOUND.  FULL FILENAME INCLUDING
            SUBDIRECTORY WHERE FILE WAS FOUND RETURNED IN FILNAM BUFFER.
         1--FILE NOT FOUND.
         2--ERROR OTHER THAN FILE NOT FOUND RETURNED FROM THE LOOKUP PROCEDURE.
*)

CONST DRIVESEP  = ':';
      SUBDIRSEP = '\';
      NEWFIL    = FALSE;

VAR C,K,RETCOD2      : CARDINAL;
    IGNORE           : INTEGER;
    TOKEN,TESTFILNAM : BUFTYP;
    TKNSTATE         : FSATYP;

BEGIN
  RETCOD := 0;
  WITH F DO
    Lookup(FILE,FILNAM.CHARS,NEWFIL);
    IF FILE.res = done THEN
      RETURN;
    ELSIF FILE.res <> notdone THEN
(*
  THE RESPONSE FIELD OF THE FILE RECORD WOULD BE NOTDONE IF FILE NOT FOUND.  IF
  IT IS ANYTHING ELSE, SOME OTHER ERROR OCCURRED.
*)
      RETCOD := 2;
      RETURN;
    END(*IF*);
(*
  ONLY HERE WHEN FILE WAS NOT FOUND, i.e., FILE.res = notdone
*)
    IF HAVEPATH THEN
      RCLTKNPOSN;
    ELSE
      GETPATH(RETCOD2);
      IF RETCOD2 <> 0 THEN
        RETCOD := 1;
        RETURN;
      END(*IF*);
    END(*IF*);
    IF FILNAM.CHARS[2] = DRIVESEP THEN
(* FILENAME BEGINS WITH A DRIVE SPECIFICATION.  ABORT SEARCH. *)
      RETCOD := 1;
      RETURN;
    END(*IF*);
    C := 1;
    WHILE (C <= FILNAM.COUNT) AND (FILNAM.CHARS[C] <> SUBDIRSEP) DO
      INC(C);
    END(*WHILE*);
    IF C <= FILNAM.COUNT THEN (* FOUND SUBDIRSEP CHAR, SO ABORT SEARCH *)
      RETCOD := 1;
      RETURN;
    END(*IF*);
    LOOP
      GETTKN(TOKEN,TKNSTATE,IGNORE,RETCOD2);
      IF (RETCOD2 <> 0) OR (TKNSTATE = DELIM) THEN
(*
  Reached end of env str.  TKNSTATE can only be DELIM if a null was
  encountered.  Hence the end of the Env string was found.
*)
        RETCOD := 1;
        RETURN;
      ELSIF (TOKEN.COUNT < 2) OR (TKNSTATE <> ALLELSE) THEN
(*
  BAD PATH STRING, IGNORE IT AND GET NEXT ONE
*)
      ELSE  (* TOKEN HAS A VALID PATH SPECIFIER.  BUILD TESTFILNAM AND ATTEMPT
               TO OPEN IT. *)
        IF (TOKEN.CHARS[TOKEN.COUNT] <> SUBDIRSEP) THEN  (* APPEND SUBDIRSEP *)
          TOKEN.CHARS[TOKEN.COUNT+1] := SUBDIRSEP;
          INC(TOKEN.COUNT);
          TOKEN.CHARS[TOKEN.COUNT+1] := NULL;
        END(*IF*);
        TESTFILNAM := TOKEN;
        FOR K := 1 TO FILNAM.COUNT+1 (* INCLUDING NULL TERMINATOR *) DO
          TESTFILNAM.CHARS[K + TOKEN.COUNT] := FILNAM.CHARS[K];
        END(*FOR*);
        INC(TESTFILNAM.COUNT,FILNAM.COUNT);
        Lookup(FILE,TESTFILNAM.CHARS,NEWFIL);
        IF FILE.res = done THEN
(* FOUND FILE.  GET READY TO RETURN. *)
          FILNAM := TESTFILNAM;
          RETURN;
        ELSIF FILE.res <> notdone THEN
(* SOME OTHER I/O ERROR OCCURRED.  SIGNAL THIS AND RETURN. *)
          RETCOD := 2;
          RETURN;
(*      ELSE--FILE.res = notdone SO FILE WAS NOT FOUND.  ITERATE AGAIN. *)
        END(*IF*);
      END(*IF*);
    END(*LOOP*);
  END(*WITH*);
END OPNFIL;

BEGIN  (* ENVRTNS MODULE BODY FOR INITIALIZATIONS *)
  MORE := FALSE;
  ENDOFENV := FALSE;
  HAVEPATH := FALSE;
  PATH := 'PATH';
END ENVRTNS.