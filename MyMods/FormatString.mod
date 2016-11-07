(***************************************************************************)
(*                                                                         *)
(*                        Copyright (C) 1995-2003                          *)
(*                      by Stony Brook Software                            *)
(*                                                                         *)
(*                        All rights reserved.                             *)
(*                                                                         *)
(***************************************************************************)
IMPLEMENTATION MODULE FormatString;
<*/INLINE:N*>

FROM SYSTEM IMPORT
    ADDRESS, ADDADR, VA_START, VA_ARG;

FROM Strings IMPORT
    Delete, Insert, Append;

FROM Conversions IMPORT
    CardToStr, IntToStr, LongToStr, CardBaseToStr, LongBaseToStr;

PROCEDURE FormatString(formatStr : ARRAY OF CHAR;
                       VAR OUT destStr : ARRAY OF CHAR) : BOOLEAN;
VAR
    addr        : ADDRESS;
BEGIN
    VA_START(addr);
    RETURN FormatStringEx(formatStr, destStr, addr);
END FormatString;

PROCEDURE FormatStringEx(formatStr : ARRAY OF CHAR;
                         VAR OUT destStr : ARRAY OF CHAR;
                         dataPtr : ADDRESS) : BOOLEAN;
TYPE
    variant =
        <*/PUSH/PACK*>
        RECORD
        CASE : CARDINAL OF
        0: c    : CARDINAL;|
        1: i    : INTEGER;|
        2: li   : LONGINT;|
        3: lc   : LONGCARD;|
        4: ch   : CHAR;|
        5: a    : ADDRESS;
        ELSE
        END;
        END;
        <*/POP*>
VAR
    i                   : CARDINAL;
    j                   : CARDINAL;
    st                  : CARDINAL;
    l                   : CARDINAL;
    width               : CARDINAL;
    leftJust            : BOOLEAN;
    ok                  : BOOLEAN;
    dataType            : CHAR;
    padChar             : CHAR;
    ptr                 : POINTER TO variant;
    text                : ARRAY [0..255] OF CHAR;

    PROCEDURE insertText;
    VAR
        textLen : CARDINAL;
    BEGIN
        textLen := LENGTH(text);

        WHILE textLen < width DO
            IF leftJust THEN
                Append(padChar, text);
            ELSE
                Insert(padChar, 0, text);
            END;
            INC(textLen);
        END;

        Insert(text, i, destStr);
        i := i + textLen;
        l := l + textLen;

        (* did we just blow the capacity of destStr *)

        IF l-1 > HIGH(destStr) THEN
            ok := FALSE;
        END;
    END insertText;

    PROCEDURE getHexNum(VAR hexNum : CARDINAL) : BOOLEAN;
    VAR
        ch      : CHAR;
        num     : CARDINAL;
        gotOne  : BOOLEAN;
    BEGIN
        gotOne := FALSE;
        num := 0;
        WHILE (i < l) AND
              (
               (
                (destStr[i] >= '0') AND
                (destStr[i] <= '9')
               )
               OR
               (
                (CAP(destStr[i]) >= 'A') AND
                (CAP(destStr[i]) <= 'F')
               )
              )
        DO
            gotOne := TRUE;
            ch := CAP(destStr[i]);
            Delete(destStr, i, 1);
            DEC(l);

            num := num * 16;
            IF (ch >= '0') AND (ch <= '9') THEN
                num := num + (ORD(ch) - ORD('0'));
            ELSE
                num := num + (ORD(ch) - (ORD('A') + 10));
            END;
        END;

        IF gotOne AND
           (num >= ORD(MIN(CHAR))) AND (num <= ORD(MAX(CHAR)))
        THEN
            hexNum := num;
            RETURN TRUE;
        END;
        RETURN FALSE;
    END getHexNum;

    PROCEDURE getControlChar() : BOOLEAN;
    VAR
        ctrlChar        : CHAR;
        hexNum          : CARDINAL;
    BEGIN
        ctrlChar := destStr[i+1];
        Delete(destStr, i, 2);
        l := l - 2;

        CASE CAP(ctrlChar) OF
        'N':
            %IF %NOT UNIX %THEN
                Insert(CHR(13), i, destStr);
                INC(i);
                INC(l);
            %END
            Insert(CHR(10), i, destStr);
            INC(l);
        |
        'T':
            Insert(CHR(9), i, destStr);
            INC(l);
        |
        'V':
            Insert(CHR(11), i, destStr);
            INC(l);
        |
        'F':
            Insert(CHR(12), i, destStr);
            INC(l);
        |
        'X':
            IF getHexNum(hexNum) THEN
                Insert(CHR(hexNum), i, destStr);
                INC(l);
            ELSE
                RETURN FALSE;
            END;
        ELSE
            Insert(ctrlChar, i, destStr);
            INC(l);
        END;

        RETURN TRUE;
    END getControlChar;

BEGIN
    (* doing the code this way allows the formatStr and destStr *)
    (* to be the same string *)
    (* copying from one to the other on the fly will not work *)
    (* unless a copy of the formatStr is made *)

    destStr := formatStr;

    (* could be faster, but it would be much more complex *)

    ok := TRUE;
    l := LENGTH(destStr);
    i := 0;
    LOOP
        IF (i >= l) OR NOT ok THEN
            EXIT;
        ELSIF destStr[i] = '\' THEN
            IF i+1 < l THEN
                IF destStr[i+1] = '\' THEN
                    Delete(destStr, i, 1);
                    INC(i);
                    DEC(l);
                ELSE
                    IF NOT getControlChar() THEN
                        ok := FALSE;
                        EXIT;
                    END;
                END;
            ELSE
                ok := FALSE;
                EXIT;
            END;
        ELSIF destStr[i] <> '%' THEN
            INC(i);
        ELSIF (i+1 < l) AND (destStr[i+1] = '%') THEN
            Delete(destStr, i, 1);
            INC(i);
            DEC(l);
        ELSE
            st := i;
            INC(i);

            IF i >= l THEN
                ok := FALSE;
                EXIT;
            END;

            leftJust := FALSE;
            IF destStr[i] = '-' THEN
                leftJust := TRUE;
                INC(i);
            END;

            IF i >= l THEN
                ok := FALSE;
                EXIT;
            END;

            padChar := ' ';
            IF destStr[i] = "'" THEN
                INC(i);

                IF i >= l THEN
                    ok := FALSE;
                    EXIT;
                END;

                padChar := destStr[i];
                INC(i);
            END;

            IF i >= l THEN
                ok := FALSE;
                EXIT;
            END;

            width := 0;
            IF destStr[i] = '*' THEN
                INC(i);

                IF i >= l THEN
                    ok := FALSE;
                    EXIT;
                END;

                ptr := VA_ARG(dataPtr, CARDINAL);
                width := ptr^.c;
            ELSE
                WHILE (i < l) AND
                      (destStr[i] >= '0') AND
                      (destStr[i] <= '9')
                DO
                    width := (width * 10) + (ORD(destStr[i]) - ORD('0'));
                    INC(i);
                END;
            END;

            IF (i >= l) OR (width > 256) THEN
                ok := FALSE;
                EXIT;
            END;

            dataType := CAP(destStr[i]);

            Delete(destStr, st, i-st+1);
            l := l - (i-st+1);

            i := st;

            CASE dataType OF
            'C':
                ptr := VA_ARG(dataPtr, CARDINAL);
                IF CardToStr(ptr^.c, text) THEN
                    insertText;
                END;
            |
            'I':
                ptr := VA_ARG(dataPtr, INTEGER);
                IF IntToStr(ptr^.i, text) THEN
                    insertText;
                END;
            |
            'L':
                ptr := VA_ARG(dataPtr, LONGINT);
                IF LongToStr(ptr^.li, text) THEN
                    insertText;
                END;
            |
            'U':
                ptr := VA_ARG(dataPtr, LONGCARD);
                IF LongBaseToStr(ptr^.lc, 10, text) THEN
                    insertText;
                END;
            |
            'H':
                ptr := VA_ARG(dataPtr, CARDINAL);
                IF CardBaseToStr(ptr^.c, 16, text) THEN
                    insertText;
                END;
            |
            'X':
                ptr := VA_ARG(dataPtr, LONGCARD);
                IF LongBaseToStr(ptr^.lc, 16, text) THEN
                    insertText;
                END;
            |
            'S':
                ptr := VA_ARG(dataPtr, ADDRESS);
                ptr := ptr^.a;(*argument is a pointer to string*)

                j := 0;
                WHILE ptr^.ch <> '' DO
                    IF j < HIGH(text) THEN
                        text[j] := ptr^.ch;
                        INC(j);
                    END;
                    ptr := ADDADR(ptr, SIZE(CHAR));
                END;
                text[j] := '';
                insertText;
            ELSE
                ok := FALSE;
                EXIT;
            END;
        END;
    END;
    RETURN ok;
END FormatStringEx;

END FormatString.
