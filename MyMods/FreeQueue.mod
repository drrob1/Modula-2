      MODULE FREEQUEUE;
      IMPORT IO, Lib;
      FROM Storage IMPORT ALLOCATE, DEALLOCATE;
      TYPE
         QUEUE = RECORD
                   first : POINTER TO ELEMENT;
                   last  : POINTER TO ELEMENT;
                 END;
         ELEMENT = RECORD
                   elemsize: CARDINAL;
                   elemptr : POINTER TO BYTE;
                   nextelem: POINTER TO ELEMENT;
                 END;
       
      PROCEDURE INITQ(VAR Q:QUEUE);
      BEGIN
          Q.first:=NIL;
          Q.last :=NIL;
  19.
      END INITQ;
  20.
       
  21.
      PROCEDURE EMPTYQ(Q:QUEUE):BOOLEAN;
  22.
      BEGIN
  23.
          RETURN Q.first=NIL
  24.
      END EMPTYQ;
  25.
       
  26.
      PROCEDURE ADD2Q(VAR Q: QUEUE; data: ARRAY OF BYTE);
  27.
      (* Add an element to the end of the queue. *)
  28.
      VAR
  29.
        i, sz : CARDINAL;
  30.
        el : ELEMENT;
  31.
        tmpptr: POINTER TO BYTE;
  32.
      BEGIN
  33.
          sz := SIZE(data);
  34.
          el.elemsize := sz;
  35.
          NEW(el.elemptr);
  36.
          el.nextelem := NIL;
  37.
          ALLOCATE(el.elemptr, sz);
  38.
          tmpptr := ADR(el.elemptr^);
  39.
          FOR i:= 0 TO sz-1 DO
  40.
              tmpptr^ := data[i];
  41.
      (*        IO.WrStr(data[i]);*)
  42.
              Lib.IncAddr(tmpptr, 1);
  43.
          END;
  44.
      (*    IO.WrLn;*)
  45.
       
  46.
          IF EMPTYQ(Q) THEN
  47.
          (* Add first element to the queue.
  48.
             IO.WrStr("Q empty. Add first element to the queue."); *)
  49.
             NEW(Q.first);
  50.
             NEW(Q.last);
  51.
             Q.first^ := el;
  52.
             Q.last   := ADR(Q.first^);
  53.
          ELSE
  54.
      (*       IO.WrStr("Q NOT empty. Add element.");*)
  55.
             NEW(Q.last^.nextelem);
  56.
             Q.last^.nextelem^ := el;
  57.
             Q.last := ADR(Q.last^.nextelem^);
  58.
          END;
  59.
      (*    IO.WrLn;*)
  60.
      END ADD2Q;
  61.
       
  62.
      PROCEDURE POPQ(VAR Q:QUEUE; VAR data : ARRAY OF BYTE);
  63.
      (* Get the first element in the queue and erase it. *)
  64.
      VAR
  65.
        i, sz : CARDINAL;
  66.
        qhead : ELEMENT;
  67.
        tmpptr: POINTER TO BYTE;
  68.
      BEGIN
  69.
      IF NOT EMPTYQ(Q) THEN
  70.
              sz := Q.first^.elemsize;
  71.
              qhead := Q.first^;
  72.
              NEW(tmpptr);
  73.
              tmpptr := ADR(qhead.elemptr^);
  74.
      (*      IO.WrCard(sz, 5);*)
  75.
              FOR i := 0 TO sz - 1 DO
  76.
                  data[i] := tmpptr^;
  77.
                  Lib.IncAddr(tmpptr, 1);
  78.
      (*          IO.WrStr(tmpptr^);
  79.
                  IO.WrStr(data[i]);*)
  80.
              END;
  81.
      (*      IO.WrLn;*)
  82.
              IF Q.first = Q.last THEN
  83.
              (* The last element. Queue becomes empty. *)
  84.
                 Q.first := NIL;
  85.
                 Q.last := NIL;
  86.
              ELSE
  87.
                 Q.first := ADR(Q.first^.nextelem^);
  88.
              END;
  89.
              DEALLOCATE(qhead.elemptr, qhead.elemsize);
  90.
      ELSE
  91.
         IO.WrStr("POP FAIL : Queue is empty");
  92.
         IO.WrLn;
  93.
      END;
  94.
      END POPQ;
  95.
       
  96.
      VAR
  97.
        qu : QUEUE;
  98.
        strelem: ARRAY [1..20] OF CHAR;
  99.
        crdelem: CARDINAL;
 100.
        rlelem : REAL;
 101.
       
 102.
      BEGIN
 103.
          INITQ(qu);
 104.
          IO.WrStr("Q Init done.");
 105.
          IO.WrLn;
 106.
       
 107.
          strelem:="Aleksandar M";
 108.
          ADD2Q(qu, strelem);
 109.
          IO.WrStr("Q Add: ");
 110.
          IO.WrStr(strelem);
 111.
          IO.WrLn;
 112.
       
 113.
          strelem:="test br 2 test broj2";
 114.
          ADD2Q(qu, strelem);
 115.
          IO.WrStr("Q Add: ");
 116.
          IO.WrStr(strelem);
 117.
          IO.WrLn;
 118.
       
 119.
          strelem := "";
 120.
          POPQ(qu, strelem);
 121.
          IO.WrStr("POP Q: ");
 122.
          IO.WrStr(strelem);
 123.
          IO.WrLn;
 124.
       
 125.
          strelem := "";
 126.
          POPQ(qu, strelem);
 127.
          IO.WrStr("POP Q: ");
 128.
          IO.WrStr(strelem);
 129.
          IO.WrLn;
 130.
       
 131.
          crdelem := 1974;
 132.
          ADD2Q(qu, crdelem);
 133.
          IO.WrStr("Q Add: ");
 134.
          IO.WrCard(crdelem, 10);
 135.
          IO.WrLn;
 136.
       
 137.
          rlelem := 1974.1974;
 138.
          ADD2Q(qu, rlelem);
 139.
          IO.WrStr("Q Add: ");
 140.
          IO.WrReal(rlelem, 10, 10);
 141.
          IO.WrLn;
 142.
       
 143.
          crdelem := 0;
 144.
          POPQ(qu, crdelem);
 145.
          IO.WrStr("POP Q: ");
 146.
          IO.WrCard(crdelem, 10);
 147.
          IO.WrLn;
 148.
       
 149.
          rlelem := 0.0;
 150.
          POPQ(qu, rlelem);
 151.
          IO.WrStr("POP Q: ");
 152.
          IO.WrReal(rlelem, 10, 10);
 153.
          IO.WrLn;
 154.
       
 155.
      END FREEQUEUE.

