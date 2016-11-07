MODULE test;

FROM WINUSER IMPORT MessageBox,MB_OK,MB_ICONEXCLAMATION;

VAR res : INTEGER;
BEGIN
    res:=MessageBox(NIL, "Narf!", "Pinky says...",
                    MB_OK BOR MB_ICONEXCLAMATION);
END test.

