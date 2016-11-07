MODULE tcolor;

FROM SYSTEM IMPORT
    FUNC;

FROM TextWindows IMPORT
    TextWindow, Colors, TWMessageRec, ResponseType,
    TextWindowsMsg, CloseModes, CloseWindow,
    NormalWindow, DefaultFontInfo, NormalFont, COORDINATE,
    CreateWindow, WindowTypes, DispatchMessages,
    WriteStringAt, ComposeAttribute, SpecialKeys,
    TerminateDispatchMessages;

CONST
    Spaces      = "                                ";

PROCEDURE WndProc(tw : TextWindow; msg : TWMessageRec) : ResponseType;
VAR
    clr         : Colors;
    y           : COORDINATE;
BEGIN
    CASE msg.msg OF
    TWM_CLOSE:
        IF msg.closeMode = CM_DICTATE THEN
            TerminateDispatchMessages(0);
        END;
        RETURN OkayToClose;
    |
    TWM_PAINT:
        FOR y := msg.paintRect.y1 TO msg.paintRect.y2 DO
            clr := VAL(Colors, y);
            WriteStringAt(tw,
                          msg.paintRect.x1,
                          y,
                          Spaces[0..msg.paintRect.x2-msg.paintRect.x1+1],
                          ComposeAttribute(White, clr, NormalFont));
        END;
    |
    TWM_KEY:
        IF (msg.k_special = KEY_NOTSPECIAL) AND (msg.k_ch = CHR(27)) THEN
            FUNC CloseWindow(tw, CM_REQUEST);
        END;
    ELSE
    END;
    RETURN DEFAULT_HANDLE;
END WndProc;

VAR
    Win         : TextWindow;
BEGIN
(* create a new window
   parent = as WinShell
   name = as WinShell
   menu = the menu for the window. Can be "".
   icon =  as WinShell
   attribs = as WinShell
   wndProc = the window procedure
   createParam = an arbitrary value you can use to pass information
                 to the window procedure of the window. this value is
                 passed in the WSM_CREATE message.
   font = the font to use for this window
   background = the background color for this window
   gutter = TRUE then the text window will always have a blank "gutter"
            on the left edge of the text window.
            FALSE the text will start at the left edge of the client area.
TYPE COORDINATE  = INTEGER;
   x, y = the initial screen coordinates for the window to be displayed
          if a parameter is -1 then the operating system will choose
          a default location for that coordinate.
          these positions are in pixels and are relative to the
          parent window client area origin for child windows
          or relative to the screen origin for all other windows.
   xSize, ySize = the initial width and height in character cells
                  if -1 then a system default size will be used
   xBuffer, yBuffer = the size of the screen buffer. the window can never
                      be larger than the screen buffer. if either xBuffer
                      or yBuffer is -1 the screen buffer is a variable size
                      and is sized to the number of cells the window client
                      area currently is capable displaying.
   returns the window handle is successfull, otherwise NIL
*)
    Win := CreateWindow(TopLevel,      (* Window Types *)
                        NIL,           (* parent : WinShell.Window *)
                        "Colors Test", (* name : ARRAY OF CHAR *)
                        "",            (* menu :   "           *)
                        "",            (* icon :   "           *)
                        -1, -1,        (* x, y : COORDINATE    *)
                        SIZE(Spaces), ORD(MAX(Colors))+1, (* xSize,ySize:COORDINATE *)
                        SIZE(Spaces), ORD(MAX(Colors))+1, (* xBuf,yBuf:COORDINATE *)
                        TRUE,           (* gutter : BOOLEAN *)
                        DefaultFontInfo,(* font : FontInfo *)
                        ComposeAttribute(Black, White, NormalFont), (* backgrnd : ScreenAtrribute *)
                        WndProc,     (* wndProc: TextWindowProcedure *)
                        NormalWindow,(* attribs: WinAttrSet *)
                        NIL);        (* createParam : ADDRESS *) : TextWindow;
    IF Win <> NIL THEN
        FUNC DispatchMessages();
    END;
END tcolor.
(******************************************************************************)
MODULE tattrib;

FROM SYSTEM IMPORT
    FUNC;

FROM TextWindows IMPORT
    TextWindow, Colors, TWMessageRec, ResponseType,
    TextWindowsMsg, CloseModes, ScreenAttribute,
    NormalWindow, DefaultFontInfo, NormalFont, BoldFont, ItalicFont,
    CreateWindow, WindowTypes, DispatchMessages,
    WriteString, WriteLn, ComposeAttribute, MoveCaretTo, EraseRect,
    TerminateDispatchMessages, FontInfo;

FROM BasicDialogs  IMPORT
    PromptChooseFont, FontOptionSet, FontOptions;

VAR
    Attribs     : ARRAY [0..3] OF ScreenAttribute;
    font        : FontInfo;
CONST
    Text        = "THEN Abc := +-/*<> ";

PROCEDURE DoPaint(tw : TextWindow);
VAR
    i   : CARDINAL;
BEGIN
    MoveCaretTo(tw, 0, 0);
    FOR i := 0 TO HIGH(Attribs) DO
        WriteString(tw, Text, Attribs[i]);
        WriteString(tw, Text, Attribs[0]);
        WriteLn(tw);
    END;
    FOR i := 1 TO 15 DO
        WriteString(tw, "<>", Attribs[0]);
        WriteString(tw, "<>", Attribs[3]);
    END;
    WriteLn(tw);
    FOR i := 1 TO 15 DO
        WriteString(tw, "<>", Attribs[0]);
        WriteString(tw, "<>", Attribs[0]);
    END;
    WriteLn(tw);
END DoPaint;

PROCEDURE WndProc(tw : TextWindow; msg : TWMessageRec) : ResponseType;
BEGIN
    CASE msg.msg OF
    TWM_CLOSE:
        IF msg.closeMode = CM_DICTATE THEN
            TerminateDispatchMessages(0);
        END;
        RETURN OkayToClose;
    |
    TWM_PAINT:
        EraseRect(tw, msg.paintRect, Attribs[0]);
        DoPaint(tw);
    ELSE
    END;
    RETURN DEFAULT_HANDLE;
END WndProc;

BEGIN
    Attribs[0] := ComposeAttribute(Black, White, NormalFont);
    Attribs[1] := ComposeAttribute(Black, White, BoldFont);
    Attribs[2] := ComposeAttribute(Black, White, ItalicFont);
    Attribs[3] := ComposeAttribute(Black, White, BoldFont+ItalicFont);

    font := DefaultFontInfo;
    IF PromptChooseFont(font, FontOptionSet{FixedPitchOnly}) THEN
        IF CreateWindow(TopLevel,
                        NIL,
                        "Attributes Test",
                        "",
                        "",
                        -1, -1,
                        60, 6,
                        60, 6,
                        TRUE,
                        font,
                        ComposeAttribute(Black, White, NormalFont),
                        WndProc,
                        NormalWindow,
                        NIL) <> NIL
        THEN
            FUNC DispatchMessages();
        END;
    END;
END tattrib.
(*************************************************************************)
