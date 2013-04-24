unit UMLUtils;

interface {********************************************************************}

function HTMLEscape(const Value: PChar; const ValueLen: Integer; const Escaped: PChar; const EscapedLen: Integer): Integer; overload;
function HTMLEscape(const Value: string): string; overload;
function XMLEscape(const Value: PChar; const ValueLen: Integer; const Escaped: PChar; const EscapedLen: Integer): Integer; overload;
function XMLEscape(const Value: string): string; overload;

implementation {***************************************************************}

function HTMLEscape(const Value: PChar; const ValueLen: Integer; const Escaped: PChar; const EscapedLen: Integer): Integer; overload;
label
  StartL,
  StringL, String2, String3, String4, String5,
  MoveReplace, MoveReplaceL, MoveReplaceE,
  MoveBin, MoveBinE,
  PosL, PosE,
  FindSearchPos, FindSearchPos2,
  FindBinPos, FindBinPosL, FindBinPosLE, FindBinPosE,
  Error,
  Finish;
const
  SearchLen = 5;
  Search: array [0 .. SearchLen - 1] of Char = (#10, #13, '"', '<', '>');
  Replace: array [0 .. SearchLen - 1] of PChar = ('<br>' + #13#10, '', '&quot;', '&lt;', '&gt;');
var
  Len: Integer;
  Positions: packed array [0 .. SearchLen] of Cardinal;
begin
  Result := 0;
  Len := EscapedLen;

  asm
        PUSH ES
        PUSH ESI
        PUSH EDI
        PUSH EBX

        PUSH DS                          // string operations uses ES
        POP ES
        CLD                              // string operations uses forward direction

        MOV ESI,PChar(Value)             // Copy characters from Value
        MOV EDI,Escaped                  //   to Escaped
        MOV ECX,ValueLen                 // Length of Value string

        CMP ECX,0                        // Empty string?
        JE Error                         // Yes!

      // -------------------

        MOV EBX,0                        // Numbers of characters in Search
      StartL:
        CALL FindSearchPos               // Find Search character Pos
        INC EBX                          // Next character in Search
        CMP EBX,SearchLen                // All Search characters handled?
        JNE StartL                       // No!
        CALL FindBinPos

      // -------------------

      StringL:
        PUSH ECX

        MOV ECX,0                        // Numbers of characters in Search
        MOV EBX,-1                       // Index of first Pos
        MOV EAX,0                        // Last character
        LEA EDX,Positions
      PosL:
        CMP [EDX + ECX * 4],EAX          // Pos before other Positions?
        JB PosE                          // No!
        MOV EBX,ECX                      // Index of first Pos
        MOV EAX,[EDX + EBX * 4]          // Value of first Pos
      PosE:
        INC ECX                          // Next Pos
        CMP ECX,SearchLen                // All Positions compared?
        JBE PosL                         // No!

        POP ECX

        SUB ECX,EAX                      // Copy normal characters from Value
        JZ String3                       // End of Value!
        ADD @Result,ECX                  // Characters to copy
        CMP Escaped,0                    // Calculate length only?
        JE String2                       // Yes!
        CMP Len,ECX                      // Enough character left in Escaped?
        JB Error                         // No!
        SUB Len,ECX                      // Calc new len space in Escaped
        REPNE MOVSW                      // Copy normal characters to Result
        JMP String3
      String2:
        SHL ECX,1
        ADD ESI,ECX

      String3:
        MOV ECX,EAX
        CMP ECX,0                        // End of Value?
        JE Finish                        // Yes!

        CMP EBX,SearchLen
        JE MoveBin

      MoveReplace:
        PUSH ESI
        LEA EDX,Replace                  // Insert Replace string
        MOV ESI,[EDX + EBX * 4]
      MoveReplaceL:
        LODSW                            // Get Replace character
        CMP AX,0                         // End of Replace?
        JE MoveReplaceE                  // Yes!
        INC @Result                      // One characters in replace
        CMP Escaped,0                    // Calculate length only?
        JE MoveReplaceL                  // Yes!
        CMP Len,ECX                      // Enough character left in Escaped?
        JB Error                         // No!
        STOSW                            // Put character in Result
        JMP MoveReplaceL
      MoveReplaceE:
        POP ESI
        ADD ESI,2                        // Step of Search character
        JMP String4

      MoveBin:
        INC @Result                      // One characters needed
        CMP Escaped,0                    // Calculate length only?
        JE MoveBinE                      // Yes!
        MOV AX,'_'
        STOSW
      MoveBinE:
        ADD ESI,2                        // Step of Search character

      String4:
        DEC ECX                          // Ignore Search character
        JZ Finish                        // All character in Value handled!
        CMP EBX,SearchLen
        JE String5
        CALL FindSearchPos               // Find Search character
      String5:
        CALL FindBinPos                  // Find binary character
        JMP StringL

      // -------------------

      FindSearchPos:
        PUSH ECX
        PUSH EDI
        LEA EDI,Search                   // Character to Search
        MOV AX,[EDI + EBX * 2]
        MOV EDI,ESI                      // Search in Value
        REPNE SCASW                      // Find Search character
        JNE FindSearchPos2               // Search character not found!
        INC ECX
      FindSearchPos2:
        LEA EDI,Positions
        MOV [EDI + EBX * 4],ECX          // Store found Position
        POP EDI
        POP ECX
        RET

      // -------------------

      FindBinPos:
        PUSH ECX
        PUSH ESI
      FindBinPosL:
        MOV AX,[ESI]                     // Current character in Value
        CMP AX,9                         // Tabulator?
        JE FindBinPosLE                  // Yes!
        CMP AX,10                        // LineFeed?
        JE FindBinPosLE                  // Yes!
        CMP AX,13                        // CarriadgeReturn?
        JE FindBinPosLE                  // Yes!
        CMP AX,31                        // Binary character (#0 .. #31)?
        JA FindBinPosLE                  // No!
        LEA ESI,Positions
        MOV [ESI + SearchLen * 4],ECX    // Store found Position
        JMP FindBinPosE
      FindBinPosLE:
        ADD ESI,2                        // Next character in Value
        DEC ECX                          // One character handled
        JNZ FindBinPosL                  // Characters left in Value!
        LEA ESI,Positions
        MOV [ESI + SearchLen * 4],0      // Store found Position
      FindBinPosE:
        POP ESI
        POP ECX
        RET

      // -------------------

      Error:
        MOV @Result,0                    // Too few space in Escaped

      Finish:
        POP EBX
        POP EDI
        POP ESI
        POP ES
  end;
end;

function HTMLEscape(const Value: string): string;
var
  Len: Integer;
begin
  Len := HTMLEscape(PChar(Value), Length(Value), nil, 0);
  SetLength(Result, Len);
  if (Len > 0) then
    HTMLEscape(PChar(Value), Length(Value), PChar(Result), Len);
end;

function XMLEscape(const Value: PChar; const ValueLen: Integer; const Escaped: PChar; const EscapedLen: Integer): Integer;
label
  StartL,
  StringL, String2, String3, String4, String5,
  MoveReplace, MoveReplaceL, MoveReplaceE,
  MoveBin, MoveBinE,
  PosL, PosE,
  FindSearchPos, FindSearchPos2,
  FindBinPos, FindBinPosL, FindBinPosLE, FindBinPosE,
  Error,
  Finish;
const
  SearchLen = 5;
  Search: array [0 .. SearchLen - 1] of Char = ('&', '"', '''', '<', '>');
  Replace: array [0 .. SearchLen - 1] of PChar = ('&amp;', '&quot;', '&apos;', '&lt;', '&gt;');
  Hex: PChar = '0123456789ABCDEF';
var
  Len: Integer;
  Positions: packed array [0 .. SearchLen] of Cardinal;
begin
  Result := 0;
  Len := EscapedLen;

  asm
        PUSH ES
        PUSH ESI
        PUSH EDI
        PUSH EBX

        PUSH DS                          // string operations uses ES
        POP ES
        CLD                              // string operations uses forward direction

        MOV ESI,PChar(Value)             // Copy characters from Value
        MOV EDI,Escaped                  //   to Escaped
        MOV ECX,ValueLen                 // Length of Value string

        CMP ECX,0                        // Empty string?
        JE Error                         // Yes!

      // -------------------

        MOV EBX,0                        // Numbers of characters in Search
      StartL:
        CALL FindSearchPos               // Find Search character Pos
        INC EBX                          // Next character in Search
        CMP EBX,SearchLen                // All Search characters handled?
        JNE StartL                       // No!
        CALL FindBinPos

      // -------------------

      StringL:
        PUSH ECX

        MOV ECX,0                        // Numbers of characters in Search
        MOV EBX,-1                       // Index of first Pos
        MOV EAX,0                        // Last character
        LEA EDX,Positions
      PosL:
        CMP [EDX + ECX * 4],EAX          // Pos before other Positions?
        JB PosE                          // No!
        MOV EBX,ECX                      // Index of first Pos
        MOV EAX,[EDX + EBX * 4]          // Value of first Pos
      PosE:
        INC ECX                          // Next Pos
        CMP ECX,SearchLen                // All Positions compared?
        JBE PosL                         // No!

        POP ECX

        SUB ECX,EAX                      // Copy normal characters from Value
        JZ String3                       // End of Value!
        ADD @Result,ECX                  // Characters to copy
        CMP Escaped,0                    // Calculate length only?
        JE String2                       // Yes!
        CMP Len,ECX                      // Enough character left in Escaped?
        JB Error                         // No!
        SUB Len,ECX                      // Calc new len space in Escaped
        REPNE MOVSW                      // Copy normal characters to Result
        JMP String3
      String2:
        SHL ECX,1
        ADD ESI,ECX

      String3:
        MOV ECX,EAX
        CMP ECX,0                        // End of Value?
        JE Finish                        // Yes!

        CMP EBX,SearchLen
        JE MoveBin

      MoveReplace:
        PUSH ESI
        LEA EDX,Replace                  // Insert Replace string
        MOV ESI,[EDX + EBX * 4]
      MoveReplaceL:
        LODSW                            // Get Replace character
        CMP AX,0                         // End of Replace?
        JE MoveReplaceE                  // Yes!
        INC @Result                      // One characters in replace
        CMP Escaped,0                    // Calculate length only?
        JE MoveReplaceL                  // Yes!
        CMP Len,ECX                      // Enough character left in Escaped?
        JB Error                         // No!
        STOSW                            // Put character in Result
        JMP MoveReplaceL
      MoveReplaceE:
        POP ESI
        ADD ESI,2                        // Step of Search character
        JMP String4

      MoveBin:
        ADD @Result,6                    // 6 characters needed: &#xhh;
        CMP Escaped,0                    // Calculate length only?
        JE MoveBinE                      // Yes!
        CMP Escaped,6                    // Enough space in Escaped?
        JB Error                         // No!
        MOV AX,'&'
        STOSW
        MOV AX,'#'
        STOSW
        MOV AX,'x'
        STOSW
        MOV EDX,Hex
        MOV AX,[ESI]
        AND AX,$00F0
        SHR AX,4
        MOV AX,[EDX + EAX * 2]
        STOSW
        MOV AX,[ESI]
        AND AX,$000F
        MOV AX,[EDX + EAX * 2]
        STOSW
        MOV AX,';'
        STOSW
      MoveBinE:
        ADD ESI,2                        // Step of Search character

      String4:
        DEC ECX                          // Ignore Search character
        JZ Finish                        // All character in Value handled!
        CMP EBX,SearchLen
        JE String5
        CALL FindSearchPos               // Find Search character
      String5:
        CALL FindBinPos                  // Find binary character
        JMP StringL

      // -------------------

      FindSearchPos:
        PUSH ECX
        PUSH EDI
        LEA EDI,Search                   // Character to Search
        MOV AX,[EDI + EBX * 2]
        MOV EDI,ESI                      // Search in Value
        REPNE SCASW                      // Find Search character
        JNE FindSearchPos2               // Search character not found!
        INC ECX
      FindSearchPos2:
        LEA EDI,Positions
        MOV [EDI + EBX * 4],ECX          // Store found Position
        POP EDI
        POP ECX
        RET

      // -------------------

      FindBinPos:
        PUSH ECX
        PUSH ESI
      FindBinPosL:
        MOV AX,[ESI]                     // Current character in Value
        CMP AX,9                         // Tabulator?
        JE FindBinPosLE                  // Yes!
        CMP AX,10                        // LineFeed?
        JE FindBinPosLE                  // Yes!
        CMP AX,13                        // CarriadgeReturn?
        JE FindBinPosLE                  // Yes!
        CMP AX,31                        // Binary character (#0 .. #31)?
        JA FindBinPosLE                  // No!
        LEA ESI,Positions
        MOV [ESI + SearchLen * 4],ECX    // Store found Position
        JMP FindBinPosE
      FindBinPosLE:
        ADD ESI,2                        // Next character in Value
        DEC ECX                          // One character handled
        JNZ FindBinPosL                  // Characters left in Value!
        LEA ESI,Positions
        MOV [ESI + SearchLen * 4],0      // Store found Position
      FindBinPosE:
        POP ESI
        POP ECX
        RET

      // -------------------

      Error:
        MOV @Result,0                    // Too few space in Escaped

      Finish:
        POP EBX
        POP EDI
        POP ESI
        POP ES
  end;
end;

function XMLEscape(const Value: string): string;
var
  Len: Integer;
begin
  Len := XMLEscape(PChar(Value), Length(Value), nil, 0);
  SetLength(Result, Len);
  if (Len > 0) then
    XMLEscape(PChar(Value), Length(Value), PChar(Result), Len);
end;

begin
  HTMLEscape('Hallo' + #3#13#10 + 'Test');
end.
