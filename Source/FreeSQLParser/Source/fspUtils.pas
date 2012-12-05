unit fspUtils;

interface {********************************************************************}

const
  CP_UNICODE = 1200;
  BOM_UTF8: PAnsiChar = Chr($EF) + Chr($BB) + Chr($BF);
  BOM_UNICODE_LE: PAnsiChar = Chr($FF) + Chr($FE);

function HTMLEscape(const Value: PChar; const ValueLen: Integer; const Escaped: PChar; const EscapedLen: Integer): Integer; overload;
function HTMLEscape(const Value: string): string; overload;

implementation {***************************************************************}

function HTMLEscape(const Value: PChar; const ValueLen: Integer; const Escaped: PChar; const EscapedLen: Integer): Integer; overload;
label
  StartL,
  StringL, String2, String3, String4,
  MoveReplace, MoveReplaceL, MoveReplaceE,
  PosL, PosE,
  FindPos, FindPos2,
  Error,
  Finish;
const
  SearchLen = 6;
  Search: array [0 .. SearchLen - 1] of Char = (#0, #10, #13, '"', '<', '>');
  Replace: array [0 .. SearchLen - 1] of PChar = ('', '<br>' + #13#10, '', '&quot;', '&lt;', '&gt;');
var
  Len: Integer;
  Poss: packed array [0 .. SearchLen - 1] of Cardinal;
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

      // -------------------

        MOV EBX,0                        // Numbers of characters in Search
      StartL:
        CALL FindPos                     // Find Search character Pos
        INC EBX                          // Next character in Search
        CMP EBX,SearchLen                // All Search characters handled?
        JNE StartL                       // No!

      // -------------------

        CMP ECX,0                        // Empty string?
        JE Finish                        // Yes!
      StringL:
        PUSH ECX

        MOV ECX,0                        // Numbers of characters in Search
        MOV EBX,-1                       // Index of first Pos
        MOV EAX,0                        // Last character
        LEA EDX,Poss
      PosL:
        CMP [EDX + ECX * 4],EAX          // Pos before other Poss?
        JB PosE                          // No!
        MOV EBX,ECX                      // Index of first Pos
        MOV EAX,[EDX + EBX * 4]          // Value of first Pos
      PosE:
        INC ECX                          // Next Pos
        CMP ECX,SearchLen                // All Poss compared?
        JNE PosL                         // No!

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
        JECXZ Finish                     // End of Value!

        ADD ESI,2                        // Step of Search character


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


      String4:
        DEC ECX                          // Ignore Search character
        JZ Finish                        // All character in Value handled!
        CALL FindPos                     // Find Search character
        JMP StringL

      // -------------------

      FindPos:
        PUSH ECX
        PUSH EDI
        LEA EDI,Search                   // Character to Search
        MOV AX,[EDI + EBX * 2]
        MOV EDI,ESI                      // Search in Value
        REPNE SCASW                      // Find Search character
        JNE FindPos2                     // Search character not found!
        INC ECX
      FindPos2:
        LEA EDI,Poss
        MOV [EDI + EBX * 4],ECX          // Store found Position
        POP EDI
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

function HTMLEscape(const Value: string): string; overload;
var
  Len: Integer;
begin
  Len := HTMLEscape(PChar(Value), Length(Value), nil, 0);
  SetLength(Result, Len);
  HTMLEscape(PChar(Value), Length(Value), PChar(Result), Len);
end;

end.
