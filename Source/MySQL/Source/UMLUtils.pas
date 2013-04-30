unit UMLUtils;

interface {********************************************************************}

function HTMLEscape(const Value: PChar; const ValueLen: Integer; const Escaped: PChar; const EscapedLen: Integer): Integer; overload;
function HTMLEscape(const Value: string): string; overload;
function XMLEscape(const Value: PChar; const ValueLen: Integer; const Escaped: PChar; const EscapedLen: Integer): Integer; overload;
function XMLEscape(const Value: string): string; overload;

implementation {***************************************************************}

function HTMLEscape(const Value: PChar; const ValueLen: Integer; const Escaped: PChar; const EscapedLen: Integer): Integer; overload;
label
  StringL, String2, String3, String4, String5, String6, String7, StringLE,
  Error,
  Finish;
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
        MOV EDX,EscapedLen               // Length of Escaped

        MOV @Result,0
        CMP ECX,0                        // Empty string?
        JE Error                         // Yes!

      StringL:
        LODSW                            // Character from Value

        CMP AX,9                         // #9 ?
        JE String7                       // Yes!
        CMP AX,10                        // #10 ?
        JE String7                       // Yes!

        CMP AX,13                        // #13 ?
        JNE String2                      // No!
        ADD @Result,5                    // 5 characters needed in Escaped
        CMP Escaped,0                    // Calculate length only?
        JE StringLE                      // Yes!
        SUB EDX,5                        // 5 characters left in Escaped?
        JC Error                         // No!
        MOV AX,'<'
        STOSW
        MOV AX,'b'
        STOSW
        MOV AX,'r'
        STOSW
        MOV AX,'>'
        STOSW
        MOV AX,13
        STOSW
        JMP StringLE

      String2:
        CMP AX,31                        // <= #31 ?
        JA String3                       // No!
        INC @Result                      // 1 characters needed in Escaped
        CMP Escaped,0                    // Calculate length only?
        JE StringLE                      // Yes!
        DEC EDX                          // 1 characters left in Escaped?
        JC Error                         // No!
        PUSH EAX
        MOV AX,'_'
        STOSW
        JMP StringLE

      String3:
        CMP AX,'"'                       // '"' ?
        JNE String4                      // No!
        ADD @Result,6                    // 6 characters needed in Escaped
        CMP Escaped,0                    // Calculate length only?
        JE StringLE                      // Yes!
        SUB EDX,6                        // 6 characters left in Escaped?
        JC Error                         // No!
        MOV AX,'&'
        STOSW
        MOV AX,'q'
        STOSW
        MOV AX,'u'
        STOSW
        MOV AX,'o'
        STOSW
        MOV AX,'t'
        STOSW
        MOV AX,';'
        STOSW
        JMP StringLE

      String4:                           // "normal" character
        CMP AX,'&'                       // "&" ?
        JNE String5                      // No!
        ADD @Result,5                    // 5 characters needed in Escaped
        CMP Escaped,0                    // Calculate length only?
        JE StringLE                      // Yes!
        SUB EDX,5                        // 5 characters left in Escaped?
        JC Error                         // No!
        STOSW
        MOV AX,'a'
        STOSW
        MOV AX,'m'
        STOSW
        MOV AX,'p'
        STOSW
        MOV AX,';'
        STOSW
        JMP StringLE

      String5:
        CMP AX,'<'                       // "<" ?
        JNE String6                      // No!
        ADD @Result,4                    // 4 characters needed in Escaped
        CMP Escaped,0                    // Calculate length only?
        JE StringLE                      // Yes!
        SUB EDX,4                        // 4 characters left in Escaped?
        JC Error                         // No!
        MOV AX,'&'
        STOSW
        MOV AX,'l'
        STOSW
        MOV AX,'t'
        STOSW
        MOV AX,';'
        STOSW
        JMP StringLE

      String6:
        CMP AX,'>'                       // ">" ?
        JNE String7                      // No!
        ADD @Result,4                    // 4 characters needed in Escaped
        CMP Escaped,0                    // Calculate length only?
        JE StringLE                      // Yes!
        SUB EDX,4                        // 4 characters left in Escaped?
        JC Error                         // No!
        MOV AX,'&'
        STOSW
        MOV AX,'g'
        STOSW
        MOV AX,'t'
        STOSW
        MOV AX,';'
        STOSW
        JMP StringLE

      String7:                           // "normal" character
        INC @Result                      // One character needed
        CMP Escaped,0                    // Calculate length only?
        JE StringLE                      // Yes!
        DEC EDX                          // One character left in Escaped?
        JC Error                         // No!
        STOSW

      StringLE:
        DEC ECX
        JNZ StringL
        JMP Finish

      // -------------------

      Error:
        MOV @Result,0                    // Too few space in Escaped

      Finish:
        POP EBX
        POP EDI
        POP ESI
        POP ES
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
  StringL, String2, String3, String4, String5, String6, String7, String8, StringLE,
  Error,
  Finish;
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
        MOV EDX,EscapedLen               // Length of Escaped

        MOV @Result,0
        CMP ECX,0                        // Empty string?
        JE Error                         // Yes!

      StringL:
        LODSW                            // Character from Value

        CMP AX,9                         // #9 ?
        JE String8                       // Yes!
        CMP AX,10                        // #10 ?
        JE String8                       // Yes!
        CMP AX,13                        // #13 ?
        JE String8                       // Yes!

        CMP AX,9                         // <= #9 ?
        JA String2                       // No!
        ADD @Result,4                    // 4 characters needed in Escaped
        CMP Escaped,0                    // Calculate length only?
        JE StringLE                      // Yes!
        SUB EDX,4                        // 4 characters left in Escaped?
        JC Error                         // No!
        PUSH EAX
        MOV AX,'&'
        STOSW
        MOV AX,'#'
        STOSW
        POP EAX
        ADD AX,'0'
        STOSW
        MOV AX,';'
        STOSW
        JMP StringLE

      String2:
        CMP AX,31                        // <= #31 ?
        JA String3                       // No!
        ADD @Result,5                    // 5 characters needed in Escaped
        CMP Escaped,0                    // Calculate length only?
        JE StringLE                      // Yes!
        SUB EDX,5                        // 5 characters left in Escaped?
        JC Error                         // No!
        PUSH EAX
        MOV AX,'&'
        STOSW
        MOV AX,'#'
        STOSW
        POP EAX
        PUSH EDX
        MOV EDX,00
        MOV EBX,10
        DIV BX
        ADD AX,'0'
        STOSW
        MOV AX,DX
        POP EDX
        ADD AX,'0'
        STOSW
        MOV AX,';'
        STOSW
        JMP StringLE

      String3:
        CMP AX,'"'                       // '"' ?
        JNE String4                      // No!
        ADD @Result,6                    // 6 characters needed in Escaped
        CMP Escaped,0                    // Calculate length only?
        JE StringLE                      // Yes!
        SUB EDX,6                        // 6 characters left in Escaped?
        JC Error                         // No!
        MOV AX,'&'
        STOSW
        MOV AX,'q'
        STOSW
        MOV AX,'u'
        STOSW
        MOV AX,'o'
        STOSW
        MOV AX,'t'
        STOSW
        MOV AX,';'
        STOSW
        JMP StringLE

      String4:                           // "normal" character
        CMP AX,'&'                       // "&" ?
        JNE String5                      // No!
        ADD @Result,5                    // 5 characters needed in Escaped
        CMP Escaped,0                    // Calculate length only?
        JE StringLE                      // Yes!
        SUB EDX,5                        // 5 characters left in Escaped?
        JC Error                         // No!
        STOSW
        MOV AX,'a'
        STOSW
        MOV AX,'m'
        STOSW
        MOV AX,'p'
        STOSW
        MOV AX,';'
        STOSW
        JMP StringLE

      String5:
        CMP AX,''''                      // "'" ?
        JNE String6                      // No!
        ADD @Result,6                    // 6 characters needed in Escaped
        CMP Escaped,0                    // Calculate length only?
        JE StringLE                      // Yes!
        SUB EDX,6                        // 6 characters left in Escaped?
        JC Error                         // No!
        MOV AX,'&'
        STOSW
        MOV AX,'a'
        STOSW
        MOV AX,'p'
        STOSW
        MOV AX,'o'
        STOSW
        MOV AX,'s'
        STOSW
        MOV AX,';'
        STOSW
        JMP StringLE

      String6:
        CMP AX,'<'                       // "<" ?
        JNE String7                      // No!
        ADD @Result,4                    // 4 characters needed in Escaped
        CMP Escaped,0                    // Calculate length only?
        JE StringLE                      // Yes!
        SUB EDX,4                        // 4 characters left in Escaped?
        JC Error                         // No!
        MOV AX,'&'
        STOSW
        MOV AX,'l'
        STOSW
        MOV AX,'t'
        STOSW
        MOV AX,';'
        STOSW
        JMP StringLE

      String7:
        CMP AX,'>'                       // ">" ?
        JNE String8                      // No!
        ADD @Result,4                    // 4 characters needed in Escaped
        CMP Escaped,0                    // Calculate length only?
        JE StringLE                      // Yes!
        SUB EDX,4                        // 4 characters left in Escaped?
        JC Error                         // No!
        MOV AX,'&'
        STOSW
        MOV AX,'g'
        STOSW
        MOV AX,'t'
        STOSW
        MOV AX,';'
        STOSW
        JMP StringLE

      String8:                           // "normal" character
        INC @Result                      // One character needed
        CMP Escaped,0                    // Calculate length only?
        JE StringLE                      // Yes!
        DEC EDX                          // One character left in Escaped?
        JC Error                         // No!
        STOSW

      StringLE:
        DEC ECX
        JNZ StringL
        JMP Finish

      // -------------------

      Error:
        MOV @Result,0                    // Too few space in Escaped

      Finish:
        POP EBX
        POP EDI
        POP ESI
        POP ES
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

end.
