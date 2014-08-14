unit CSVUtils;

interface {********************************************************************}

type
  TCSVStrings = array of string;
  TCSVValues = array of record
    Text: PChar;
    Length: Integer;
  end;

function CSVBinary(const Data: PChar; const Length: Integer; const Quoter: Char = '"'): RawByteString;
function CSVEscape(const Bin: PAnsiChar; const Length: Integer; const Quoter: Char = '"'; const Quote: Boolean = True): string; overload;
function CSVEscape(const Value: PChar; const ValueLen: Integer; const Escaped: PChar; const EscapedLen: Integer; const Quoter: Char = '"'; const Quote: Boolean = True): Integer; overload;
function CSVEscape(const Value: string; const Quoter: Char = '"'; const Quote: Boolean = True): string; overload;
procedure CSVSplitValues(const TextLine: string; const Delimiter, Quoter: Char; var Values: TCSVStrings); overload;
function CSVSplitValues(const Text: string; var Index: Integer; const Delimiter, Quoter: Char; var Values: TCSVValues): Boolean; overload;
function CSVUnescape(const Text: string; const Quoter: Char = '"'): string; overload;
function CSVUnescape(const Value: PChar; const ValueLen: Integer; const Quoter: Char = '"'): string; overload;
function CSVUnescape(const Value: PChar; const ValueLen: Integer; const Unescaped: PChar; const UnescapedLen: Integer; const Quoter: Char = '"'): Integer; overload;

implementation {***************************************************************}

uses
  Windows;

function CSVBinary(const Data: PChar; const Length: Integer; const Quoter: Char = '"'): RawByteString;
label
  UnquotedL,
  QuotedL, QuotedB, QuotedLE,
  Finish;
var
  Len: Integer;
begin
  if (Length = 0) then
    Result := ''
  else if (Data[0] <> Quoter) then
  begin
    SetLength(Result, Length);

    asm
        PUSH ES
        PUSH ESI
        PUSH EDI

        PUSH DS                          // string operations uses ES
        POP ES
        CLD                              // string operations uses forward direction

        MOV ESI,Data                     // Copy characters from Data
        MOV EAX,Result                   //   to Result
        MOV EDI,[EAX]

        MOV ECX,Length                   // Number of characters

        MOV AH,0                         // Clear AH, since AL will be load, but AX stored
      UnquotedL:
        LODSW                            // Load character from Data
        STOSB                            // Store character into Result
        LOOP UnquotedL                   // Next character in Data!

        POP EDI
        POP ESI
        POP ES
    end;
  end
  else
  begin
    SetLength(Result, Length - 2);

    asm
        PUSH ES
        PUSH ESI
        PUSH EDI

        PUSH DS                          // string operations uses ES
        POP ES
        CLD                              // string operations uses forward direction

        MOV ESI,Data                     // Copy characters from Data
        MOV EAX,Result                   //   to Result
        MOV EDI,[EAX]

        MOV ECX,Length                   // Number of characters

      // -------------------

        LODSW                            // Step over starting Quoter
        DEC ECX                          // Ignore the starting Quoter
        JZ Finish                        // No characters left!
      QuotedL:
        LODSW                            // Load character from Data
        CMP AX,Quoter                    // Previous character = Quoter?
        JNE QuotedB                      // No!
        DEC ECX                          // Ignore Quoter
        JZ Finish                        // End of Data!
        MOV AX,[ESI]
        CMP AX,Quoter                    // Second Quoter?
        JNE Finish                       // No!
        ADD ESI,2                        // Step over second Quoter
      QuotedB:
        STOSW                            // Store character into Result
      QuotedLE:
        LOOP QuotedL                     // Next character in Data!

      // -------------------

      Finish:
        MOV EAX,Result                   // Calculate new length of Result
        MOV EAX,[EAX]
        SUB EDI,EAX
        SHR EDI,1                        // 2 Bytes = 1 character
        MOV Len,EDI

        POP EDI
        POP ESI
        POP ES
    end;

    if (Len <> Length - 2) then
      SetLength(Result, Len);
  end;
end;

function CSVEscape(const Bin: PAnsiChar; const Length: Integer; const Quoter: Char = '"'; const Quote: Boolean = True): string;
label
  StringI, StringL, StringS, StringLE,
  Finish;
var
  Len: Integer;
begin
  if (not Assigned(Bin) or (Length = 0)) then
    Result := Quoter + Quoter
  else
  begin
    SetLength(Result, 2 * Length + 2);

    asm
        PUSH ES
        PUSH ESI
        PUSH EDI
        PUSH EBX

        PUSH DS                          // string operations uses ES
        POP ES
        CLD                              // string operations uses forward direction

        MOV ESI,Bin                      // Copy bytes from Bin
        MOV EAX,Result                   //   to Result
        MOV EDI,[EAX]
        MOV ECX,Length                  // Number of characters

      // -------------------

        CMP Quote,False                  // Quote value?
        JE StringI                       // No!
        MOV AX,Quoter                    // Put starting Quoter
        STOSW                            //   in Result

      StringI:
        MOV AH,0                         // Clear AH, since AL will be loaded, but AX stored
      StringL:
        LODSB                            // Get character from Value
        STOSW                            // Put character to Result
        CMP AX,Quoter                    // Current character = Quoter?
        JNE StringLE                     // No!
        STOSW                            // Put Quoter again to Result
      StringLE:
        LOOP StringL                     // Next character in Value!

        CMP Quote,False                  // Quote value?
        JE Finish                        // No!
        MOV AX,Quoter                    // Put ending Quoter
        STOSW                            //   in Result

      // -------------------

      Finish:
        MOV EAX,Result                   // Calculate length of Result
        MOV EAX,[EAX]
        SUB EDI,EAX
        SHR EDI,1                        // 2 Bytes = 1 character
        MOV Len,EDI

        POP EBX
        POP EDI
        POP ESI
        POP ES
    end;

    SetLength(Result, Len);
  end;
end;

function CSVEscape(const Value: string; const Quoter: Char = '"'; const Quote: Boolean = True): string;
var
  Len: Integer;
begin
  Len := CSVEscape(PChar(Value), Length(Value), nil, 0, '"', Quote);
  SetLength(Result, Len);
  CSVEscape(PChar(Value), Length(Value), PChar(Result), Len, '"', Quote);
end;

function CSVEscape(const Value: PChar; const ValueLen: Integer; const Escaped: PChar; const EscapedLen: Integer; const Quoter: Char = '"'; const Quote: Boolean = True): Integer;
label
  ValueL, ValueQuot, ValueLE, ValueE,

  Error,
  Finish;
begin
    asm
        PUSH ES
        PUSH ESI
        PUSH EDI
        PUSH EBX

        PUSH DS                          // string operations uses ES
        POP ES
        CLD                              // string operations uses forward direction

        MOV EBX,0

        MOV ESI,PChar(Value)             // Copy characters from Value
        MOV EDI,Escaped                  //   to Escaped
        MOV ECX,ValueLen                 // Length of Value string
        MOV EDX,EscapedLen               // Length of Escaped

      // -------------------

        CMP Quote,False                  // Quote value?
        JE ValueL                        // No!
        INC EBX                          // 1 characters needed in Escaped
        CMP Escaped,0                    // Calculate length only?
        JE ValueL                        // Yes!
        DEC EDX                          // 1 characters left in Escaped?
        JC Error                         // No!
        MOV AX,Quoter                    // Put starting Quoter
        STOSW                            //   in Result

      ValueL:
        CMP ECX,0                        // All characters handled?
        JE ValueE                        // Yes!
        LODSW                            // Get character from Value
        INC EBX                          // 1 characters needed in Escaped
        CMP Escaped,0                    // Calculate length only?
        JE ValueQuot                     // Yes!
        DEC EDX                          // 1 characters left in Escaped?
        JC Error                         // No!
        STOSW                            // Put character to Escaped
      ValueQuot:
        CMP AX,Quoter                    // Current character = Quoter?
        JNE ValueLE                      // No!
        INC EBX                          // 1 characters needed in Escaped
        CMP Escaped,0                    // Calculate length only?
        JE ValueLE                       // Yes!
        DEC EDX                          // 1 characters left in Escaped?
        JC Error                         // No!
        STOSW                            // Put Quoter again to Result
      ValueLE:
        LOOP ValueL                      // Next character in Value

      ValueE:
        MOV @Result,EBX
        CMP Quote,False                  // Quote value?
        JE Finish                        // No!
        INC EBX                          // 1 characters needed in Escaped
        MOV @Result,EBX
        CMP Escaped,0                    // Calculate length only?
        JE Finish                        // Yes!
        DEC EDX                          // 1 characters left in Escaped?
        JC Error                         // No!
        MOV AX,Quoter                    // Put ending Quoter
        STOSW                            //   in Result
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
end;

procedure CSVSplitValues(const TextLine: string; const Delimiter, Quoter: Char; var Values: TCSVStrings);
var
  I, Index: Integer;
  CSVValues: TCSVValues;
begin
  Index := 1;
  CSVSplitValues(TextLine, Index, Delimiter, Quoter, CSVValues);
  SetLength(Values, Length(CSVValues));
  for I := 0 to Length(Values) - 1 do
  begin
    SetLength(Values[I], CSVUnescape(CSVValues[I].Text, CSVValues[I].Length, nil, 0));
    CSVUnescape(CSVValues[I].Text, CSVValues[I].Length, PChar(Values[I]), Length(Values[I]));
  end;
end;

function CSVSplitValues(const Text: string; var Index: Integer; const Delimiter, Quoter: Char; var Values: TCSVValues): Boolean; overload;
label
  StartL, StartL2, StartLE, StartE,
  Unquoted, UnquotedL, UnquotedE,
  Quoted, QuotedL, QuotedLE, QuotedE, QuotedE2,
  IgnoreL,
  Finish, Finish1, Finish2, Finish3, Finish4, FinishE;
var
  Value, Len, ValueLength: Integer;
  ValueData: PChar;
  EOL, EOF: LongBool;
begin
  if (Index > Length(Text)) then
  begin
    SetLength(Values, 0);
    Result := False;
  end
  else
  begin
    Value := 0;
    repeat
      if (Value >= Length(Values)) then
        SetLength(Values, 2 * Value + 1);

      Len := Length(Text) - (Index - 1);
      asm
        PUSH ES
        PUSH ESI
        PUSH EDI
        PUSH EBX

        MOV ESI,PChar(Text)              // Get character from Text
        MOV EBX,Index
        ADD ESI,[EBX]                    // Add Index twice
        ADD ESI,[EBX]                    //   since character = 2 byte
        SUB ESI,2                        // Index based on "1" in string

        MOV ECX,Len
        MOV EOF,False
        MOV BYTE PTR [Result],True

      // -------------------

        CMP ECX,0                        // End of Text?
        JE Finish                        // Yes!
      StartL:
        MOV AX,[ESI]                     // Get character from Text
        CMP Value,0                      // First value?
        JNE StartL2                      // No!
        CMP AX,10                        // Character in Text = NewLine?
        JE StartLE                       // Yes!
        CMP AX,13                        // Character in Text = Carrige Return?
        JE StartLE                       // Yes!
        CMP AX,26                        // Character in Text = EndOfFile?
        JNE StartL2                      // No!
        MOV EOF,True
        JMP FinishE
      StartL2:
        CMP AX,Quoter                    // Character in Text = Quoter?
        JE Quoted                        // Yes!
        JMP Unquoted
      StartLE:
        ADD ESI,2                        // Step over Line Break
        DEC Len                          // Ignore the Line Break
        LOOP StartL
      StartE:
        MOV EOF,True
        JMP Finish

      // -------------------

      Unquoted:
        MOV ValueData,ESI                // Start of value
      UnquotedL:
        MOV AX,[ESI]                     // Get character from Text
        CMP AX,Delimiter                 // Character = Delimiter?
        JE UnquotedE                     // Yes!
        CMP AX,13                        // Character = CarrigeReturn?
        JE UnquotedE                     // Yes!
        CMP AX,10                        // Character = NewLine?
        JE UnquotedE                     // Yes!
        CMP AX,26                        // Character = EndOfFile?
        JE UnquotedE                     // Yes!
        ADD ESI,2                        // Next character!
        LOOP UnquotedL
      UnquotedE:
        MOV EAX,ESI                      // Calculate length of Values[Value]
        SUB EAX,ValueData
        SHR EAX,1                        // 2 bytes = 1 character
        MOV ValueLength,EAX
        JMP Finish

      // -------------------

      Quoted:
        MOV ValueData,ESI                // Start of value
        ADD ESI,2                        // Step over starting Quoter
        DEC ECX                          // Starting Quoter handled
        JZ QuotedE                       // End of Text!
      QuotedL:
        MOV AX,[ESI]                     // Get character from Text
        CMP AX,Quoter                    // Character = Quoter?
        JNE QuotedLE                     // No!
        ADD ESI,2                        // Step over Quoter
        DEC ECX                          // Quoter handled in Text
        JZ QuotedE                       // End of Text!
        MOV AX,[ESI]                     // Get character from Text
        CMP AX,Quoter                    // Character = second Quoter?
        JNE QuotedE                      // No!
      QuotedLE:
        ADD ESI,2                        // Next character!
        LOOP QuotedL
      QuotedE:
        MOV EAX,ESI                      // Calculate length of Values[Value]
        SUB EAX,ValueData
        SHR EAX,1                        // 2 bytes = 1 character
        MOV ValueLength,EAX
        CMP ECX,0                        // Further character in Text?
        JE Finish                        // No!

      IgnoreL:
        MOV AX,WORD PTR [ESI]            // Get next character
        CMP AX,Delimiter                 // Next character = Delimiter?
        JE Finish                        // Yes!
        CMP AX,10                        // Next character = NewLine?
        JE Finish                        // Yes!
        CMP AX,13                        // Next character = CarrigeReturn?
        JE Finish                        // Yes!
        CMP AX,26                        // Next character = EndOfFile?
        JE Finish                        // Yes!
        ADD ESI,2                        // Ignore character
        LOOP IgnoreL

      // -------------------

      Finish:
        CMP ECX,1                        // Is there one characters left in SQL?
        JGE Finish1                      // Yes!
        MOV BYTE PTR [Result],False
        JMP FinishE
      Finish1:
        MOV AX,[ESI]
        CMP AX,Delimiter                 // Delimiter?
        JNE Finish2
        ADD ESI,2                        // Step over Delimiter
        DEC ECX                          // Ignore Delimiter
        MOV EOL,False
        JMP FinishE
      Finish2:
        CMP ECX,2                        // Are there two characters left in SQL?
        JB Finish3                       // No!
        MOV EAX,[ESI]
        CMP EAX,$000A000D                // CarriageReturn + LineFeed?
        JNE Finish3
        ADD ESI,4                        // Step over CarriageReturn + LineFeed
        SUB ECX,2                        // Ignore CarriageReturn + LineFeed
        MOV EOL,True
        JMP FinishE
      Finish3:
        CMP ECX,1                        // Are there one characters left in SQL?
        JB FinishE                       // No!
        CMP AX,13                        // CarriageReturn?
        JNE Finish4
        ADD ESI,2                        // Step over CarriageReturn
        DEC ECX                          // Ignore CarriageReturn
        MOV EOL,True
        JMP FinishE
      Finish4:
        CMP AX,10                        // LineFeed?
        JNE Finish4
        ADD ESI,2                        // Step over LineFeed
        DEC ECX                          // Ignore LineFeed
        MOV EOL,True
        JMP FinishE
      FinishE:
        MOV EAX,ESI                      // Calculate new Index in Text
        SUB EAX,PChar(Text)
        SHR EAX,1                        // 2 bytes = 1 character
        INC EAX                          // Index based on "1" in string
        MOV [EBX],EAX

        POP EBX
        POP EDI
        POP ESI
        POP ES
      end;

      if (not EOF) then
      begin
        Values[Value].Text := ValueData;
        Values[Value].Length := ValueLength;
        Inc(Value);
      end;
    until (EOL or not Result);

    if (Value <> Length(Values)) then
      SetLength(Values, Value);
  end;
end;

function CSVUnescape(const Text: string; const Quoter: Char = '"'): string;
var
  Len: Integer;
begin
  Len := CSVUnescape(PChar(Text), Length(Text), nil, 0, Quoter);
  SetLength(Result, Len);
  CSVUnescape(PChar(Text), Length(Text), PChar(Result), Length(Result), Quoter);
end;

function CSVUnescape(const Value: PChar; const ValueLen: Integer; const Quoter: Char = '"'): string;
var
  Len: Integer;
begin
  Len := CSVUnescape(Value, ValueLen, nil, 0, Quoter);
  SetLength(Result, Len);
  CSVUnescape(Value, ValueLen, PChar(Result), Length(Result), Quoter);
end;

function CSVUnescape(const Value: PChar; const ValueLen: Integer; const Unescaped: PChar; const UnescapedLen: Integer; const Quoter: Char = '"'): Integer;
label
  StringL, StringS, StringQ, StringLE,
  Error, Finish;
var
  Quoted: Boolean;
begin
  asm
        PUSH ES
        PUSH ESI
        PUSH EDI
        PUSH EBX

        PUSH DS                          // string operations uses ES
        POP ES
        CLD                              // string operations uses forward direction

        MOV ESI,Value                    // Copy characters from Value
        MOV EDI,Unescaped                //   to Unescaped
        MOV ECX,ValueLen                 // Length of Value string
        MOV EDX,UnescapedLen             // Length of Unescaped

        MOV EBX,0                        // Result

        CMP ECX,0                        // Empty string?
        JE Error                         // Yes!

        MOV Quoted,False
        MOV AX,Quoter
        CMP [ESI],AX                     // Quoted Value?
        JNE StringL                      // No!
        MOV Quoted,True
        DEC ECX                          // One character handled
        ADD ESI,2                        // Step over starting Quoter

      StringL:
        LODSW                            // Character from Value
        CMP AX,Quoter                    // Quoter in Value?
        JNE StringS                      // No!
        MOV @Result,EBX
        CMP ECX,0                        // End of Value?
        JE Finish
        CMP [ESI],AX                     // Two Quoters?
        JE StringQ
        CMP Quoted,True                  // Quoted Value?
        JE Finish                        // Yes!
      StringQ:
        DEC ECX                          // One character handled
        LODSW                            // Character from Value
        CMP AX,Quoter                    // Second Quoter
        JE StringS                       // Yes!
        MOV @Result,EBX
        JMP Finish                       //

      StringS:
        INC EBX                          // One character needed
        CMP EDI,0                        // Calculate length only?
        JE StringLE                      // Yes!
        STOSW                            // Store character into Unescaped

      StringLE:
        LOOP StringL                     // Next character in Data!
        MOV @Result,EBX
        JMP Finish

      // -------------------

      Error:
        MOV @Result,0                    // Too few space in Escaped!

      Finish:
        POP EBX
        POP EDI
        POP ESI
        POP ES
  end;
  end;

function CSVUnquote(const Quoted: PChar; const QuotedLength: Integer; const Unquoted: PChar; const UnquotedLength: Integer; const Quoter: Char = '"'): Integer;
label
  Copy,
  Unquote, UnquoteL, UnquoteS, UnquoteLE, UnquoteE,
  Finish;
asm
        PUSH ES
        PUSH ECX
        PUSH EAX
        PUSH EDX
        PUSH ESI
        PUSH EDI

        MOV ESI,Quoted                   // Copy characters from Quoted
        MOV EDI,Unquoted                 //   to Unquoted
        MOV ECX,QuotedLength             // Number of characters of Quoted
        MOV EDX,UnquotedLength           // Number of characters of Unquoted

        PUSH DS                          // string operations uses ES
        POP ES
        CLD                              // string operations uses forward direction

        MOV Result,0
        TEST ESI,-1                      // Quoted = nil?
        JZ Finish                        // Yes!
        TEST QuotedLength,-1             // QuotedLength = 0?
        JZ Finish                        // Yes!
        MOV AX,[ESI]                     // Quoted[0] = Quoter?
        CMP AX,Quoter
        JE Unquote                       // Yes!

      Copy:
        CMP UnquotedLength,QuotedLength  // Enough space in Unquoted?
        JB Finish                        // No!
        TEST EDI,-1                      // Unquoted = nil?
        JZ Finish                        // Yes!
        MOV Result,ECX
        REPNE MOVSW                      // Copy normal characters to Result
        JMP Finish

      Unquote:
        ADD ESI,2                        // Step over starting Quoter
        DEC ECX                          // Ignore the starting Quoter
        JZ Finish                        // No characters left!

      UnquoteL:
        LODSW                            // Load character from Data
        CMP AX,Quoter                    // Previous character = Quoter?
        JNE UnquoteS                     // No!
        DEC ECX                          // Ignore Quoter
        JZ UnquoteE                      // End of Data!
        MOV AX,[ESI]
        CMP AX,Quoter                    // Second Quoter?
        JNE UnquoteE                     // No!
        ADD ESI,2                        // Step over second Quoter
      UnquoteS:
        TEST EDI,-1                      // Unquoted = nil?
        JZ UnquoteLE                     // Yes!
        CMP EDX,0                        // Space left in Unquoted?
        JE Finish                        // No!
        STOSW                            // Store character into Unquoted
        DEC EDX
      UnquoteLE:
        LOOP UnquoteL                    // Next character

      UnquoteE:
        CMP ECX,0                        // All characters handled?
        JNE Finish                       // No!

        MOV EAX,UnquotedLength           // Calculate new Unquoted Length
        SUB EAX,EDX
        MOV Result,EAX

      Finish:
        POP EDI
        POP ESI
        POP EDX
        POP ECX
        POP EAX
        POP ES
end;

end.
