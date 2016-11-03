unit MySQLDBGrid;

interface {********************************************************************}

uses
  Windows, Classes, Controls, Types, Grids, Messages, DB, Graphics, DBGrids,
  StdActns, DBCtrls, ComCtrls,
  StdActns_Ext;

type
  TMySQLDBGrid = class(TDBGrid)
  type
    TFilterChange = procedure(Sender: TObject; Index: Integer) of object;

    TDBMySQLInplaceEdit = class(TInplaceEditList)
    private
      DoNotRemove: Integer; // Why is this needed??? Without this, there is Access Violation while freeing TMySQLDBGrid, if the InplaceEditor has been used in Delphi XE2
    protected
      procedure CloseUp(Accept: Boolean); override;
      procedure DoEditButtonClick(); override;
      procedure DropDown(); override;
      procedure KeyPress(var Key: Char); override;
    public
      constructor Create(Owner: TComponent); override;
      property Font;
    end;

  const
    tiShowHint = 1;
    tiHideHint = 2;
    CF_MYSQLRECORD = CF_PRIVATEFIRST + 80;
  private
    FIgnoreKeyPress: Boolean;
    FHeaderControl: THeaderControl;
    FHintWindow: THintWindow;
    FKeyDownShiftState: TShiftState;
    FListView: HWND;
    FMouseDownShiftState: TShiftState;
    FMouseDownPoint: TPoint;
    FMouseMoveCell: TGridCoord;
    FOnCanEditShow: TNotifyEvent;
    FOnCanEditShowExecuted: Boolean;
    FOnFilterChange: TFilterChange;
    FOnSelect: TNotifyEvent;
    IgnoreTitleClick: Boolean;
    IgnoreTitleChange: Boolean;
    TitleBoldFont: TFont;
    procedure ActivateHint();
    function CanvasTextWidth(const Text: string): Integer; inline;
    function EditCopyExecute(): Boolean;
    function EditCutExecute(): Boolean;
    function EditDeleteExecute(): Boolean;
    function GetCurrentRow(): Boolean;
    function GetHeader(): HWND;
    procedure HeaderMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure HeaderSectionClick(HeaderControl: THeaderControl; Section: THeaderSection);
    procedure HeaderSectionDrag(Sender: TObject; FromSection, ToSection: THeaderSection; var AllowDrag: Boolean);
    procedure HeaderSectionResize(HeaderControl: THeaderControl; Section: THeaderSection);
    procedure SetHeaderColumnArrows();
    procedure CMFontChanged(var Message); message CM_FONTCHANGED;
    procedure WMNotify(var Message: TWMNotify); message WM_NOTIFY;
    procedure WMTimer(var Message: TWMTimer); message WM_TIMER;
  protected
    property IgnoreKeyPress: Boolean read FIgnoreKeyPress;
    function CanEditShow(): Boolean; override;
    function CanGridAcceptKey(Key: Word; Shift: TShiftState): Boolean; override;
    procedure ColEnter(); override;
    function CreateEditor(): TInplaceEdit; override;
    procedure CreateWnd(); override;
    procedure DoEnter(); override;
    function DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;
      MousePos: TPoint): Boolean; override;
    procedure DrawCell(ACol, ARow: Longint; ARect: TRect; AState: TGridDrawState); override;
    function GetEditLimit(): Integer; override;
    function GetSelText(): string; virtual;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure Resize(); override;
    procedure SetColumnAttributes(); override;
    procedure TitleClick(Column: TColumn); override;
    procedure TopLeftChanged(); override;
  public
    procedure CopyToClipboard(); virtual;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy(); override;
    function ExecuteAction(Action: TBasicAction): Boolean; override;
    procedure LayoutChanged(); override;
    function PasteFromClipboard(): Boolean; virtual;
    procedure SelectAll(); virtual;
    function UpdateAction(Action: TBasicAction): Boolean; override;
    procedure UpdateHeader(); virtual;
    property CurrentRow: Boolean read GetCurrentRow;
    property Header: HWND read GetHeader;
    property KeyDownShiftState: TShiftState read FKeyDownShiftState;
    property MouseDownShiftState: TShiftState read FMouseDownShiftState;
    property SelText: string read GetSelText;
    property DefaultRowHeight;
    property GridLineWidth;
    property InplaceEditor;
    property LeftCol;
    property Row;
    property RowCount;
    property TopRow;
  published
    property OnCanEditShow: TNotifyEvent read FOnCanEditShow write FOnCanEditShow;
    property OnFilterChange: TFilterChange read FOnFilterChange write FOnFilterChange;
    property OnSelect: TNotifyEvent read FOnSelect write FOnSelect;
  end;

implementation {***************************************************************}

uses
  Forms, SysUtils, Clipbrd, Dialogs, Consts, CommCtrl,
  DBActns, StrUtils, Math, Variants,
  MySQLDB, CSVUtils;

{ TDBMySQLGrid.TDBMySQLInplaceEdit ********************************************}

procedure TMySQLDBGrid.TDBMySQLInplaceEdit.CloseUp(Accept: Boolean);
begin
  inherited;

  if (Accept and Modified) then
  begin
    TMySQLDBGrid(Grid).SelectedField.AsString := Text;
    TMySQLDBGrid(Grid).DataLink.Modified();
    TMySQLDBGrid(Grid).DataSource.DataSet.Edit();
  end;
end;

constructor TMySQLDBGrid.TDBMySQLInplaceEdit.Create(Owner: TComponent);
begin
  inherited;

  DoNotRemove := 0; // This avoids compiler warning only
end;

procedure TMySQLDBGrid.TDBMySQLInplaceEdit.DoEditButtonClick();
begin
  inherited;

  TMySQLDBGrid(Grid).EditButtonClick();
end;

procedure TMySQLDBGrid.TDBMySQLInplaceEdit.DropDown();
var
  Column: TColumn;
begin
  if (not ListVisible) then
  begin
    Column := TMySQLDBGrid(Grid).Columns[TMySQLDBGrid(Grid).SelectedIndex];
    if (ActiveList = PickList) then
    begin
      PickList.Items.Assign(Column.PickList);
      DropDownRows := Column.DropDownRows;
    end;
  end;

  inherited;
end;

procedure TMySQLDBGrid.TDBMySQLInplaceEdit.KeyPress(var Key: Char);
begin
  if (TMySQLDBGrid(Grid).IgnoreKeyPress) then
    Key := #0
  else
    inherited;
end;

{ TMySQLDBGrid ****************************************************************}

procedure TMySQLDBGrid.ActivateHint();
var
  I: Integer;
  LogFont: TLogFont;
  NonClientMetrics: TNonClientMetrics;
  OldActiveRecord: Integer;
  Rect: TRect;
  StringList: TStringList;
begin
  if ((0 <= FMouseMoveCell.X) and (FMouseMoveCell.X < FieldCount)
    and not (Columns[FMouseMoveCell.X].Field.DataType in BinaryDataTypes)
    and not EditorMode) then
  begin
    if (not Assigned(FHintWindow)) then
    begin
      FHintWindow := THintWindow.Create(Self);
      FHintWindow.Color := clInfoBk;
    end;

    OldActiveRecord := DataLink.ActiveRecord;
    DataLink.ActiveRecord := FMouseMoveCell.Y - 1;

    StringList := TStringList.Create();
    if (FMouseMoveCell.Y = 0) then
      StringList.Text := Columns[FMouseMoveCell.X].Field.DisplayName
    else if (Columns[FMouseMoveCell.X].Field.IsNull) then
      StringList.Text := ''
    else if (Columns[FMouseMoveCell.X].Field.DataType = ftWideMemo) then
      StringList.Text := Columns[FMouseMoveCell.X].Field.AsString
    else
      StringList.Text := Columns[FMouseMoveCell.X].Field.DisplayText;

    if (Length(StringList.Text) < 10 * 1024) then
    begin
      if (SystemParametersInfo(SPI_GETNONCLIENTMETRICS, SizeOf(NonClientMetrics), @NonClientMetrics, 0)
        and (GetObject(Columns[FMouseMoveCell.X].Font.Handle, SizeOf(LogFont), @LogFont) <> 0)) then
      begin
        LogFont.lfQuality  := NonClientMetrics.lfMessageFont.lfQuality;
        FHintWindow.Canvas.Font.Handle := CreateFontIndirect(LogFont);
      end
      else
        FHintWindow.Canvas.Font := Columns[FMouseMoveCell.X].Font;

      Rect.Left := 0;
      for I := LeftCol to FMouseMoveCell.X - 1 do
        if (Columns[I].Visible) then
          if (dgRowLines in Options) then
            Inc(Rect.Left, Columns[I].Width + GridLineWidth)
          else
            Inc(Rect.Left, Columns[I].Width);
      Rect.Top := 0;
      for I := 0 to FMouseMoveCell.Y - 1 do
        if ((I > 0) and (dgRowLines in Options)) then
          Inc(Rect.Top, RowHeights[I] + GridLineWidth)
        else
          Inc(Rect.Top, RowHeights[I]);

      Rect.Left := ClientToScreen(Point(Rect.Left, Rect.Top)).X - 1;
      Rect.Top := ClientToScreen(Point(Rect.Left, Rect.Top)).Y;

      Rect.Right := 0;
      for I := 0 to StringList.Count - 1 do
        Rect.Right := Max(Rect.Right, Rect.Left + FHintWindow.Canvas.TextWidth(StringList[I]) + 6);
      Rect.Bottom := Rect.Top + FHintWindow.Canvas.TextHeight('H') * StringList.Count + 2;

      if ((Rect.Right - Rect.Left - 2 > Columns[FMouseMoveCell.X].Width)
        or (Columns[FMouseMoveCell.X].Field.DataType = ftWideMemo)
        or (StringList.Count > 1)) then
      begin
        FHintWindow.ActivateHint(Rect, StringList.Text);
        SetTimer(Handle, tiHideHint, Application.HintHidePause, nil);
      end
      else
        FreeAndNil(FHintWindow);

      StringList.Free();
      DataLink.ActiveRecord := OldActiveRecord;
    end;
  end;
end;

function TMySQLDBGrid.CanEditShow(): Boolean;
begin
  if (Assigned(FHintWindow)) then
    FreeAndNil(FHintWindow);

  Result := inherited CanEditShow();

  if (Result and not FOnCanEditShowExecuted and Assigned(FOnCanEditShow)) then
  begin
    FOnCanEditShowExecuted := True;
    FOnCanEditShow(Self);
  end;
end;

function TMySQLDBGrid.CanGridAcceptKey(Key: Word; Shift: TShiftState): Boolean;
begin
  Result := not ((Key in [VK_INSERT]))
    and not ((SelectedField = Columns[0].Field) and (Key in [VK_TAB]) and (ssShift in Shift))
    and not ((SelectedField = Columns[Columns.Count - 1].Field) and (Key in [VK_TAB]) and not (ssShift in Shift));
end;

function TMySQLDBGrid.CanvasTextWidth(const Text: string): Integer;
begin
  Result := Canvas.TextWidth(Text);
end;

procedure TMySQLDBGrid.CMFontChanged(var Message);
begin
  inherited;

  if (FListView > 0) then
    SendMessage(FListView, WM_SETFONT, WPARAM(Font.Handle), LPARAM(TRUE));
  if (Assigned(FHeaderControl)) then
    FHeaderControl.Font := TitleFont;
  Resize();
end;

procedure TMySQLDBGrid.CopyToClipboard();
var
  ClipboardData: HGLOBAL;
  Content: string;
  FirstContent: Boolean;
  FormatSettings: TFormatSettings;
  I: Integer;
  J: Integer;
  Len: Cardinal;
  OldRecNo: Integer;
begin
  FormatSettings := TFormatSettings.Create(LOCALE_USER_DEFAULT);

  if (Assigned(InplaceEditor) and InplaceEditor.Visible) then
    InplaceEditor.CopyToClipboard()
  else if (OpenClipboard(Handle)) then
  begin
    EmptyClipboard();

    if (not Assigned(SelectedField) or (SelectedRows.Count = 0)) then
    begin
      Content := SelectedField.AsString;

      Len := Length(Content);
      ClipboardData := GlobalAlloc(GMEM_MOVEABLE + GMEM_DDESHARE, (Len + 1) * SizeOf(Content[1]));
      Move(PChar(Content)^, GlobalLock(ClipboardData)^, (Len + 1) * SizeOf(Content[1]));
      SetClipboardData(CF_UNICODETEXT, ClipboardData);
      GlobalUnlock(ClipboardData);
    end
    else if (DataLink.DataSet is TMySQLDataSet) then
    begin
      Content := SelText;

      ClipboardData := GlobalAlloc(GMEM_MOVEABLE + GMEM_DDESHARE, (Length(Content) + 1) * SizeOf(Char));
      Move(PChar(Content)^, GlobalLock(ClipboardData)^, (Length(Content) + 1) * SizeOf(Char));
      SetClipboardData(CF_UNICODETEXT, ClipboardData);
      GlobalUnlock(ClipboardData);


      DataLink.DataSet.DisableControls();
      OldRecNo := DataLink.DataSet.RecNo;

      Content := SelText;

      Len := WideCharToAnsiChar(CP_ACP, PChar(Content), Length(Content), nil, 0);
      ClipboardData := GlobalAlloc(GMEM_MOVEABLE + GMEM_DDESHARE, (Len + 1));
      WideCharToAnsiChar(CP_ACP, PChar(Content), Length(Content), GlobalLock(ClipboardData), Len);
      PAnsiChar(GlobalLock(ClipboardData))[Len] := #0;
      SetClipboardData(CF_DSPTEXT, ClipboardData);
      GlobalUnlock(ClipboardData);


      Content := '';
      for I := 0 to SelectedRows.Count - 1 do
      begin
        DataLink.DataSet.Bookmark := SelectedRows.Items[I];
        FirstContent := True;
        for J := 0 to Columns.Count - 1 do
          if (Columns[J].Visible) then
          begin
            if (FirstContent) then FirstContent := False else Content := Content + #9;
            Content := Content + CSVEscape(Columns[J].Field.AsString);
          end;
        Content := Content + #13#10;
      end;
      Len := Length(Content);

      ClipboardData := GlobalAlloc(GMEM_MOVEABLE + GMEM_DDESHARE, Len * SizeOf(Char));
      SetClipboardData(CF_MYSQLRECORD, ClipboardData);
      Move(PChar(Content)^, GlobalLock(ClipboardData)^, Len * SizeOf(Char));
      GlobalUnlock(ClipboardData);

      DataLink.DataSet.RecNo := OldRecNo;
      DataLink.DataSet.EnableControls();
    end;

    CloseClipboard();
  end;
end;

procedure TMySQLDBGrid.ColEnter();
begin
  if (Assigned(InplaceEditor)) then
  begin
    if (InplaceEditor is TDBMySQLInplaceEdit) then
      TDBMySQLInplaceEdit(InplaceEditor).Font := Columns[SelectedIndex].Font;
    if (Columns[SelectedIndex].Alignment <> taRightJustify) then
      SetWindowLong(InplaceEditor.Handle, GWL_STYLE, GetWindowLong(InplaceEditor.Handle, GWL_STYLE) and not ES_RIGHT)
    else
      SetWindowLong(InplaceEditor.Handle, GWL_STYLE, GetWindowLong(InplaceEditor.Handle, GWL_STYLE) or ES_RIGHT);
    if (Columns[SelectedIndex].Field.CanModify) then
      SetWindowLong(InplaceEditor.Handle, GWL_STYLE, GetWindowLong(InplaceEditor.Handle, GWL_STYLE) and not ES_READONLY)
    else
      SetWindowLong(InplaceEditor.Handle, GWL_STYLE, GetWindowLong(InplaceEditor.Handle, GWL_STYLE) or ES_READONLY);
  end;

  inherited;
end;

constructor TMySQLDBGrid.Create(AOwner: TComponent);
begin
  FIgnoreKeyPress := False;
  FHeaderControl := nil;
  FListView := 0;
  IgnoreTitleClick := False;
  IgnoreTitleChange := False;
  FOnCanEditShowExecuted := False;
  TitleBoldFont := nil;

  FMouseMoveCell.X := -1;
  FMouseMoveCell.Y := -1;

  inherited;
end;

function TMySQLDBGrid.CreateEditor(): TInplaceEdit;
begin
  Result := TDBMySQLInplaceEdit.Create(Self);

  if (Assigned(Result)) then
  begin
    Result.Parent := Self;
    TDBMySQLInplaceEdit(Result).Font := Columns[SelectedIndex].Font;
    if (Columns[SelectedIndex].Alignment <> taRightJustify) then
      SetWindowLong(Result.Handle, GWL_STYLE, GetWindowLong(Result.Handle, GWL_STYLE) and not ES_RIGHT)
    else
      SetWindowLong(Result.Handle, GWL_STYLE, GetWindowLong(Result.Handle, GWL_STYLE) or ES_RIGHT);
  end;
end;

procedure TMySQLDBGrid.CreateWnd();
var
  LVColumn: TLVColumn;
begin
  inherited;

  if (FListView > 0) then CloseHandle(FListView);
  FListView := CreateWindow(WC_LISTVIEW, nil, WS_CHILD, 0, 0, 50, 50, Handle, 0, hInstance, nil);
  SendMessage(FListView, WM_SETFONT, WPARAM(Font.Handle), LPARAM(TRUE));
  LVColumn.mask := LVCF_TEXT;
  LVColumn.pszText := 'Test';
  LVColumn.cchTextMax := StrLen(LVColumn.pszText);
  SendMessage(FListView, LVM_INSERTCOLUMN, 0, LPARAM(@LVColumn));

  if (Assigned(FHeaderControl)) then FHeaderControl.Free();
  FHeaderControl := THeaderControl.Create(Self);
  FHeaderControl.ControlStyle := FHeaderControl.ControlStyle + [csDoubleClicks];
  FHeaderControl.DoubleBuffered := True;
  FHeaderControl.NoSizing := not (dgColumnResize in Options);
  if (not Assigned(OnTitleClick) or not (dgTitleClick in Options)) then
    FHeaderControl.Style := hsFlat
  else
    FHeaderControl.Style := hsButtons;
  FHeaderControl.OnMouseMove := HeaderMouseMove;
  FHeaderControl.OnSectionClick := HeaderSectionClick;
  FHeaderControl.OnSectionDrag := HeaderSectionDrag;
  FHeaderControl.OnSectionResize := HeaderSectionResize;
  if (not Assigned(OnTitleClick) or not (dgTitleClick in Options)) then
    FHeaderControl.Style := hsFlat
  else
    FHeaderControl.Style := hsButtons;
  FHeaderControl.Parent := Self;

  SetColumnAttributes();
  Resize();
end;

destructor TMySQLDBGrid.Destroy();
begin
  if (Assigned(FHintWindow)) then
    FHintWindow.Free();

  if (Assigned(TitleBoldFont)) then
    TitleBoldFont.Free();

  if (Assigned(FHeaderControl)) then
  begin
    FHeaderControl.Parent := nil;
    FHeaderControl.Free();
  end;

  inherited;
end;

procedure TMySQLDBGrid.DoEnter();
begin
  inherited;

  if (Assigned(InplaceEditor)) then
    if (Columns[SelectedIndex].Alignment <> taRightJustify) then
      SetWindowLong(InplaceEditor.Handle, GWL_STYLE, GetWindowLong(InplaceEditor.Handle, GWL_STYLE) and not ES_RIGHT)
    else
      SetWindowLong(InplaceEditor.Handle, GWL_STYLE, GetWindowLong(InplaceEditor.Handle, GWL_STYLE) or ES_RIGHT);
end;

function TMySQLDBGrid.DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;
  MousePos: TPoint): Boolean;
begin
  Result :=  Assigned(DataSource) and Assigned(DataLink.DataSet) and DataLink.DataSet.Active and (not (ssShift in FMouseDownShiftState) and not (ssCtrl in FMouseDownShiftState));

  if (Result) then
    DataLink.DataSet.MoveBy(- WheelDelta div WHEEL_DELTA);
end;

procedure TMySQLDBGrid.DrawCell(ACol, ARow: Longint; ARect: TRect; AState: TGridDrawState);
begin
  if ((ARow > 0) or not Assigned(FHeaderControl)) then // The header row has been replaced with the FHeaderControl
    inherited;
end;

function TMySQLDBGrid.EditCopyExecute(): Boolean;
begin
  Result := True;

  if (EditorMode and Assigned(InplaceEditor)) then
    InplaceEditor.CopyToClipboard()
  else
    CopyToClipboard();
end;

function TMySQLDBGrid.EditCutExecute(): Boolean;
begin
  Result := CanEditModify();

  if (Result) then
  begin
    if (EditorMode and Assigned(InplaceEditor)) then
      InplaceEditor.CopyToClipboard()
    else
      CopyToClipboard();

    Result := EditDeleteExecute();
  end;
end;

function TMySQLDBGrid.EditDeleteExecute(): Boolean;
begin
  Result := Assigned(DataLink.DataSet);

  if (Result) then
  begin
    DataLink.DataSet.Edit();
    if (EditorMode and Assigned(InplaceEditor)) then
      InplaceEditor.SelText := ''
    else
      SelectedField.Clear();
  end;
end;

function TMySQLDBGrid.ExecuteAction(Action: TBasicAction): Boolean;
begin
  if (Action is TEditCut) then
    Result := EditCutExecute()
  else if (Action is TEditCopy) then
    Result := EditCopyExecute()
  else if (Action is TEditPaste) then
    Result := PasteFromClipboard()
  else if (Action is TEditDelete) then
    Result := EditDeleteExecute()
  else if (Action is TEditSelectAll) then
    begin Result := True; SelectAll(); end
  else
    Result := inherited ExecuteAction(Action);
end;

function TMySQLDBGrid.GetCurrentRow(): Boolean;
begin
  Result := Row - 1 = DataLink.ActiveRecord;
end;

function TMySQLDBGrid.GetEditLimit(): Integer;
begin
  if (not Assigned(SelectedField)) then
    Result := 0
  else
    case (SelectedField.DataType) of
      ftString: Result := SelectedField.DataSize;
      ftVarBytes:  Result := SelectedField.Size - 1;
      ftSmallInt,
      ftInteger:
        { Workaround, because TIntegerField.MinValue is sometimes not set }
        if ((TIntegerField(SelectedField).MinValue = 0) and (TIntegerField(SelectedField).MaxValue = $7FFFFFFF)) then
          Result := Length('-2147483648')
        else if ((TIntegerField(SelectedField).MinValue = 0) and (TIntegerField(SelectedField).MaxValue = 0)) then
          Result := 0
        else
          Result := Max(Length(IntToStr(TIntegerField(SelectedField).MinValue)), Length(IntToStr(TIntegerField(SelectedField).MaxValue)));
      ftWord: Result :=  max(Length(IntToStr(TWordField(SelectedField).MinValue)), Length(IntToStr(TWordField(SelectedField).MaxValue)));
      ftCurrency,
      ftFloat:
        if ((TFloatField(SelectedField).MinValue = 0) and (TFloatField(SelectedField).MaxValue = 0)) then
          Result := 0
        else
          Result := max(Length(FloatToStr(TFloatField(SelectedField).MinValue, TMySQLDataSet(DataLink.DataSet).Connection.FormatSettings)), Length(FloatToStr(TFloatField(SelectedField).MaxValue, TMySQLDataSet(DataLink.DataSet).Connection.FormatSettings)));
      ftDate: Result := Length(MySQLDB.DateToStr(Now(), TMySQLDataSet(DataLink.DataSet).Connection.FormatSettings));
      ftTime: Result := Length(TimeToStr(Now(), TMySQLDataSet(DataLink.DataSet).Connection.FormatSettings));
      ftDateTime: Result := Length(MySQLDB.DateTimeToStr(Now(), TMySQLDataSet(DataLink.DataSet).Connection.FormatSettings));
      ftBlob,
      ftWideMemo: Result := 0;
      ftLargeint:
        if ((TLargeintField(SelectedField).MinValue = 0) and (TLargeintField(SelectedField).MaxValue = 0)) then
          Result := 0
        else
          Result := Max(Length(IntToStr(TLargeintField(SelectedField).MinValue)), Length(IntToStr(TLargeintField(SelectedField).MaxValue)));
      ftTimeStamp: Result := SelectedField.Size;
      else Result := inherited GetEditLimit();
    end;
end;

function TMySQLDBGrid.GetHeader(): HWND;
begin
  if (not Assigned(FHeaderControl)) then
    HandleNeeded();

  if (not Assigned(FHeaderControl)) then
    Result := 0
  else
    Result := FHeaderControl.Handle;
end;

function TMySQLDBGrid.GetSelText(): string;
var
  FirstContent: Boolean;
  FormatSettings: TFormatSettings;
  I: Integer;
  J: Integer;
  OldRecNo: Integer;
begin
  FormatSettings := TFormatSettings.Create(LOCALE_USER_DEFAULT);

  if (SelectedRows.Count = 0) then
    Result := SelectedField.AsString
  else
  begin
    DataLink.DataSet.DisableControls();
    OldRecNo := DataLink.DataSet.RecNo;

    for I := 0 to SelectedRows.Count - 1 do
    begin
      FirstContent := True;
      DataLink.DataSet.Bookmark := SelectedRows.Items[I];
      for J := 0 to Columns.Count - 1 do
        if (Columns[J].Visible) then
        begin
          if (FirstContent) then FirstContent := False else Result := Result + #9;
          Result := Result + Columns[J].Field.AsString;
        end;
      Result := Result + #13#10;
    end;

    DataLink.DataSet.RecNo := OldRecNo;
    DataLink.DataSet.EnableControls();
  end;
end;

procedure TMySQLDBGrid.HeaderMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  HDItem: THDItem;
  Index: Integer;
  NeededWidth: Integer;
begin
  FHeaderControl.Hint := '';
  for Index := 0 to FHeaderControl.Sections.Count - 1 do
    if ((FHeaderControl.Sections[Index].Left <= X) and (X <= FHeaderControl.Sections[Index].Right)) then
    begin
      if (Columns[LeftCol + Index].Field.IsIndexField and Assigned(TitleBoldFont)) then
        Canvas.Font := TitleBoldFont
      else if (Assigned(TitleFont)) then
        Canvas.Font := TitleFont;
      NeededWidth := Canvas.TextWidth(Columns[LeftCol + Index].DisplayName) + FHeaderControl.Height;
      Canvas.Font := Font;

      HDItem.Mask := HDI_FORMAT;
      if (BOOL(SendMessage(FHeaderControl.Handle, HDM_GETITEM, Index, LParam(@HDItem))) and (HDItem.fmt and (HDF_SORTUP or HDF_SORTUP) <> 0)) then
        Inc(NeededWidth, 2 * FHeaderControl.Height);

      if (FHeaderControl.Sections[Index].Width < NeededWidth) then
        FHeaderControl.Hint := Columns[LeftCol + Index].DisplayName;
    end;
end;

procedure TMySQLDBGrid.HeaderSectionClick(HeaderControl: THeaderControl; Section: THeaderSection);
begin
  TitleClick(Columns[Section.Index + LeftCol]);
end;

procedure TMySQLDBGrid.HeaderSectionDrag(Sender: TObject; FromSection, ToSection: THeaderSection; var AllowDrag: Boolean);
begin
  Columns[FromSection.Index + LeftCol].Index := ToSection.Index;
end;

procedure TMySQLDBGrid.HeaderSectionResize(HeaderControl: THeaderControl; Section: THeaderSection);
begin
  if (EditorMode) then Perform(CM_Exit, 0, 0);
  if (dgColLines in Options) then
    Columns[Section.Index + LeftCol].Width := Section.Width - GridLineWidth
  else
    Columns[Section.Index + LeftCol].Width := Section.Width;
end;

procedure TMySQLDBGrid.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if (Assigned(FHintWindow)) then
    FreeAndNil(FHintWindow);

  FMouseDownShiftState := Shift + (FMouseDownShiftState - [ssShift, ssCtrl, ssAlt]);

  if ((Key = VK_UP) and (Shift = [ssCtrl]) and Assigned(DataLink.DataSet) and (DataLink.DataSet.State = dsBrowse)) then
  begin
    if (SelectedRows.Count = 0) then
      SelectedRows.CurrentRowSelected := True;
    if DataLink.DataSet.RecNo > 0 then
      DataLink.DataSet.MoveBy(-1);
    FIgnoreKeyPress := True;
  end
  else if ((Key = VK_DOWN) and (Shift = [ssCtrl]) and Assigned(DataLink.DataSet) and (DataLink.DataSet.State = dsBrowse)) then
  begin
    if (SelectedRows.Count = 0) then
      SelectedRows.CurrentRowSelected := True;
    if DataLink.DataSet.RecNo < DataLink.DataSet.RecordCount - 1 then
      DataLink.DataSet.MoveBy(+1);
    FIgnoreKeyPress := True;
  end
  else if ((Key = VK_TAB) and (Shift = []) and (SelectedIndex = Columns.Count - 1) and DataSource.DataSet.FindNext() and not TMySQLDataSet(DataSource.DataSet).CachedUpdates) then
    SelectedIndex := 0
  else if ((Key = VK_TAB) and (ssShift in Shift) and (SelectedIndex = 0) and DataSource.DataSet.FindPrior() and not TMySQLDataSet(DataSource.DataSet).CachedUpdates) then
    SelectedIndex := Columns.Count - 1
  else if (((Key = VK_HOME) or (Key = VK_END) or (Key = VK_LEFT) or (Key = VK_RIGHT)) and (Shift = [ssShift]) and not EditorMode) then
    SelectedRows.CurrentRowSelected := not SelectedRows.CurrentRowSelected
  else if ((Key = VK_SPACE) and (Shift = [ssCtrl]) and Assigned(DataLink.DataSet) and (DataLink.DataSet.State = dsBrowse)) then
  begin
    SelectedRows.CurrentRowSelected := not SelectedRows.CurrentRowSelected;
    FIgnoreKeyPress := True;
    Options := Options - [dgEditing];
  end
  else if ((Key = VK_ESCAPE) and (Shift = []) and EditorMode) then
  begin
    Datalink.Reset();
    if not (dgAlwaysShowEditor in Options) then
      HideEditor();
  end
  else if ((Key = Ord('A')) and (Shift = [ssCtrl])) then
    SelectAll()
  else if (((Key = Ord('X')) and (Shift = [ssCtrl]) or (Key = VK_DELETE) and (Shift = [ssShift])) and (SelectedRows.Count = 0)) then
    EditCutExecute()
  else if ((Key = Ord('C')) and (Shift = [ssCtrl]) or (Key = VK_INSERT) and (Shift = [ssCtrl])) then
    EditCopyExecute()
  else if ((Key = Ord('V')) and (Shift = [ssCtrl]) or (Key = VK_INSERT) and (Shift = [ssShift])) then
    PasteFromClipboard()
  else if ((Key = VK_DELETE) and (Shift = [])) then
    EditDeleteExecute()
  else if ((Key = VK_DOWN) and (Shift = [ssAlt]) and (Columns[SelectedIndex].ButtonStyle = cbsEllipsis)) then
    EditButtonClick()
  else
    inherited;
end;

procedure TMySQLDBGrid.KeyPress(var Key: Char);
begin                                                                                
  if (IgnoreKeyPress) then
  begin
    Key := #0;
    FIgnoreKeyPress := False;
    Options := Options + [dgEditing];
  end;

  inherited;
end;

procedure TMySQLDBGrid.KeyUp(var Key: Word; Shift: TShiftState);
begin
  FMouseDownShiftState := Shift + (FMouseDownShiftState - [ssShift, ssCtrl, ssAlt]);

  inherited;
end;

procedure TMySQLDBGrid.LayoutChanged();
begin
  inherited;

  FOnCanEditShowExecuted := False;
end;

procedure TMySQLDBGrid.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  Cell: TGridCoord;
  Coord: TGridCoord;
  I: Integer;
  NewBookmark: TBookmark;
  NewRecord: Integer;
  OldBookmark: TBookmark;
  OldRecord: Integer;
  OldSelectedRows: Integer;
begin
  if (Assigned(FHintWindow)) then
    FreeAndNil(FHintWindow);

  if (Assigned(DataSource) and Assigned(DataLink.DataSet) and DataLink.DataSet.Active) then
  begin
    FMouseDownPoint.X := X; FMouseDownPoint.Y := Y;
    FMouseDownShiftState := Shift + (FMouseDownShiftState - [ssLeft, ssRight, ssMiddle, ssDouble]);

    Coord := MouseCoord(X, Y);
    if ((Coord.X = -1) and (Coord.Y = -1) and EditorMode) then
      try
        SelectedField.AsString := TDBMySQLInplaceEdit(InplaceEditor).Text;
      except
      end;

    if (Y <= RowHeights[0]) then
      OldRecord := -1
    else
      OldRecord := DataLink.ActiveRecord;
    OldBookmark := DataLink.DataSet.Bookmark;
    OldSelectedRows := SelectedRows.Count;

    inherited;

    if (Y <= RowHeights[0]) then
      NewRecord := -1
    else
      NewRecord := DataLink.ActiveRecord;

    Cell := MouseCoord(X, Y);
    if (((Cell.X > 0) or not (dgIndicator in Options)) and (Cell.Y > 0) and (Button = mbLeft)) then
    begin
      if (ssShift in Shift) then
      begin
        DataLink.DataSet.DisableControls();
        SelectedRows.CurrentRowSelected := True;
        if (NewRecord < OldRecord) then
          for I := OldRecord downto NewRecord do
          begin
            DataLink.ActiveRecord := I;
            SelectedRows.CurrentRowSelected := True;
          end;
        if (NewRecord > OldRecord) then
          for I := OldRecord to NewRecord do
          begin
            DataLink.ActiveRecord := I;
            SelectedRows.CurrentRowSelected := True;
          end;
        DataLink.DataSet.EnableControls();
      end
      else if (ssCtrl in Shift) then
      begin
        if (OldSelectedRows = 0) then
        begin
          DataLink.DataSet.DisableControls();
          NewBookmark := DataLink.DataSet.Bookmark;
          DataLink.DataSet.Bookmark := OldBookmark;
          SelectedRows.CurrentRowSelected := True;
          DataLink.DataSet.Bookmark := NewBookmark;
          DataLink.DataSet.EnableControls();
        end;
      end
      else
        SelectedRows.Clear();
    end;
  end;
end;

procedure TMySQLDBGrid.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  Cell: TGridCoord;
begin
  inherited;

  Cell := MouseCoord(X, Y);
  if (not ShowHint and not ParentShowHint or (Hint = '')) then
    if (((FMouseMoveCell.X >= 0) or (FMouseMoveCell.Y >= 0)) and ((Cell.X <> FMouseMoveCell.X) or (Cell.Y <> FMouseMoveCell.Y))) then
    begin
      FMouseMoveCell.X := -1; FMouseMoveCell.Y := -1;
      ReleaseCapture();
      if (Assigned(FHintWindow)) then
        FreeAndNil(FHintWindow);
    end
    else if ((Cell.X >= 0) and (Cell.Y >= 0) and ((Cell.X <> FMouseMoveCell.X) or (Cell.Y <> FMouseMoveCell.Y))) then
    begin
      FMouseMoveCell := Cell;
      SetCapture(Handle);
      SetTimer(Handle, tiShowHint, Application.HintPause, nil);
    end;
end;

procedure TMySQLDBGrid.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited;

  FMouseDownShiftState := Shift + (FMouseDownShiftState - [ssShift, ssCtrl, ssAlt]);
end;

function TMySQLDBGrid.PasteFromClipboard(): Boolean;
var
  Bookmarks: array of TBookmark;
  ClipboardData: HGLOBAL;
  Content: string;
  I: Integer;
  Index: Integer;
  RecNo: Integer;
  S: AnsiString;
  Start: Integer;
  Value: Integer;
  Values: TCSVValues;
begin
  Result := not ReadOnly;

  if (Result) then
    if (EditorMode and Assigned(InplaceEditor)) then
    begin
      InplaceEditor.PasteFromClipboard();
      Result := True;
    end
    else if ((DataLink.DataSet is TMySQLDataSet) and (Clipboard.HasFormat(CF_MYSQLRECORD) or Clipboard.HasFormat(CF_TEXT) or Clipboard.HasFormat(CF_UNICODETEXT)) and OpenClipboard(Handle)) then
    begin
      try
        if (Clipboard.HasFormat(CF_MYSQLRECORD)) then
        begin
          ClipboardData := GetClipboardData(CF_MYSQLRECORD);
          SetString(Content, PChar(GlobalLock(ClipboardData)), GlobalSize(ClipboardData) div SizeOf(Content[1]));
          GlobalUnlock(ClipboardData);
        end
        else if (Clipboard.HasFormat(CF_UNICODETEXT)) then
        begin
          ClipboardData := GetClipboardData(CF_UNICODETEXT);
          SetString(Content, PChar(GlobalLock(ClipboardData)), GlobalSize(ClipboardData) div SizeOf(Content[1]));
          GlobalUnlock(ClipboardData);
        end
        else
        begin
          ClipboardData := GetClipboardData(CF_TEXT);
          SetString(S, PAnsiChar(GlobalLock(ClipboardData)), GlobalSize(ClipboardData) div SizeOf(S[1]));
          SetLength(Content, AnsiCharToWideChar(CP_ACP, PAnsiChar(S), Length(S), nil, 0));
          if (Length(Content) > 0) then
            SetLength(Content, AnsiCharToWideChar(CP_ACP, PAnsiChar(S), Length(S), PChar(Content), Length(Content)));
          GlobalUnlock(ClipboardData);
        end;
      finally
        CloseClipboard();
      end;

      Index := 1;
      if (CSVSplitValues(Content, Index, #9, '"', Values) and ((Length(Values) > 1) or (Index <= Length(Content)))) then
      begin
        if (DataLink.DataSet.State <> dsInsert) then
          DataLink.DataSet.CheckBrowseMode();
        DataLink.DataSet.DisableControls();
        try
          if (SelectedRows.Count > 0) then
          begin
            SetLength(Bookmarks, SelectedRows.Count);
            for I := 0 to Length(Bookmarks) - 1 do
              Bookmarks[I] := SelectedRows.Items[I];
            SelectedRows.Clear();
            TMySQLDataSet(DataLink.DataSet).Delete(Bookmarks);
            SetLength(Bookmarks, 0);
          end;

          RecNo := 0;
          try
            repeat
              if (Length(Values) > 0) then
              begin
                if ((DataLink.DataSet.State = dsInsert) or (RecNo > 0) and (DataLink.DataSet.RecNo = DataLink.DataSet.RecordCount - 1)) then
                  DataLink.DataSet.Append()
                else
                begin
                  if (RecNo > 1) then DataLink.DataSet.Next();
                  DataLink.DataSet.Insert();
                end;

                Value := 0; Start := 0;
                for I := 0 to Columns.Count - 1 do
                  if (Columns[I].Field = SelectedField) then
                    Start := I;
                for I := Start to Start + Min(Length(Values), Columns.Count - Start) - 1 do
                begin
                  if (Values[Value].Length = 0) then
                    Columns[I].Field.Clear()
                  else if (Columns[I].Field.AutoGenerateValue <> arAutoInc) then
                    try
                      Columns[I].Field.AsString := CSVUnescape(Values[Value].Text, Values[Value].Length);
                    except
                      MessageBeep(MB_ICONERROR);
                      DataLink.DataSet.Fields[I].Clear();
                    end;
                  Inc(Value);
                end;

                if ((RecNo > 0) or (Index <= Length(Content))) then
                  try
                    DataLink.DataSet.Post();
                  except
                    on Error: EDatabaseError do
                      begin
                        DataLink.DataSet.Cancel();
                        DataLink.DataSet.Prior();
                      end;
                  end;
                Inc(RecNo);
              end;
            until (not CSVSplitValues(Content, Index, #9, '"', Values) or (Length(Values) = 0));
          finally
            SetLength(Values, 0);
          end;
        finally
          DataLink.DataSet.EnableControls();
        end;
      end
      else if (not SelectedField.ReadOnly) then
      begin
        ShowEditor();
        if (EditorMode and Assigned(InplaceEditor)) then
          InplaceEditor.PasteFromClipboard()
        else
        begin
          DataLink.DataSet.Edit();
          SelectedField.AsString := Content;
        end;
      end;
    end
    else
      Result := False;
end;

procedure TMySQLDBGrid.Resize();
var
  HDLayout: THDLayout;
  HDRect: TRect;
  HDWindowPos: TWindowPos;
  I: Integer;
begin
  inherited;

  if (Assigned(DataLink) and Assigned(DataSource) and Assigned(DataLink.DataSet)) then
    DataLink.BufferCount := RowCount - 1;

  if (Assigned(FHeaderControl)) then
  begin
    HDLayout.Rect := @HDRect;
    HDLayout.WindowPos := @HDWindowPos;
    if ((FListView > 0) and Header_Layout(ListView_GetHeader(FListView), @HDLayout)) then
      FHeaderControl.Height := HDWindowPos.cy;

    if (EditorMode) then Perform(CM_Exit, 0, 0);
    if (dgRowLines in Options) then
      RowHeights[0] := FHeaderControl.Height - GridLineWidth
    else
      RowHeights[0] := FHeaderControl.Height;

    for I := 0 to FHeaderControl.Sections.Count - 1 do
      FHeaderControl.Sections[I].MaxWidth := Width - FHeaderControl.Height - GetSystemMetrics(SM_CXVSCROLL);
  end;
end;

procedure TMySQLDBGrid.SelectAll();
var
  OldActive: Boolean;
  OldCursor: TCursor;
  OldRecNo: Integer;
  OldVisible: Boolean;
begin
  DataLink.DataSet.CheckBrowseMode();

  OldCursor := Screen.Cursor;
  Screen.Cursor := crHourGlass;
  OldVisible := Visible;
  OldActive := Focused();
  Visible := False;
  OldRecNo := DataLink.DataSet.RecNo;

  DataLink.DataSet.DisableControls();

  if ((DataLink.DataSet is TMySQLTable) and TMySQLTable(DataLink.DataSet).LimitedDataReceived) then
  begin
    TMySQLTable(DataLink.DataSet).Limit := 0;
    TMySQLTable(DataLink.DataSet).Refresh();
  end;

  SelectedRows.Clear();
  if (DataLink.DataSet.FindFirst()) then
    repeat
      SelectedRows.CurrentRowSelected := True;
    until (not DataLink.DataSet.FindNext());

  DataLink.DataSet.RecNo := OldRecNo;
  DataLink.DataSet.EnableControls();

  Screen.Cursor := OldCursor;
  Visible := OldVisible;
  if (Visible and OldActive) then
    SetFocus();
end;

procedure TMySQLDBGrid.SetColumnAttributes();
var
  I: Integer;
  Section: THeaderSection;
begin
  inherited;

  if (not IgnoreTitleChange) then
  begin
    if (Assigned(FHeaderControl)) then
    begin
      FHeaderControl.Sections.BeginUpdate();
      FHeaderControl.Sections.Clear();

      for I := 0 to Columns.Count - 1 do
      begin
        with Columns[I] do
          TabStops[I + IndicatorOffset] := Showing and DataLink.Active and
            Assigned(Field) and not (Field.FieldKind = fkCalculated);

        if (I >= LeftCol) then
        begin
          Section := FHeaderControl.Sections.Insert(I - LeftCol);
          Section.MinWidth := FHeaderControl.Height;
          Section.MaxWidth := Width - FHeaderControl.Height - GetSystemMetrics(SM_CXVSCROLL);
          if (dgColLines in Options) then
            Section.Width := Columns[I].Width + GridLineWidth
          else
            Section.Width := Columns[I].Width;
          if (Assigned(Columns[I].Field)) then
            Section.Text := Columns[I].Field.DisplayName;
        end;

        if (Assigned(Columns[I].Field) and Columns[I].Field.IsIndexField) then
          Columns[I].Font.Style := Columns[I].Font.Style + [fsBold]
        else
          Columns[I].Font.Style := Columns[I].Font.Style - [fsBold];
      end;

      FHeaderControl.Sections.EndUpdate();
    end;

    SetHeaderColumnArrows();
    Resize();
  end;
end;

procedure TMySQLDBGrid.SetHeaderColumnArrows();
var
  HDItem: THDItem;
  I: Integer;
  Index: Integer;
begin
  Index := 0;
  HDItem.Mask := HDI_FORMAT;
  if (DataLink.DataSet is TMySQLDataSet) then
    for I := LeftCol to Columns.Count - 1 do
      if (Columns[I].Visible and BOOL(SendMessage(Header, HDM_GETITEM, Index, LParam(@HDItem)))) then
      begin
        if (Columns[I].Field.Tag and ftAscSortedField <> 0) then
          HDItem.fmt := HDItem.fmt and not HDF_SORTUP or HDF_SORTUP
        else if (Columns[I].Field.Tag and ftDescSortedField <> 0) then
          HDItem.fmt := HDItem.fmt and not HDF_SORTDOWN or HDF_SORTDOWN
        else
          HDItem.fmt := HDItem.fmt and not HDF_SORTUP and not HDF_SORTDOWN;
        SendMessage(Header, HDM_SETITEM, Index, LPARAM(@HDItem));
        Inc(Index);
      end;
end;

procedure TMySQLDBGrid.TitleClick(Column: TColumn);
begin
  if (not IgnoreTitleClick) then
  begin
    FMouseDownShiftState := [ssLeft];
    if (GetKeyState(VK_SHIFT) < 0) then FMouseDownShiftState := FMouseDownShiftState + [ssShift];
    if (GetKeyState(VK_CONTROL) < 0) then FMouseDownShiftState := FMouseDownShiftState + [ssCtrl];
    inherited;
  end;

  IgnoreTitleClick := False;
end;

procedure TMySQLDBGrid.TopLeftChanged();
begin
  inherited;

  SetColumnAttributes();
end;

function TMySQLDBGrid.UpdateAction(Action: TBasicAction): Boolean;
begin
  if (Action is TEditAction) then
  begin
    Result := Focused() and Assigned(DataLink.DataSet) and DataLink.DataSet.Active and Assigned(SelectedField);

    if (Result) then
      if (Action is TEditCut) then
        TEditCut(Action).Enabled := not SelectedField.IsNull and not SelectedField.Required and SelectedField.CanModify and (not EditorMode or Assigned(InplaceEditor) and (InplaceEditor.SelText <> ''))
      else if (Action is TEditCopy) then
        TEditCopy(Action).Enabled := EditorMode and Assigned(InplaceEditor) and (InplaceEditor.SelText <> '') or not EditorMode and (not SelectedRows.CurrentRowSelected and not SelectedField.IsNull or SelectedRows.CurrentRowSelected and (DataSource.DataSet is TMySQLDataSet) and (DataSource.DataSet.State <> dsInsert))
      else if (Action is TEditPaste) then
        TEditPaste(Action).Enabled := not ReadOnly and SelectedField.CanModify and (EditorMode and Clipboard.HasFormat(CF_UNICODETEXT) or not EditorMode and Clipboard.HasFormat(CF_UNICODETEXT))
      else if (Action is TEditDelete) then
        TEditDelete(Action).Enabled := (SelectedRows.Count = 0) and not SelectedField.IsNull and not SelectedField.Required and SelectedField.CanModify and (not EditorMode or Assigned(InplaceEditor) and (InplaceEditor.SelText <> ''))
      else if (Action is TEditSelectAll) then
        TEditSelectAll(Action).Enabled := (DataLink.DataSet.RecordCount > 0)
      else
        Result := False;
  end
  else
    Result := inherited UpdateAction(Action);
end;

procedure TMySQLDBGrid.UpdateHeader();
begin
  SetColumnAttributes();
end;

procedure TMySQLDBGrid.WMNotify(var Message: TWMNotify);
var
  Column: TColumn;
  HDItem: THDItem;
  HDNotify: PHDNotify;
  HDCustomDraw: PNMCustomDraw;
  LogFont: TLogFont;
  NewWidth: Integer;
begin
  HDNotify := PHDNotify(Message.NMHdr);
  if (not Assigned(FHeaderControl) or not Assigned(FHeaderControl.Parent) or (HDNotify^.Hdr.hwndFrom <> FHeaderControl.Handle)) then
    inherited
  else
    case (HDNotify^.Hdr.code) of
      HDN_DIVIDERDBLCLICK:
        begin
          Column := Columns[LeftCol + HDNotify^.Item];
          if ((DataLink.DataSet is TMySQLDataSet) and not (Column.Field is TBlobField)) then
          begin
            HDItem.Mask := HDI_WIDTH;
            if (BOOL(SendMessage(Header, HDM_GETITEM, HDNotify^.Item, LPARAM(@HDItem)))) then
            begin
              IgnoreTitleChange := True;

              Canvas.Font := Column.Font;
              NewWidth := TMySQLDataSet(DataLink.DataSet).GetMaxTextWidth(Column.Field, CanvasTextWidth) + 4 + GridLineWidth;
              if (NewWidth < FHeaderControl.Sections[LeftCol + HDNotify^.Item].MinWidth) then
                NewWidth := FHeaderControl.Sections[LeftCol + HDNotify^.Item].MinWidth;
              if (NewWidth > Width - RowHeights[0]) then
                NewWidth := Width - RowHeights[0];

              HDItem.cxy := NewWidth;
              if (dgColLines in Options) then
                Inc(HDItem.cxy, GridLineWidth);
              while (not BOOL(SendMessage(Header, HDM_SETITEM, HDNotify^.Item, LPARAM(@HDItem)))) do
                Inc(HDItem.cxy, GridLineWidth);

              IgnoreTitleChange := False;

              Resize();
            end;
          end;
        end;
      NM_CUSTOMDRAW:
        begin
          HDCustomDraw := PNMCustomDraw(HDNotify);
          case (HDCustomDraw^.dwDrawStage) of
            CDDS_PREPAINT:
              Message.Result := CDRF_NOTIFYITEMDRAW;
            CDDS_ITEMPREPAINT:
              if ((Columns.Count <= LeftCol + Integer(HDCustomDraw^.dwItemSpec)) or not Assigned(Columns[LeftCol + Integer(HDCustomDraw^.dwItemSpec)])) then
                inherited
              else if ((LeftCol + Integer(HDCustomDraw^.dwItemSpec) < Columns.Count)
                and Columns[LeftCol + Integer(HDCustomDraw^.dwItemSpec)].Field.IsIndexField
                and (Assigned(TitleBoldFont) or (GetObject(TitleFont.Handle, SizeOf(LogFont), @LogFont) <> 0))) then
              begin
                if (not Assigned(TitleBoldFont)) then
                begin
                  TitleBoldFont := TFont.Create();
                  TitleBoldFont.Handle := CreateFontIndirect(LogFont);
                  TitleBoldFont.Style := TitleBoldFont.Style + [fsBold];
                end;

                SelectObject(HDCustomDraw^.hdc, TitleBoldFont.Handle);
                Message.Result := CDRF_NEWFONT;
              end
              else
              begin
                SelectObject(HDCustomDraw^.hdc, TitleFont.Handle);
                Message.Result := CDRF_NEWFONT;
              end;
            else
              inherited;
          end;
        end;
      else
        inherited;
    end;
end;

procedure TMySQLDBGrid.WMTimer(var Message: TWMTimer);
begin
  inherited;

  case (Message.TimerID) of
    tiShowHint:
      begin
        KillTimer(Handle, Message.TimerID);
        if (Visible) then
          ActivateHint();
      end;
    tiHideHint:
      begin
        KillTimer(Handle, Message.TimerID);
        if (Assigned(FHintWindow)) then
          FreeAndNil(FHintWindow);
      end;
  end;
end;

end.

