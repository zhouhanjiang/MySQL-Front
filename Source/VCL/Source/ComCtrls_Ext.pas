unit ComCtrls_Ext;

interface {********************************************************************}

uses
  Windows, SysUtils, Classes, Controls, ComCtrls, Messages, CommCtrl;

type
  TLVColumnResizeEvent = procedure(Sender: TObject; Column: TListColumn) of object;
  TListView_Ext = class(TListView)
  private
    FOnColumnResize: TLVColumnResizeEvent;
    procedure WMNotify(var Message: TWMNotify); message WM_NOTIFY;
  public
    constructor Create(AOwner: TComponent); override;
    function ExecuteAction(Action: TBasicAction): Boolean; override;
    function UpdateAction(Action: TBasicAction): Boolean; override;
  published
    property OnColumnResize: TLVColumnResizeEvent read FOnColumnResize write FOnColumnResize;
  end;

type
  TTreeView_Ext = class(TTreeView)
  private
    ShiftDownSelected: TTreeNode;
  protected
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
  end;

procedure Register();

implementation {***************************************************************}

uses
  StdActns, StdCtrls, Math, Graphics, SysConst;

procedure Register();
begin
  RegisterComponents('VCL Extensions', [TListView_Ext]);
  RegisterComponents('VCL Extensions', [TTreeView_Ext]);
end;

{ TListView_Ext ***************************************************************}

constructor TListView_Ext.Create(AOwner: TComponent);
begin
  inherited;

  FOnColumnResize := nil;
end;

function TListView_Ext.ExecuteAction(Action: TBasicAction): Boolean;
begin
  if (Action is TEditSelectAll) then
    begin SelectAll(); Result := True; end
  else
    Result := inherited ExecuteAction(Action);
end;

procedure TListView_Ext.WMNotify(var Message: TWMNotify);
begin
  inherited;

  with PHDNotify(Message.NMHdr)^ do
    case (Hdr.Code) of
      HDN_ITEMCHANGED:
        if (((PItem^.mask and HDI_WIDTH) <> 0) and Assigned(FOnColumnResize)) then
          FOnColumnResize(Self, Columns[PHDNotify(Message.NMHdr)^.Item]);
    end;
end;

function TListView_Ext.UpdateAction(Action: TBasicAction): Boolean;
begin
  if (Action is TEditSelectAll) then
  begin
    Result := Focused;
    if (Result) then
      TEditSelectAll(Action).Enabled := Items.Count > 0;
  end
  else
    Result := inherited UpdateAction(Action);
end;

{ TTreeView_Ext ***************************************************************}

procedure TTreeView_Ext.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if (ssShift in Shift) then
  begin
    ShiftDownSelected := Selected;

    // Debug 2016-11-26
    if (not (TObject(ShiftDownSelected) is TTreeNode)) then
      raise ERangeError.Create(SRangeError);
  end;

  inherited;
end;

procedure TTreeView_Ext.KeyUp(var Key: Word; Shift: TShiftState);
begin
  inherited;

  if (ssShift in Shift) then
    ShiftDownSelected := nil;
end;

procedure TTreeView_Ext.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  B: Boolean; // Debug 2016-11-23
  Child: TTreeNode;
  Found: Boolean; // Debug 2016-12-08
  I: Integer; // Debug 2016-12-08
  Node: TTreeNode;
  NodeIndex: Integer;
  OldSelectedIndex: Integer;
begin
  if (not Assigned(Parent)) then
    Node := nil
  else
    Node := GetNodeAt(X, Y);

  inherited;

  B := MultiSelect;
  B := B and Assigned(ShiftDownSelected);
  B := B and Assigned(Node);

  if (B) then
  begin
    Found := False;
    for I := 0 to Items.Count - 1 do
      if (Items[I] = ShiftDownSelected) then
        Found := True;
    if (not Found) then
      raise ERangeError.Create(SRangeError);
  end;

  // Debug 2016-11-25
  if (B) then
    if (Assigned(ShiftDownSelected)) then
      if (not (TObject(ShiftDownSelected) is TTreeNode)) then
        if (TObject(ShiftDownSelected) is TObject) then
          raise ERangeError.Create(SRangeError + ' - ' + TObject(ShiftDownSelected).ClassName)
        else
          raise ERangeError.Create(SRangeError);
  if (B) then
    if (not (TObject(Node) is TTreeNode)) then
      raise ERangeError.Create(SRangeError);

  B := B and Assigned(Node.Parent); // Debug 2016-11-25
  B := B and (ShiftDownSelected.Parent = Node.Parent);
  B := B and (Shift = [ssShift, ssLeft]);

  if (B) then
  begin
    OldSelectedIndex := ShiftDownSelected.Parent.IndexOf(ShiftDownSelected);
    NodeIndex := Node.Parent.IndexOf(Node);

    Child := Node.Parent.getFirstChild();
    repeat
      if ((Child.Parent.IndexOf(Child) in [Min(OldSelectedIndex, NodeIndex) .. Max(OldSelectedIndex, NodeIndex)]) and not Child.Selected) then
        Select(Child, [ssCtrl]);
      Child := Node.Parent.GetNextChild(Child);
    until (not Assigned(Child));
  end;
end;

end.
