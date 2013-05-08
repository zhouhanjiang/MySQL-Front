object DGoto: TDGoto
  Left = 364
  Top = 363
  BorderIcons = []
  BorderStyle = bsDialog
  Caption = 'DGoto'
  ClientHeight = 106
  ClientWidth = 282
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnHide = FormHide
  OnShow = FormShow
  PixelsPerInch = 106
  TextHeight = 13
  object FLField: TLabel
    Left = 8
    Top = 12
    Width = 34
    Height = 13
    Caption = 'FLField'
    FocusControl = FField
  end
  object FBOk: TButton
    Left = 112
    Top = 74
    Width = 75
    Height = 25
    Caption = 'FBOk'
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
  object FBCancel: TButton
    Left = 200
    Top = 74
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'FBCancel'
    ModalResult = 2
    TabOrder = 2
  end
  object FField: TComboBox
    Left = 8
    Top = 31
    Width = 267
    Height = 22
    Style = csOwnerDrawFixed
    TabOrder = 0
    OnChange = FFieldChange
    OnDrawItem = FFieldDrawItem
  end
end
