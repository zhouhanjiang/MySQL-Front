object DLogin: TDLogin
  Left = 591
  Top = 306
  BorderStyle = bsDialog
  Caption = 'DLogin'
  ClientHeight = 137
  ClientWidth = 290
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
  object FBOk: TButton
    Left = 118
    Top = 104
    Width = 75
    Height = 25
    Caption = 'FBOk'
    Default = True
    ModalResult = 1
    TabOrder = 2
  end
  object FBCancel: TButton
    Left = 206
    Top = 104
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'FBCancel'
    ModalResult = 2
    TabOrder = 3
  end
  object GAccount: TGroupBox_Ext
    Left = 8
    Top = 8
    Width = 273
    Height = 79
    Caption = 'GAccount'
    TabOrder = 0
    object FLUsername: TLabel
      Left = 8
      Top = 20
      Width = 60
      Height = 13
      Caption = 'FLUsername'
    end
    object FLPassword: TLabel
      Left = 8
      Top = 52
      Width = 58
      Height = 13
      Caption = 'FLPassword'
    end
    object FUsername: TEdit
      Left = 88
      Top = 16
      Width = 177
      Height = 21
      MaxLength = 50
      TabOrder = 0
      Text = 'FUsername'
    end
    object FPassword: TEdit
      Left = 88
      Top = 48
      Width = 177
      Height = 21
      MaxLength = 50
      TabOrder = 1
      Text = 'FPassword'
    end
  end
  object FBSettings: TButton
    Left = 6
    Top = 104
    Width = 81
    Height = 25
    Caption = 'FBSettings'
    TabOrder = 1
    OnClick = FBSettingsClick
  end
end
