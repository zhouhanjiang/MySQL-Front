object DInfo: TDInfo
  Left = 633
  Top = 357
  HelpContext = 1075
  BorderStyle = bsDialog
  Caption = 'DInfo'
  ClientHeight = 243
  ClientWidth = 377
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 106
  TextHeight = 14
  object FVersion: TLabel
    Left = 207
    Top = 69
    Width = 66
    Height = 18
    Caption = 'FVersion'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -15
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object FBuild: TLabel
    Left = 207
    Top = 93
    Width = 29
    Height = 13
    Caption = 'FBuild'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object FURI: TLabel
    Left = 207
    Top = 125
    Width = 25
    Height = 13
    Cursor = crHandPoint
    Caption = 'FURI'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlue
    Font.Height = -12
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsUnderline]
    ParentFont = False
    OnClick = FURIClick
  end
  object FName: TLabel
    Left = 207
    Top = 34
    Width = 63
    Height = 22
    Caption = 'FName'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -18
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object PImage: TPanel
    Left = 17
    Top = 17
    Width = 173
    Height = 173
    BevelOuter = bvLowered
    Color = clWhite
    ParentBackground = False
    TabOrder = 1
    object FImage: TImage
      Left = 17
      Top = 17
      Width = 138
      Height = 138
    end
  end
  object FBOk: TButton
    Left = 293
    Top = 209
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'FBOk'
    Default = True
    ModalResult = 1
    TabOrder = 0
  end
end
