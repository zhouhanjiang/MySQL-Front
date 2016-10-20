object DInfo: TDInfo
  Left = 633
  Top = 357
  HelpContext = 1075
  BorderStyle = bsDialog
  Caption = 'DInfo'
  ClientHeight = 226
  ClientWidth = 350
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object FVersion: TLabel
    Left = 192
    Top = 64
    Width = 55
    Height = 16
    Caption = 'FVersion'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object FBuild: TLabel
    Left = 192
    Top = 86
    Width = 29
    Height = 13
    Caption = 'FBuild'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object FURI: TLabel
    Left = 192
    Top = 116
    Width = 25
    Height = 13
    Cursor = crHandPoint
    Caption = 'FURI'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlue
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsUnderline]
    ParentFont = False
    OnClick = FURIClick
  end
  object FName: TLabel
    Left = 192
    Top = 32
    Width = 52
    Height = 18
    Caption = 'FName'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -15
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object PImage: TPanel
    Left = 16
    Top = 16
    Width = 160
    Height = 160
    BevelOuter = bvLowered
    Color = clWhite
    ParentBackground = False
    TabOrder = 1
    object FImage: TImage
      Left = 16
      Top = 16
      Width = 128
      Height = 128
    end
  end
  object FBOk: TButton
    Left = 272
    Top = 194
    Width = 70
    Height = 23
    Cancel = True
    Caption = 'FBOk'
    Default = True
    ModalResult = 1
    TabOrder = 0
  end
end
