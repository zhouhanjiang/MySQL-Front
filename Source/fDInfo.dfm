object DInfo: TDInfo
  Left = 633
  Top = 357
  HelpContext = 1075
  BorderStyle = bsDialog
  Caption = 'DInfo'
  ClientHeight = 354
  ClientWidth = 498
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
  object FImage: TImage
    Left = 0
    Top = 0
    Width = 500
    Height = 300
    HelpContext = 1075
  end
  object FVersion: TLabel
    Left = 344
    Top = 80
    Width = 73
    Height = 20
    Alignment = taRightJustify
    Caption = 'FVersion'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    Transparent = True
  end
  object FURI: TLabel
    Left = 440
    Top = 112
    Width = 25
    Height = 13
    Cursor = crHandPoint
    Alignment = taRightJustify
    Caption = 'FURI'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlue
    Font.Height = -12
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsUnderline]
    ParentFont = False
    Transparent = True
    OnClick = FURIClick
  end
  object FBuild: TLabel
    Left = 437
    Top = 83
    Width = 29
    Height = 13
    Alignment = taRightJustify
    Caption = 'FBuild'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    Transparent = True
  end
  object FBOk: TButton
    Left = 416
    Top = 320
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'FBOk'
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
  object FLine: TPanel_Ext
    Left = 0
    Top = 300
    Width = 500
    Height = 2
    BevelOuter = bvLowered
    ParentBackground = False
    TabOrder = 0
  end
end
