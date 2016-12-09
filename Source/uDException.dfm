object DException: TDException
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'DException'
  ClientHeight = 223
  ClientWidth = 428
  Color = clWindow
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 106
  TextHeight = 14
  object FIcon: TImage
    Left = 16
    Top = 16
    Width = 32
    Height = 32
  end
  object FLHeader: TLabel
    Left = 64
    Top = 16
    Width = 139
    Height = 14
    AutoSize = False
    Caption = 'Internal Program Error'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object FLTitle: TLabel
    Left = 64
    Top = 48
    Width = 347
    Height = 41
    AutoSize = False
    Caption = 
      'The application has encountered a problem. Please help to improv' +
      'e this application by sending a prepared error report to the dev' +
      'eloper.'
    WordWrap = True
  end
  object FLDescription: TLabel
    Left = 64
    Top = 104
    Width = 94
    Height = 14
    Caption = 'Error Description:'
  end
  object FLMessage: TLabel
    Left = 64
    Top = 128
    Width = 58
    Height = 14
    AutoSize = False
    Caption = 'FLMessage'
    WordWrap = True
  end
  object Panel: TPanel
    Left = 0
    Top = 166
    Width = 428
    Height = 57
    Align = alBottom
    BevelOuter = bvNone
    ParentBackground = False
    TabOrder = 0
    DesignSize = (
      428
      57)
    object FBOk: TButton
      Left = 204
      Top = 16
      Width = 116
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'Create E-Mail...'
      Default = True
      ModalResult = 1
      TabOrder = 1
    end
    object FBCancel: TButton
      Left = 338
      Top = 16
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 2
    end
    object FBClipboard: TButton
      Left = 16
      Top = 16
      Width = 129
      Height = 25
      Caption = 'Copy to clipboard'
      TabOrder = 0
      OnClick = FBClipboardClick
    end
  end
end
