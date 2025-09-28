object MainForm: TMainForm
  Left = 0
  Top = 0
  Caption = 'MainForm'
  ClientHeight = 441
  ClientWidth = 624
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  TextHeight = 15
  object pnlTop: TPanel
    Left = 0
    Top = 0
    Width = 624
    Height = 41
    Align = alTop
    BevelOuter = bvNone
    ShowCaption = False
    TabOrder = 0
    object lblNumber: TLabel
      Left = 8
      Top = 13
      Width = 47
      Height = 15
      Caption = 'Number:'
    end
    object edtNumber: TNumberBox
      Left = 72
      Top = 10
      Width = 121
      Height = 23
      Alignment = taRightJustify
      TabOrder = 0
      SpinButtonOptions.Placement = nbspInline
    end
    object btnDoSomething: TButton
      Left = 208
      Top = 9
      Width = 113
      Height = 25
      Caption = 'Do Something'
      TabOrder = 1
      OnClick = btnDoSomethingClick
    end
  end
  object pnlMain: TPanel
    Left = 0
    Top = 41
    Width = 624
    Height = 400
    Align = alClient
    BevelOuter = bvNone
    ShowCaption = False
    TabOrder = 1
    object dspSomething: TMemo
      AlignWithMargins = True
      Left = 3
      Top = 3
      Width = 618
      Height = 394
      Align = alClient
      TabOrder = 0
    end
  end
end
