object BaseForm: TBaseForm
  Left = 0
  Top = 0
  Caption = 'Secondary Form'
  ClientHeight = 441
  ClientWidth = 624
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  DesignSize = (
    624
    441)
  TextHeight = 15
  object InfoLabel: TLabel
    Left = 96
    Top = 392
    Width = 21
    Height = 15
    Anchors = [akLeft, akBottom]
    Caption = 'Info'
  end
  object AddFrameButton: TButton
    Left = 8
    Top = 388
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Add Frame'
    TabOrder = 0
    OnClick = AddFrameButtonClick
  end
end
