object DemoFrame: TDemoFrame
  Left = 0
  Top = 0
  Width = 320
  Height = 240
  TabOrder = 0
  object TitleLabel: TLabel
    Left = 32
    Top = 3
    Width = 50
    Height = 15
    Caption = 'TitleLabel'
  end
  object SomeTextEdit: TLabeledEdit
    Left = 32
    Top = 64
    Width = 185
    Height = 23
    EditLabel.Width = 54
    EditLabel.Height = 15
    EditLabel.Caption = 'Some Text'
    TabOrder = 0
    Text = ''
  end
  object SomeIndexSelector: TRadioGroup
    Left = 32
    Top = 99
    Width = 185
    Height = 105
    Caption = 'Some Index'
    Columns = 2
    Items.Strings = (
      'Index 0'
      'Index 1'
      'Index 2'
      'Index 3')
    TabOrder = 1
  end
end
