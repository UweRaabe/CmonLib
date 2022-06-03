inherited DemoMainForm: TDemoMainForm
  Caption = 'DataStorage Demo'
  ClientHeight = 411
  ClientWidth = 852
  ExplicitWidth = 868
  ExplicitHeight = 450
  TextHeight = 13
  object TitleLabel: TLabel
    Left = 32
    Top = 3
    Width = 45
    Height = 13
    Caption = 'TitleLabel'
  end
  object SomeTextEdit: TLabeledEdit
    Left = 32
    Top = 64
    Width = 185
    Height = 21
    EditLabel.Width = 51
    EditLabel.Height = 13
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
  inline DemoFrame1: TDemoFrame
    Left = 264
    Top = 0
    Width = 241
    Height = 240
    TabOrder = 2
    ExplicitLeft = 264
    ExplicitWidth = 241
    inherited TitleLabel: TLabel
      Width = 45
      Height = 13
      ExplicitWidth = 45
      ExplicitHeight = 13
    end
    inherited SomeTextEdit: TLabeledEdit
      Height = 21
      EditLabel.Width = 51
      EditLabel.Height = 13
      EditLabel.ExplicitLeft = 32
      EditLabel.ExplicitTop = 48
      EditLabel.ExplicitWidth = 51
      EditLabel.ExplicitHeight = 13
      ExplicitHeight = 21
    end
  end
  inline DemoFrame2: TDemoFrame
    Left = 520
    Top = 0
    Width = 241
    Height = 240
    TabOrder = 3
    ExplicitLeft = 520
    ExplicitWidth = 241
    inherited TitleLabel: TLabel
      Width = 45
      Height = 13
      ExplicitWidth = 45
      ExplicitHeight = 13
    end
    inherited SomeTextEdit: TLabeledEdit
      Height = 21
      EditLabel.Width = 51
      EditLabel.Height = 13
      EditLabel.ExplicitLeft = 32
      EditLabel.ExplicitTop = 48
      EditLabel.ExplicitWidth = 51
      EditLabel.ExplicitHeight = 13
      ExplicitHeight = 21
    end
  end
  object SomeEnumSelector: TRadioGroup
    Left = 32
    Top = 224
    Width = 193
    Height = 145
    Caption = 'Some Enum'
    TabOrder = 4
  end
  object SomeBooleanCheck: TCheckBox
    Left = 264
    Top = 352
    Width = 97
    Height = 17
    Caption = 'Some Boolean'
    TabOrder = 5
  end
  object SaveSettingsButton: TButton
    Left = 456
    Top = 256
    Width = 175
    Height = 41
    Caption = 'Save Settings'
    Style = bsCommandLink
    TabOrder = 6
    OnClick = SaveSettingsButtonClick
  end
  object LoadSettingsButton: TButton
    Left = 456
    Top = 320
    Width = 175
    Height = 41
    Caption = 'Load Settings'
    Style = bsCommandLink
    TabOrder = 7
    OnClick = LoadSettingsButtonClick
  end
end
