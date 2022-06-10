object DemoMainForm: TDemoMainForm
  Left = 0
  Top = 0
  Caption = 'DataStorage Demo'
  ClientHeight = 539
  ClientWidth = 852
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  TextHeight = 15
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
  inline DemoFrame1: TDemoFrame
    Left = 264
    Top = 0
    Width = 241
    Height = 240
    TabOrder = 2
    ExplicitLeft = 264
    ExplicitWidth = 241
    inherited SomeTextEdit: TLabeledEdit
      EditLabel.ExplicitLeft = 32
      EditLabel.ExplicitTop = 46
      EditLabel.ExplicitWidth = 54
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
    inherited SomeTextEdit: TLabeledEdit
      EditLabel.ExplicitLeft = 32
      EditLabel.ExplicitTop = 46
      EditLabel.ExplicitWidth = 54
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
  object RestoreDefaultsButton: TButton
    Left = 456
    Top = 384
    Width = 175
    Height = 41
    Caption = 'Restore Defaults'
    Style = bsCommandLink
    TabOrder = 8
    OnClick = RestoreDefaultsButtonClick
  end
  object MainDataTree: TTreeView
    Left = 648
    Top = 256
    Width = 153
    Height = 169
    AutoExpand = True
    Indent = 19
    TabOrder = 9
    OnDblClick = MainDataTreeDblClick
    OnEdited = MainDataTreeEdited
    OnEditing = MainDataTreeEditing
    OnKeyDown = MainDataTreeKeyDown
  end
  object LoadSettingsDialog: TFileOpenDialog
    FavoriteLinks = <>
    FileTypes = <>
    Options = [fdoStrictFileTypes, fdoPathMustExist, fdoFileMustExist]
    Left = 680
    Top = 328
  end
  object SaveSettingsDialog: TFileSaveDialog
    FavoriteLinks = <>
    FileTypes = <>
    Options = [fdoOverWritePrompt, fdoStrictFileTypes, fdoPathMustExist]
    Left = 680
    Top = 256
  end
end
