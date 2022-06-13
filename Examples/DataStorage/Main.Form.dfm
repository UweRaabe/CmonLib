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
  TextHeight = 15
  object splLeft: TSplitter
    Left = 249
    Top = 0
    Height = 416
    Beveled = True
    ResizeStyle = rsUpdate
    ExplicitTop = 8
    ExplicitHeight = 217
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 416
    Width = 852
    Height = 123
    Align = alBottom
    BevelOuter = bvNone
    ParentBackground = False
    ShowCaption = False
    TabOrder = 0
    object LoadSettingsButton: TButton
      Left = 32
      Top = 18
      Width = 175
      Height = 41
      Caption = 'Load Settings'
      Style = bsCommandLink
      TabOrder = 0
      OnClick = LoadSettingsButtonClick
    end
    object RestoreDefaultsButton: TButton
      Left = 480
      Top = 18
      Width = 175
      Height = 41
      Caption = 'Restore Defaults'
      Style = bsCommandLink
      TabOrder = 1
      OnClick = RestoreDefaultsButtonClick
    end
    object SaveSettingsButton: TButton
      Left = 248
      Top = 18
      Width = 175
      Height = 41
      Caption = 'Save Settings'
      Style = bsCommandLink
      TabOrder = 2
      OnClick = SaveSettingsButtonClick
    end
    object SomeBooleanCheck: TCheckBox
      Left = 731
      Top = 34
      Width = 97
      Height = 17
      Caption = 'Some Boolean'
      TabOrder = 3
    end
    object LoadLayoutButton: TButton
      Left = 32
      Top = 65
      Width = 175
      Height = 41
      Caption = 'Load Layout'
      Style = bsCommandLink
      TabOrder = 4
      OnClick = LoadLayoutButtonClick
    end
    object SaveLayoutButton: TButton
      Left = 248
      Top = 65
      Width = 175
      Height = 41
      Caption = 'Save Layout'
      Style = bsCommandLink
      TabOrder = 5
      OnClick = SaveLayoutButtonClick
    end
    object RestoreLayoutButton: TButton
      Left = 480
      Top = 65
      Width = 175
      Height = 41
      Caption = 'Restore Layout'
      Style = bsCommandLink
      TabOrder = 6
      OnClick = RestoreLayoutButtonClick
    end
  end
  object pnlLeft: TPanel
    Left = 0
    Top = 0
    Width = 249
    Height = 416
    Align = alLeft
    BevelOuter = bvNone
    ParentBackground = False
    ShowCaption = False
    TabOrder = 1
    object TitleLabel: TLabel
      Left = 32
      Top = 3
      Width = 50
      Height = 15
      Caption = 'TitleLabel'
    end
    object SomeEnumSelector: TRadioGroup
      Left = 32
      Top = 224
      Width = 185
      Height = 145
      Caption = 'Some Enum'
      TabOrder = 0
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
    object SomeTextEdit: TLabeledEdit
      Left = 32
      Top = 64
      Width = 185
      Height = 23
      EditLabel.Width = 54
      EditLabel.Height = 15
      EditLabel.Caption = 'Some Text'
      TabOrder = 2
      Text = ''
    end
  end
  object pnlCenter: TPanel
    Left = 252
    Top = 0
    Width = 600
    Height = 416
    Align = alClient
    BevelOuter = bvNone
    ParentBackground = False
    ShowCaption = False
    TabOrder = 2
    object splMainData: TSplitter
      Left = 0
      Top = 217
      Width = 600
      Height = 3
      Cursor = crVSplit
      Align = alTop
      Beveled = True
      ResizeStyle = rsUpdate
      ExplicitWidth = 239
    end
    object MainDataTree: TTreeView
      AlignWithMargins = True
      Left = 3
      Top = 223
      Width = 594
      Height = 190
      Align = alClient
      AutoExpand = True
      Indent = 19
      TabOrder = 0
      OnDblClick = MainDataTreeDblClick
      OnEdited = MainDataTreeEdited
      OnEditing = MainDataTreeEditing
      OnKeyDown = MainDataTreeKeyDown
    end
    object pnlFrames: TPanel
      Left = 0
      Top = 0
      Width = 600
      Height = 217
      Align = alTop
      BevelOuter = bvNone
      ParentBackground = False
      ShowCaption = False
      TabOrder = 1
      object splFrames: TSplitter
        Left = 241
        Top = 0
        Height = 217
        Beveled = True
        ResizeStyle = rsUpdate
        ExplicitLeft = 304
        ExplicitTop = 56
        ExplicitHeight = 100
      end
      inline DemoFrame1: TDemoFrame
        Left = 0
        Top = 0
        Width = 241
        Height = 217
        Align = alLeft
        TabOrder = 0
        ExplicitWidth = 241
        ExplicitHeight = 217
        inherited SomeTextEdit: TLabeledEdit
          EditLabel.ExplicitLeft = 32
          EditLabel.ExplicitTop = 46
          EditLabel.ExplicitWidth = 54
        end
        inherited SomeIndexSelector: TRadioGroup
          Top = 93
          ExplicitTop = 93
        end
      end
      inline DemoFrame2: TDemoFrame
        Left = 244
        Top = 0
        Width = 356
        Height = 217
        Align = alClient
        TabOrder = 1
        ExplicitLeft = 244
        ExplicitWidth = 356
        ExplicitHeight = 217
        inherited SomeTextEdit: TLabeledEdit
          EditLabel.ExplicitLeft = 32
          EditLabel.ExplicitTop = 46
          EditLabel.ExplicitWidth = 54
        end
      end
    end
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
