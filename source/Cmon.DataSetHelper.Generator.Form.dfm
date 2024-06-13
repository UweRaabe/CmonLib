object MappingsGeneratorForm: TMappingsGeneratorForm
  Left = 0
  Top = 0
  Caption = 'DataSet Mappings Generator'
  ClientHeight = 464
  ClientWidth = 555
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  ShowHint = True
  OnCreate = FormCreate
  TextHeight = 15
  object pnlMain: TPanel
    Left = 0
    Top = 201
    Width = 555
    Height = 215
    Align = alClient
    BevelOuter = bvNone
    ParentBackground = False
    ShowCaption = False
    TabOrder = 0
    object tkey: TRadioGroup
      AlignWithMargins = True
      Left = 3
      Top = 3
      Width = 549
      Height = 209
      Align = alClient
      Caption = 'tkey'
      TabOrder = 0
    end
    object edtTypeNames: TValueListEditor
      AlignWithMargins = True
      Left = 3
      Top = 3
      Width = 549
      Height = 209
      Align = alClient
      TabOrder = 1
      TitleCaptions.Strings = (
        'DataSet'
        'Base Type Name')
      ColWidths = (
        150
        393)
    end
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 416
    Width = 555
    Height = 48
    Align = alBottom
    BevelOuter = bvNone
    ParentBackground = False
    ShowCaption = False
    TabOrder = 1
    object btnOK: TButton
      Left = 372
      Top = 6
      Width = 75
      Height = 25
      Action = actOK
      TabOrder = 0
    end
    object btnCancel: TButton
      Left = 453
      Top = 6
      Width = 75
      Height = 25
      Action = actCancel
      TabOrder = 1
    end
  end
  object pnlTop: TPanel
    Left = 0
    Top = 0
    Width = 555
    Height = 201
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 2
    object grpCreateMappings: TGroupBox
      AlignWithMargins = True
      Left = 3
      Top = 3
      Width = 161
      Height = 99
      Align = alLeft
      Caption = ' Create mappings as'
      TabOrder = 0
      object selCreateRecord: TRadioButton
        Left = 16
        Top = 24
        Width = 128
        Height = 17
        Action = actCreateRecord
        TabOrder = 0
      end
      object selCreateClass: TRadioButton
        Left = 16
        Top = 47
        Width = 128
        Height = 17
        Action = actCreateClass
        TabOrder = 1
      end
    end
    object grpOptions: TGroupBox
      AlignWithMargins = True
      Left = 170
      Top = 3
      Width = 382
      Height = 99
      Align = alClient
      Caption = ' Options '
      TabOrder = 1
      object selUseNameConstants: TCheckBox
        Left = 200
        Top = 24
        Width = 153
        Height = 17
        Action = actUseNameConstants
        TabOrder = 0
      end
      object selMapAuto: TRadioButton
        Left = 16
        Top = 24
        Width = 153
        Height = 17
        Action = actMapAuto
        TabOrder = 1
      end
      object selMapManual: TRadioButton
        Left = 16
        Top = 47
        Width = 153
        Height = 17
        Action = actMapManual
        TabOrder = 2
      end
      object selCreateFields: TCheckBox
        Left = 200
        Top = 47
        Width = 153
        Height = 17
        Action = actCreateFields
        TabOrder = 3
      end
      object selSorted: TCheckBox
        Left = 200
        Top = 70
        Width = 153
        Height = 17
        Action = actSorted
        TabOrder = 4
      end
    end
    object grpTypeName: TGroupBox
      AlignWithMargins = True
      Left = 3
      Top = 108
      Width = 549
      Height = 90
      Align = alBottom
      Caption = ' determine type name from dataset name '
      TabOrder = 2
      object selStripPrefix: TRadioButton
        Left = 16
        Top = 27
        Width = 81
        Height = 17
        Action = actStripPrefix
        TabOrder = 0
      end
      object selUseRegEx: TRadioButton
        Left = 16
        Top = 56
        Width = 81
        Height = 17
        Action = actUseRegex
        TabOrder = 1
      end
      object edtPrefixes: TEdit
        Left = 103
        Top = 24
        Width = 417
        Height = 23
        Hint = 'Enter multiple prefixes as comma separated list.'
        TabOrder = 2
        TextHint = 'enter prefixes'
      end
      object edtRegex: TEdit
        Left = 103
        Top = 53
        Width = 417
        Height = 23
        Hint = 
          'The regular expression must return the  desired string in the fi' +
          'rst match!'
        TabOrder = 3
        TextHint = 'enter regular expression'
      end
    end
  end
  object MainActionList: TActionList
    Left = 344
    Top = 112
    object actCreateRecord: TAction
      Caption = 'record'
      Hint = 'create mappings as record'
      OnExecute = actCreateRecordExecute
      OnUpdate = actCreateRecordUpdate
    end
    object actCreateClass: TAction
      Caption = 'class'
      Hint = 'create mappings as class'
      OnExecute = actCreateClassExecute
      OnUpdate = actCreateClassUpdate
    end
    object actCreateFields: TAction
      Caption = 'add field access'
      Hint = 'add TRecordFields class'
      OnExecute = actCreateFieldsExecute
      OnUpdate = actCreateFieldsUpdate
    end
    object actMapAuto: TAction
      Caption = 'use automatic mapping'
      Hint = 'adds mapAuto attribute'
      OnExecute = actMapAutoExecute
      OnUpdate = actMapAutoUpdate
    end
    object actMapManual: TAction
      Caption = 'use manual mapping'
      Hint = '@adds individual attributes to fields|'
      OnExecute = actMapManualExecute
      OnUpdate = actMapManualUpdate
    end
    object actUseNameConstants: TAction
      Caption = 'use name constants'
      Hint = 'adds type containing field names as constants'
      OnExecute = actUseNameConstantsExecute
      OnUpdate = actUseNameConstantsUpdate
    end
    object actStripPrefix: TAction
      Caption = 'strip prefix'
      Hint = 'remove prefixes from dataset name to get type name'
      OnExecute = actStripPrefixExecute
      OnUpdate = actStripPrefixUpdate
    end
    object actUseRegex: TAction
      Caption = 'use regex'
      Hint = 'use regular expression to get type name from dataset name'
      OnExecute = actUseRegexExecute
      OnUpdate = actUseRegexUpdate
    end
    object actOK: TAction
      Caption = 'OK'
      OnExecute = actOKExecute
    end
    object actCancel: TAction
      Caption = 'Cancel'
      OnExecute = actCancelExecute
    end
    object actSorted: TAction
      Caption = 'sorted'
      OnExecute = actSortedExecute
      OnUpdate = actSortedUpdate
    end
  end
end
