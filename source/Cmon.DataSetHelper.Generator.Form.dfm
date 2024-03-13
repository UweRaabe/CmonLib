object MappingsGeneratorForm: TMappingsGeneratorForm
  Left = 0
  Top = 0
  Caption = 'DataSet Mappings Generator'
  ClientHeight = 260
  ClientWidth = 565
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  TextHeight = 15
  object pnlMain: TPanel
    Left = 0
    Top = 0
    Width = 565
    Height = 212
    Align = alClient
    BevelOuter = bvNone
    ParentBackground = False
    ShowCaption = False
    TabOrder = 0
    object grpCreateMappings: TGroupBox
      Left = 8
      Top = 16
      Width = 161
      Height = 81
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
      Left = 175
      Top = 16
      Width = 378
      Height = 81
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
    end
    object grpTypeName: TGroupBox
      Left = 8
      Top = 103
      Width = 545
      Height = 90
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
        TabOrder = 2
        TextHint = 'enter prefixes'
      end
      object edtRegex: TEdit
        Left = 103
        Top = 53
        Width = 417
        Height = 23
        TabOrder = 3
        TextHint = 'enter regulart expression'
      end
    end
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 212
    Width = 565
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
  object MainActionList: TActionList
    Left = 344
    Top = 112
    object actCreateRecord: TAction
      Caption = 'record'
      OnExecute = actCreateRecordExecute
      OnUpdate = actCreateRecordUpdate
    end
    object actCreateClass: TAction
      Caption = 'class'
      OnExecute = actCreateClassExecute
      OnUpdate = actCreateClassUpdate
    end
    object actCreateFields: TAction
      Caption = 'add field access'
      OnExecute = actCreateFieldsExecute
      OnUpdate = actCreateFieldsUpdate
    end
    object actMapAuto: TAction
      Caption = 'use automatic mapping'
      OnExecute = actMapAutoExecute
      OnUpdate = actMapAutoUpdate
    end
    object actMapManual: TAction
      Caption = 'use manual mapping'
      OnExecute = actMapManualExecute
      OnUpdate = actMapManualUpdate
    end
    object actUseNameConstants: TAction
      Caption = 'use name constants'
      OnExecute = actUseNameConstantsExecute
      OnUpdate = actUseNameConstantsUpdate
    end
    object actStripPrefix: TAction
      Caption = 'strip prefix'
      OnExecute = actStripPrefixExecute
      OnUpdate = actStripPrefixUpdate
    end
    object actUseRegex: TAction
      Caption = 'use regex'
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
  end
end
