inherited MainForm: TMainForm
  Caption = 'Cmon References Demo'
  TextHeight = 15
  inherited AddFrameButton: TButton
    TabOrder = 1
  end
  inherited ToolBar1: TToolBar
    TabOrder = 0
  end
  inline StaticFrame: TMainFrame
    Left = 0
    Top = 29
    Width = 624
    Height = 29
    Align = alTop
    AutoSize = True
    TabOrder = 2
    ExplicitTop = 29
    ExplicitWidth = 624
    ExplicitHeight = 29
    inherited ToolBar1: TToolBar
      Width = 624
      ExplicitWidth = 624
    end
  end
  object NewFormButton: TButton
    Left = 476
    Top = 378
    Width = 140
    Height = 45
    Anchors = [akRight, akBottom]
    Caption = 'New Form'
    TabOrder = 3
    OnClick = NewFormButtonClick
  end
end
