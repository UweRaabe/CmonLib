object DemoMainForm: TDemoMainForm
  Left = 0
  Top = 0
  Caption = 'Observer Demo'
  ClientHeight = 441
  ClientWidth = 624
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  TextHeight = 15
  object LogMemo: TMemo
    Left = 32
    Top = 231
    Width = 369
    Height = 161
    TabStop = False
    TabOrder = 4
  end
  object MyStringEdit: TEdit
    Left = 32
    Top = 48
    Width = 185
    Height = 23
    TabOrder = 0
  end
  object MyLinesMemo: TMemo
    Left = 32
    Top = 77
    Width = 185
    Height = 148
    TabOrder = 1
  end
  object MySelectedComboBox: TComboBox
    Left = 223
    Top = 48
    Width = 178
    Height = 23
    TabOrder = 2
    Items.Strings = (
      'Combo 1'
      'Combo 2'
      'Combo 3'
      'Combo 4'
      'Combo 5')
  end
  object MyListItemListBox: TListBox
    Left = 223
    Top = 77
    Width = 178
    Height = 148
    ItemHeight = 15
    Items.Strings = (
      'List 1'
      'List 2'
      'List 3'
      'List 4'
      'List 5')
    TabOrder = 3
  end
end
