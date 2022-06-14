object MainForm: TMainForm
  Left = 500
  Top = 300
  Caption = 'MainForm'
  ClientHeight = 411
  ClientWidth = 852
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Position = poDesigned
  TextHeight = 13
  object TextEdit: TMemo
    AlignWithMargins = True
    Left = 3
    Top = 44
    Width = 846
    Height = 364
    Align = alClient
    Lines.Strings = (
      
        'Es herrscht B'#252'rgerkrieg. Die Rebellen, deren Raumschiffe von ein' +
        'em '
      
        'geheimen St'#252'tzpunkt aus angreifen, haben ihren ersten Sieg gegen' +
        ' das '
      'b'#246'se galaktische Imperium errungen.'
      ''
      
        'W'#228'hrend der Schlacht ist es Spionen der Rebellen gelungen, Gehei' +
        'mpl'#228'ne '
      
        #252'ber die absolute Waffe des Imperiums in ihren Besitz zu bringen' +
        ', den '
      
        'TODESSTERN, eine Raumstation, deren Feuerkraft ausreicht, um ein' +
        'en '
      'ganzen Planeten zu vernichten.'
      ''
      
        'Verfolgt von den finsteren Agenten des Imperiums, jagt Prinzessi' +
        'n Leia an '
      
        'Bord ihres Sternenschiffes nach Hause, als H'#252'terin der erbeutete' +
        'n Pl'#228'ne, '
      
        'die ihr Volk retten und der Galaxis die Freiheit wiedergeben k'#246'n' +
        'nten....')
    TabOrder = 0
    ExplicitLeft = 16
    ExplicitTop = 17
    ExplicitWidth = 365
    ExplicitHeight = 365
  end
  object pnlTop: TPanel
    Left = 0
    Top = 0
    Width = 852
    Height = 41
    Align = alTop
    BevelOuter = bvNone
    Caption = 'pnlTop'
    ParentBackground = False
    ShowCaption = False
    TabOrder = 1
    ExplicitLeft = -3
    ExplicitTop = -3
    object LoadButton: TButton
      Left = 12
      Top = 9
      Width = 75
      Height = 25
      Caption = 'Load'
      TabOrder = 0
      OnClick = LoadButtonClick
    end
    object SaveButton: TButton
      Left = 93
      Top = 9
      Width = 75
      Height = 25
      Caption = 'Save'
      TabOrder = 1
      OnClick = SaveButtonClick
    end
  end
  object OpenDialog: TOpenTextFileDialog
    DefaultExt = 'txt'
    Left = 424
    Top = 88
  end
  object SaveDialog: TSaveTextFileDialog
    DefaultExt = 'txt'
    Left = 424
    Top = 156
  end
end
