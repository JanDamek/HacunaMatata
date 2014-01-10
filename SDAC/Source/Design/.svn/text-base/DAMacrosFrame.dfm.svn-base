inherited DAMacrosFrame: TDAMacrosFrame
  object lbMName: TLabel [0]
    Left = 19
    Top = 8
    Width = 61
    Height = 13
    Caption = 'Macro Name'
  end
  object lbMacroLog: TLabel [1]
    Left = 8
    Top = 7
    Width = 11
    Height = 16
    Caption = '&&'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clNavy
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Pitch = fpVariable
    Font.Style = [fsBold]
    ParentFont = False
  end
  inherited PanelItem: TPanel
    object lbMValue: TLabel
      Left = 16
      Top = 12
      Width = 27
      Height = 13
      Caption = 'Value'
    end
    object lbActive: TLabel
      Left = 33
      Top = 191
      Width = 30
      Height = 13
      Anchors = [akLeft, akBottom]
      Caption = 'Active'
    end
    object meMacroValue: TMemo
      Left = 14
      Top = 28
      Width = 238
      Height = 157
      Anchors = [akLeft, akTop, akRight, akBottom]
      Lines.Strings = (
        '')
      ScrollBars = ssBoth
      TabOrder = 0
      WordWrap = False
      OnExit = meMacroValueExit
    end
    object cbMacroActive: TCheckBox
      Left = 14
      Top = 189
      Width = 15
      Height = 17
      Anchors = [akLeft, akBottom]
      TabOrder = 1
      OnClick = cbMacroActiveClick
    end
  end
end
