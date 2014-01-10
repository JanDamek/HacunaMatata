inherited DASQLFrame: TDASQLFrame
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 481
    Height = 229
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    object meSQL: TMemo
      Left = 8
      Top = 8
      Width = 465
      Height = 213
      Anchors = [akLeft, akTop, akRight, akBottom]
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Courier New'
      Font.Pitch = fpVariable
      Font.Style = []
      Lines.Strings = (
        '')
      ParentFont = False
      ScrollBars = ssBoth
      TabOrder = 0
      WordWrap = False
      OnExit = meSQLExit
    end
  end
end
