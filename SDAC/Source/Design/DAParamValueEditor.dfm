inherited DAParamValueEditor: TDAParamValueEditor
  Left = 366
  Top = 221
  Height = 332
  HorzScrollBar.Visible = False
  VertScrollBar.Visible = False
  BorderStyle = bsSizeToolWin
  Caption = 'Param.Value Editor'
  Constraints.MinHeight = 332
  Constraints.MinWidth = 513
  PixelsPerInch = 96
  TextHeight = 13
  inherited BtnPanel: TPanel
    Top = 257
    TabOrder = 1
  end
  object Memo: TMemo
    Left = 0
    Top = 0
    Width = 505
    Height = 257
    Align = alClient
    TabOrder = 0
    OnChange = MemoChange
  end
end
