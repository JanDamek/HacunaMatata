inherited DAUpdateSQLFrame: TDAUpdateSQLFrame
  Width = 450
  Height = 230
  inherited Panel1: TPanel
    Top = 42
    Width = 450
    Height = 188
    TabOrder = 1
    inherited meSQL: TMemo
      Width = 434
      Height = 172
      OnChange = meSQLChange
    end
  end
  object pnlTop: TPanel
    Left = 0
    Top = 0
    Width = 450
    Height = 42
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    object btClear: TSpeedButton
      Left = 420
      Top = 9
      Width = 21
      Height = 31
      Hint = 'Clear'
      Anchors = [akTop, akRight]
      Flat = True
      Glyph.Data = {
        4E010000424D4E01000000000000760000002800000012000000120000000100
        040000000000D800000000000000000000001000000010000000000000000000
        8000008000000080800080000000800080008080000080808000C0C0C0000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00888888888888
        888888FFFFFF888888888888888888FFFFFF888888888888881F88FFFFFF8888
        1F888888888888FFFFFF888111F8888881F888FFFFFF888111F888881F8888FF
        FFFF8888111F88811F8888FFFFFF88888111F811F88888FFFFFF88888811111F
        888888FFFFFF8888888111F8888888FFFFFF88888811111F888888FFFFFF8888
        8111F811888888FFFFFF8881111F88811F8888FFFFFF881111F8888811F888FF
        FFFF8811F8888888811F88FFFFFF888888888888888888FFFFFF888888888888
        888888FFFFFF888888888888888888FFFFFF}
      ParentShowHint = False
      ShowHint = True
      OnClick = btClearClick
    end
    object gbStatementType: TGroupBox
      Left = 7
      Top = 3
      Width = 410
      Height = 37
      Anchors = [akLeft, akTop, akRight]
      Caption = ' Statement Type '
      TabOrder = 0
    end
  end
end
