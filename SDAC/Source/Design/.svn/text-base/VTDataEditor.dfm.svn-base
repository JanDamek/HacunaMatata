inherited VTDataEditorForm: TVTDataEditorForm
  Width = 671
  Caption = 'VTDataEditorForm'
  PixelsPerInch = 96
  TextHeight = 13
  inherited BtnPanel: TPanel
    Width = 663
    inherited imCorner: TImage
      Left = 651
    end
    inherited btOk: TBitBtn
      Left = 499
    end
    inherited btCancel: TBitBtn
      Left = 580
    end
  end
  inherited StatusBar: TStatusBar
    Width = 663
  end
  inherited pnlToolBar: TPanel
    Width = 663
    inherited btClose: TSpeedButton
      OnClick = CloseClick
    end
    object btSave: TSpeedButton [1]
      Left = 0
      Top = 0
      Width = 75
      Height = 23
      Caption = 'Save'
      OnClick = SaveClick
    end
    object btClear: TSpeedButton [2]
      Left = 162
      Top = 0
      Width = 75
      Height = 23
      Caption = 'Clear'
      OnClick = btClearClick
    end
    object btLoadFromFile: TSpeedButton [3]
      Left = 237
      Top = 0
      Width = 75
      Height = 23
      Caption = 'Load from file'
      OnClick = btLoadFromFileClick
    end
    object btSaveToFile: TSpeedButton [4]
      Left = 312
      Top = 0
      Width = 75
      Height = 23
      Caption = 'Save to file'
      OnClick = btSaveToFileClick
    end
    inherited DBNavigator: TDBNavigator
      Left = 399
      Hints.Strings = ()
    end
  end
  inherited DBGrid: TDBGrid
    Width = 663
  end
  object OpenDialog: TOpenDialog
    Filter = 
      'Virtual Table Data (*.vtd)|*.vtd|XML (*.xml)|*.xml|Any File (*.*' +
      ')|*.*'
    Left = 352
    Top = 34
  end
  object SaveDialog: TSaveDialog
    FileName = 'Data1'
    Filter = 
      'Virtual Table Data (*.vtd)|*.vtd|XML (*.xml)|*.xml|Any File (*.*' +
      ')|*.*'
    Left = 384
    Top = 34
  end
end
