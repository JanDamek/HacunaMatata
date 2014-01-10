inherited DADataEditorForm: TDADataEditorForm
  Caption = 'DADataEditorForm'
  PixelsPerInch = 96
  TextHeight = 13
  inherited pnlToolBar: TPanel
    inherited btClose: TSpeedButton
      OnClick = btCloseClick
    end
    object btOpen: TSpeedButton [1]
      Left = 0
      Top = 0
      Width = 75
      Height = 23
      Caption = '&Open'
      OnClick = btOpenClick
    end
    object btnExit: TSpeedButton [2]
      Left = 233
      Top = 0
      Width = 75
      Height = 23
      Caption = 'Exit'
      OnClick = CloseClick
    end
    object btSaveToFile: TSpeedButton [3]
      Left = 154
      Top = 0
      Width = 75
      Height = 23
      Caption = '&Save to file'
      OnClick = btSaveToFileClick
    end
    inherited DBNavigator: TDBNavigator
      Left = 336
      Hints.Strings = ()
    end
  end
  object SaveDialog: TSaveDialog
    DefaultExt = 'xml'
    FileName = 'Data1.xml'
    Filter = 'XML (*.xml)|*.xml|Any File (*.*)|*.*'
    Left = 384
    Top = 34
  end
end
