inherited CRDataEditorForm: TCRDataEditorForm
  Width = 697
  Height = 360
  Caption = 'CRDataEditorForm'
  PixelsPerInch = 96
  TextHeight = 13
  inherited BtnPanel: TPanel
    Top = 292
    Width = 689
    TabOrder = 2
    Visible = False
    inherited imCorner: TImage
      Left = 677
    end
    inherited btOk: TBitBtn
      Left = 525
    end
    inherited btCancel: TBitBtn
      Left = 606
    end
  end
  object StatusBar: TStatusBar
    Left = 0
    Top = 273
    Width = 689
    Height = 19
    Panels = <
      item
        Width = 120
      end
      item
        Width = 120
      end
      item
        Width = 50
      end>
  end
  object pnlToolBar: TPanel
    Left = 0
    Top = 0
    Width = 689
    Height = 22
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    object btClose: TSpeedButton
      Left = 75
      Top = 0
      Width = 75
      Height = 23
      Caption = '&Close'
    end
    object DBNavigator: TDBNavigator
      Left = 425
      Top = 0
      Width = 240
      Height = 22
      DataSource = DataSource
      TabOrder = 0
    end
  end
  object DBGrid: TDBGrid
    Left = 0
    Top = 22
    Width = 689
    Height = 251
    Align = alClient
    DataSource = DataSource
    TabOrder = 1
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = 13
    TitleFont.Name = 'Tahoma'
    TitleFont.Pitch = fpVariable
    TitleFont.Style = []
  end
  object DataSource: TDataSource
    OnStateChange = DataSourceStateChange
    OnDataChange = DataSourceDataChange
    Left = 16
    Top = 48
  end
end
