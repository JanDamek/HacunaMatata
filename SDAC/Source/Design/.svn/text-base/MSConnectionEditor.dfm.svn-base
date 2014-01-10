inherited MSConnectionEditorForm: TMSConnectionEditorForm
  Caption = 'MSConnectionEditorForm'
  PixelsPerInch = 96
  TextHeight = 13
  inherited PageControl: TPageControl
    inherited shConnect: TTabSheet
      inherited Panel: TPanel
        object lbDatabase: TLabel [3]
          Left = 16
          Top = 118
          Width = 46
          Height = 13
          Caption = 'Database'
        end
        object edDatabase: TComboBox
          Left = 104
          Top = 114
          Width = 153
          Height = 21
          ItemHeight = 13
          TabOrder = 3
          OnChange = edDatabaseChange
          OnDropDown = edDatabaseDropDown
          OnExit = edDatabaseExit
          OnKeyUp = edDatabaseKeyUp
        end
      end
      object rgAuth: TRadioGroup
        Left = 296
        Top = 32
        Width = 121
        Height = 65
        Caption = 'Authentication'
        Items.Strings = (
          'Windows'
          'SQL Server')
        TabOrder = 4
        OnClick = rgAuthClick
      end
    end
    inherited shAbout: TTabSheet
      inherited Label1: TLabel
        Width = 351
        Caption = 'SQL Server Data Access Components'
      end
      inherited lbWeb: TLabel
        Width = 101
        Caption = 'www.devart.com/sdac'
      end
      inherited lbMail: TLabel
        Width = 80
        Caption = 'sdac@devart.com'
      end
    end
  end
  object btQueryAnalyzer: TButton
    Left = 8
    Top = 249
    Width = 99
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Query Analyzer'
    TabOrder = 3
    OnClick = btQueryAnalyzerClick
  end
  object btManagementStudio: TButton
    Left = 112
    Top = 249
    Width = 99
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Management Studio'
    TabOrder = 4
    OnClick = btManagementStudioClick
  end
end
