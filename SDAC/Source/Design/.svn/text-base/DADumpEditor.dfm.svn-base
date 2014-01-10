inherited DADumpEditorForm: TDADumpEditorForm
  Left = 336
  Top = 182
  Height = 468
  Caption = 'DADumpEditorForm'
  PixelsPerInch = 96
  TextHeight = 13
  inherited BtnPanel: TPanel
    Top = 393
  end
  object ClientPanel: TPanel
    Left = 0
    Top = 0
    Width = 505
    Height = 393
    Align = alClient
    Constraints.MinHeight = 257
    Constraints.MinWidth = 489
    TabOrder = 1
    object meSQL: TMemo
      Left = 233
      Top = 1
      Width = 271
      Height = 391
      Align = alClient
      ScrollBars = ssBoth
      TabOrder = 1
      WordWrap = False
      OnExit = meSQLExit
    end
    object LeftPanel: TPanel
      Left = 1
      Top = 1
      Width = 232
      Height = 391
      Align = alLeft
      BevelOuter = bvNone
      BevelWidth = 5
      Constraints.MinHeight = 335
      TabOrder = 0
      object gbBackupOptions: TGroupBox
        Left = 8
        Top = 8
        Width = 217
        Height = 313
        Caption = 'Backup Options'
        TabOrder = 0
        object lbTableNames: TLabel
          Left = 8
          Top = 20
          Width = 63
          Height = 13
          Caption = 'Table Names'
        end
        object cbTableNames: TComboBox
          Left = 8
          Top = 40
          Width = 201
          Height = 21
          Anchors = [akLeft, akTop, akRight]
          DropDownCount = 16
          ItemHeight = 13
          TabOrder = 1
          OnChange = cbTableNamesChange
          OnDropDown = cbTableNamesDropDown
        end
        object cbGenerateHeader: TCheckBox
          Left = 8
          Top = 128
          Width = 201
          Height = 17
          Caption = 'Generate Header'
          TabOrder = 0
        end
      end
      object btBackup: TBitBtn
        Left = 16
        Top = 328
        Width = 97
        Height = 25
        Caption = 'Backup'
        TabOrder = 1
        OnClick = btBackupClick
      end
      object btRestore: TBitBtn
        Left = 121
        Top = 328
        Width = 96
        Height = 25
        Caption = 'Restore'
        TabOrder = 2
        OnClick = btRestoreClick
      end
      object btImport: TBitBtn
        Left = 16
        Top = 359
        Width = 97
        Height = 25
        Caption = 'Import from file'
        TabOrder = 3
        OnClick = btImportClick
      end
      object btExport: TBitBtn
        Left = 120
        Top = 359
        Width = 97
        Height = 25
        Caption = 'Export to file'
        TabOrder = 4
        OnClick = btExportClick
      end
    end
  end
  object OpenDialog: TOpenDialog
    DefaultExt = 'sql'
    Filter = 'Query files (*.sql)|*.sql|All files (*.*)|*.*'
    FilterIndex = 0
    Options = [ofHideReadOnly, ofFileMustExist, ofEnableSizing]
    Left = 321
    Top = 25
  end
  object SaveDialog: TSaveDialog
    DefaultExt = 'sql'
    Filter = 'Query files (*.sql)|*.sql|All files (*.*)|*.*'
    FilterIndex = 0
    Options = [ofOverwritePrompt, ofHideReadOnly, ofEnableSizing]
    Left = 353
    Top = 25
  end
end
