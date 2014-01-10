inherited DAUpdateSQLEditorForm: TDAUpdateSQLEditorForm
  Caption = 'DAUpdateSQLEditorForm'
  PixelsPerInch = 96
  TextHeight = 13
  inherited PageControl: TPageControl
    ActivePage = shEditSQL
    object shEditSQL: TTabSheet
      Caption = '&Update SQLs'
    end
    object shGenerator: TTabSheet
      Caption = 'SQL &Generator'
      ImageIndex = 1
    end
  end
end
