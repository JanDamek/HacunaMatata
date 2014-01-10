inherited DASQLGeneratorFrame: TDASQLGeneratorFrame
  Height = 245
  object pnSQLGenerator: TPanel
    Left = 8
    Top = 6
    Width = 465
    Height = 229
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 0
    OnResize = pnSQLGeneratorResize
    object lbTableName: TLabel
      Left = 13
      Top = 13
      Width = 61
      Height = 13
      Caption = 'Table Name:'
    end
    object lbKeyFieldsLabel: TLabel
      Left = 182
      Top = 12
      Width = 51
      Height = 13
      Caption = 'Key Fields:'
    end
    object lbUpdateFieldsLabel: TLabel
      Left = 321
      Top = 11
      Width = 68
      Height = 13
      Anchors = [akTop, akRight]
      Caption = 'Update Fields:'
    end
    object cbTables: TComboBox
      Left = 13
      Top = 29
      Width = 156
      Height = 21
      ItemHeight = 13
      TabOrder = 0
      OnChange = cbTablesChange
      OnDropDown = cbTablesDropDown
    end
    object btGenerate: TButton
      Left = 13
      Top = 82
      Width = 155
      Height = 22
      Caption = 'Generate SQL'
      TabOrder = 2
      OnClick = btGenerateClick
    end
    object lbKeyFields: TListBox
      Left = 180
      Top = 28
      Width = 133
      Height = 188
      Anchors = [akLeft, akTop, akBottom]
      ItemHeight = 13
      MultiSelect = True
      TabOrder = 9
      OnClick = lbUpdateFieldsClick
    end
    object lbUpdateFields: TListBox
      Left = 321
      Top = 28
      Width = 133
      Height = 188
      Anchors = [akTop, akRight, akBottom]
      ItemHeight = 13
      MultiSelect = True
      TabOrder = 10
      OnClick = lbUpdateFieldsClick
    end
    object btGetFields: TButton
      Left = 13
      Top = 56
      Width = 155
      Height = 22
      Caption = 'Get Fields'
      Enabled = False
      TabOrder = 1
      OnClick = btGetFieldsClick
    end
    object cbInsert: TCheckBox
      Left = 15
      Top = 109
      Width = 146
      Height = 17
      Caption = 'Insert'
      Checked = True
      State = cbChecked
      TabOrder = 3
      OnClick = cbIUDRClick
    end
    object cbUpdate: TCheckBox
      Left = 15
      Top = 125
      Width = 146
      Height = 17
      Caption = 'Update'
      Checked = True
      State = cbChecked
      TabOrder = 4
      OnClick = cbIUDRClick
    end
    object cbDelete: TCheckBox
      Left = 15
      Top = 141
      Width = 146
      Height = 17
      Caption = 'Delete'
      Checked = True
      State = cbChecked
      TabOrder = 5
      OnClick = cbIUDRClick
    end
    object cbRefresh: TCheckBox
      Left = 15
      Top = 173
      Width = 146
      Height = 17
      Caption = 'Refresh'
      Checked = True
      State = cbChecked
      TabOrder = 7
      OnClick = cbIUDRClick
    end
    object cbQuoteFields: TCheckBox
      Left = 15
      Top = 189
      Width = 146
      Height = 17
      Caption = 'Quote names'
      TabOrder = 8
    end
    object cbLock: TCheckBox
      Left = 15
      Top = 157
      Width = 146
      Height = 17
      Caption = 'Lock'
      Checked = True
      State = cbChecked
      TabOrder = 6
      OnClick = cbIUDRClick
    end
  end
end
