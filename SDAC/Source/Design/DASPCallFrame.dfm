inherited DASPCallFrame: TDASPCallFrame
  Width = 473
  Height = 332
  inherited Panel1: TPanel
    Top = 146
    Width = 473
    Height = 186
    TabOrder = 3
    inherited meSQL: TMemo
      Width = 457
      Height = 168
      Color = clBtnFace
      ReadOnly = True
    end
  end
  inherited pnlTop: TPanel
    Top = 56
    Width = 473
    TabOrder = 1
    Visible = False
    inherited btClear: TSpeedButton
      Left = 465
      Width = 7
      Visible = False
    end
    inherited gbStatementType: TGroupBox
      Width = 458
    end
  end
  object pnSQL: TPanel
    Left = 0
    Top = 0
    Width = 473
    Height = 56
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    Visible = False
    object Label14: TLabel
      Left = 8
      Top = 8
      Width = 114
      Height = 13
      Caption = 'Stored Procedure Name'
    end
    object cbStoredProcName: TComboBox
      Left = 8
      Top = 24
      Width = 321
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      DropDownCount = 16
      ItemHeight = 13
      TabOrder = 0
      OnChange = cbStoredProcNameChange
      OnDropDown = cbStoredProcNameDropDown
    end
    object btGenerate: TButton
      Left = 344
      Top = 22
      Width = 121
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'Generate'
      Enabled = False
      TabOrder = 1
      OnClick = btGenerateClick
    end
  end
  object pnSQLSP: TPanel
    Left = 0
    Top = 98
    Width = 473
    Height = 48
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 2
    Visible = False
    object Label2: TLabel
      Left = 5
      Top = 20
      Width = 84
      Height = 13
      Caption = 'StoredProc Name'
    end
    object cbStoredProcNameSP: TComboBox
      Left = 97
      Top = 16
      Width = 376
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      DropDownCount = 16
      ItemHeight = 13
      TabOrder = 0
      OnDropDown = cbStoredProcNameDropDown
      OnExit = cbStoredProcNameSelect
    end
  end
end
