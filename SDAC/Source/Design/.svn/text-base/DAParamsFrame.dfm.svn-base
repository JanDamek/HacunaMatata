inherited DAParamsFrame: TDAParamsFrame
  object lbPName: TLabel [0]
    Left = 16
    Top = 8
    Width = 79
    Height = 13
    Caption = 'Parameter Name'
  end
  object lbParamLog: TLabel [1]
    Left = 9
    Top = 6
    Width = 5
    Height = 16
    Caption = ':'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clNavy
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Pitch = fpVariable
    Font.Style = [fsBold]
    ParentFont = False
  end
  inherited PanelItem: TPanel
    Top = 24
    Height = 197
    object lbPType: TLabel
      Left = 16
      Top = 16
      Width = 50
      Height = 13
      Caption = 'Data Type'
    end
    object lbParamType: TLabel
      Left = 177
      Top = 16
      Width = 57
      Height = 13
      Anchors = [akTop, akRight]
      Caption = 'Param Type'
    end
    object lbPValue: TLabel
      Left = 16
      Top = 64
      Width = 27
      Height = 13
      Caption = 'Value'
    end
    object lbNullValue: TLabel
      Left = 34
      Top = 125
      Width = 48
      Height = 13
      Caption = 'Null Value'
      FocusControl = cbNullValue
    end
    object lbSize: TLabel
      Left = 136
      Top = 125
      Width = 20
      Height = 13
      Caption = 'Size'
    end
    object cbDataType: TComboBox
      Left = 16
      Top = 32
      Width = 154
      Height = 21
      Style = csDropDownList
      Anchors = [akLeft, akTop, akRight]
      DropDownCount = 15
      ItemHeight = 13
      TabOrder = 0
      OnChange = cbDataTypeChange
    end
    object cbParamType: TComboBox
      Left = 177
      Top = 32
      Width = 73
      Height = 21
      Style = csDropDownList
      Anchors = [akTop, akRight]
      DropDownCount = 9
      ItemHeight = 13
      TabOrder = 1
      OnChange = cbParamTypeChange
    end
    object edValue: TEdit
      Left = 16
      Top = 80
      Width = 212
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 2
      OnChange = edValueChange
    end
    object bEdValue: TButton
      Left = 228
      Top = 80
      Width = 21
      Height = 21
      Anchors = [akTop, akRight]
      Caption = '...'
      TabOrder = 3
      OnClick = bEdValueClick
    end
    object cbNullValue: TCheckBox
      Left = 16
      Top = 124
      Width = 13
      Height = 17
      TabOrder = 4
      OnClick = cbNullValueClick
    end
    object edSize: TEdit
      Left = 192
      Top = 121
      Width = 58
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 5
      OnChange = edSizeChange
    end
  end
end
