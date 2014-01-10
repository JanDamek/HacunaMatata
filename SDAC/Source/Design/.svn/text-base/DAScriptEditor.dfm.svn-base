inherited DAScriptEditorForm: TDAScriptEditorForm
  PixelsPerInch = 96
  TextHeight = 13
  inherited BtnPanel: TPanel
    inherited btOk: TBitBtn
      TabOrder = 4
    end
    inherited btCancel: TBitBtn
      TabOrder = 5
    end
    object btExecute: TButton
      Left = 8
      Top = 8
      Width = 80
      Height = 25
      Anchors = [akLeft, akBottom]
      Caption = 'Execute'
      TabOrder = 0
      OnClick = btExecuteClick
    end
    object cbDebug: TCheckBox
      Left = 269
      Top = 12
      Width = 62
      Height = 17
      Caption = '&Debug'
      TabOrder = 3
      OnClick = cbDebugClick
    end
    object btOpen: TButton
      Left = 94
      Top = 8
      Width = 80
      Height = 25
      Hint = 'Load from file'
      Caption = '&Load...'
      TabOrder = 1
      OnClick = btOpenClick
    end
    object btSave: TButton
      Left = 180
      Top = 8
      Width = 80
      Height = 25
      Hint = 'Save to file'
      Caption = '&Save...'
      TabOrder = 2
      OnClick = btSaveClick
    end
  end
  inherited PageControl: TPageControl
    ActivePage = shSQL
    object shSQL: TTabSheet
      Caption = 'S&QL'
    end
    object shMacros: TTabSheet
      Caption = '&Macros'
      ImageIndex = 2
    end
  end
end
