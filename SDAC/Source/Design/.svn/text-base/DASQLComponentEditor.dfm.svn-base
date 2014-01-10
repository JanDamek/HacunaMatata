inherited DASQLEditorForm: TDASQLEditorForm
  PixelsPerInch = 96
  TextHeight = 13
  inherited BtnPanel: TPanel
    inherited btOk: TBitBtn
      TabOrder = 1
    end
    inherited btCancel: TBitBtn
      TabOrder = 2
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
  end
  inherited PageControl: TPageControl
    ActivePage = shSQL
    object shSQL: TTabSheet
      Caption = 'S&QL'
    end
    object shParameters: TTabSheet
      Caption = '&Parameters'
      ImageIndex = 1
    end
    object shMacros: TTabSheet
      Caption = '&Macros'
      ImageIndex = 2
    end
    object shGeneratorSPC: TTabSheet
      Caption = 'S&tored Proc Call Generator'
      ImageIndex = 3
    end
  end
end
