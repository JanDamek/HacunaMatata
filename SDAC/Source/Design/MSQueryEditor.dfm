inherited MSQueryEditorForm: TMSQueryEditorForm
  Width = 599
  Caption = 'MSQueryEditorForm'
  PixelsPerInch = 96
  TextHeight = 13
  inherited BtnPanel: TPanel
    Width = 591
    inherited imCorner: TImage
      Left = 579
    end
    inherited btOk: TBitBtn
      Left = 427
    end
    inherited btCancel: TBitBtn
      Left = 508
    end
    inherited btExecute: TButton
      Width = 99
    end
    inherited btnDataEditor: TBitBtn
      Left = 110
      Width = 99
    end
    object btQueryAnalyzer: TBitBtn
      Left = 212
      Top = 8
      Width = 99
      Height = 25
      Anchors = [akLeft, akBottom]
      Caption = 'Query Analyzer'
      TabOrder = 4
      OnClick = btQueryAnalyzerClick
    end
    object btManagementStudio: TBitBtn
      Left = 314
      Top = 8
      Width = 99
      Height = 25
      Anchors = [akLeft, akBottom]
      Caption = 'Management Studio'
      TabOrder = 5
      OnClick = btManagementStudioClick
    end
  end
  inherited PageControl: TPageControl
    Width = 575
  end
end
