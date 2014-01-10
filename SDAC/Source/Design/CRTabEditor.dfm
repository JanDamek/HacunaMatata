inherited CRTabEditorForm: TCRTabEditorForm
  Left = 295
  Top = 204
  Width = 512
  Height = 338
  HorzScrollBar.Visible = False
  VertScrollBar.Visible = False
  BorderIcons = [biSystemMenu]
  Constraints.MinHeight = 332
  Constraints.MinWidth = 512
  Font.Height = -11
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  inherited BtnPanel: TPanel
    Top = 270
    Width = 504
    TabOrder = 1
    inherited imCorner: TImage
      Left = 492
    end
    inherited btOk: TBitBtn
      Left = 340
    end
    inherited btCancel: TBitBtn
      Left = 421
    end
  end
  object PageControl: TPageControl
    Left = 8
    Top = 8
    Width = 488
    Height = 255
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 0
    TabStop = False
    OnChange = PageControlChange
    OnChanging = PageControlChanging
  end
end
