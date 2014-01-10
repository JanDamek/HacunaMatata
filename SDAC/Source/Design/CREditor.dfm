object CREditorForm: TCREditorForm
  Left = 302
  Top = 181
  Width = 511
  Height = 73
  Caption = 'CREditorForm'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = 13
  Font.Name = 'Tahoma'
  Font.Pitch = fpVariable
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = True
  Position = poDefaultPosOnly
  Scaled = False
  OnCloseQuery = FormCloseQuery
  OnHide = FormHide
  OnKeyPress = FormKeyPress
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object BtnPanel: TPanel
    Left = 0
    Top = 5
    Width = 503
    Height = 41
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    object imCorner: TImage
      Left = 491
      Top = 29
      Width = 12
      Height = 12
      Anchors = [akRight, akBottom]
      Picture.Data = {
        07544269746D6170D6000000424DD60000000000000076000000280000000C00
        00000C0000000100040000000000600000000000000000000000100000001000
        0000000000000000800000800000008080008000000080008000808000008080
        8000C0C0C0000000FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFF
        FF008778F778F77800008F778F778F77000088F778F778F70000888F778F778F
        00008888F778F778000088888F778F770000888888F778F700008888888F778F
        000088888888F7780000888888888F7700008888888888F70000888888888888
        0000}
      Transparent = True
    end
    object btOk: TBitBtn
      Left = 339
      Top = 8
      Width = 75
      Height = 25
      Anchors = [akRight, akBottom]
      Caption = 'OK'
      Default = True
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Pitch = fpVariable
      Font.Style = []
      ParentFont = False
      TabOrder = 0
      OnClick = SaveClick
    end
    object btCancel: TBitBtn
      Left = 420
      Top = 8
      Width = 75
      Height = 25
      Anchors = [akRight, akBottom]
      Cancel = True
      Caption = 'Cancel'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Pitch = fpVariable
      Font.Style = []
      ParentFont = False
      TabOrder = 1
      OnClick = CloseClick
      NumGlyphs = 2
    end
  end
end
