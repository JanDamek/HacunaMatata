inherited DAConnectionEditorForm: TDAConnectionEditorForm
  Left = 307
  Top = 150
  VertScrollBar.Range = 0
  ActiveControl = edUsername
  BorderStyle = bsDialog
  Caption = 'DAConnectionEditorForm'
  ClientHeight = 281
  ClientWidth = 450
  PixelsPerInch = 96
  TextHeight = 13
  inherited BtnPanel: TPanel
    Top = 256
    Width = 25
    Height = 25
    Align = alNone
    TabOrder = 2
    Visible = False
    inherited imCorner: TImage
      Left = 68
      Top = 13
    end
    inherited btOk: TBitBtn
      Left = 40
      Top = 16
    end
    inherited btCancel: TBitBtn
      Left = 40
      Top = 0
    end
  end
  object PageControl: TPageControl
    Left = 8
    Top = 8
    Width = 434
    Height = 232
    ActivePage = shConnect
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 0
    OnChange = PageControlChange
    object shConnect: TTabSheet
      Caption = 'C&onnect'
      object shRed: TShape
        Left = 24
        Top = 175
        Width = 15
        Height = 15
        Anchors = [akLeft, akBottom]
        Brush.Color = clBtnFace
        Pen.Color = clBtnShadow
        Shape = stCircle
      end
      object shYellow: TShape
        Left = 41
        Top = 175
        Width = 15
        Height = 15
        Anchors = [akLeft, akBottom]
        Brush.Color = clBtnFace
        Enabled = False
        Pen.Color = clBtnShadow
        Shape = stCircle
      end
      object shGreen: TShape
        Left = 58
        Top = 175
        Width = 15
        Height = 15
        Anchors = [akLeft, akBottom]
        Brush.Color = clBtnFace
        Enabled = False
        Pen.Color = clBtnShadow
        Shape = stCircle
      end
      object Panel: TPanel
        Left = 8
        Top = 8
        Width = 273
        Height = 150
        Anchors = [akLeft, akTop, akBottom]
        BevelInner = bvRaised
        BevelOuter = bvLowered
        TabOrder = 0
        object lbUsername: TLabel
          Left = 16
          Top = 20
          Width = 48
          Height = 13
          Caption = 'Username'
        end
        object lbPassword: TLabel
          Left = 16
          Top = 53
          Width = 46
          Height = 13
          Caption = 'Password'
        end
        object lbServer: TLabel
          Left = 16
          Top = 86
          Width = 31
          Height = 13
          Caption = 'Server'
        end
        object edServer: TComboBox
          Left = 104
          Top = 82
          Width = 153
          Height = 21
          ItemHeight = 13
          Sorted = True
          TabOrder = 2
          OnChange = edServerChange
          OnDropDown = edServerDropDown
          OnExit = edServerExit
          OnKeyUp = edServerKeyUp
        end
        object edUsername: TEdit
          Left = 104
          Top = 16
          Width = 153
          Height = 21
          TabOrder = 0
          OnChange = edUsernameChange
          OnExit = edServerExit
        end
        object edPassword: TMaskEdit
          Left = 104
          Top = 49
          Width = 153
          Height = 21
          PasswordChar = '*'
          TabOrder = 1
          OnChange = edPasswordChange
        end
      end
      object btConnect: TButton
        Left = 92
        Top = 170
        Width = 89
        Height = 25
        Anchors = [akLeft, akBottom]
        Caption = '&Connect'
        Default = True
        TabOrder = 2
        OnClick = btConnectClick
      end
      object btDisconnect: TButton
        Left = 192
        Top = 170
        Width = 89
        Height = 25
        Anchors = [akLeft, akBottom]
        Caption = '&Disconnect'
        TabOrder = 3
        OnClick = btDisconnectClick
      end
      object cbLoginPrompt: TCheckBox
        Left = 300
        Top = 8
        Width = 121
        Height = 17
        Caption = '&LoginPrompt'
        TabOrder = 1
        OnClick = cbLoginPromptClick
      end
    end
    object shInfo: TTabSheet
      Caption = '&Info'
      ImageIndex = 1
      object meInfo: TMemo
        Left = 8
        Top = 8
        Width = 409
        Height = 185
        Anchors = [akLeft, akTop, akRight, akBottom]
        ReadOnly = True
        TabOrder = 0
      end
    end
    object shAbout: TTabSheet
      Caption = '&About'
      ImageIndex = 2
      OnMouseMove = shAboutMouseMove
      object Label1: TLabel
        Left = 10
        Top = 6
        Width = 240
        Height = 23
        Caption = 'Data Access Components'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clNavy
        Font.Height = -19
        Font.Name = 'Tahoma'
        Font.Pitch = fpVariable
        Font.Style = [fsBold]
        ParentFont = False
      end
      object Label2: TLabel
        Left = 10
        Top = 68
        Width = 38
        Height = 13
        Caption = 'Version '
      end
      object lbVersion: TLabel
        Left = 56
        Top = 68
        Width = 45
        Height = 13
        Caption = '3.20.0.20'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clNavy
        Font.Height = 13
        Font.Name = 'Tahoma'
        Font.Pitch = fpVariable
        Font.Style = []
        ParentFont = False
      end
      object Label3: TLabel
        Left = 10
        Top = 92
        Width = 243
        Height = 13
        Caption = 'Copyright '#169' 1997-2012 Devart. All right reserved.'
      end
      object Label5: TLabel
        Left = 10
        Top = 116
        Width = 26
        Height = 13
        Caption = 'Web:'
      end
      object Label6: TLabel
        Left = 10
        Top = 140
        Width = 31
        Height = 13
        Caption = 'E-mail:'
      end
      object lbWeb: TLabel
        Left = 56
        Top = 116
        Width = 73
        Height = 13
        Cursor = crHandPoint
        Caption = 'www.devart.com'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlue
        Font.Height = 13
        Font.Name = 'Tahoma'
        Font.Pitch = fpVariable
        Font.Style = [fsUnderline]
        ParentFont = False
        OnClick = lbWebClick
        OnMouseMove = lbWebMouseMove
      end
      object lbMail: TLabel
        Left = 56
        Top = 140
        Width = 92
        Height = 13
        Cursor = crHandPoint
        Caption = 'support@devart.com'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlue
        Font.Height = 13
        Font.Name = 'Tahoma'
        Font.Pitch = fpVariable
        Font.Style = [fsUnderline]
        ParentFont = False
        OnClick = lbMailClick
        OnMouseMove = lbMailMouseMove
      end
      object lbIDE: TLabel
        Left = 115
        Top = 68
        Width = 54
        Height = 13
        Caption = 'for Delphi 6'
      end
      object imPeng: TImage
        Left = 376
        Top = 8
        Width = 40
        Height = 48
        Picture.Data = {
          07544269746D6170B60B0000424DB60B00000000000036040000280000002800
          000030000000010008000000000080070000120B0000120B0000000100000000
          000000000000000080000080000000808000800000008000800080800000C0C0
          C000C0DCC000F0CAA6000020400000206000002080000020A0000020C0000020
          E00000400000004020000040400000406000004080000040A0000040C0000040
          E00000600000006020000060400000606000006080000060A0000060C0000060
          E00000800000008020000080400000806000008080000080A0000080C0000080
          E00000A0000000A0200000A0400000A0600000A0800000A0A00000A0C00000A0
          E00000C0000000C0200000C0400000C0600000C0800000C0A00000C0C00000C0
          E00000E0000000E0200000E0400000E0600000E0800000E0A00000E0C00000E0
          E00040000000400020004000400040006000400080004000A0004000C0004000
          E00040200000402020004020400040206000402080004020A0004020C0004020
          E00040400000404020004040400040406000404080004040A0004040C0004040
          E00040600000406020004060400040606000406080004060A0004060C0004060
          E00040800000408020004080400040806000408080004080A0004080C0004080
          E00040A0000040A0200040A0400040A0600040A0800040A0A00040A0C00040A0
          E00040C0000040C0200040C0400040C0600040C0800040C0A00040C0C00040C0
          E00040E0000040E0200040E0400040E0600040E0800040E0A00040E0C00040E0
          E00080000000800020008000400080006000800080008000A0008000C0008000
          E00080200000802020008020400080206000802080008020A0008020C0008020
          E00080400000804020008040400080406000804080008040A0008040C0008040
          E00080600000806020008060400080606000806080008060A0008060C0008060
          E00080800000808020008080400080806000808080008080A0008080C0008080
          E00080A0000080A0200080A0400080A0600080A0800080A0A00080A0C00080A0
          E00080C0000080C0200080C0400080C0600080C0800080C0A00080C0C00080C0
          E00080E0000080E0200080E0400080E0600080E0800080E0A00080E0C00080E0
          E000C0000000C0002000C0004000C0006000C0008000C000A000C000C000C000
          E000C0200000C0202000C0204000C0206000C0208000C020A000C020C000C020
          E000C0400000C0402000C0404000C0406000C0408000C040A000C040C000C040
          E000C0600000C0602000C0604000C0606000C0608000C060A000C060C000C060
          E000C0800000C0802000C0804000C0806000C0808000C080A000C080C000C080
          E000C0A00000C0A02000C0A04000C0A06000C0A08000C0A0A000C0A0C000C0A0
          E000C0C00000C0C02000C0C04000C0C06000C0C08000C0C0A000F0FBFF00A4A0
          A000808080000000FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFF
          FF00030303030303030303030303030303030303030303030303030303030303
          0303030303030303030303030303030303030303030303030303030303030303
          0303030303030364030303030303030303030303030303030303031D1C1D1C13
          5C03030303030303030303035B1C25251D650303030303030303030303030366
          2525262F2F2F2F25130A529BA49BA49B9B9B5B5214262F2F2F26666603030303
          030303032626262F2F3737373737372F250A000000000000000000001C2F3737
          37372F2F2F030303030303262F373737373737373737372F1D00525B5B524900
          000000001C2F3737373737372F6F2F03030303032F373737373737373737372F
          6507FFFFFFFFF6079B0000001C2F37373737373737372F2F030303032F373737
          373737373737372FF6FFFFFFFFFFFFFFFF0749001C2F37373737373737373737
          2F0303032F3737373737373737373713A4F6FFFFFFFFFFFFFFFFF652132F3737
          37373737373737372F0303032F3737373737373737371C0000F7FFFFFFFFFFFF
          FFFFFF08652F37372F3737373737372F2F0303032F37373737373737372F0000
          0007FFFFFFFFFFFFFFFFFF08AE2F2F2F2F2F2F3737372F03030303032F373737
          373737373713000007FFFFFFFFFFFFFFFFFFFF086E2F2F251C1C263737772F03
          030303032F2F2F2F3737373726005208FFFFFFFFFFFFFFFFFFFFFFF6B737260A
          00000025372F2F03030303030303036F2F37372F005BF6FFFFFFF6FFFFFFFFFF
          FFFFFFFFB73726000000000A372F030303030303030303036F3737129BFFFFFF
          FFFFF6FFFFFFFFFFFFFFFFFFBF7F6D00000000002F0303030303030303030303
          531C0A52FFFFFFFFFFF6F6FFFFFFFFFFFFFFFFFFFFFFA4000000004949030303
          03030303030303030000009BFFFFFFFFFFFFF6FFFFFFFFFFFFFFFFFFFFFFF700
          0000000000030303030303030303030349004952FFFFFFFFFFFFF6FFFFFFFFFF
          FFFFFFFFFFFF0700000000000003030303030303030303039B000052FFFFFFFF
          FFFFF6FFFFFFFFFFFFFFFFFFFFFFF70000000000000303030303030303030303
          0349000008FFFFFFFFFFF6FFFFFFFFFFFFFFFFFFFFFFF7000000000000030303
          0303030303030303039B0000F7FFFFFFFFFFF6FFFFFFFFFFFFFFFFFFFFFFA400
          000000005203030303030303030303030303490052FFFFFFFFFFF6FFFFFFFFFF
          FFFFFFFFFFFF9B00000000009B03030303030303030303030303520000F7FFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFF6524949000000030303030303030303030303
          03039B000052F6FFFFFFFFFFFFFFFFFFFFFFFFFFF6F700000000005203030303
          0303030303030303030303000000F7F6FFFFFFF6FFFFFFFFFFF6F60808520000
          0000009B0303030303030303030303030303039B00005207F6FFF6F6F6FFFFF6
          08070707A4000000000049030303030303030303030303030303030349004908
          FFFFFFF6F6FFFFFFF6F6F6080000000000000303030303030303030303030303
          0303030303000007FFFFFFFFFFFFFFFFFFFFFF07000000000003030303030303
          030303030303030303030303039B005BFFFFF60808F6FFFFFFFFFFA400000000
          9B0303030303030303030303030303030303030303039B0007FF07070707F6FF
          FFFFF64900000052030303030303030303030303030303030303030303030349
          490807AD65AD0708FFFFA4000000000303030303030303030303030303030303
          030303030303030300F76526262666AE0807000052005B030303030303030303
          0303030303030303030303030303030300652E372F2E262E6E5C00525B000303
          030303030303030303030303030303030303030303030303122E37377F37372E
          2F1C000000520303030303030303030303030303030303030303030303030303
          0A263737377F3F3F37250000009B030303030303030303030303030303030303
          030303030303030349AE2F37377F3F2FB75B0000000303030303030303030303
          030303030303030303030303030303A49B52122E2E6D65009BF7000000030303
          0303030303030303030303030303030303030303030303A49B009BA400A4A400
          520700000003030303030303030303030303030303030303030303030303039B
          5BF7079B00A4F65B07A400000003030303030303030303030303030303030303
          030303030303035B0007F7000000F70807490000000303030303030303030303
          0303030303030303030303030303039B00000000000000525200000000030303
          0303030303030303030303030303030303030303030303030000000000000000
          0000000052030303030303030303030303030303030303030303030303030303
          0000000000000000000000009B03030303030303030303030303030303030303
          030303030303030300000000000000005B520000030303030303030303030303
          0303030303030303030303030303030303000000000000005B49000303030303
          030303030303030303030303030303030303030303030303035B000000000000
          0049030303030303030303030303030303030303030303030303030303030303
          0303035B49000052030303030303030303030303030303030303030303030303
          0303030303030303030303030303030303030303030303030303030303030303
          0303}
        Transparent = True
        Visible = False
      end
      object lbEdition: TLabel
        Left = 23
        Top = 31
        Width = 104
        Height = 18
        Caption = 'Standard Edition'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clNavy
        Font.Height = -15
        Font.Name = 'Tahoma'
        Font.Pitch = fpVariable
        Font.Style = []
        ParentFont = False
      end
    end
  end
  object btClose: TButton
    Left = 366
    Top = 249
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Close'
    TabOrder = 1
    OnClick = SaveClick
  end
end
