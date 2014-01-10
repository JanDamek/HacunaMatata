inherited MSCompactConnectionEditorForm: TMSCompactConnectionEditorForm
  ActiveControl = nil
  Caption = 'MSCompactConnectionEditorForm'
  ClientHeight = 242
  PixelsPerInch = 96
  TextHeight = 13
  inherited PageControl: TPageControl
    Height = 193
    inherited shConnect: TTabSheet
      inherited shRed: TShape
        Top = 136
      end
      inherited shYellow: TShape
        Top = 136
      end
      inherited shGreen: TShape
        Top = 136
      end
      inherited Panel: TPanel
        Height = 111
        inherited lbUsername: TLabel
          Top = 116
          Visible = False
        end
        inherited lbPassword: TLabel
          Top = 69
        end
        inherited lbServer: TLabel
          Top = 116
          Visible = False
        end
        inherited lbDatabase: TLabel
          Top = 30
        end
        inherited edServer: TComboBox
          Top = 112
          Visible = False
        end
        inherited edUsername: TEdit
          Top = 112
          Visible = False
        end
        inherited edPassword: TMaskEdit
          Top = 65
        end
        inherited edDatabase: TComboBox
          Top = 26
        end
      end
      inherited btConnect: TButton
        Top = 131
      end
      inherited btDisconnect: TButton
        Top = 131
      end
      inherited rgAuth: TRadioGroup
        Visible = False
      end
    end
    inherited shInfo: TTabSheet
      inherited meInfo: TMemo
        Height = 146
      end
    end
  end
  inherited btClose: TButton
    Top = 210
  end
  inherited btQueryAnalyzer: TButton
    Left = 112
    Top = 210
    Visible = False
  end
  inherited btManagementStudio: TButton
    Left = 8
    Top = 210
  end
end
