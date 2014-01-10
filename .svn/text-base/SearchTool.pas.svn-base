unit SearchTool;

interface

uses
  System.SysUtils, System.Classes, FMX.Types, FMX.Controls, HeGrid;

type
  TSearchTool = class(TPanel)
  private
    { Private declarations }
    fHeGrid: THeGrid;
    procedure setHeGrid(const Value: THeGrid);
  protected
    { Protected declarations }
  public
    { Public declarations }
  published
    { Published declarations }
    constructor Create(AOwner: TComponent); override;

    property HeGrid: THeGrid read fHeGrid write setHeGrid;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('HacunaMatata', [TSearchTool]);
end;

{ TSearchTool }

constructor TSearchTool.Create(AOwner: TComponent);
begin
  inherited;
end;

procedure TSearchTool.setHeGrid(const Value: THeGrid);
begin
  fHeGrid := Value;
end;

initialization

RegisterFmxClasses([TSearchTool]);

end.
