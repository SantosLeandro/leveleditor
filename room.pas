unit room;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, layer, texture;

type
  TRoom = class
  private
    FName: string;
    FX: Integer;
    FY: Integer;

    FWidth: Integer;
    FHeight: Integer;

    FTilesize: Integer;
    FScale: Integer;

    FLayers: array of TLayer;

    procedure SetLayer(Index: Integer; ALayer: TLayer);
    function GetLayer(Index: Integer): TLayer;

  public
    constructor Create(ALayerCount: Integer);
    destructor Destroy; override;

    function LayerCount: Integer;

    procedure AddLayer(ALayer: TLayer);
    procedure RemoveLayer(Index: Integer);

    property Name: string read FName write FName;

    property X: Integer read FX write FX;
    property Y: Integer read FY write FY;

    property Width: Integer read FWidth write FWidth;
    property Height: Integer read FHeight write FHeight;

    property Tilesize: Integer read FTilesize write FTilesize;
    property Scale: Integer read FScale write FScale;

    property Layer[Index: Integer]: TLayer read GetLayer write SetLayer;
  end;

implementation

constructor TRoom.Create(ALayerCount: Integer);
begin
  SetLength(FLayers, ALayerCount);
  FScale := 1;
end;

destructor TRoom.Destroy;
var
  i: Integer;
begin
  for i := 0 to High(FLayers) do
    if Assigned(FLayers[i]) then
      FLayers[i].Free;

  inherited Destroy;
end;

function TRoom.LayerCount: Integer;
begin
  Result := Length(FLayers);
end;

procedure TRoom.SetLayer(Index: Integer; ALayer: TLayer);
begin
  FLayers[Index] := ALayer;
end;

function TRoom.GetLayer(Index: Integer): TLayer;
begin
  Result := FLayers[Index];
end;

procedure TRoom.AddLayer(ALayer: TLayer);
var
  i: Integer;
begin
  for i := 0 to High(FLayers) do
    if FLayers[i] = nil then
    begin
      FLayers[i] := ALayer;
      Exit;
    end;

  SetLength(FLayers, Length(FLayers) + 1);
  FLayers[High(FLayers)] := ALayer;
end;

procedure TRoom.RemoveLayer(Index: Integer);
var
  i: Integer;
begin
  if (Index < 0) or (Index > High(FLayers)) then Exit;

  FLayers[Index].Free;

  for i := Index to High(FLayers) - 1 do
    FLayers[i] := FLayers[i + 1];

  SetLength(FLayers, Length(FLayers) - 1);
end;

end.
