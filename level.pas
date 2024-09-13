unit level;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, layer;

type
  TIntegerArray = array of array of Integer;
  TLevel = class
    protected
     FData: array of TLayer;
     FLayers: array of TIntegerArray;
     FTilesize: integer;
     procedure SetData(Index: integer; l: TLayer);
     function GetData(Index: integer): TLayer;
    public
     constructor Create(numLayers:integer);
     procedure AddLayer(w:integer;h:integer);
     procedure SwapLayer(first: integer; second: integer);
     procedure MoveUp(index: integer);

     procedure InsertTile(index: integer; w: integer; h:integer; tile: integer);
     function IsEmpty(index: integer): boolean;
     function GetLayer(Index: Integer): TIntegerArray;
     procedure SetLayer(Index: Integer; arr: TIntegerArray);
     //property Layer[index: integer]: TIntegerArray read GetLayer write SetLayer;
     property Tilesize: integer read FTilesize write FTilesize;
     property Layer[index: integer]: TLayer read GetData write SetData;
  end;

implementation

constructor TLevel.Create(numLayers:integer);
begin
  SetLength(FLayers, numLayers);
  SetLength(FData, numLayers);
end;

procedure TLevel.SetData(Index: integer; l: TLayer);
begin
   FData[index] := l;
end;

function TLevel.GetData(Index: integer): TLayer;
begin
  result := FData[index];
end;

function TLevel.GetLayer(Index: Integer): TIntegerArray;
begin
    result := FLayers[index];
end;

procedure TLevel.SetLayer(Index: Integer; arr: TIntegerArray);
begin
    FLayers[index] := arr;
end;

procedure TLevel.AddLayer(w:integer;h:integer);
begin
end;

function TLevel.IsEmpty(index: integer): boolean;
var
  i: integer;
begin
  result := false;
end;

procedure TLevel.MoveUp(index: integer);
var
  temp: TIntegerArray;
begin
  if(index > 0 ) then
  begin
       temp := FLayers[index];
       FLayers[index] := FLayers[index - 1];
       FLayers[index - 1] := temp;
  end;
end;

procedure TLevel.SwapLayer(first: integer; second: integer);
var
  tempFirst, tempSecond: TIntegerArray;
begin
  tempFirst := FLayers[first];
  tempSecond := FLayers[second];
  FLayers[first] := tempSecond;
  FLayers[second] := tempFirst;
end;

procedure TLevel.InsertTile(index: integer; w: integer; h:integer; tile: integer);
begin
   FLayers[index][h][w] := tile;
end;

end.

