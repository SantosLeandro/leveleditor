unit level;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, fpjson, jsonparser, jsonConf, layer, Dialogs, fgl, texture;

type
  TIntegerArray = array of array of integer;

  TLevel = class
  protected
    FData: array of TLayer;
    FLayers: array of TIntegerArray;
    FTilesize: integer;
    FName: string;
    FWidth: integer;
    FHeight: integer;
    procedure SetData(Index: integer; l: TLayer);
    function GetData(Index: integer): TLayer;

  public
    constructor Create(numLayers: integer);
    procedure AddLayer(w: integer; h: integer);
    procedure SwapLayer(First: integer; second: integer);
    procedure MoveUp(index: integer);

    procedure InsertTile(index: integer; w: integer; h: integer; tile: integer);
    function LayerCount(): integer;
    function IsEmpty(index: integer): boolean;
    function GetLayer(Index: integer): TIntegerArray;
    procedure SetLayer(Index: integer; arr: TIntegerArray);
    //property Layer[index: integer]: TIntegerArray read GetLayer write SetLayer;
    property Tilesize: integer read FTilesize write FTilesize;
    property Layer[index: integer]: TLayer read GetData write SetData;
    property Name: string read FName write FName;
    property Width: integer read  FWidth write FWidth;
    property Height: integer read  FHeight write FHeight;
    function Load(filename: string): TLevel;
  end;

implementation

constructor TLevel.Create(numLayers: integer);
begin
  SetLength(FLayers, numLayers);
  SetLength(FData, numLayers);

end;

function TLevel.LayerCount(): integer;
begin
  Result := High(FData);
end;

procedure TLevel.SetData(Index: integer; l: TLayer);
begin
  FData[index] := l;
end;

function TLevel.GetData(Index: integer): TLayer;
begin
  Result := FData[index];
end;

function TLevel.GetLayer(Index: integer): TIntegerArray;
begin
  Result := FLayers[index];
end;

procedure TLevel.SetLayer(Index: integer; arr: TIntegerArray);
begin
  FLayers[index] := arr;
end;

procedure TLevel.AddLayer(w: integer; h: integer);
begin
end;

function TLevel.IsEmpty(index: integer): boolean;
var
  i: integer;
begin
  Result := False;
end;

procedure TLevel.MoveUp(index: integer);
var
  temp: TIntegerArray;
begin
  if (index > 0) then
  begin
    temp := FLayers[index];
    FLayers[index] := FLayers[index - 1];
    FLayers[index - 1] := temp;
  end;
end;

procedure TLevel.SwapLayer(First: integer; second: integer);
var
  tempFirst, tempSecond: TIntegerArray;
begin
  tempFirst := FLayers[First];
  tempSecond := FLayers[second];
  FLayers[First] := tempSecond;
  FLayers[second] := tempFirst;
end;

procedure TLevel.InsertTile(index: integer; w: integer; h: integer; tile: integer);
begin
  FLayers[index][h][w] := tile;
end;

function TLevel.Load(filename: string): TLevel;
var
  level: TLevel;
  fileStream: TFileStream;
  jData: TJSONData;
  jObject: TJSONObject;
  jArray: TJSONArray;
  jEntity: TJSONArray;
  entityArray: TJSONArray;
  P: TJSONParser;
  i, j, z: integer;
  strData: TStringArray;
  tmpData: string;
  Texture: TTexture;
  w, h: integer;
  layerName: string;
begin
  fileStream := TFileStream.Create(filename, fmOpenRead);
  jData := GetJSON(fileStream);

  jArray := jData.GetPath('layer') as TJSONArray;

  level := TLevel.Create(jArray.Count);
  level.Name := jData.GetPath('name').AsString;
  level.Width := jData.GetPath('width').AsInteger;
  level.Height := jData.GetPath('height').AsInteger;
  level.Tilesize :=  jData.GetPath('tilesize').AsInteger;
  for i := 0 to jArray.Count - 1 do
  begin
    Texture := TTexture.Create();
    Texture.LoadFromFile(jArray[i].GetPath('texture').AsString);
    tmpData := jArray[i].GetPath('data').AsString;
    w := jArray[i].GetPath('width').AsInteger;
    h := jArray[i].GetPath('height').AsInteger;
    layerName := jArray[i].GetPath('name').AsString;
    level.layer[i] := TLayer.Create(texture, tmpData, w, h, layerName);
    Texture.Free;
    //level.layer[i].SetData(tmpData);
  end;
  Result := level;
end;

end.
