unit FileHelper;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, level,fpjson, jsonparser, jsonConf, layer, Dialogs, fgl, texture;

type

  TLevelFile = class
    public
      function Load(filename: string): TLevel;
  end;

implementation

function TLevelFile.Load(filename: string): TLevel;
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
  end;
  Result := level;
end;

end.

