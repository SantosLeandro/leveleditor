unit FileHelper;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, level,fpjson, jsonparser, jsonConf, layer, Dialogs, fgl, texture, gameobject;

type
  TArrayGameObject = array of TGameObject;
  TLevelFile = class
    private
      Dict: TStringList;
      Sprites: Array of TSprite;
    public
      function GetSprite(name: string): TSprite;
      function Load(filename: string): TLevel;
      function LoadGameObject(filename: string): TArrayGameObject;
      procedure Save(filename: string; Level: TLevel);
  end;

implementation

function TLevelFile.GetSprite(name: string): TSprite;
var
  i: integer;
  str: string;
begin
  str := Dict.values[name];
  i:= StrToInt(Dict.values[name]);
  result := Sprites[i];
end;

function TLevelFile.LoadGameObject(filename: string): TArrayGameObject;
var
  fileStream: TFileStream;
  jData: TJSONData;
  jObject: TJSONObject;
  jSprite: TJSONObject;
  jArray: TJSONArray;
  GameObjects: TArrayGameObject;
  x,y,w,h,i: integer;
  name: string;
begin
  Dict.free;
  Dict :=  TStringList.Create;
  fileStream := TFileStream.Create(filename, fmOpenRead);
  jData := GetJSON(fileStream);
  jArray := jData.GetPath('gameobjects') as TJSONArray;
  SetLength(GameObjects,jArray.Count);
  SetLength(Sprites, jArray.Count);
  for i := 0 to jArray.Count - 1 do
  begin
    x := jArray[i].GetPath('x').AsInteger;
    y := jArray[i].GetPath('y').AsInteger;
    w := jArray[i].GetPath('width').AsInteger;
    h := jArray[i].GetPath('height').AsInteger;
    name := jArray[i].GetPath('name').AsString;
    GameObjects[i] := TGameObject.Create(w,h,name);
    Sprites[i].x := x;
    Sprites[i].y := y;
    Sprites[i].w := w;
    Sprites[i].h := h;
    Dict.values[name] := i.toString;
  end;
  result := GameObjects;
end;

function TLevelFile.Load(filename: string): TLevel;
var
  level: TLevel;
  fileStream: TFileStream;
  jData: TJSONData;
  jObject: TJSONObject;
  jSprite: TJSONObject;
  jArray: TJSONArray;
  jGameObject: TJSONArray;
  entityArray: TJSONArray;
  P: TJSONParser;
  i, j, z: integer;
  strData: TStringArray;
  tmpData: string;
  Texture: TTexture;
  w, h: integer;
  layerName: string;
  tmpSprite: TSprite;
begin
  fileStream := TFileStream.Create(filename, fmOpenRead);
  jData := GetJSON(fileStream);
  jArray := jData.GetPath('layer') as TJSONArray;
  level := TLevel.Create(jArray.Count);
  level.Name := jData.GetPath('name').AsString;
  level.Width := jData.GetPath('width').AsInteger;
  level.Height := jData.GetPath('height').AsInteger;
  level.Tilesize :=  jData.GetPath('tilesize').AsInteger;
  //level.Props:= jData.GetPath('props').AsJSON;
  for i := 0 to jArray.Count - 1 do
  begin
    Texture := TTexture.Create();
    Texture.LoadFromFile(jArray[i].GetPath('texture').AsString);
    tmpData := jArray[i].GetPath('data').AsString;
    w := jArray[i].GetPath('width').AsInteger;
    h := jArray[i].GetPath('height').AsInteger;
    layerName := jArray[i].GetPath('name').AsString;
    level.layer[i] := TLayer.Create(texture, tmpData, w, h, layerName);

    jGameObject := jArray[i].GetPath('gameobject') as TJSONArray;
    if(jGameObject.Count > 0 ) then
    begin
      for j := 0 to jGameObject.count - 1 do
      begin
          try
             level.layer[i].AddGameObject(
                jGameObject[j].GetPath('x').AsInteger,
                jGameObject[j].GetPath('y').AsInteger,
                GetSprite(jGameObject[j].GetPath('name').AsString).w,
                GetSprite(jGameObject[j].GetPath('name').AsString).h,
                jGameObject[j].GetPath('name').AsString
             );

          finally


          end;

      end;
    end;
  end;
  fileStream.Destroy;
  Result := level;
end;

procedure TLevelFile.Save(filename: string; Level: TLevel);
   var
  i,j: integer;
  jObject: TJSONObject;
  jSubObj: TJSONObject;
  jProperty: TJSONObject;
  jLayerArr: TJSONArray;
  jTilesetArr: TJSONArray;
  jEntityArr: TJSONArray;
  jData: TJSONData;
  tfOut: TextFile;
begin
  AssignFile(tfOut, filename);
  jObject := TJSONObject.Create;
  jSubObj := TJSONObject.Create;
  jProperty := TJSONObject.Create;
  jLayerArr := TJSONArray.Create;
  jTilesetArr := TJSONArray.Create;
  jEntityArr := TJSONArray.Create;

  // Level Property
  //jProperty.Add('width',      Level.Width);
  //jProperty.Add('height',     Level.Height);
  //jProperty.Add('name',       Level.Name);
  //jProperty.Add('tileWidth',  Level.TileWidth);
  //jProperty.Add('tileHeight', Level.TileHeight);

  // Tilesets
  //for i := 0 to level.Tileset.Count - 1 do
  //begin
  //  jTilesetArr.Add(
  //    TJSONObject.Create([
  //     'texture', level.GetTileset(i).BitmapFilename,
  //     'name', level.GetTileset(i).Name,
  //     'tilesize', level.GetTileset(i).Tilesize
  //    ])
  //  );
  //end;


  //  Layers
  for i := 0 to level.LayerCount do
  begin
      jEntityArr.Clear;

       for j := 0 to High(level.Layer[i].GameObject) do
       begin
        if(level.Layer[i].GameObject[j] <> nil ) then
        begin
          jEntityArr.Add(TJSONObject.Create([
               'x',level.Layer[i].GameObject[j].X,
               'y',level.Layer[i].GameObject[j].Y,
               'name',level.Layer[i].GameObject[j].Name
          ]));
        end;
       end;

      JLayerArr.Add(TJSONObject.Create([
        'name',     level.Layer[i].Name,
        'width',    High(level.Layer[i].Data[0])+1,
        'height',   High(level.Layer[i].Data)+1,
        'texture',  level.Layer[i].Texture.Name,
        'data',     level.Layer[i].GetDataToString(),
        'gameobject', jEntityArr
      ]));
    //  end
    //else
    //begin
    //  jEntityArr.Clear;
    //  for j := 0 to level.GetLayer(i).Entity.Count - 1 do
    //  begin
    //     jEntityArr.Add(TJSONObject.Create([
    //       'x',level.GetLayer(i).GetEntity(j).X,
    //       'y',level.GetLayer(i).GetEntity(j).Y,
    //       'name',level.GetLayer(i).GetEntity(j).Name
    //     ]));
    //  end;
    //  JLayerArr.Add(TJSONObject.Create([
    //    'name',     level.GetLayer(i).Name,
    //    'mode',     level.GetLayer(i).Mode,
    //    'width',    level.GetLayer(i).Width,
    //    'height',   level.GetLayer(i).Height,
    //    'visible', ' true',
    //    'data',     jEntityArr
    //  ]));
    //end;
  end;

  jObject.Add('app','LevelEditor');
  jObject.Add('version','0.01');
  jObject.Add('tilesize',Level.Tilesize);
  jObject.Add('width',Level.Width);
  jObject.Add('height',Level.Height);
  jObject.Add('name',Level.Name);
  jObject.Add(TJSONObject.Create(['props',Level.Props]));
  jObject.Add('layer',JLayerArr);

  //{meta}
  //jObject.Add('meta', TJSONObject.Create(['app', self.APP_NAME, 'version', self.APP_VER]));
  //{add level}
  //jObject.Add('level', TJSONObject.Create(['property', jProperty,
  //  'camera', GetJSON(Level.Camera), 'tilesets', jTilesetArr, 'layers', jLayerArr]));

  //jObject.Add('myarray', jArray);
  jData := jObject;

  //ShowMessage(jData.AsJSON);
  rewrite(tfOut);
  writeln(tfOut, jData.FormatJSON());
  CloseFile(tfOut);

end;

end.

