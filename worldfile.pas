unit WorldFile;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, level,fpjson, jsonparser, jsonConf, layer, Dialogs, fgl, texture, gameobject, world, room;

type
  TArrayGameObject = array of TGameObject;
  TWorldFile = class
    private
      Dict: TStringList;
      Sprites: Array of TSprite;
    public
      function GetSprite(name: string): TSprite;
      function Load(filename: string): TWorld;
      function LoadGameObject(filename: string): TArrayGameObject;
      procedure Save(filename: string; World: TWorld);
  end;

implementation

function TWorldFile.GetSprite(name: string): TSprite;
var
  i: integer;
  str: string;
begin
  str := Dict.values[name];
  i:= StrToInt(Dict.values[name]);
  result := Sprites[i];
end;

function TWorldFile.LoadGameObject(filename: string): TArrayGameObject;
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

function TWorldFile.Load(filename: string): TWorld;
var
  fileStream: TFileStream;
  jData: TJSONData;
  jRoomArr: TJSONArray;
  jLayerArr: TJSONArray;
  jGameObject: TJSONArray;
  i, j, k: integer;
  World: TWorld;
  Room: TRoom;
  Layer: TLayer;
  Texture: TTexture;
  tmpData: string;
  w, h: integer;
  layerName: string;
  sprite: TSprite;
begin
  World := TWorld.Create;

  fileStream := TFileStream.Create(filename, fmOpenRead);
  jData := GetJSON(fileStream);

  jRoomArr := jData.GetPath('room') as TJSONArray;

  for i := 0 to jRoomArr.Count - 1 do
  begin
    Room := TRoom.Create(1);

    Room.Name := jRoomArr[i].GetPath('name').AsString;
    Room.Width := jRoomArr[i].GetPath('width').AsInteger;
    Room.Height := jRoomArr[i].GetPath('height').AsInteger;
    Room.Tilesize := jRoomArr[i].GetPath('tilesize').AsInteger;
    Room.X := jRoomArr[i].GetPath('x').AsInteger;
    Room.Y := jRoomArr[i].GetPath('y').AsInteger;

    jLayerArr := jRoomArr[i].GetPath('layer') as TJSONArray;

    for j := 0 to jLayerArr.Count - 1 do
    begin
      Texture := TTexture.Create;
      Texture.LoadFromFile(jLayerArr[j].GetPath('texture').AsString);

      tmpData := jLayerArr[j].GetPath('data').AsString;
      w := jLayerArr[j].GetPath('width').AsInteger;
      h := jLayerArr[j].GetPath('height').AsInteger;
      layerName := jLayerArr[j].GetPath('name').AsString;

      Layer := TLayer.Create(Texture, tmpData, w, h, layerName);
      Room.AddLayer(Layer);

      // Load GameObjects
      jGameObject := jLayerArr[j].GetPath('gameobject') as TJSONArray;

      if jGameObject <> nil then
      begin
        for k := 0 to jGameObject.Count - 1 do
        begin
          sprite := GetSprite(jGameObject[k].GetPath('name').AsString);

          Layer.AddGameObject(
            jGameObject[k].GetPath('x').AsInteger,
            jGameObject[k].GetPath('y').AsInteger,
            sprite.w,
            sprite.h,
            jGameObject[k].GetPath('name').AsString
          );
        end;
      end;
    end;

    World.AddRoom(Room);
  end;

  fileStream.Free;
  Result := World;
end;

procedure TWorldFile.Save(filename: string; World: TWorld);
   var
  i,j, k: integer;
  jObject: TJSONObject;
  jSubObj: TJSONObject;
  jProperty: TJSONObject;
  jLayerArr: TJSONArray;
  jTilesetArr: TJSONArray;
  jEntityArr: TJSONArray;
  jRoomArr: TJSONArray;
  jData: TJSONData;
  tfOut: TextFile;
  Layer: TLayer;
  Room: TRoom;
begin
  AssignFile(tfOut, filename);
  jObject := TJSONObject.Create;
  jSubObj := TJSONObject.Create;
  jProperty := TJSONObject.Create;
  jLayerArr := TJSONArray.Create;
  jTilesetArr := TJSONArray.Create;
  jEntityArr := TJSONArray.Create;
  jRoomArr :=  TJSONArray.Create;


  //Rooms

  for i := 0 to World.RoomCount - 1 do
  begin
     jLayerArr := TJSONArray.Create;
     Room := World.GetRoom(i);
     for j := 0 to Room.LayerCount - 1 do
     begin
       jEntityArr.Clear;
       Layer := Room.Layer[j];
       if (Layer <> nil) then
       begin
         for k := 0 to High(Layer.GameObject) do
         begin
          if(Layer.GameObject[k] <> nil ) then
          begin
            jEntityArr.Add(TJSONObject.Create([
                 'x',Layer.GameObject[k].X,
                 'y',Layer.GameObject[k].Y,
                 'name',Layer.GameObject[k].Name
            ]));
          end;
         end;
         //Add to json layer
         JLayerArr.Add(TJSONObject.Create([
          'name',    Layer.Name,
          'width',    High(Layer.Data[0])+1,
          'height',   High(Layer.Data)+1,
          'texture',  Layer.Texture.Name,
          'data',     Layer.GetDataToString(),
          'gameobject', jEntityArr
         ]));
       end;
     end;
     // add to json room
     JRoomArr.Add(TJSONObject.Create([
       'name',Room.Name,
       'width',Room.Width,
       'height',Room.Height,
       'tilesize',Room.Tilesize,
       'x',Room.X,
       'y',Room.Y,
       'layer',JLayerArr
     ]));
  end;

  jObject.Add('app','LevelEditor');
  jObject.Add('version','0.01');
  //jObject.Add('props',GetJSON(level.props));
  jObject.Add('room',JRoomArr);

  //jObject.Add('myarray', jArray);
  jData := jObject;

  //ShowMessage(jData.AsJSON);
  rewrite(tfOut);
  writeln(tfOut, jData.FormatJSON());
  CloseFile(tfOut);

end;

end.

