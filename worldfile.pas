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
  level: TWorld;
  fileStream: TFileStream;
  jData: TJSONData;
  jObject: TJSONObject;
  jSprite: TJSONObject;
  jArray: TJSONArray;
  jArrRoom: TJSONArray;
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
  Room: TRoom;
begin
  //create world
  level := TWorld.Create();

  fileStream := TFileStream.Create(filename, fmOpenRead);
  jData := GetJSON(fileStream);

  //GetRooms
  jArrRoom := jData.GetPath('room') as TJSONArray;



  jArray := jArrRoom.GetPath('layer') as TJSONArray;

  //level.Name := jData.GetPath('name').AsString;
  //level.Width := jData.GetPath('width').AsInteger;
  //level.Height := jData.GetPath('height').AsInteger;
  //level.Tilesize :=  jData.GetPath('tilesize').AsInteger;
  //for i := 0 to jArray.Count - 1 do
  //begin
  //  Texture := TTexture.Create();
  //  Texture.LoadFromFile(jArray[i].GetPath('texture').AsString);
  //  tmpData := jArray[i].GetPath('data').AsString;
  //  w := jArray[i].GetPath('width').AsInteger;
  //  h := jArray[i].GetPath('height').AsInteger;
  //  layerName := jArray[i].GetPath('name').AsString;
  //  //level.AddRoom();.Create(texture, tmpData, w, h, layerName);
  //
  //  jGameObject := jArray[i].GetPath('gameobject') as TJSONArray;
  //  if(jGameObject.Count > 0 ) then
  //  begin
  //    for j := 0 to jGameObject.count - 1 do
  //    begin
  //        try
  //           level.layer[i].AddGameObject(
  //              jGameObject[j].GetPath('x').AsInteger,
  //              jGameObject[j].GetPath('y').AsInteger,
  //              GetSprite(jGameObject[j].GetPath('name').AsString).w,
  //              GetSprite(jGameObject[j].GetPath('name').AsString).h,
  //              jGameObject[j].GetPath('name').AsString
  //           );
  //
  //        finally
  //
  //
  //        end;
  //
  //    end;
  //  end;
  //end;
  fileStream.Destroy;
  Result := level;
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

