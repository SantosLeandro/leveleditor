unit layer;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, texture, gameobject;

type
  TIntegerArray = array of array of integer;
  TArrayGameObject = array of TGameObject;


  TLayer = class
  protected
    FName: string;
    FData: TIntegerArray;

    FTexture: TTexture;
    FWidth: integer;
    FHeight: integer;
    FGameObject: array of TGameObject;
    GameObjectIndex: integer;
  public
    constructor Create(Texture: TTexture; arr: TIntegerArray; Name: string);
    constructor Create(Texture: TTexture; strData: string; w: integer;
      h: integer; Name: string);
    procedure SetData(s: string);
    procedure RemoveGameObject(x: integer; y:integer);
    procedure AddGameObject(x: integer; y:integer; name:string);
    procedure AddGameObject(x: integer; y:integer;w:integer;h:integer; name:string);
    property Name: string read FName write FName;
    property Data: TIntegerArray read FData write FData;
    property Texture: TTexture read FTexture write FTexture;
    property GameObject: TArrayGameObject read FGameObject write FGameObject;
    function GetDataToString(): string;

  end;

implementation

procedure TLayer.RemoveGameObject(x: integer; y:integer);
var
  i: integer;
  Go: TGameObject;
begin
  for i := 0 to GameObjectIndex do
  begin
    if (GameObject[i] <> nil) and
    ( x >= GameObject[i].x) and (x <= (GameObject[i].w + GameObject[i].x)) and
    (y >= GameObject[i].y) and (y <= (GameObject[i].h + GameObject[i].y)) then
    begin
       Go := GameObject[GameObjectIndex - 1];
       GameObject[i].Free;
       GameObject[i] := Go;
       GameObject[GameObjectIndex - 1] := nil;
       GameObjectIndex := GameObjectIndex - 1;
    end;
  end;
end;

procedure TLayer.AddGameObject(x: integer; y:integer; name:string);
begin
    FGameObject[GameObjectIndex] := TGameObject.Create(x,y,name);
    Inc(GameObjectIndex,1);
end;
procedure TLayer.AddGameObject(x: integer; y:integer;w:integer;h:integer; name:string);
begin
    FGameObject[GameObjectIndex] := TGameObject.Create(x,y,w,h,name);
    Inc(GameObjectIndex,1);
end;

constructor TLayer.Create(Texture: TTexture; arr: TIntegerArray; Name: string);
begin
  FTexture := texture;
  FName := Name;
  FData := arr;
  FWidth := High(arr[0]);
  FHeight := High(arr);
  SetLength(FGameObject, 1000);
  GameObjectIndex := 0;
end;

constructor TLayer.Create(Texture: TTexture; strData: string; w: integer;
  h: integer; Name: string);
var
  i: integer;
begin
  FTexture := texture;
  FName := Name;
  SetLength(FData, h);
  for i := 0 to High(FData) do
  begin
    SetLength(FData[i], w);
  end;
  setData(strData);
  FWidth := w;
  FHeight := h;
  SetLength(FGameObject, 1000);
  GameObjectIndex := 0;
end;

procedure TLayer.SetData(s: string);
var
  i, j, sum: integer;
  str: TStringArray;
begin
  sum := 0;
  str := s.Split(',');
  for i := Low(self.FData) to High(self.FData) do
  begin
    for j := Low(self.FData[i]) to High(self.FData[i]) do
    begin
      if (sum < Length(str)) then
      begin
        self.FData[i][j] := StrToInt(str[sum]);
      end
      else
      begin
        self.FData[i][j] := -1;
      end;
      sum := sum + 1;
    end;
  end;
end;

function TLayer.GetDataToString(): string;
var
  i,j: integer;
  dataStr: String;
  tmpStr: String;
begin
  dataStr:='';
  for i:= 0 to High(self.FData) do
  begin
    for j:= 0 to High(self.FData[i]) do
    begin
         tmpStr := IntToStr(self.FData[i][j]);
         if(dataStr='') then
           dataStr := concat(dataStr,tmpStr)
         else
           dataStr := concat(dataStr,',',tmpStr);
    end;
  end;
  //showMessage(dataStr);
  result := dataStr;
end;

end.
