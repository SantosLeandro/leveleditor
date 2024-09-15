unit layer;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, texture;

type
  TIntegerArray = array of array of integer;

  TGameObject = class
  public
    id: integer;
    Name: string;
    x: integer;
    y: integer;
    w: integer;
    h: integer;
  end;

  TLayer = class
  protected
    FName: string;
    FData: TIntegerArray;
    FGameObject: array of TGameObject;
    FTexture: TTexture;
    FWidth: integer;
    FHeight: integer;
  public
    constructor Create(Texture: TTexture; arr: TIntegerArray; Name: string);
    constructor Create(Texture: TTexture; strData: string; w: integer;
      h: integer; Name: string);
    procedure SetData(s: string);
    property Name: string read FName write FName;
    property Data: TIntegerArray read FData write FData;
    property Texture: TTexture read FTexture write FTexture;

  end;

implementation

constructor TLayer.Create(Texture: TTexture; arr: TIntegerArray; Name: string);
begin
  FTexture := texture;
  FName := Name;
  FData := arr;
  FWidth := High(arr[0]);
  FHeight := High(arr);
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
        self.FData[i][j] := 0;
      end;
      sum := sum + 1;
    end;
  end;
end;

end.
