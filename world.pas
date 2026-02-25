unit world;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, fgl, room;

type
  TRoomMap = specialize TFPGMap<string, TRoom>;

  TWorld = class
  private
    FRooms: TRoomMap;

    function MakeKey(X, Y: Integer): string;
  public
    constructor Create;
    destructor Destroy; override;

    procedure AddRoom(Room: TRoom);
    function GetRoomByName(const Name: string): TRoom;
    function GetRoom(X, Y: Integer): TRoom;
    function GetRoom(i: Integer): TRoom;

    procedure RemoveRoom(const Name: string);

    function RoomCount: Integer;

    property Rooms: TRoomMap read FRooms;
  end;

implementation

constructor TWorld.Create;
begin
  FRooms := TRoomMap.Create;
  FRooms.Sorted := False;
end;

destructor TWorld.Destroy;
var
  i: Integer;
begin
  for i := 0 to FRooms.Count - 1 do
    FRooms.Data[i].Free;

  FRooms.Free;
  inherited Destroy;
end;

function TWorld.MakeKey(X, Y: Integer): string;
begin
  Result := IntToStr(X) + '_' + IntToStr(Y);
end;

procedure TWorld.AddRoom(Room: TRoom);
var
  Key: string;
begin
  Key := MakeKey(Room.X, Room.Y);
  FRooms.Add(Key, Room);
end;

function TWorld.GetRoomByName(const Name: string): TRoom;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to FRooms.Count - 1 do
    if FRooms.Data[i].Name = Name then
      Exit(FRooms.Data[i]);
end;

function TWorld.GetRoom(X, Y: Integer): TRoom;
var
  i: Integer;
  R: TRoom;
begin
  Result := nil;

  for i := 0 to FRooms.Count - 1 do
  begin
    R := FRooms.Data[i];

    if (X * R.TileSize >= R.X) and (X * R.TileSize < R.X + R.Width * R.Tilesize) and
       (Y * R.TileSize >= R.Y) and (Y * R.TileSize < R.Y + R.Height * R.Tilesize) then
    begin
      Exit(R);
    end;
  end;
end;

function TWorld.GetRoom(i: Integer): TRoom;
var
  index:integer;
begin
  Result := FRooms.Data[i];
end;

procedure TWorld.RemoveRoom(const Name: string);
var
  i: Integer;
begin
  for i := 0 to FRooms.Count - 1 do
    if FRooms.Data[i].Name = Name then
    begin
      FRooms.Data[i].Free;
      FRooms.Delete(i);
      Break;
    end;
end;

function TWorld.RoomCount: Integer;
begin
  Result := FRooms.Count;
end;

end.
