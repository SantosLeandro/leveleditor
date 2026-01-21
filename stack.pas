unit stack;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

const MAX = 100;

type
  TCommand = class
    public
      Layer, Tile, w, h: integer;
      constructor Create(l,t,w1,h1: integer);
  end;

  TStack = class
    private
      FData: array[1..MAX] of TCommand;
      FTop: integer;
    public
      constructor Create();
      procedure Push(Command: TCommand);
      function IsEmpty(): Boolean;
      function IsFull(): Boolean;
      function Pop(): TCommand;
  end;

implementation
constructor TCommand.Create(l,t,w1,h1: integer);
begin
  layer:= l;
  tile:=t;
  w:= w1;
  h:= h1;
end;
constructor TStack.Create();
begin
  FTop := 0;
end;


function TStack.IsEmpty(): Boolean;
begin
  result := (Ftop = 0);
end;

function TStack.IsFull(): Boolean;
begin
  result := (Ftop = MAX);
end;

procedure TStack.Push(Command: TCommand);
var
  i: integer;
begin
  if not IsFull() then
  begin
    Inc(FTop);
    FData[FTop] := Command;
  end
  else
  begin
    for i := 1 to FTop - 1 do
      FData[i] := FData[i + 1];

    Dec(FTop);
    FData[FTop] := Command;
  end;
end;

function TStack.Pop(): TCommand;
var
  C: TCommand;
begin
  if not IsEmpty() then
  begin
    C := FData[FTop];
    Dec(FTop);
    result:= c;
  end;
end;


end.

