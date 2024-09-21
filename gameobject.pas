unit gameobject;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

type
   TSprite = record
     x,y,w,h:integer;
   end;

   TGameObject = class
  public
    id: integer;
    Name: string;
    x: integer;
    y: integer;
    w: integer;
    h: integer;
    Sprite: TSprite;
    constructor Create;
    constructor Create(px:integer;py:integer; pw:integer; ph: integer; n:string);
    constructor Create(px:integer;py:integer;n:string);
  end;

implementation
constructor TGameObject.Create;
begin

end;

constructor TGameObject.Create(px:integer;py:integer; pw:integer; ph: integer; n:string);
begin
  x := px;
  y := py;
  w := pw;
  h := ph;
  name := n;
end;

constructor TGameObject.Create(px:integer;py:integer;n:string);
begin
  self.x:=px;
  self.y:=py;
  name:=n;
end;

end.

