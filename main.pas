unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Menus, OpenGLContext,
  GL, glu, ExtCtrls, renderer, texture, CheckLst, StdCtrls, Level, Layer;

type

  { TFormMain }

  TFormMain = class(TForm)
    GLBox: TOpenGLControl;
    Label1: TLabel;
    ListBox1: TListBox;
    MainMenu: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    LeftPanel: TPanel;
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure GLBoxClick(Sender: TObject);
    procedure GLBoxKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure GLBoxMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure GLBoxMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer
      );
    procedure GLBoxPaint(Sender: TObject);
  private

  public
    Level: TLevel;
    Texture: TTexture;
    Renderer: TRenderer;
    tileSize: integer;
    Tilemap : TIntegerArray;
    LayerId: integer;
    MouseX, MouseY: integer;
    OffsetX, OffsetY: integer;
    function getTestMap: TIntegerArray;

  end;

var
  FormMain: TFormMain;

implementation

{$R *.lfm}

function TFormMain.getTestMap: TIntegerArray;
var
  //  tiles: TIntegerArray = (
  //  (6,7,7,7,7,7,7,7,8,9),
  //  (1,-1,0,0,7,0,10,0,0,1),
  //  (1,0,0,0,10,0,10,0,0,1),
  //  (1,0,0,0,0,0,0,0,0,1),
  //  (1,1,1,1,1,1,1,1,1,1)
  //);
  tiles: TIntegerArray;
  i: integer;
begin
    SetLength(tiles, 100);
    for i:=0 to High(tiles) do
    begin
      setLength(tiles[i],100);
    end;
    result := tiles;
end;

{ TFormMain }

procedure TFormMain.FormCreate(Sender: TObject);
begin
  GLbox := TOpenGLControl.Create(Self);
  GLbox.AutoResizeViewport := true;
  GLBox.Parent             := Self;
  GLBox.MultiSampling      := 4;
  GLBox.Align              := alClient;
  GLBox.OnPaint            := @GLboxPaint; // for "mode delphi" this would be "GLBox.OnPaint := GLboxPaint"
  GLBox.invalidate;
  GLBox.OnMouseDown := @GLBoxMouseDown;
  GLBox.OnMouseMove := @GLBoxMouseMove;
  GLBox.OnKeyDown := @GLBoxKeyDown;
  Renderer := TRenderer.Create;
  texture := nil;

  tileSize := 16;
  Level := TLevel.Create(1);
  Level.Tilesize:= 16;
  Level.Layer[0] := TLayer.Create(texture,getTestMap(),'background');
  //Level.Data[0].data := getTestMap();
  //Level.Layer[0] := getTestMap();
  OffsetX:=0;
  OffsetY:=0;
  LayerId := 0;
end;

procedure TFormMain.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin

end;

procedure TFormMain.GLBoxClick(Sender: TObject);
begin
  showMessage('click');
end;

procedure TFormMain.GLBoxKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin

  if key = 37 then
  begin
   OffsetX := OffsetX - Level.Tilesize;
  end;
  if key = 39 then
  begin
   OffsetX := OffsetX + Level.Tilesize;
  end;

  if key = 38 then
  begin
   OffsetY := OffsetY - Level.Tilesize;
  end;
  if key = 40 then
  begin
   OffsetY := OffsetY + Level.Tilesize;
  end;
end;

procedure TFormMain.GLBoxMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
   posX, posY: integer;
begin
   posX := x div tileSize;
   posY := y div tileSize;
   if( posY <= High(Level.Layer[LayerId].Data)) and (posX <= High(Level.Layer[LayerId].Data[0])) then
   begin
    if Button = mbLeft then
       begin
          Level.Layer[LayerId].Data[posY][posX] := 6;
       end;
    if Button = mbRight then
       begin
          Level.Layer[LayerId].Data[posY][posX] := -1;
       end;
       GLBox.invalidate;
   end;

end;

procedure TFormMain.GLBoxMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
   MouseX := x div tileSize * tileSize;
   MouseY := y div tileSize * tileSize;
   GLBox.Invalidate;
end;

procedure TFormMain.GLBoxPaint(Sender: TObject);
begin
  if(texture = nil) then
  begin
   texture := TTexture.Create();
   texture.LoadFromFile('nature.png');
   Level.Layer[LayerId].Texture := texture;
  end;

  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
  glEnable( GL_BLEND );
  Renderer.ClearScreen;
  Renderer.Mode2D;
  glTranslatef(offsetX,offsetY,0);
  Renderer.DrawTilemap(Level.Layer[LayerId].data,Level.Layer[LayerId].texture,Level.Tilesize);
   glColor3f(1,1,0);
   glBegin(GL_TRIANGLES);
     glVertex3f(mouseX, mouseY,0);
     glVertex3f(mouseX+Level.Tilesize, mouseY,0);
     glVertex3f(mouseX+Level.Tilesize, mouseY+Level.Tilesize,0);
   glEnd();

  GLbox.SwapBuffers;
end;

end.

