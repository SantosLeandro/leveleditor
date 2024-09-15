unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Menus, OpenGLContext,
  GL, glu, ExtCtrls, renderer, texture, StdCtrls, Level, Layer, Vector2;

type

  { TFormMain }

  TFormMain = class(TForm)
    GLBox: TOpenGLControl;
    Label1: TLabel;
    ListBoxLayers: TListBox;
    MainMenu: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItemOpen: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    LeftPanel: TPanel;
    OpenDialog: TOpenDialog;
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure GLBoxClick(Sender: TObject);
    procedure GLBoxKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure GLBoxKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure GLBoxMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure GLBoxMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer
      );
    procedure GLBoxMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure GLBoxPaint(Sender: TObject);
    procedure ListBoxLayersSelectionChange(Sender: TObject; User: boolean);
    procedure MenuItemOpenClick(Sender: TObject);
  private

  public
    Level: TLevel;
    Texture: TTexture;
    Renderer: TRenderer;
    tileSize: integer;
    Tilemap : TIntegerArray;
    LayerId: integer;
    MouseX, MouseY: integer;
    OldMouseX, OldMouseY: integer;
    OffsetX, OffsetY: integer;
    Offset: TPoint2D;
    CanMove: boolean;
    MouseLeftBtn: boolean;
    MouseMiddleBtn: boolean;
    Scale: integer;
    function getTestMap: TIntegerArray;

  end;

var
  FormMain: TFormMain;

implementation

{$R *.lfm}

function TFormMain.getTestMap: TIntegerArray;
var
  tiles: TIntegerArray;
  i: integer;
  j: integer;
begin
    SetLength(tiles, 10);
    for i:=0 to High(tiles) do
    begin
      setLength(tiles[i],10);
    end;

    for i:=0 to High(tiles) do
    begin
      for j:=0 to high(tiles[i]) do
      begin
        tiles[i][j] := -1;
      end;
    end;
    result := tiles;
end;

{ TFormMain }

procedure TFormMain.FormCreate(Sender: TObject);
var
  i: integer;
begin
  GLbox := TOpenGLControl.Create(Self);
  GLbox.AutoResizeViewport := true;
  GLBox.Parent             := Self;
  GLBox.MultiSampling      := 4;
  GLBox.Align              := alClient;
  GLBox.OnPaint            := @GLboxPaint; // for "mode delphi" this would be "GLBox.OnPaint := GLboxPaint"
  GLBox.invalidate;
  GLBox.OnMouseDown := @GLBoxMouseDown;
  GLBox.OnMouseUp := @GLBoxMouseUp;
  GLBox.OnMouseMove := @GLBoxMouseMove;
  GLBox.OnKeyDown := @GLBoxKeyDown;
  GLBox.OnKeyUp := @GLBoxKeyUp;
  Renderer := TRenderer.Create;
  texture := nil;
  canMove := false;
  tileSize := 16;
  scale := 1;
  Level := TLevel.Create(1);
  Level.Tilesize:= 16;
  Level.Width := 10;
  Level.Height := 10;
  Level.Scale := 2;
  Level.Layer[0] := TLayer.Create(texture,getTestMap(),'background');
  OffsetX:=0;
  OffsetY:=0;
  LayerId := 0;

  for i:= 0 to Level.LayerCount do
  begin
     ListBoxLayers.AddItem(Level.Layer[i].Name,Level.Layer[i]);
  end;

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
  //if key = 17 then
  //begin
  //   GLBox.Cursor:= crSizeAll;
  //   canMove := true;
  //end;
  //
  //if key = 37 then
  //begin
  // OffsetX := OffsetX - Level.Tilesize;
  //end;
  //if key = 39 then
  //begin
  // OffsetX := OffsetX + Level.Tilesize;
  //end;
  //
  //if key = 38 then
  //begin
  // OffsetY := OffsetY - Level.Tilesize;
  //end;
  //if key = 40 then
  //begin
  // OffsetY := OffsetY + Level.Tilesize;
  //end;
end;

procedure TFormMain.GLBoxKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if key = 17 then
  begin
     GLBox.Cursor:= crArrow;
     canMove := false;
  end;

end;

procedure TFormMain.GLBoxMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
   posX, posY: integer;
begin
   MouseLeftBtn := false;
   posX := (x - offsetX) div Level.tileSize;
   posY := (y - offsetY) div Level.tileSize;
   if( posY <= High(Level.Layer[LayerId].Data)) and (posX <= High(Level.Layer[LayerId].Data[0])) then
   begin
    if Button = mbLeft then
       begin
          MouseLeftBtn := true;
          Level.Layer[LayerId].Data[posY][posX] := 6;
       end;
    if Button = mbRight then
       begin
          Level.Layer[LayerId].Data[posY][posX] := -1;
       end;
       GLBox.invalidate;
   end;

   if Button = mbMiddle then
   begin
     MouseMiddleBtn := true;
   end;


end;

procedure TFormMain.GLBoxMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
var
   movex,movey: integer;
begin

   if MouseMiddleBtn then
   begin
     if( x > OldMouseX) then
         OffsetX := OffsetX + 8;
     if ( x < OldMouseX) then
         OffsetX := OffsetX - 8;
     if( y > OldMouseY) then
         OffsetY := OffsetY + 8;
     if ( y < OldMouseY) then
         OffsetY := OffsetY - 8;
     OldMouseX := X;
     OldMouseY := Y;
   end;
   MouseX := (x - OffsetX) div Level.tileSize * Level.tileSize;
   MouseY := (y - OffsetY) div Level.tileSize * Level.tileSize;
   GLBox.Invalidate;
end;

procedure TFormMain.GLBoxMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
   MouseLeftBtn := (Button = mbLeft) and MouseLeftBtn = false;

   if Button = mbMiddle then
   begin
     MouseMiddleBtn := false;
   end;

end;

procedure TFormMain.GLBoxPaint(Sender: TObject);
var
   i: integer;
begin
  if(texture = nil) then
  begin
   texture := TTexture.Create();
   texture.LoadFromFile('nature.png');
   Level.Layer[LayerId].Texture := texture;
  end;

  // prepare to draw
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
  glEnable( GL_BLEND );
  Renderer.ClearScreen;
  Renderer.Mode2D;
  glTranslatef(offsetX,offsetY,0);
  glPolygonMode( GL_FRONT_AND_BACK, GL_FILL);

  // draw level
  if( level <> nil ) then
  begin
    Renderer.DrawBackground(level.width,level.height,Level.TileSize);
    Renderer.DrawGrid(level.width,level.height,Level.TileSize);
    for i := 0 to Level.LayerCount do
    begin
      Renderer.DrawTilemap(Level.Layer[i].data,Level.Layer[LayerId].texture,Level.Tilesize * scale);
    end;
   end;

  // Draw Mouse Cursor
   glBindTexture(GL_TEXTURE_2D, 0);
   glColor4f(1,1,0,0.2);
   glBegin(GL_QUADS);
     glVertex3f(mouseX, mouseY,0);
     glVertex3f(mouseX+Level.Tilesize, mouseY,0);
     glVertex3f(mouseX+Level.Tilesize, mouseY+Level.Tilesize,0);
     glVertex3f(mouseX, mouseY+Level.Tilesize,0);
   glEnd();

  // present renderer
  GLbox.SwapBuffers;
end;

procedure TFormMain.ListBoxLayersSelectionChange(Sender: TObject; User: boolean);
begin
  ShowMessage(ListBoxLayers.ItemIndex.toString);
end;

procedure TFormMain.MenuItemOpenClick(Sender: TObject);
begin
   OpenDialog.Execute;
end;

end.

