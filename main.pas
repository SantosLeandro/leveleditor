unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Menus, OpenGLContext,
  GL, glu, ExtCtrls, renderer, texture, StdCtrls, Level, Layer, Vector2,FileHelper,
  ComCtrls, BCListBox, BGRASpriteAnimation, BGRABitmap, BCTypes;

type

  { TFormMain }

  TFormMain = class(TForm)
    PageControl1: TPageControl;
    ScrollBox1: TScrollBox;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    Tileset: TBGRASpriteAnimation;
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
    StatusBar1: TStatusBar;
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
    procedure TilesetClick(Sender: TObject);
    procedure TilesetMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure TilesetMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure TilesetRedrawAfter(Sender: TObject; Bitmap: TBGRABitmap);
    procedure TilesetRedrawBefore(Sender: TObject; Bitmap: TBGRABitmap);
  private

  public
    Level: TLevel;
    Texture: TTexture;
    Renderer: TRenderer;
    LevelFile: TLevelFile;
    tileSize: integer;
    Tilemap : TIntegerArray;
    LayerId: integer;
    MouseX, MouseY: integer;
    OldMouseX, OldMouseY: integer;
    OffsetX, OffsetY: integer;
    Offset: TPoint2D;
    TilesetCursor: TPoint2D;
    CanMove: boolean;
    MouseLeftBtn: boolean;
    MouseMiddleBtn: boolean;
    Scale: integer;
    TileId: integer;
    function getTestMap: TIntegerArray;
    procedure InitLevel;

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

procedure TFormMain.InitLevel;
var
  i: integer;
begin
    LevelFile := TLevelFile.Create;
    //Level := Level.Load('level.json');
    Level := TLevel.Create(1);
    Level.Tilesize:= 16;
    Level.Width := 10;
    Level.Height := 10;
    Level.Scale := 1;
    Texture := TTexture.Create();
    Level.Layer[0] := TLayer.Create(texture,getTestMap(),'background');
    LayerId := 0;

    for i:= 0 to Level.LayerCount do
    begin
       ListBoxLayers.AddItem(Level.Layer[i].Name,Level.Layer[i]);
    end;
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
  GLBox.OnMouseUp := @GLBoxMouseUp;
  GLBox.OnMouseMove := @GLBoxMouseMove;
  GLBox.OnKeyDown := @GLBoxKeyDown;
  GLBox.OnKeyUp := @GLBoxKeyUp;
  Renderer := TRenderer.Create;
  canMove := false;
  scale := 1;
  OffsetX:=0;
  OffsetY:=0;
  LayerId := 0;
  InitLevel;



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
   if (posY >= 0) and ( posY <= High(Level.Layer[LayerId].Data)) and (posX >=0 ) and (posX <= High(Level.Layer[LayerId].Data[0])) then
   begin
    if Button = mbLeft then
       begin
          MouseLeftBtn := true;
          Level.Layer[LayerId].Data[posY][posX] := tileID;
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
         OffsetX := OffsetX + 4;
     if ( x < OldMouseX) then
         OffsetX := OffsetX - 4;
     if( y > OldMouseY) then
         OffsetY := OffsetY + 4;
     if ( y < OldMouseY) then
         OffsetY := OffsetY - 4;
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
  LayerId := ListBoxLayers.ItemIndex;
  if Level.Layer[LayerId].Texture.Bitmap <> nil then
  begin
    Tileset.Sprite := Level.Layer[LayerId].Texture.Bitmap.Bitmap;
  end;
end;

procedure TFormMain.MenuItemOpenClick(Sender: TObject);
var
   I: integer;
begin
   if OpenDialog.Execute then
    begin
      if fileExists(OpenDialog.Filename) then
        Level.free;
        Level := LevelFile.Load(OpenDialog.Filename);
        ListBoxLayers.Clear;
        for i:= 0 to Level.LayerCount do
        begin
           ListBoxLayers.AddItem(Level.Layer[i].Name,Level.Layer[i]);
        end;
    end
    else
      ShowMessage('No file selected');
    end;

procedure TFormMain.TilesetClick(Sender: TObject);
begin

end;

procedure TFormMain.TilesetMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  row, col: integer;
begin
  col := x div Level.Tilesize;
  row := y div Level.Tilesize;
  tileId := col + (row * ( Level.Layer[LayerId].Texture.Width div Level.Tilesize));
end;

procedure TFormMain.TilesetMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  TilesetCursor.x := x div Level.Tilesize * Level.Tilesize;
  TilesetCursor.y := y div Level.Tilesize * Level.Tilesize;
  Tileset.Invalidate;
end;

procedure TFormMain.TilesetRedrawAfter(Sender: TObject; Bitmap: TBGRABitmap);
var
   rect : TRect;
begin
  rect.Top:= TilesetCursor.y;
  rect.Bottom:= TilesetCursor.y + Level.tilesize;
  rect.Left:= TilesetCursor.x;
  rect.Right := TilesetCursor.x + Level.tilesize;
  if  Level.Layer[LayerId].Texture.Bitmap <> nil then
  begin
     Tileset.Sprite := Level.Layer[LayerId].Texture.Bitmap.Bitmap;
     Tileset.Sprite.Canvas.DrawFocusRect(rect);
  end;
end;

procedure TFormMain.TilesetRedrawBefore(Sender: TObject; Bitmap: TBGRABitmap);
begin

end;

end.

