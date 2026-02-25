unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Menus, OpenGLContext,
  GL, glu, glut, ExtCtrls, renderer, texture, StdCtrls, Level, Layer, world, room,
  GameObject, Vector2, FileHelper, Stack, ComCtrls, BCListBox,
  BGRASpriteAnimation, BGRABitmap, BCTypes, BGRAGraphicControl,BGRATransform,BGRABitmapTypes, Types;

type
  EditingMode = (ED_TILEMAP, ED_GAMEOBJECT);
  { TFormMain }

  TFormMain = class(TForm)

    BtnApply: TButton;
    edtObjectTag: TEdit;
    EdtLvlWidth: TLabeledEdit;
    EdtLvlHeight: TLabeledEdit;
    EdtLvlName: TLabeledEdit;
    EdtLvlTilesize: TLabeledEdit;
    ListBoxObject: TListBox;
    MemoProps: TMemo;
    MainPageControl: TPageControl;
    MenuItem3: TMenuItem;
    menuUndo: TMenuItem;
    RadioEditing: TRadioGroup;
    SaveDialog: TSaveDialog;
    ScrollBox1: TScrollBox;
    TabTileset: TTabSheet;
    TabGameObject: TTabSheet;
    TabLevel: TTabSheet;
    Tileset: TBGRAGraphicControl;
    GLBox: TOpenGLControl;
    Label1: TLabel;
    ListBoxLayers: TListBox;
    MainMenu: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItemOpen: TMenuItem;
    MenuItemSave: TMenuItem;
    MenuItemSaveAs: TMenuItem;
    MenuItem6: TMenuItem;
    LeftPanel: TPanel;
    OpenDialog: TOpenDialog;
    MainStatusBar: TStatusBar;
    TimerInit: TTimer;
    TrackBar1: TTrackBar;
    procedure FormActivate(Sender: TObject);
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
    procedure GLBoxMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure GLBoxMouseWheelDown(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);
    procedure GLBoxPaint(Sender: TObject);
    procedure ListBoxLayersSelectionChange(Sender: TObject; User: boolean);
    procedure menuUndoClick(Sender: TObject);
    procedure MenuItemSaveAsClick(Sender: TObject);
    procedure MenuItemSaveClick(Sender: TObject);
    procedure MenuItemOpenClick(Sender: TObject);
    procedure RadioEditingItemEnter(Sender: TObject);
    procedure RadioEditingSelectionChanged(Sender: TObject);
    procedure TabLevelContextPopup(Sender: TObject; MousePos: TPoint;
      var Handled: Boolean);
    procedure TilesetClick(Sender: TObject);
    procedure TilesetMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure TilesetMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure TilesetPaint(Sender: TObject);
    procedure TrackBar1Change(Sender: TObject);
  private

  public
    World: TWorld;
    roomName: String;
    Level: TLevel;
    Texture: TTexture;
    Renderer: TRenderer;
    LevelFile: TLevelFile;
    tileSize: integer;
    Tilemap : TIntegerArray;
    LayerId: integer;
    MouseX, MouseY: integer;
    ObjMouseX, ObjMouseY: integer;
    OldMouseX, OldMouseY: integer;
    OffsetX, OffsetY: integer;
    Offset: TPoint2D;
    TilesetCursor: TPoint2D;
    CanMove: boolean;
    MouseLeftBtn: boolean;
    MouseMiddleBtn: boolean;
    MouseRightBtn: boolean;
    Scale: integer;
    TileId: integer;
    Zoom: integer;
    GameObjects: TArrayGameObject;
    EdMode: string;
    GameObjectId:integer;
    GameObjectSelectedId: integer;
    function getTestMap: TIntegerArray;
    procedure InitLevel;
    procedure OnInitTimer(Sender: TObject);

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
        tiles[i][j] := 0;
      end;
    end;
    result := tiles;
end;

procedure TFormMain.InitLevel;
var
  i: integer;
  FirstRoom: TRoom;
  SecondRoom: TRoom;
  Texture2: TTexture;
  tiledata: String;
begin
    Texture := TTexture.Create();
    Texture2 := TTexture.Create();

    tiledata := '1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,1,1,0,0,0,0,0,0,0,0,1,1,0,0,0,0,0,0,0,0,1,1,0,0,0,0,0,0,0,0,1,1,0,0,0,0,0,0,0,0,1,1,0,0,0,0,0,0,0,0,1,1,0,0,0,0,0,0,0,0,1,1,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1';


    Texture.LoadFromFile('tileset_1616.png');
    Texture2.LoadFromFile('tileset_1616.png');
    World := TWorld.Create;
    FirstRoom := TRoom.Create(1);
    RoomName :=  'first_room';
    FirstRoom.Name:= RoomName;
    FirstRoom.AddLayer(TLayer.Create(Texture2,tileData,10,10,'background'));
    FirstRoom.Width:= 10;
    FirstRoom.Height:= 10;
    FirstRoom.Tilesize:= 16;
    FirstRoom.X := 0;
    FirstRoom.Y := 0;
    World.AddRoom(FirstRoom);

    SecondRoom := TRoom.Create(1);
    SecondRoom.Name:= 'second_room';
    SecondRoom.AddLayer(TLayer.Create(Texture2,tileData,10,10,'background'));
    SecondRoom.Width:= 10;
    SecondRoom.Height:= 10;
    SecondRoom.Tilesize:= 16;
    SecondRoom.X := 10 * 16;
    SecondRoom.Y := 0 * 16;
    World.AddRoom(SecondRoom);


    LevelFile := TLevelFile.Create;
    //Level := Level.Load('level.json');
    Level := TLevel.Create(1);
    Level.Tilesize:= 16;
    Level.Width := 10;
    Level.Height := 10;
    Level.Scale := 1;

    Level.Layer[0] := TLayer.Create(texture,getTestMap(),'background');
    LayerId := 0;

    for i:= 0 to Level.LayerCount do
    begin
       ListBoxLayers.AddItem(Level.Layer[i].Name,Level.Layer[i]);
    end;

    GameObjects := LevelFile.LoadGameObject('gameobject.json');

    for i:= 0 to High(GameObjects) do
    begin
      ListBoxObject.AddItem(GameObjects[i].Name,GameObjects[i]);
    end;

    ListBoxObject.ItemIndex:= 0;
end;

procedure TFormMain.OnInitTimer(Sender: TObject);
begin
  TimerInit.Enabled := False; // executa uma vez
  InitLevel;
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
  GLBox.OnMouseWheel := @GLBoxMouseWheel;
  GLBox.MakeCurrent;
  Renderer := TRenderer.Create;
  canMove := false;
  scale := 1;
  OffsetX:=0;
  OffsetY:=0;
  LayerId := 0;
  EdMode := 'tile';
  MouseRightBtn:=false;
  Zoom := 1;
  GameObjectSelectedId:= -1;
  InitLevel;

  //TimerInit.Interval := 2000; // 2 segundos
  //TimerInit.Enabled := True;
  //TimerInit.OnTimer := @OnInitTimer;

end;

procedure TFormMain.FormActivate(Sender: TObject);
begin

end;

procedure TFormMain.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin

end;

procedure TFormMain.GLBoxClick(Sender: TObject);
begin

end;

procedure TFormMain.GLBoxKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if(key = 46) then
  begin
    if(GameObjectSelectedId > -1) then
    begin
      Level.Layer[LayerId].RemoveGameObject(GameObjectSelectedId);
      GameObjectSelectedId:= -1;
        GLBox.Invalidate;
    end;
  end;
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
   objX, objY: integer;
   goName: string;
   goId: integer;
   Room: TRoom;
   tmpLayer: TLayer;
begin
   MouseLeftBtn := false;
   posX := (x - offsetX) div (Level.tileSize * Scale);
   posY := (y - offsetY) div (Level.tileSize * Scale);

   Room := world.GetRoom(posX,posY);
   if (Room <> nil) then
   begin
     Room.Layer[0].Data[PosY][PosX] := tileId;
     MainStatusBar.SimpleText:= Room.Name;
     GLBox.Invalidate;
   end;
   //tmpLayer := Room.Layer[0];

   if (posY >= 0) and ( posY <= High(Level.Layer[LayerId].Data)) and (posX >=0 ) and (posX <= High(Level.Layer[LayerId].Data[0])) then
   begin
    if Button = mbLeft then
       begin
          MouseLeftBtn := true;
          if (EdMode = 'tile') then
          begin


             if Level.Layer[LayerId].Data[posY][posX] <> tileId then
             begin
                  Level.SaveCommand(LayerId,Level.Layer[LayerId].Data[posY][posX], posX, posY);
                  Level.InsertTile(layerId,posX,posY,tileID);
             end
            //Level.Layer[LayerId].Data[posY][posX] := tileID;

          end
          else
          begin
            goName := ListBoxObject.GetSelectedText;
            objX := (x - offsetX) div ( Scale);
            objY := (y - offsetY) div ( Scale);
            Level.Layer[LayerId].AddGameObject(objX,objY,
            LevelFile.GetSprite(goName).w,
            LevelFile.GetSprite(goName).h,
            goName);
          end;

       end;
    if Button = mbRight then
       begin
          MouseRightBtn := true;
          if (EdMode = 'tile') then
          begin
            Level.Layer[LayerId].Data[posY][posX] := -1;
          end
          else
          begin
            objx := (x - offsetx) div scale;
            objy := (y - offsety) div scale;
            GameObjectSelectedId := Level.Layer[LayerId].GetGameObject(objX,objY);
          end;
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
   deltax,deltay: integer;
   objx,objy:integer;
   posX, posY: integer;
begin
     DeltaX := X - OldMouseX;
     DeltaY := Y - OldMouseY;
     OldMouseX := X;
     OldMouseY := Y;

     if MouseMiddleBtn then
     begin
      OffsetX := OffsetX + DeltaX;
      OffsetY := OffsetY + DeltaY;
     end;

     if MouseLeftBtn = true then
     begin
        posX := (x - offsetX) div (Level.tileSize * Scale);
        posY := (y - offsetY) div (Level.tileSize * Scale);
        if (posY >= 0) and ( posY <= High(Level.Layer[LayerId].Data)) and (posX >=0 ) and (posX <= High(Level.Layer[LayerId].Data[0])) then
        begin
          if (EdMode = 'tile') then
          begin
             if Level.Layer[LayerId].Data[posY][posX] <> tileId then
             begin
                  Level.SaveCommand(LayerId,Level.Layer[LayerId].Data[posY][posX], posX, posY);
                  Level.InsertTile(layerId,posX,posY,tileID);
             end

             //Level.Layer[LayerId].Data[posY][posX] := tileID;
          end
        end
     end;

     if MouseRightBtn = true then
     begin
        posX := (x - offsetX) div (Level.tileSize * Scale);
        posY := (y - offsetY) div (Level.tileSize * Scale);
        if (EdMode = 'tile') then
        begin
           if (posY >= 0) and ( posY <= High(Level.Layer[LayerId].Data)) and (posX >=0 ) and (posX <= High(Level.Layer[LayerId].Data[0])) then
           Level.Layer[LayerId].Data[posY][posX] := -1;
        end
     end;

     MouseX := (x - OffsetX) div (Level.tileSize * Scale) * (Level.tileSize*Scale);
     MouseY := (y - OffsetY) div (Level.tileSize * Scale) * (Level.tileSize*Scale);

     objx := (x - offsetx) div scale;
     objy := (y - offsety) div scale;
     MainStatusBar.SimpleText := 'x '+IntToStr(objx)+' | y '+IntToStr(objy);
     GLBox.Invalidate;
end;

procedure TFormMain.GLBoxMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
   if Button = mbLeft then
   begin
     MouseLeftBtn := false;
   end;

   if Button = mbRight then
   begin
     MouseRightBtn := false;
   end;

   if Button = mbMiddle then
   begin
     MouseMiddleBtn := false;
   end;

end;

procedure TFormMain.GLBoxMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  if WheelDelta > 0 then
  begin
    scale := scale + 1;
  end
  else if (WheelDelta < 0) and (scale > 1) then
  begin
     scale := scale - 1;
  end;
  Renderer.Scale := scale;
  GLBox.Invalidate;
end;

procedure TFormMain.GLBoxMouseWheelDown(Sender: TObject; Shift: TShiftState;
  MousePos: TPoint; var Handled: Boolean);
begin

end;

procedure TFormMain.GLBoxPaint(Sender: TObject);
var
   i: integer;
   j: integer;
   go: TSprite;
   l: integer;
   tmpLayer: TLayer;
   tmpName: String;
begin
  Renderer.ColorR := 1.0;
  Renderer.ColorG := 1.0;
  Renderer.ColorB := 1.0;
  Renderer.ColorA := 1.0;
  go.x := 16;
  go.y := 16;
  go.w := 16;
  go.h := 64;
  if(texture.id = 0) then
  begin
   texture := TTexture.Create();
   texture.LoadFromFile('tileset_1616.png');
  end;

  // prepare to draw
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
  glEnable( GL_BLEND );
  Renderer.ClearScreen;
  Renderer.Mode2D;
  //glScalef(scale,scale,0);

  if( world <> nil) then
  begin
       for i:= 0 to World.RoomCount - 1 do
       begin
          glLoadIdentity();
          glTranslatef(world.GetRoom(i).X * Renderer.Scale ,world.GetRoom(i).Y * Renderer.Scale,0);
          glTranslatef(offsetX, offsetY, 0);

          Renderer.DrawBackground(
            world.GetRoom(i).Width,
            world.GetRoom(i).Height,
            world.GetRoom(i).Tilesize
          );

          Renderer.DrawGrid(
            world.GetRoom(i).Width,
            world.GetRoom(i).Height,
            world.GetRoom(i).Tilesize
          );
          for l := 0 to world.GetRoom(i).LayerCount -1 do
          begin
             //tmpLayer := world.GetRoom(i).Layer[l];
             //tmpName :=  tmpLayer.Name;

             Renderer.DrawTilemap(world.GetRoom(i).Layer[l].Data, world.GetRoom(i).Layer[l].texture, world.GetRoom(i).Tilesize);
          end;

       end;

  end;

  // draw level
  if( level <> nil) and (1 = 2 ) then
  begin
    //glTranslatef(world.GetRoomByName(RoomName).X, world.GetRoomByName(RoomName).Y,0);
    //glTranslatef(offsetX,offsetY,0);
    //Renderer.DrawBackground(world.GetRoomByName(RoomName).Width,world.GetRoomByName(RoomName).Height,Level.TileSize);
    //Renderer.DrawGrid(world.GetRoomByName(RoomName).Width,world.GetRoomByName(RoomName).Height, World.GetRoomByName(RoomName).Tilesize);
    //for i := 0 to Level.LayerCount do
    //begin
    //  Renderer.ColorA  := 0.5;
    //  if(i = layerId) then
    //       Renderer.ColorA := 1.0;
    //  Renderer.DrawTilemap(Level.Layer[i].data,Level.Layer[i].texture, Level.Tilesize);
    //
    //  for j:=0 to High(Level.Layer[i].GameObject) do
    //  begin
    //     if (Level.Layer[i].GameObject[j] <> nil) then
    //     begin
    //       if(j = GameObjectSelectedId ) then
    //       begin
    //            Renderer.ColorG := 0.0;
    //            Renderer.ColorB := 0.0
    //       end
    //       else
    //       begin
    //           Renderer.ColorG := 1.0;
    //           Renderer.ColorB := 1.0;
    //       end;
    //
    //       Renderer.DrawGameObject(
    //         Level.Layer[i].GameObject[j].x,
    //         Level.Layer[i].GameObject[j].y,
    //         LevelFile.GetSprite(Level.Layer[i].GameObject[j].Name),
    //       Texture);
    //     end;
    //  end;
    //end;
   end;


  if(EdMode <> 'tile') then
  begin
     Renderer.ColorR := 0.0;
     Renderer.ColorG := 1.0;
     Renderer.ColorB := 0.0;
     Renderer.ColorA := 0.3;
     Renderer.DrawGameObject(
           (oldMouseX - OffsetX) div scale,
           (oldMouseY - OffsetY) div scale,
           LevelFile.GetSprite(ListBoxObject.GetSelectedText),
           Texture);

  end
  else
  begin
  // Draw Mouse Cursor
   glLoadIdentity();
   glTranslatef(offsetX, offsetY, 0);
   glBindTexture(GL_TEXTURE_2D, 0);
   glColor4f(1,1,0,0.2);
   glBegin(GL_QUADS);
     glVertex3f(mouseX, mouseY,0);
     glVertex3f(mouseX+Level.Tilesize * Scale, mouseY,0);
     glVertex3f(mouseX+Level.Tilesize * Scale, mouseY+Level.Tilesize * Scale,0);
     glVertex3f(mouseX, mouseY+Level.Tilesize * Scale,0);
   glEnd();
  end;

  // present renderer
  GLbox.SwapBuffers;
end;

procedure TFormMain.ListBoxLayersSelectionChange(Sender: TObject; User: boolean);
begin
  LayerId := ListBoxLayers.ItemIndex;
  if Level.Layer[LayerId].Texture.Bitmap <> nil then
  begin
    //Tileset.Bitmap.Bitmap := Level.Layer[LayerId].Texture.Bitmap.Bitmap;
   //Tileset2t.Sprite := Level.Layer[LayerId].Texture.Bitmap.Bitmap;
  end;
end;

procedure TFormMain.menuUndoClick(Sender: TObject);
var
   oldCommand :TCommand;
begin
   if (Level.Undo(oldCommand)) then
   begin
     Level.InsertTile(oldCommand.layer,oldCommand.w,oldCommand.h,oldCommand.tile);
     oldCommand.Free;
     GLBox.Invalidate;
   end;
end;

procedure TFormMain.MenuItemSaveAsClick(Sender: TObject);
begin
   if SaveDialog.Execute then
   begin
     LevelFile.Save(SaveDialog.filename,Level);
   end;
end;

procedure TFormMain.MenuItemSaveClick(Sender: TObject);
begin
    LevelFile.Save('testSave.json',Level);
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
        Tileset.Invalidate;
        EdtLvlWidth.Text := IntToStr(Level.Width);
        EdtLvlHeight.Text := IntToStr(Level.Height);
        EdtLvlTilesize.Text := IntToStr(Level.Tilesize);
        EdtLvlName.Text := Level.Name;
        MemoProps.Append(Level.Props);
    end
    else
      ShowMessage('No file selected');
    end;

procedure TFormMain.RadioEditingItemEnter(Sender: TObject);
begin

end;

procedure TFormMain.RadioEditingSelectionChanged(Sender: TObject);
begin
   if RadioEditing.ItemIndex = 0 then
   begin
      EdMode := 'tile';
      MainPageControl.ActivePage :=TabTileset;
   end
   else
   begin
      MainPageControl.ActivePage :=TabGameObject;
      EdMode := 'object';
   end;
end;

procedure TFormMain.TabLevelContextPopup(Sender: TObject; MousePos: TPoint;
  var Handled: Boolean);
begin

end;


procedure TFormMain.TilesetMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  row, col: integer;
begin
  //col := x div Level.Tilesize;
  //row := y div Level.Tilesize;
   col := TilesetCursor.x div (Level.Tilesize * zoom);
   row := TilesetCursor.y div (Level.Tilesize * zoom);
   MainStatusBar.SimpleText := 'COL: ' + IntToStr(Col);
   tileId := col + (row * ( Level.Layer[LayerId].Texture.Width div Level.Tilesize));
end;

procedure TFormMain.TilesetMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  TilesetCursor.x := (x div (Level.Tilesize * zoom) * (Level.Tilesize * zoom));
  TilesetCursor.y := (y div (Level.Tilesize * zoom) * (Level.Tilesize * zoom));
  Tileset.Invalidate;

end;

//procedure TFormMain.Tileset2RedrawAfter(Sender: TObject; Bitmap: TBGRABitmap);
//var
//   rect,dstRect : TRect;
//   mx, my: integer;
//begin
//  //mX := ((TilesetCursor.x) div (Level.Tilesize * zoom)) * (tileSize * zoom);
//  //mY := ((TilesetCursor.y) div (Level.Tilesize * zoom)) * (tileSize * zoom);
//  mx :=  TilesetCursor.x;
//  my :=  TilesetCursor.y;
//  rect.Top:= my;
//  rect.Bottom:= my + (Level.tilesize * zoom);
//  rect.Left:= mx;
//  rect.Right := mx + (Level.tilesize * zoom);
//
//
//  if  Level.Layer[LayerId].Texture.Bitmap <> nil then
//  begin
//     dstRect.Top := 0;
//     dstRect.Left := 0;
//     dstRect.Bottom:= Level.Layer[LayerId].Texture.Bitmap.Bitmap.Height * zoom;
//     dstRect.Right := Level.Layer[LayerId].Texture.Bitmap.Bitmap.Width * zoom;
//     //Tileset2t.sprite.Clear;
//
//    //Tileset2t.Sprite.SetSize(1000,1000);
//     //Tileset2t.Sprite := Level.Layer[LayerId].Texture.Bitmap.Bitmap;
//
//     //Tileset2t.Width:= Level.Layer[LayerId].Texture.Bitmap.Bitmap.Width * zoom;
//     //Tileset2t.Height:= Level.Layer[LayerId].Texture.Bitmap.Bitmap.Height * zoom;
//     //Tileset2t.Sprite.Width := Level.Layer[LayerId].Texture.Bitmap.Bitmap.Width;
//     //Tileset2t.Sprite.Height := Level.Layer[LayerId].Texture.Bitmap.Bitmap.Height;
//
//    //Tileset2t.Sprite.Canvas.StretchDraw(dstRect, Level.Layer[LayerId].Texture.Bitmap.Bitmap);
//
//
//    Tileset.Bitmap.Canvas.DrawFocusRect(rect);
//  end;
//end;
//
//procedure TFormMain.Tileset2RedrawBefore(Sender: TObject; Bitmap: TBGRABitmap);
//begin
//
//end;

procedure TFormMain.TilesetClick(Sender: TObject);
begin

end;





procedure TFormMain.TilesetPaint(Sender: TObject);
var
   rect,dstRect : TRect;
   mx, my: integer;
begin
  //mX := ((TilesetCursor.x) div (Level.Tilesize * zoom)) * (tileSize * zoom);
  //mY := ((TilesetCursor.y) div (Level.Tilesize * zoom)) * (tileSize * zoom);
  mx :=  TilesetCursor.x;
  my :=  TilesetCursor.y;
  rect.Top:= my;
  rect.Bottom:= my + (Level.tilesize * zoom);
  rect.Left:= mx;
  rect.Right := mx + (Level.tilesize * zoom);


  if  Level.Layer[LayerId].Texture.Bitmap <> nil then
  begin
     dstRect.Top := 0;
     dstRect.Left := 0;
     dstRect.Bottom:= Level.Layer[LayerId].Texture.Bitmap.Bitmap.Height * zoom;
     dstRect.Right := Level.Layer[LayerId].Texture.Bitmap.Bitmap.Width * zoom;

     Tileset.Bitmap.Fill(BGRAPixelTransparent);
     Tileset.Bitmap.StretchPutImage(dstRect,Level.Layer[LayerId].Texture.Bitmap,TDrawMode.dmLinearBlend);

    Tileset.Bitmap.Canvas.AntialiasingMode := amOff;
    Tileset.Bitmap.Canvas.DrawFocusRect(rect);
  end;
end;

procedure TFormMain.TrackBar1Change(Sender: TObject);
begin
  zoom := TrackBAr1.Position;
  Tileset.Width := Level.Layer[LayerId].Texture.Bitmap.Bitmap.Width * zoom;
     Tileset.Height := Level.Layer[LayerId].Texture.Bitmap.Bitmap.Height * zoom;
  Tileset.Invalidate;
end;

end.

