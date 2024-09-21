unit renderer;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, OpenGLContext, GL, glu, BGRABitmap, Texture, Dialogs, gameobject;

type
  TIntegerArray = array of array of integer;

  TRenderer = class
    FScale: integer;
    constructor Create;
    procedure Mode2D;
    procedure ClearScreen;
    procedure Draw;
    procedure DrawTilemap(arr: TIntegerArray; texture: TTexture; t: integer);
    procedure DrawGrid(Width: integer; Height: integer; t: integer);
    procedure DrawBackground(Width: integer; Height: integer; t: integer);
    procedure DrawGameObject(x: integer; y:integer; Sprite: TSprite; Texture: TTexture);
    function glGetViewportWidth: integer;
    function glGetViewportHeight: integer;
    function LoadGLTexture(const FileName: string): GLuint;
    procedure glWrite(X, Y: GLfloat; Font: Pointer; Text: String);
    property Scale: integer read FScale write FScale;
  end;

implementation
constructor TRenderer.Create;
begin
  FScale := 1;
end;

function TRenderer.glGetViewportWidth: integer;
var
  Rect: array[0..3] of integer;
begin
  glGetIntegerv(GL_VIEWPORT, @Rect);
  Result := Rect[2] - Rect[0];
end;

function TRenderer.glGetViewportHeight: integer;
var
  Rect: array[0..3] of integer;
begin
  glGetIntegerv(GL_VIEWPORT, @Rect);
  Result := Rect[3] - Rect[1];
end;

procedure TRenderer.Draw;
var
  i: integer;
  _width, w: integer;
  _height, h: integer;
  x, y: integer;
  t: integer;
  c: integer;
  tiles: array[0..4, 0..9] of integer = ((1, 1, 1, 1, 1, 1, 1, 1, 1, 1),
    (1, 0, 0, 0, 1, 0, 1, 0, 0, 1), (1, 0, 0, 0, 1, 0, 1, 0, 0, 1), (1, 0, 0, 0, 0, 0, 0, 0, 0, 1),
    (1, 1, 1, 1, 1, 1, 1, 1, 1, 1));
begin
  t := 32;
  _width := 10;
  _height := 5;
  ClearScreen;
  Mode2D;
  glBegin(GL_QUADS);
  for h := 0 to _height - 1 do
  begin
    for w := 0 to _width - 1 do
    begin
      glColor3f(1, tiles[h][w], 0);
      glVertex3f(w * t, h * t, 0);
      glVertex3f(w * t, h * t + t, 0);
      glVertex3f(w * t + t, h * t + t, 0);
      glVertex3f(w * t + t, h * t, 0);
    end;
  end;
  glEnd;
end;

procedure TRenderer.Mode2D;
begin
  glMatrixMode(GL_PROJECTION);
  glPushMatrix;
  glLoadIdentity;
  gluOrtho2D(0, glGetViewportWidth, glGetViewportHeight, 0);

  glMatrixMode(GL_MODELVIEW);
  glPushMatrix;
  glLoadIdentity;
end;

procedure TRenderer.ClearScreen;
begin
  glClearColor(0.27, 0.53, 0.71, 1.0);
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
  glLoadIdentity;
end;

procedure TRenderer.DrawTilemap(arr: TIntegerArray; texture: TTexture; t: integer);
var
  h, w: integer;
  tile: integer;
  srcX, srcY: real;
  srcW, srcH: real;
begin
  try
  glPolygonMode(GL_FRONT_AND_BACK, GL_FILL);
  glEnable(GL_TEXTURE_2D);

  glBindTexture(GL_TEXTURE_2D, texture.Id);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
  glColor3f(1, 1, 1);
  glBegin(GL_QUADS);
  for h := Low(arr) to High(arr) do
  begin
    for w := Low(arr[h]) to High(arr[h]) do
    begin
      if (arr[h][w] < 0) then
      begin
        continue;
      end;
      srcY := (arr[h][w] div (texture.Width div t) * t) / texture.Height;
      srcX := (arr[h][w] mod (texture.Width div t) * t) / texture.Width;
      srcW := t / texture.Width;
      srcH := t / texture.Height;
      //glColor3f(1, arr[h][w], 0);
      tile := t * FScale;
      //**
      glTexCoord2f(srcX, srcY);
      glVertex3f(w * tile, h * tile, 0);
      //**
      glTexCoord2f(srcX, srcY + srcH);
      glVertex3f(w * tile, h * tile + tile, 0);
      //**
      glTexCoord2f(srcX + srcW, srcY + srcH);
      glVertex3f(w * tile + tile, h * tile + tile, 0);
      //**
      glTexCoord2f(srcX + srcW, srcY);
      glVertex3f(w * tile + tile, h * tile, 0);
    end;
  end;
  glEnd;
  except

  end;
  glEnable(GL_LINE_STIPPLE);
  glLineStipple(3,43690);
  glPolygonMode(GL_FRONT_AND_BACK, GL_LINE);
  glColor3f(1, 1, 1);
  glLineWidth(2);
  glBegin(GL_QUADS);

     glVertex3f(0, 0, 0);
     glVertex3f((High(arr[h])+1) * tile, 0, 0);
     glVertex3f((High(arr[h])+1) * tile, (High(arr)+1) * tile, 0);
     glVertex3f(0, (High(arr)+1) * tile, 0);
  glEnd;
end;

procedure TRenderer.DrawGrid(Width: integer; Height: integer; t: integer);
var
  w, h, tile: integer;
begin
  glBindTexture(GL_TEXTURE_2D, 0);
  glPolygonMode(GL_FRONT_AND_BACK, GL_LINE);
  glEnable(GL_LINE_STIPPLE);
  glLineStipple(3,43690);
  glPolygonMode(GL_FRONT_AND_BACK, GL_LINE);
  glColor3f(0.5, 0.5, 0.5);
  glBegin(GL_QUADS);
  for h := 0 to Height - 1 do
  begin
    for w := 0 to Width - 1 do
    begin
      tile := t * FScale;
      //**
      glVertex3f(w * tile, h * tile, 0);
      //**
      glVertex3f(w * tile, h * tile + tile, 0);
      //**
      glVertex3f(w * tile + tile, h * tile + tile, 0);
      //**
      glVertex3f(w * tile + tile, h * tile, 0);
    end;
  end;
  glEnd;
end;

procedure TRenderer.DrawBackground(Width: integer; Height: integer; t: integer);
begin
  glBindTexture(GL_TEXTURE_2D, 0);
  glPolygonMode(GL_FRONT_AND_BACK, GL_FILL);
  glBegin(GL_QUADS);
    glColor3f(0.3,0.3,0.3);
      glVertex3f(0, 0, 0);
      glVertex3f(0, Height *t * FScale, 0);
      glVertex3f(Width * t * FScale, Height * t * FScale, 0);
      glVertex3f(Width * t * FScale, 0, 0);
  glEnd;
end;

function TRenderer.LoadGLTexture(const FileName: string): GLuint;
var
  Bitmap: TBGRABitmap;
  TextureID: GLuint;
  PixelFormat: GLenum;
  Data: Pointer;
begin
  // Initialize the texture ID
  //TextureID:= 0;
  glGenTextures(1, @TextureID);
  glBindTexture(GL_TEXTURE_2D, TextureID);

  // Load the image using BGRABitmap
  Bitmap := TBGRABitmap.Create(FileName);
  try
    // Choose the appropriate OpenGL pixel format
    if Bitmap.HasTransparentPixels then
      PixelFormat := GL_RGBA
    else
      PixelFormat := GL_RGB;

    Data := Bitmap.Data;  // Directly access raw pixel data

    // Set OpenGL texture parameters
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);

    // Load the texture data into OpenGL
    glTexImage2D(GL_TEXTURE_2D, 0, PixelFormat, Bitmap.Width,
      Bitmap.Height, 0, PixelFormat, GL_UNSIGNED_BYTE, Data);

    Result := TextureID;

  finally
    Bitmap.Free;
  end;
end;

procedure TRenderer.glWrite(X, Y: GLfloat; Font: Pointer; Text: String);
var
  I: Integer;
begin

end;

procedure TRenderer.DrawGameObject(x: integer; y:integer; Sprite: TSprite; Texture: TTexture);
var
  srcX, srcY: real;
  srcW, srcH: real;
begin
  glPolygonMode(GL_FRONT_AND_BACK, GL_FILL);
  glEnable(GL_TEXTURE_2D);
  glBindTexture(GL_TEXTURE_2D, texture.Id);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
  glColor3f(1, 1, 1);
  glBegin(GL_QUADS);
      srcY := sprite.y  / texture.Height;
      srcX := sprite.x / texture.Width;
      srcW := (sprite.w / texture.Width);
      srcH := (sprite.h / texture.Height);
      //**
      glTexCoord2f(srcX, srcY);
      glVertex3f(x*scale, y*scale, 0);
      //**
      glTexCoord2f(srcX, srcY + srcH);
      glVertex3f(x*scale, (y + sprite.h) *scale, 0);
      //**
      glTexCoord2f(srcX + srcW, srcY + srcH);
      glVertex3f((x+sprite.w) *scale, (y + sprite.h) *scale, 0);
      //**
      glTexCoord2f(srcX + srcW, srcY);
      glVertex3f((x+sprite.w) *scale, (y*scale), 0);
  glEnd;
end;

end.
