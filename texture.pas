unit Texture;

{$mode ObjFPC}{$H+}

interface

  uses
    Classes, SysUtils, GL, GLu, BGRABitmap, BGRABitmapTypes;

  type
    TTexture = class
    private
      FTextureID: GLuint;
      FWidth: Integer;
      FHeight: Integer;
    public
      constructor Create;
      destructor Destroy; override;

      procedure LoadFromFile(const FileName: string);
      procedure Bind;
      procedure Unbind;

      property Id: GLuint read FTextureID;
      property Width: Integer read FWidth;
      property Height: Integer read FHeight;
    end;

  { TTexture }
 implementation
  constructor TTexture.Create;
  begin
    inherited Create;
    glGenTextures(1, @FTextureID);  // Generate OpenGL texture ID
    FWidth := 0;
    FHeight := 0;
  end;

  destructor TTexture.Destroy;
  begin
    // Delete texture when no longer needed
    if FTextureID <> 0 then
      glDeleteTextures(1, @FTextureID);

    inherited Destroy;
  end;

  procedure TTexture.LoadFromFile(const FileName: string);
  var
    Bitmap: TBGRABitmap;
    PixelFormat: GLenum;
  begin
    // Load image using BGRABitmap
    Bitmap := TBGRABitmap.Create(FileName);
    try
      FWidth := Bitmap.Width;
      FHeight := Bitmap.Height;

      // Determine pixel format based on whether the image has an alpha channel
      if Bitmap.HasTransparentPixels then
        PixelFormat := GL_RGBA
      else
        PixelFormat := GL_RGB;

      // Bind the texture and upload the image data
      glBindTexture(GL_TEXTURE_2D, FTextureID);

      // Set texture parameters
      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT);
      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);
      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);

      // Upload the texture to OpenGL
      glTexImage2D(GL_TEXTURE_2D, 0, PixelFormat, FWidth, FHeight, 0, PixelFormat, GL_UNSIGNED_BYTE, Bitmap.Data);

    finally
      Bitmap.Free;
    end;
  end;

  procedure TTexture.Bind;
  begin
    // Bind the texture for use
    glBindTexture(GL_TEXTURE_2D, FTextureID);
  end;

  procedure TTexture.Unbind;
  begin
    // Unbind the texture
    glBindTexture(GL_TEXTURE_2D, 0);
  end;
end.
