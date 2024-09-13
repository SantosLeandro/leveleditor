unit layer;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, texture;

type
  TIntegerArray = array of array of Integer;
  TLayer = class
   protected
     FName: string;
     FData: TIntegerArray;
     FTexture: TTexture;
     FWidth: integer;
     FHeight: integer;
   public
     constructor Create(Texture: TTexture; arr: TIntegerArray; name: string);
     property Name: string read FName write FName;
     property Data: TIntegerArray read FData write FData;
     property Texture: TTexture read FTexture write FTexture;

  end;

implementation
constructor TLayer.Create(Texture: TTexture; arr: TIntegerArray; name: string);
begin
  FTexture := texture;
  FName := name;
  FData :=  arr;
  FWidth := High(arr[0]);
  FHeight := High(arr);
end;

end.

