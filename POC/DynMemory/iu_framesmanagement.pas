unit IU_FramesManagement;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

Type
  IU_Pixel = record
    R,G,B : dword;
  end;

  IU_Frame = record
    Width, Height : integer;
    Pixels : array of IU_Pixel;
    StartX, StartY : integer;
  end;

  procedure IU_AllocFrame (var frame : IU_Frame ; Width, Height, StartX, StartY : integer) ;
  procedure IU_ReleaseFrame (var frame : IU_Frame);

implementation

procedure IU_AllocFrame (var frame : IU_Frame ; Width, Height, StartX, StartY : integer) ;
var
  x: integer;
begin
  // Trying to set array size
  SetLength (frame.Pixels, Width*Height);
  frame.Width := Width;
  frame.Height := Height;
  frame.StartX := StartX;
  frame.StartY := StartY;
  for x := 0 to Width*Height - 1 do
      begin
        frame.Pixels[x].R:=0;
        frame.Pixels[x].G:=0;
        frame.Pixels[x].B:=0;
      end;
end;

procedure IU_ReleaseFrame (var frame : IU_Frame);
begin
  // Trying to release frame
  SetLength (frame.Pixels,0);
  frame.Width := 0;
  frame.Height := 0;
  frame.StartX := 0;
  frame.StartY := 0;
end;



end.

