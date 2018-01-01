unit IU_Pix;
// Unit provides picture pixels access services

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  IU_Types, IU_ImagesUtils, IU_exceptions;

function IU_getPixelPosition (var in_Picture : T_IU_Picture ; x, y : word) : longword;
// Function return the pixel position in the picture at coord x and y
// Raise an IU_outofbounds exception if x or y are outside the picture size limit

implementation
function IU_getPixelPosition (var in_Picture : T_IU_Picture ; x, y : word) : longword;
var
  iu_PixelPosition : longword;
begin
  // setting return default value to 0
  iu_PixelPosition := longword(0);
  // Checking pixel position in the picture
  // x and y are word then only x > width and y > height tests are needed
  if (x > in_Picture.iu_width) then
    begin
      IU_getPixelPosition := iu_PixelPosition;
      raise IU_EOutOfBounds.Create('x out of picture width !');
    end
  else if (y > in_Picture.iu_height) then
    begin
      IU_getPixelPosition := iu_PixelPosition;
      raise IU_EOutOfBounds.Create('y out of picture height !') ;
    end
  else
    begin
      iu_PixelPosition := longword(y) * longword(in_Picture.width) + longword(x);
    end;
end;

end.

