unit IU_strechutils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

procedure StrechUtils_StrechLimits(SourceWidth, SourceHeight, DestWidth, DestHeight : integer ; var StrechWidth, StrechHeight : integer);

implementation
procedure StrechUtils_StrechLimits(SourceWidth, SourceHeight, DestWidth, DestHeight : integer ; var StrechWidth, StrechHeight : integer);
var
    coefsource, coefdest, coef : real;
begin
  coefsource := SourceWidth / SourceHeight ;
  coefdest := DestWidth / DestHeight;
  if coefdest > coefsource then
    coef := DestHeight / SourceHeight
  else
    coef := DestWidth / SourceWidth;
  StrechWidth := trunc(SourceWidth * coef);
  StrechHeight := trunc(SourceHeight * coef);
end;

end.

