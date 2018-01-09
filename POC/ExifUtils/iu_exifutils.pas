unit IU_ExifUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils
  {$ifdef linux}
  ,unix
  {$else}
  ,ShellApi
  {$endif}
  ;


type
  IU_T_StringArray = array of string;

function getExif (filename : string) : IU_T_StringArray;

implementation

function getExif (filename : string) : IU_T_StringArray;
var
  {$ifdef linux}
  s : longint;
  {$else}
  s : widestring;
  {$endif}
begin
  // exec command
  {$ifdef linux}
  s := fpSystem('exiv2 -g Exif ' + filename + ' > test.txt');
  {$else}
  ShellExecute(0, 'open', PWideChar(getcurrentdir + '\exiv2 -g Exif ' + filename + ' > test.txt'), nil,nil, 1);
  {$endif}
end;

end.

