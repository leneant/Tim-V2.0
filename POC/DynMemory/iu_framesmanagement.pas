unit IU_FramesManagement;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils
  , math
  ;

Type
  IU_T_Pixel = record
    R,G,B : dword;
  end;

  IU_T_Frame_Descriptor = record
    Width, Height, StartX, StartY : integer;
  end;

  IU_T_Frame_Data = array of IU_T_Pixel;

  IU_T_Frame = record
    _descriptor : IU_T_Frame_Descriptor;
    _pixels : IU_T_Frame_Data;
  end;

  procedure IU_AllocFrame (var frame : IU_T_Frame ; Width, Height, StartX, StartY : integer ; initValue : char) ;
  procedure IU_ReleaseFrame (var frame : IU_T_Frame);
  procedure IU_FrameSave (var frame : IU_T_Frame ; filename : string);
  procedure IU_FrameLoad (var frame : IU_T_Frame ; filename : string);
  procedure IU_CopyFrame (var frameSource, frameTarget : IU_T_Frame);

implementation

const MAX_BlocSize =  524288000 ; // 500Mo


function _privatemin (a, b : qword) : qword;
begin
  if a < b then _privatemin := a else _privatemin := b;
end;


procedure IU_AllocFrame (var frame : IU_T_Frame ; Width, Height, StartX, StartY : integer ; initValue : char) ;
var
  _allocsize : qword;
begin
  // Trying to set array size
  _allocsize := (Width*Height)*sizeof(IU_T_Pixel);
  SetLength(frame._pixels, Width * Height);
  frame._descriptor.Width := Width;
  frame._descriptor.Height := Height;
  frame._descriptor.StartX := StartX;
  frame._descriptor.StartY := StartY;
  fillchar((frame._pixels[0]), _allocsize, initValue);
end;

procedure IU_ReleaseFrame (var frame : IU_T_Frame);
begin
  // Trying to release frame
  SetLength(frame._pixels, 0);
  frame._descriptor.Width := 0;
  frame._descriptor.Height := 0;
  frame._descriptor.StartX := 0;
  frame._descriptor.StartY := 0;
end;

procedure IU_CopyFrame (var frameSource, frameTarget : IU_T_Frame);
var copysize : qword;
  _size : qword;
begin
  frameTarget._descriptor := frameSource._descriptor;
  _size := frameTarget._descriptor.Width*frameTarget._descriptor.Height;
  copysize := _size * sizeof(IU_T_Pixel);
  setLength(frameTarget._pixels, _size);
  move(frameSource._pixels[0], frameTarget._pixels[0], copysize);

end;

procedure IU_FrameSave (var frame : IU_T_Frame ; filename : string);
var _file : File of Byte;
    _allocsize, _maxsave : qword;
    i : integer;
    pt : ^Byte ; // pointer to follow saving area
    _saved, _diff : qword; // how many bytes saved
begin
  _allocsize := (frame._descriptor.Width*frame._descriptor.Height)*sizeof(IU_T_Pixel);
  // create file for writing
  assign (_file, filename);
  rewrite(_file);
  // #1 writing descriptor
  Blockwrite(_file, frame._descriptor, sizeof(frame._descriptor));
  // #2 writing data
  // #2.1 Start saving address
  pt := PByte(@(frame._pixels[0]));
  // #2.2 max save block
  _maxsave := _privatemin(_allocsize,MAX_BlocSize);
  _saved := 0;
  while _saved < _allocsize do begin
     Blockwrite(_file, pt^, _maxsave);
     pt := pt + _maxsave;
     inc(_saved, _maxsave);
     _diff := _allocsize-_saved;
     _maxsave := _privatemin(_diff, _maxsave);
  end;
  close(_file);
end;

procedure IU_FrameLoad (var frame : IU_T_Frame ; filename : string);
var _file : File of Byte;
    _allocsize, _maxsave : qword;
    i : integer;
    pt : ^Byte ; // pointer to follow saving area
    _loaded, _diff : qword; // how many bytes saved
begin
  // Open file for reading
  assign(_file, filename);
  reset(_file);
  // #1 reading descriptor
  Blockread(_file, frame._descriptor, sizeof(frame._descriptor));
  // #2 call data size
  _allocsize := frame._descriptor.Width * frame._descriptor.Height;
  // #3 set data size for the frame
  setLength(frame._pixels, _allocsize);
  // #4 read data
  // #4.1 Start saving address
  pt := PByte(@(frame._pixels[0]));
  // #4.2 max save block
  _maxsave := _privatemin(_allocsize,MAX_BlocSize);
  _loaded := 0;
  _allocsize := _allocsize * sizeof(IU_T_Pixel);
  while _loaded < _allocsize do begin
     Blockread(_file, pt^, _maxsave);
     pt := pt + _maxsave;
     inc(_loaded, _maxsave);
     _diff := _allocsize-_loaded;
     _maxsave := _privatemin(_diff, _maxsave);
  end;
  close (_file);
end;

end.

