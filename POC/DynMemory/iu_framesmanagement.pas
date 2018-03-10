unit IU_FramesManagement;

{$mode objfpc}{$H+}
// ***
// * Unit provides all needed types and globals var, const objects, procedures and function for frames management
// * Creation Date : 2018 March
// *
// * Version : 0.1
// * Version Date : 2018 March
// * Version Contributors : Pascal Lemaître
// *
// * @authors : Pascal Lemaître
// *
// * @see :
// *
// *
// * Team : TIm (Traitement d'Images)
// *
// * 2018
// ***

interface

uses
  Classes, SysUtils
  , math
  ;

Type
  // Default pixels structure.
  // 1 chanel for red (32 bits)
  // 1 chanel for green (32 bits)
  // 1 chanel for blue (32 bits)
  IU_T_Pixel = record
    R,G,B : dword;
  end;

  // Default frame descriptor
  // A frame is a part of a picture
  // Frame start from StartX position of the picture and from StartY position
  // The frame size is Witdh (number or rows) and Height (number of lines)
  IU_T_Frame_Descriptor = record
    Width, Height, StartX, StartY : integer;
  end;

  // Default pixels data of a frame
  // Is an array without limits. Limits should be definied accordind to Memory pluged on the computer
  IU_T_Frame_Data = array of IU_T_Pixel;

  // Defaut frame definition
  // A frame has a description
  // A frame has data (pixels)
  IU_T_Frame = record
    _descriptor : IU_T_Frame_Descriptor;
    _pixels : IU_T_Frame_Data;
  end;

  // ***
  // * Allocating frame memory
  // *
  // * @author : Pascal Lemaître
  // *
  // * in-out : frame of wich one memory must be allocated or reallocated
  // * in : Width of the frame (number of rows)
  // * in : Height of the frame (number of cols)
  // * in : StartX of the frame. StartX is the first row of the pix to wich one frame is aligned
  // * in : StartY of the frame. StartY is the first line of the pix to wich one frame is aligned
  // * in : InitValue : Byte to initialize ram area of the frame data
  // *
  // ***
  procedure IU_AllocFrame (var frame : IU_T_Frame ; Width, Height, StartX, StartY : integer ; initValue : char) ;

  // ***
  // * Free frame memory and reset frame descriptor
  // *
  // * @author : Pascal Lemaître
  // *
  // * in-out : frame to free ram space
  // *
  // ***
  procedure IU_ReleaseFrame (var frame : IU_T_Frame);

  // ***
  // * Save frame from memory to disk
  // *
  // * @author : Pascal Lemaître
  // *
  // * in-out : frame to write
  // * in : filename for saving
  // *
  // ***
  procedure IU_FrameSave (var frame : IU_T_Frame ; filename : string);

  // ***
  // * Load frame from disk
  // *
  // * @author : Pascal Lemaître
  // *
  // * in-out : frame to load
  // * in : filename for loading
  // *
  // ***
  procedure IU_FrameLoad (var frame : IU_T_Frame ; filename : string);

  // ***
  // * Copy source frame into target frame (Alloc target frame is neede)
  // *
  // * @author : Pascal Lemaître
  // *
  // * in-out : frameSource source of the copy
  // * in-out : frameTarget target of the copy
  // *
  // ***
  procedure IU_CopyFrame (var frameSource, frameTarget : IU_T_Frame);

implementation

// BlockWrite has a size limite. Then to be quiet max block write size is set to 500 Mega Bytes
const MAX_BlocSize =  524288000 ; // 500Mb


// ***
// * Return the min value between two qword (64 bits unsigned value)
// *
// * @author : Pascal Lemaître
// *
// * in : a first 64 bits unsigned value
// * in : b second 64 bits unsigned value
// *
// * return the min value between a and b
// *
// ***
function _privatemin (a, b : qword) : qword;
begin
  if a < b then _privatemin := a else _privatemin := b;
end;


// ***
// * Allocating frame memory
// *
// * @author : Pascal Lemaître
// *
// * in-out : frame of wich one memory must be allocated or reallocated
// * in : Width of the frame (number of rows)
// * in : Height of the frame (number of cols)
// * in : StartX of the frame. StartX is the first row of the pix to wich one frame is aligned
// * in : StartY of the frame. StartY is the first line of the pix to wich one frame is aligned
// * in : InitValue : Byte to initialize ram area of the frame data
// *
// ***
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

// ***
// * Free frame memory and reset frame descriptor
// *
// * @author : Pascal Lemaître
// *
// * in-out : frame to free ram space
// *
// ***
procedure IU_ReleaseFrame (var frame : IU_T_Frame);
begin
  // Trying to release frame
  SetLength(frame._pixels, 0);
  frame._descriptor.Width := 0;
  frame._descriptor.Height := 0;
  frame._descriptor.StartX := 0;
  frame._descriptor.StartY := 0;
end;

// ***
// * Copy source frame into target frame (Alloc target frame is neede)
// *
// * @author : Pascal Lemaître
// *
// * in-out : frameSource source of the copy
// * in-out : frameTarget target of the copy
// *
// ***
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

// ***
// * Save frame from memory to disk
// *
// * @author : Pascal Lemaître
// *
// * in-out : frame to write
// * in : filename for saving
// *
// ***
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
  // Loop until all frame data saved (save by MAX_BlocSize bytes blocks max)
  while _saved < _allocsize do begin
     Blockwrite(_file, pt^, _maxsave);
     pt := pt + _maxsave;
     inc(_saved, _maxsave);
     _diff := _allocsize-_saved;
     _maxsave := _privatemin(_diff, _maxsave);
  end;
  close(_file);
end;

// ***
// * Load frame from disk
// *
// * @author : Pascal Lemaître
// *
// * in-out : frame to load
// * in : filename for loading
// *
// ***
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
  // Loop until all frame data loaded (load by MAX_BlocSize bytes blocks max)
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

