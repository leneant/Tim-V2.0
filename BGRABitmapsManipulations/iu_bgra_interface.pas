unit IU_BGRA_Interface;

{$mode objfpc}{$H+}
// ***
// * Unit provides a low level interface to BGRABitmap package
// * Creation Date : 2017 September
// *
// * Version : 0.1
// * Version Date : 2017 November
// * Version Contributors : Pascal Lemaître
// *
// * @authors : Pascal Lemaître
// *
// * @see :
// *
// *
// * Team : TIm (Traitement d'Images)
// *
// * 2017
// ***

interface

uses
  Classes, SysUtils, BGRABitmap, BGRABitmapTypes, IU_Types, IU_ImagesUtils;
// IU_Types and IU_ImagesUtils are needed to convert BGRABitmap format into Image32 format and vice versa

// ***
// * Create an empty BGRABitmap. Empty BGRABitmap is always created with an alpha chanel
// *
// * @author : Pascal Lemaître
// *
// * out : BGRABitmap created without loading picture _bitmap
// * in : number of rows (x) of the bitmap _width
// * in : nimber of lines (y) of the bitmap _height
// *
// ***
procedure IU_BGRA_I_CreateEmpty (var _bitmap : TBGRABitmap ; _width, _height : integer) ;
// creating a new empty BGRABitmap with the size _width, _height. BGRABitmap is always created with an alpha chanel


// ***
// * Destroy (destructor) of an existing BGRABitmap
// *
// * @author : Pascal Lemaître
// *
// * in out : BGRABitmap _bitmap
// *
// ***
procedure IU_BGRA_I_Release (var _bitmap : TBGRABitmap) ;
// Releasing an existing BGRA bitmap


// ***
// * Create a BGRABitmap from a picture readed from disk.
// *
// * @author : Pascal Lemaître
// *
// * out : BGRABitmap created without loading picture
// * in : picture file name to load _filename
// *
// ***
procedure IU_BGRA_I_CreateLoad (var _bitmap : TBGRABitmap ; _filename : ansistring) ;
// creating a new empty BGRABitmap with the size _width, _height. BGRABitmap is always created with an alpha chanel


// ***
// * Save a BGRABitmap in a file on disk.
// *
// * @author : Pascal Lemaître
// *
// * in : BGRABitmap created without loading picture
// * in : target file name on disk _filename
// *
// ***
procedure IU_BGRA_I_Save (var _bitmap : TBGRABitmap ; _filename : ansistring);
// saving (not releasing after saving)


// ***
// * Getting size properties and alpha chanel of an existing BGRABitmap.
// *
// * @author : Pascal Lemaître
// *
// * in : BGRABitmap created without loading picture
// * out : number of rows (x) of the bitmap _width
// * out : nimber of lines (y) of the bitmap _height
// * out : is or is not an alpha chanel _alpha
// *
// ***
procedure IU_BGRA_I_getSize (var _bitmap : TBGRABitmap ; var _width, _height : integer ; var _alpha : boolean) ;
// getting size of the pix and if pix has an alpha chanel or not


// ***
// * Copy a row from of an existing BGRABitmap in a buffer without alpha chanel.
// * BGRABitmap can have an alpha chanel. If parameter indicate than alpha must be applyied then
// * alpha chanel is applyied on each pixel before to be copied in the target buffer
// * else the alpha chanel of the BGRABitmap is simply omited.
// * This procedure convert 8 bits chanels of the BGRABitmap into internal 32 bits chanels
// *
// * @author : Pascal Lemaître
// *
// * in : BGRABitmap created without loading picture _bitmap
// * in : row (x) of the BGRAGBitmap to be copied _row
// * in : if there is an alpha chanel in BGRABitmus shouls be applyied before pixels copy ? _alpha
// * out : row buffer where row will be copied _out_rowbuffer
// *       row buffer must be allocated with enough room to get all pixels of the row.
// *
// ***
procedure IU_BGRA_I_getRow (var _bitmap : TBGRABitmap ; _row : integer ; _alpha : boolean ; _out_rowbuffer : T_IU_Color32_Addr) ;
// getting a row from a BGRAbitmap Pix without alpha chanel in the _out_rowbuffer.
// if the BGRA bitmap pix has an alpha chanel :
//    1- if _alpha is true then alpha chanel is applyed on color before to set the pixels into the _out_rowbuffer
//    2- if _alpha is false then alpha chanel is omited. Only chanles of colors are set into the _out_rowbuffer


// ***
// * Copy a row from of an existing BGRABitmap in a buffer with alpha chanel.
// * BGRABitmap can have an alpha chanel. If therr is an alpha chanel it is converted in 32 bits before copy
// * else the alpha chanel of the target bufer is set to $FFFFFFFF
// * This procedure convert 8 bits chanels of the BGRABitmap into internal 32 bits chanels
// *
// * @author : Pascal Lemaître
// *
// * in : BGRABitmap created without loading picture _bitmap
// * in : row (x) of the BGRAGBitmap to be copied _row
// * out : row buffer where row will be copied _out_rowbuffer
// *       row buffer must be allocated with enough room to get all pixels of the row.
// *
// ***
procedure IU_BGRA_I_getRow (var _bitmap : TBGRABitmap ; _row : integer ; _out_rowbuffer : T_IU_AlphaColor32_Addr) ;
// getting a row from a BGRAbitmap Pix
// if BGRA bitmap has an alpha chanel it is converted in 32bits and setted into _out_rowbuffer ;
// if BGRA bitmap has no alpha chanel then one is created whith $FFFFFFFF value (opaque)


// ***
// * Copy a row from of a buffer without alpha chanel into a BGRABitmap with or without an alpha chanel
// * If BGRABitmap has an alpha chanel is set to $FF
// * This procedure convert 32 bits chanels of source buffer into 8 bits chanel of the BGRABitmap
// *
// * @author : Pascal Lemaître
// *
// * in out : BGRABitmap created without loading picture _bitmap
// * in : row (x) of the BGRAGBitmap to be copied _row
// * in : row buffer from which row will be copied _in_rowbuffer
// *
// ***
procedure IU_BGRA_I_setRow (var _bitmap : TBGRABitmap ; _row : integer ; _in_rowbuffer : T_IU_Color32_Addr) ;
// setting a row of a BGRAbitmap Pix without alpha chanel in the _in_rowbuffer.
// if the BGRA bitmap pix has an alpha chanel :
//    1- _alpha is set to $FF
//    2- nothing to do with alpha chanel


// ***
// * Copy a row from of a buffer with alpha chanel into a BGRABitmap with or without an alpha chanel
// * If BGRABitmap has not an alpha chanel, alpha is applyied on pixels before write them into BGRABitmap
// * This procedure convert 32 bits chanels of source buffer into 8 bits chanel of the BGRABitmap
// *
// * @author : Pascal Lemaître
// *
// * in out : BGRABitmap created without loading picture _bitmap
// * in : row (x) of the BGRAGBitmap to be copied _row
// * in : row buffer from which row will be copied _in_rowbuffer
// *
// ***
procedure IU_BGRA_I_setRow (var _bitmap : TBGRABitmap ; _row : integer ; _in_rowbuffer : T_IU_AlphaColor32_Addr) ;
// getting a row from a BGRAbitmap Pix
// if BGRA bitmap has an alpha chanel it is converted in 8bits and setted into _out_rowbuffer ;
// if BGRA bitmap has no alpha chanel then alpha is applyed on pixel before setting pixel in BGRA bitmap


// ***
// * Validate pixels write in a BGRABitmap
// * This procedure is needed by BGRABitmap itself.
// * It is necessary to call it to take account pixels modifications
// *
// * @author : Pascal Lemaître
// *
// ***
procedure IU_BGRA_I_Validate (var _bitmap : TBGRABitmap);
// BGRA Bitmap need a validation when pixels are setted with direct access

implementation

// ***
// * Getting size properties and alpha chanel of an existing BGRABitmap.
// *
// * @author : Pascal Lemaître
// *
// * in : BGRABitmap created without loading picture
// * out : number of rows (x) of the bitmap _width
// * out : nimber of lines (y) of the bitmap _height
// * out : is or is not an alpha chanel _alpha
// *
// ***
procedure IU_BGRA_I_getSize (var _bitmap : TBGRABitmap ; var _width, _height : integer ; var _alpha : boolean) ;
// getting size of the pix and if pix has an alpha chanel or notbegin
begin
  _width := _bitmap.Width;
  _height := _bitmap.Height;
  _alpha := _bitmap.HasTransparentPixels;
end;


// ***
// * Create an empty BGRABitmap. Empty BGRABitmap is always created with an alpha chanel
// *
// * @author : Pascal Lemaître
// *
// * out : BGRABitmap created without loading picture _bitmap
// * in : number of rows (x) of the bitmap _width
// * in : nimber of lines (y) of the bitmap _height
// *
// ***
procedure IU_BGRA_I_CreateEmpty (var _bitmap : TBGRABitmap ; _width, _height : integer) ;
// creating a new empty BGRABitmap with the size _width, _height. BGRABitmap is always created with an alpha chanel
begin
  _bitmap := TBGRABitmap.Create(_width, _height);
end;



// ***
// * Destroy (destructor) of an existing BGRABitmap
// *
// * @author : Pascal Lemaître
// *
// * in out : BGRABitmap _bitmap
// *
// ***
procedure IU_BGRA_I_Release (var _bitmap : TBGRABitmap) ;
// Realising an existing BGRA bitmap
begin
  _bitmap.Free;
end;



// ***
// * Create a BGRABitmap from a picture readed from disk.
// *
// * @author : Pascal Lemaître
// *
// * out : BGRABitmap created without loading picture
// * in : picture file name to load _filename
// *
// ***
procedure IU_BGRA_I_CreateLoad (var _bitmap : TBGRABitmap ; _filename : ansistring) ;
// creating a new empty BGRABitmap with the size _width, _height. BGRABitmap is always created with an alpha chanel
begin
  _bitmap := TBGRABitmap.Create(_filename);
end;



// ***
// * Save a BGRABitmap in a file on disk.
// *
// * @author : Pascal Lemaître
// *
// * in : BGRABitmap created without loading picture
// * in : target file name on disk _filename
// *
// ***
procedure IU_BGRA_I_Save (var _bitmap : TBGRABitmap ; _filename : ansistring);
// saving (not releasing after saving)
begin
  _bitmap.SaveToFile(_filename);
end;



// ***
// * Copy a row from of an existing BGRABitmap in a buffer without alpha chanel.
// * BGRABitmap can have an alpha chanel. If parameter indicate than alpha must be applyied then
// * alpha chanel is applyied on each pixel before to be copied in the target buffer
// * else the alpha chanel of the BGRABitmap is simply omited.
// * This procedure convert 8 bits chanels of the BGRABitmap into internal 32 bits chanels
// *
// * @author : Pascal Lemaître
// *
// * in : BGRABitmap created without loading picture _bitmap
// * in : row (x) of the BGRAGBitmap to be copied _row
// * in : if there is an alpha chanel in BGRABitmus shouls be applyied before pixels copy ? _alpha
// * out : row buffer where row will be copied _out_rowbuffer
// *       row buffer must be allocated with enough room to get all pixels of the row.
// *
// ***
procedure IU_BGRA_I_getRow (var _bitmap : TBGRABitmap ; _row : integer ; _alpha : boolean ; _out_rowbuffer : T_IU_Color32_Addr) ;
// getting a row from a BGRAbitmap Pix without alpha channel in the _out_rowbuffer.
// if the BGRA bitmap pix has an alpha channel :
//    1- if _alpha is true then alpha channel is applyed on color before to set the pixels into the _out_rowbuffer
//    2- if _alpha is false then alpha channel is omited. Only chanles of colors are set into the _out_rowbuffer
var
  _lineorder : integer ; // -1 1st line is on the bottom of the pix
                         //  1 1st line is on the top of the pix
  _isalpha : boolean ;  // BGRABitmap has an alpha channel : true, else : false;
  _start_line, _end_line : integer ; // depends of the height of the pix and the _lineorder
  currentLine : integer ; // line accessed by the copy loop (see bellow)
  p : PBGRAPixel ; // BGRABitmap pixel (addr in memory)
  img32pix : T_IU_Color32_Addr ; // image32 pixel (addr in memory)
  _loop : boolean ; // loop (see bellow) while _loop is true
  BGRA_Color : T_IU_AlphaColor8 ;
  IU_Color : T_IU_AlphaColor32;
  _coefAlpha : real;

begin
  // Getting the line order
  if _bitmap.LineOrder = riloTopToBottom then begin
    _lineorder := 1 ;
    _start_line := 0 ;
    _end_line := _bitmap.Height - 1;
  end else begin
    _lineorder := -1 ;
    _start_line := _bitmap.Height - 1;
    _end_line := 0 ;
  end;
  _isalpha := _bitmap.HasTransparentPixels;
  // Init
  // Set the first value in memory of the typed pointers
  img32pix := _out_rowbuffer;
  // p not need to be init because it will be in the loop of pixels copy
  // loop depends on _lineorder
  _loop := true;
  currentLine := _start_line;
  while _loop do begin
      // Getting the first pixel (left sided) of the line in the BGRABitmap
      p := _bitmap.ScanLine[currentLine];
      // Go to the right row
      inc (p,_row);
      // getting pixels values
      BGRA_Color.Color.R :=p^.red;    // red channel
      BGRA_Color.Color.G:=p^.green;  // green channel
      BGRA_Color.Color.B:=p^.blue;   // blue channel
      if _alpha then begin  // alpha channel (see description of the procedure)
        if _isalpha then BGRA_Color.alpha := p^.alpha
        else BGRA_Color.alpha := $FF
      end else BGRA_Color.alpha := $FF;
      // Converting 8 bits channels into 32 bits channels
      IU_ConvertAlphaColor8to32(BGRA_Color, IU_Color);
      // Applying alpha channel on colors channels
      // Alpha transformation coef
      _coefAlpha := $FFFFFFFF/IU_Color.alpha;
      IU_Color.Color.R:=round(IU_Color.Color.R/_coefAlpha);
      IU_Color.Color.G:=round(IU_Color.Color.G/_coefAlpha);
      IU_Color.Color.B:=round(IU_Color.Color.B/_coefAlpha);
      // Setting out_pixel in _out_rowbuffer
      _out_rowbuffer^.R:=IU_Color.Color.R;
      _out_rowbuffer^.G:=IU_Color.Color.G;
      _out_rowbuffer^.B:=IU_Color.Color.B;
      // prepare for next line
      currentLine := currentLine + _lineorder ;
      // testing the limits
      if (_lineorder < 1) then
        begin
          if currentLine < _end_line then _loop := false;
        end else
        if currentLine > _end_line then _loop := false;
      // inc _out_rowbuffer only if _loop is true
      if _loop then inc(_out_rowbuffer);
  end;
end;



// ***
// * Copy a row from of an existing BGRABitmap in a buffer with alpha chanel.
// * BGRABitmap can have an alpha chanel. If therr is an alpha chanel it is converted in 32 bits before copy
// * else the alpha chanel of the target bufer is set to $FFFFFFFF
// * This procedure convert 8 bits chanels of the BGRABitmap into internal 32 bits chanels
// *
// * @author : Pascal Lemaître
// *
// * in : BGRABitmap created without loading picture _bitmap
// * in : row (x) of the BGRAGBitmap to be copied _row
// * out : row buffer where row will be copied _out_rowbuffer
// *       row buffer must be allocated with enough room to get all pixels of the row.
// *
// ***
procedure IU_BGRA_I_getRow (var _bitmap : TBGRABitmap ; _row : integer ; _out_rowbuffer : T_IU_AlphaColor32_Addr) ;
// getting a row from a BGRAbitmap Pix
// if BGRA bitmap has an alpha channel it is converted in 32bits and setted into _out_rowbuffer ;
// if BGRA bitmap has no alpha channel then one is created whith $FFFFFFFF value (opaque)
var
  _lineorder : integer ; // -1 1st line is on the bottom of the pix
                         //  1 1st line is on the top of the pix
  _isalpha : boolean ;  // BGRABitmap has an alpha channel : true, else : false;
  _start_line, _end_line : integer ; // depends of the height of the pix and the _lineorder
  currentLine : integer ; // line accessed by the copy loop (see bellow)
  p : PBGRAPixel ; // BGRABitmap pixel (addr in memory)
  img32pix : T_IU_AlphaColor32_Addr ; // image32 pixel (addr in memory)
  _loop : boolean ; // loop (see bellow) while _loop is true
  BGRA_Color : T_IU_AlphaColor8 ;
  IU_Color : T_IU_AlphaColor32;
  _coefAlpha : real;

begin
  // Getting the line order
  if _bitmap.LineOrder = riloTopToBottom then begin
    _lineorder := 1 ;
    _start_line := 0 ;
    _end_line := _bitmap.Height - 1;
  end else begin
    _lineorder := -1 ;
    _start_line := _bitmap.Height - 1;
    _end_line := 0 ;
  end;
  _isalpha := _bitmap.HasTransparentPixels;
  // Init
  // Set the first value in memory of the typed pointers
  img32pix := _out_rowbuffer;
  // p not need to be init because it will be in the loop of pixels copy
  // loop depends on _lineorder
  _loop := true;
  currentLine := _start_line;
  while _loop do begin
      // Getting the first pixel (left sided) of the line in the BGRABitmap
      p := _bitmap.ScanLine[currentLine];
      // Go to the right row
      inc (p,_row);
      // getting pixels values
      BGRA_Color.Color.R :=p^.red;    // red channel
      BGRA_Color.Color.G:=p^.green;  // green channel
      BGRA_Color.Color.B:=p^.blue;   // blue channel
      if _isalpha then BGRA_Color.alpha := p^.alpha
        else BGRA_Color.alpha := $FF;
      // Converting 8 bits channels into 32 bits channels
      IU_ConvertAlphaColor8to32(BGRA_Color, IU_Color);
      // Setting out_pixel in _out_rowbuffer
      _out_rowbuffer^:=IU_Color;
      // prepare for next line
      currentLine := currentLine + _lineorder ;
      // testing the limits
      if (_lineorder < 1) then
        begin
          if currentLine < _end_line then _loop := false;
        end else
        if currentLine > _end_line then _loop := false;
      // inc _out_rowbuffer only if _loop is true
      if _loop then inc(_out_rowbuffer);
  end;
end;



// ***
// * Copy a row from of a buffer without alpha chanel into a BGRABitmap with or without an alpha chanel
// * If BGRABitmap has an alpha chanel is set to $FF
// * This procedure convert 32 bits chanels of source buffer into 8 bits chanel of the BGRABitmap
// *
// * @author : Pascal Lemaître
// *
// * in out : BGRABitmap created without loading picture _bitmap
// * in : row (x) of the BGRAGBitmap to be copied _row
// * in : row buffer from which row will be copied _in_rowbuffer
// *
// ***
procedure IU_BGRA_I_setRow (var _bitmap : TBGRABitmap ; _row : integer ; _in_rowbuffer : T_IU_Color32_Addr) ;
// setting a row of a BGRAbitmap Pix without alpha channel in the _in_rowbuffer.
// if the BGRA bitmap pix has an alpha channel :
//    1- _alpha is set to $FF
//    2- nothing to do with alpha channel
var
  _lineorder : integer ; // -1 1st line is on the bottom of the pix
                         //  1 1st line is on the top of the pix
  _isalpha : boolean ;  // BGRABitmap has an alpha channel : true, else : false;
  _start_line, _end_line : integer ; // depends of the height of the pix and the _lineorder
  currentLine : integer ; // line accessed by the copy loop (see bellow)
  p : PBGRAPixel ; // BGRABitmap pixel (addr in memory)
  img32pix : T_IU_Color32_Addr ; // image32 pixel (addr in memory)
  _loop : boolean ; // loop (see bellow) while _loop is true
  BGRA_Color : T_IU_AlphaColor8 ;
  IU_Color : T_IU_AlphaColor32;
  _coefAlpha : real;

begin
  // Getting the line order
  if _bitmap.LineOrder = riloTopToBottom then begin
    _lineorder := 1 ;
    _start_line := 0 ;
    _end_line := _bitmap.Height - 1;
  end else begin
    _lineorder := -1 ;
    _start_line := _bitmap.Height - 1;
    _end_line := 0 ;
  end;
  _isalpha := _bitmap.HasTransparentPixels;
  // Init
  // Set the first value in memory of the typed pointers
  img32pix := _in_rowbuffer;
  // p not need to be init because it will be in the loop of pixels copy
  // loop depends on _lineorder
  _loop := true;
  currentLine := _start_line;
  while _loop do begin
      // Getting the first pixel (left sided) of the line in the BGRABitmap
      p := _bitmap.ScanLine[currentLine];
      // Go to the right row
      inc (p,_row);
      // getting pixels values
      IU_Color.Color.R :=_in_rowbuffer^.R;    // red channel
      IU_Color.Color.G:=_in_rowbuffer^.G;     // green channel
      IU_Color.Color.B:=_in_rowbuffer^.B;     // blue channel
      if _isalpha then // alpha channel (see description of the procedure)
        IU_Color.alpha:=$FF;
      // Converting 32 bits channels into 8 bits channels
      IU_ConvertAlphaColor32to8(IU_Color, BGRA_Color);
      // Setting out_pixel in BGRABitmap
      p^.red:=BGRA_Color.Color.R;
      p^.green:=BGRA_Color.Color.G;
      p^.blue:=BGRA_Color.Color.B;
      if _isalpha then // alpha channel (see description of the procedure)
        p^.alpha:=BGRA_Color.alpha;
      // prepare for next line
      currentLine := currentLine + _lineorder ;
      // testing the limits
      if (_lineorder < 1) then
        begin
          if currentLine < _end_line then _loop := false;
        end else
        if currentLine > _end_line then _loop := false;
      // inc _in_rowbuffer only if _loop is true
      if _loop then inc(_in_rowbuffer);
  end;
end;



// ***
// * Copy a row from of a buffer with alpha chanel into a BGRABitmap with or without an alpha chanel
// * If BGRABitmap has not an alpha chanel, alpha is applyied on pixels before write them into BGRABitmap
// * This procedure convert 32 bits chanels of source buffer into 8 bits chanel of the BGRABitmap
// *
// * @author : Pascal Lemaître
// *
// * in out : BGRABitmap created without loading picture _bitmap
// * in : row (x) of the BGRAGBitmap to be copied _row
// * in : row buffer from which row will be copied _in_rowbuffer
// *
// ***
procedure IU_BGRA_I_setRow (var _bitmap : TBGRABitmap ; _row : integer ; _in_rowbuffer : T_IU_AlphaColor32_Addr) ;
// getting a row from a BGRAbitmap Pix
// if BGRA bitmap has an alpha channel it is converted in 8bits and setted into _out_rowbuffer ;
// if BGRA bitmap has no alpha channel then alpha is applyed on pixel before setting pixel in BGRA bitmap
var
  _lineorder : integer ; // -1 1st line is on the bottom of the pix
                         //  1 1st line is on the top of the pix
  _isalpha : boolean ;  // BGRABitmap has an alpha channel : true, else : false;
  _start_line, _end_line : integer ; // depends of the height of the pix and the _lineorder
  currentLine : integer ; // line accessed by the copy loop (see bellow)
  p : PBGRAPixel ; // BGRABitmap pixel (addr in memory)
  img32pix : T_IU_AlphaColor32_Addr ; // image32 pixel (addr in memory)
  _loop : boolean ; // loop (see bellow) while _loop is true
  BGRA_Color : T_IU_AlphaColor8 ;
  IU_Color : T_IU_AlphaColor32;
  _coefAlpha : real;

begin
  // Getting the line order
  if _bitmap.LineOrder = riloTopToBottom then begin
    _lineorder := 1 ;
    _start_line := 0 ;
    _end_line := _bitmap.Height - 1;
  end else begin
    _lineorder := -1 ;
    _start_line := _bitmap.Height - 1;
    _end_line := 0 ;
  end;
  _isalpha := _bitmap.HasTransparentPixels;
  // Init
  // Set the first value in memory of the typed pointers
  img32pix := _in_rowbuffer;
  // p not need to be init because it will be in the loop of pixels copy
  // loop depends on _lineorder
  _loop := true;
  currentLine := _start_line;
  while _loop do begin
      // Getting the first pixel (left sided) of the line in the BGRABitmap
      p := _bitmap.ScanLine[currentLine];
      // Go to the right row
      inc (p,_row);
      // getting pixels values
      if not _isalpha then begin
        // Applying alpha channel on colors channels
        // Alpha transformation coef
        _coefAlpha := $FFFFFFFF/IU_Color.alpha;
        IU_Color.Color.R:=round(IU_Color.Color.R/_coefAlpha);
        IU_Color.Color.G:=round(IU_Color.Color.G/_coefAlpha);
        IU_Color.Color.B:=round(IU_Color.Color.B/_coefAlpha);
        IU_Color.alpha := $0;
      end else begin
        IU_Color.Color.R :=_in_rowbuffer^.Color.R;    // red channel
        IU_Color.Color.G:=_in_rowbuffer^.Color.G;     // green channel
        IU_Color.Color.B:=_in_rowbuffer^.Color.B;     // blue channel
        IU_Color.alpha := _in_rowbuffer^.alpha;       // alpha channel
      end;
      // Converting 32 bits channels into 8 bits channels
      IU_ConvertAlphaColor32to8(IU_Color, BGRA_Color);
      // Setting out_pixel in BGRABitmap
      p^.red:= BGRA_Color.Color.R;
      p^.green:= BGRA_Color.Color.G;
      p^.blue:= BGRA_Color.Color.B;
      if _isalpha then
        p^.alpha:=BGRA_Color.alpha;
      // prepare for next line
      currentLine := currentLine + _lineorder ;
      // testing the limits
      if (_lineorder < 1) then
        begin
          if currentLine < _end_line then _loop := false;
        end else
        if currentLine > _end_line then _loop := false;
      // inc _in_rowbuffer only if _loop is true
      if _loop then inc(_in_rowbuffer);
  end;
end;



// ***
// * Validate pixels write in a BGRABitmap
// * This procedure is needed by BGRABitmap itself.
// * It is necessary to call it to take account pixels modifications
// *
// * @author : Pascal Lemaître
// *
// ***
procedure IU_BGRA_I_Validate (var _bitmap : TBGRABitmap);
// BGRA Bitmap need a validation when pixels are setted with direct access
begin
  _bitmap.InvalidateBitmap;
end;

end.

