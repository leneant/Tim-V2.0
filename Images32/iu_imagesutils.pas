unit IU_ImagesUtils;

{$mode objfpc}{$H+}
// ***
// * Unit provides conversion services beetwen 8 bits per chanel picture and 64 bits per chanel picture
// * Creation Date : 2017 September
// *
// * Version : 1.0
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
  Classes, SysUtils, math, IU_Types, IU_Exceptions, IU_I18N_Messages;

  // ***
  // * Convert a 8 bits chanel color or alpha (in_chanel8) in a 32 bytes chanel color or alpha (out_chanel32)
  // * Expanding 8 bytes to 32 bytes
  // *
  // * @author : Pascal Lemaître
  // *
  // * in : chanel of color in 8 bytes in_chanel8
  // * out : chanel of color in 32 bytes out_chanel32
  // ***
  procedure IU_ConvertColorChanel8to32 (var in_chanel8 : T_IU_AColorChanel8 ; var out_chanel32 : T_IU_AColorChanel32) ;
  // Convert a 8 bit chanel color or alpha (in_chanel8) in a 64 bits chanel color or alpha (out_chanel64)
  // ***




  // ***
  // * Convert a 32 bytes chanel color or alpha (in_chanel8) in a 8 bytes chanel color or alpha (out_chanel32)
  // * Reducing 32 bytes into 8 bytes
  // *
  // * @author : Pascal Lemaître
  // *
  // * in : chanel of color in 32 bytes in_chanel32
  // * out : chanel of color in 8 bytes out_chanel8
  // ***
  procedure IU_ConvertColorChanel32to8 (var in_chanel32 : T_IU_AColorChanel32 ; var out_chanel8 : T_IU_AColorChanel8);
  // Convert a 8 bit chanel color or alpha (in_chanel8) in a 32 bits chanel color or alpha (out_chanel32)
  // ***




  // ***
  // * Convert a RGB color with 8 bytes chanel color in a RGB color with 32 bytes per chanel
  // * Expanding all chanels 8 bytes to 32 bytes
  // *
  // * @author : Pascal Lemaître
  // *
  // * in : RGB color with 8 bytes per chanel in_color8
  // * out : RGB color with 32 bytes per chanel out_color32
  // ***
  procedure IU_ConvertColor8to32 (var in_color8 : T_IU_Color8 ; var out_color32 : T_IU_Color32);
  // Convert 8 bits per canal color in 32 bits per canal color
  // ***




  // ***
  // * Convert a RGB color with 32 bytes chanel color into a RGB color with 8 bytes per chanel
  // * Reducing all chanels 32 bytes into 8 bytes
  // *
  // * @author : Pascal Lemaître
  // *
  // * in : RGB color with 32 bytes per chanel in_color32
  // * out : RGB color with 8 bytes per chanel out_color8
  // ***
  procedure IU_ConvertColor32to8 (var in_color32 : T_IU_Color32 ; var out_color8 : T_IU_Color8);
  // Convert 32 bits per canal color in 8 bits per canal color
  // ***




  // ***
  // * Convert a RGBA (with alpha chanel) color with 8 bytes chanel color in a RGBA color with 32 bytes per chanel
  // * Expanding all chanel 8 bytes to 32 bytes
  // *
  // * @author : Pascal Lemaître
  // *
  // * in : RGBA color with 8 bytes per chanel in_color8
  // * out : RGBA color with 32 bytes per chanel out_color32
  // ***
  procedure IU_ConvertAlphaColor8to32 (var in_color8 : T_IU_AlphaColor8 ; var out_color32 : T_IU_AlphaColor32);
  // Convert 8 bits per canal color in 32 bits per canal color
  // ***




  // ***
  // * Convert a RGBA (with alpha chanel) color with 32 bytes chanel color in a RGBA color with 8 bytes per chanel
  // * Reducing all chanel 32 bytes into 8 bytes
  // *
  // * @author : Pascal Lemaître
  // *
  // * in : RGBA color with 32 bytes per chanel in_color32
  // * out : RGBA color with 8 bytes per chanel out_color8
  // ***
  procedure IU_ConvertAlphaColor32to8 (var in_color32 : T_IU_AlphaColor32 ; var out_color8 : T_IU_AlphaColor8);
  // Convert 32 bits per canal color in 8 bits per canal color
  // ***




  // ***
  // * Calc the logical position (x, y coordonates) of the first pixel of a column in a picture
  // * Converting x, y coordonates into a logical position from the begining of the picture
  // * Logical position is a move from the begining of the picture in pixels not in bytes
  // *
  // * @author : Pascal Lemaître
  // *
  // * in : size of the picture (width and height) iu_whsize
  // * in : num of the column seeked
  // *
  // * return : The logicial position from the begining of the pix of the first line (y) of the column seeked (x)
  // ***
  function IU_LSeek_Col (iu_whsize : T_IU_WHPixSize; numcol : T_IU_PosPix) : T_IU_PosPix; inline;
  // Return the logical position of a pix in memory or file (without size of a pixel it gives the indice of the col)
  // numcol the number of the col (x)
  // ***




  // ***
  // * Calc the logical position (x, y coordonates) of the first pixel of a line and colum in a picture
  // * Converting x, y coordonates into a logical position from the begining of the picture
  // * Logical position is a move from the begining of the picture in pixels not in bytes
  // *
  // * @author : Pascal Lemaître
  // *
  // * in : size of the picture (width and height) iu_whsize
  // * in : num of the line seeked
  // * in : num of the column seeked
  // *
  // * return : The logical position from the begining of the pix of the line seeked (y) and the column seeked (x)
  // ***
  function IU_LSeek_Pix (iu_whsize : T_IU_WHPixSize; numline, numcol : T_IU_PosPix) : T_IU_PosPix; inline;
  // Return the logical position of a pix in memory or file (without size of a pixel it gives the indice of the pix)
  // numline number of the line (y)
  // numcol number of the col (x)
  // ***




  // ***
  // * Calc the physical position (x, y coordonates) of the first pixel of a column in a picture
  // * Converting x, y coordonates into a physical position from the begining of the picture
  // * Physical position is a move from the begining of the picture in bytes not in pixels
  // *
  // * @author : Pascal Lemaître
  // *
  // * in : size of the picture (width and height) iu_whsize
  // * in : num of the column seeked
  // * in : is there an alpha chanel in the picture iu_alpha
  // *
  // * return : The physical position from the begining of the pix of the first line (y) of the column seeked (x)
  // ***
  function IU_PSeek_Col (iu_whsize : T_IU_WHPixSize; numcol : T_IU_PosPix; iu_alpha:boolean) : T_IU_RealPosPix; inline ;
  // numcol number of the col (x)
  // Return the physical position of a pix in memory or file (with size of a pixel it gives the addr of the col)
  // ***




  // ***
  // * Calc the physical position (x, y coordonates) of the first pixel of a line and colum in a picture
  // * Converting x, y coordonates into a logical position from the begining of the picture
  // * Physical position is a move from the begining of the picture in bytes not in pixels
  // *
  // * @author : Pascal Lemaître
  // *
  // * in : size of the picture (width and height) iu_whsize
  // * in : num of the line seeked
  // * in : num of the column seeked
  // * in : is there an alpha chanel in the picture iu_alpha
  // *
  // * return : The physical position from the begining of the pix of the line seeked (y) and the column seeked (x)
  // ***
  function IU_PSeek_Pix (iu_whsize : T_IU_WHPixSize; numline, numcol : T_IU_PosPix; iu_alpha:boolean) : T_IU_RealPosPix; inline ;
  // Return the physical position of a pix in memory or file (with size of a pixel it gives the addr of the pix)
  // numline number of the line (y)
  // numcol number of the col (x)
  // ***



implementation

// ***
// * Convert a 8 bytes chanel color or alpha (in_chanel8) in a 32 bytes chanel color or alpha (out_chanel32)
// * Expanding 8 bytes to 32 bytes
// *
// * @author : Pascal Lemaître
// *
// * in : chanel of color in 8 bytes in_chanel8
// * out : chanel of color in 32 bytes out_chanel32
// ***
procedure IU_ConvertColorChanel8to32 (var in_chanel8 : T_IU_AColorChanel8 ; var out_chanel32 : T_IU_AColorChanel32);
begin
  // Algo
  // 256 values (0 to 255) are projected on 2^32 values
  // => 2^8 values are projected on 2^32 values
  // Then 2^8 . x = 2^32 <=> x = 2^(32-8) <=> x = 2^24
  // see IU_Types unit
  out_chanel32 := in_chanel8 * T_IU_AColorChanel32(K_IU_ConvertCoef8_32) ;
end;






// ***
// * Convert a 32 bytes chanel color or alpha (in_chanel8) in a 8 bytes chanel color or alpha (out_chanel32)
// * Reducing 32 bytes into 8 bytes
// *
// * @author : Pascal Lemaître
// *
// * in : chanel of color in 32 bytes in_chanel32
// * out : chanel of color in 8 bytes out_chanel8
// ***
procedure IU_ConvertColorChanel32to8 (var in_chanel32 : T_IU_AColorChanel32 ; var out_chanel8 : T_IU_AColorChanel8);
begin
  // opposite of IU_ConvertColorChanel8to64
  out_chanel8 := T_IU_AColorChanel8(in_chanel32 div T_IU_AColorChanel32(K_IU_ConvertCoef8_32)) ;
end;






// ***
// * Convert a RGB color with 8 bytes chanel color in a RGB color with 32 bytes per chanel
// * Expanding all chanels 8 bytes to 32 bytes
// *
// * @author : Pascal Lemaître
// *
// * in : RGB color with 8 bytes per chanel in_color8
// * out : RGB color with 32 bytes per chanel out_color32
// ***
procedure IU_ConvertColor8to32 (var in_color8 : T_IU_Color8 ; var out_color32 : T_IU_Color32);
// Convert 8 bits per canal color in 32 bits per canal color
begin
  IU_ConvertColorChanel8to32 (in_color8.R, out_Color32.R);
  IU_ConvertColorChanel8to32 (in_color8.G, out_Color32.G);
  IU_ConvertColorChanel8to32 (in_color8.B, out_Color32.B);
end;






// ***
// * Convert a RGB color with 32 bytes chanel color into a RGB color with 8 bytes per chanel
// * Reducing all chanels 32 bytes into 8 bytes
// *
// * @author : Pascal Lemaître
// *
// * in : RGB color with 32 bytes per chanel in_color32
// * out : RGB color with 8 bytes per chanel out_color8
// ***
procedure IU_ConvertColor32to8 (var in_color32 : T_IU_Color32 ; var out_color8 : T_IU_Color8);
// Convert 32 bits per canal color in 8 bits per canal color
begin
  IU_ConvertColorChanel32to8 (in_color32.R, out_Color8.R);
  IU_ConvertColorChanel32to8 (in_color32.G, out_Color8.G);
  IU_ConvertColorChanel32to8 (in_color32.B, out_Color8.B);
end;





// ***
// * Convert a RGBA (with alpha chanel) color with 8 bytes chanel color in a RGBA color with 32 bytes per chanel
// * Expanding all chanel 8 bytes to 32 bytes
// *
// * @author : Pascal Lemaître
// *
// * in : RGBA color with 8 bytes per chanel in_color8
// * out : RGBA color with 32 bytes per chanel out_color32
// ***
 procedure IU_ConvertAlphaColor8to32 (var in_color8 : T_IU_AlphaColor8 ; var out_color32 : T_IU_AlphaColor32);
// Convert 8 bits per canal color in 32 bits per canal color
 begin
   IU_ConvertColor8to32 (in_color8.color, out_Color32.color);
   IU_ConvertColorChanel8to32 (in_color8.alpha, out_color32.alpha);
 end;






// ***
// * Convert a RGBA (with alpha chanel) color with 32 bytes chanel color in a RGBA color with 8 bytes per chanel
// * Reducing all chanel 32 bytes into 8 bytes
// *
// * @author : Pascal Lemaître
// *
// * in : RGBA color with 32 bytes per chanel in_color32
// * out : RGBA color with 8 bytes per chanel out_color8
// ***
procedure IU_ConvertAlphaColor32to8 (var in_color32 : T_IU_AlphaColor32 ; var out_color8 : T_IU_AlphaColor8);
// Convert 32 bits per canal color in 8 bits per canal color
begin
  IU_ConvertColor32to8 (in_color32.color, out_Color8.color);
  IU_ConvertColorChanel32to8 (in_color32.alpha, out_color8.alpha);
end;





// ***
// * Calc the logical position (x, y coordonates) of the first pixel of a column in a picture
// * Converting x, y coordonates into a logical position from the begining of the picture
// * Logical position is a move from the begining of the picture in pixels not in bytes
// *
// * @author : Pascal Lemaître
// *
// * in : size of the picture (width and height) iu_whsize
// * in : num of the column seeked
// *
// * return : The logicial position from the begining of the pix of the first line (y) of the column seeked (x)
// ***
function IU_LSeek_Col (iu_whsize : T_IU_WHPixSize; numcol : T_IU_PosPix) : T_IU_PosPix; inline;
// Return the logical position of a column in memory or file
// num the number of the col (x)
begin
  // Algo scan cols before lines then its better to organize memory by colomns first.
  if numcol > iu_whsize.width - 1 then
    raise  IU_EOutOfBounds.Create(IU_ExceptionsMessages[IU_CurrentLang, K_IU_ExceptMSG_OutOfPixBounds]);
  IU_LSeek_Col := iu_whsize.height * numcol ;
end;






// ***
// * Calc the logical position (x, y coordonates) of the first pixel of a line and colum in a picture
// * Converting x, y coordonates into a logical position from the begining of the picture
// * Logical position is a move from the begining of the picture in pixels not in bytes
// *
// * @author : Pascal Lemaître
// *
// * in : size of the picture (width and height) iu_whsize
// * in : num of the line seeked
// * in : num of the column seeked
// *
// * return : The logical position from the begining of the pix of the line seeked (y) and the column seeked (x)
// ***
function IU_LSeek_Pix (iu_whsize : T_IU_WHPixSize; numline, numcol : T_IU_PosPix) : T_IU_PosPix; inline;
// Return the logical position of a pix in memory or file
// numline number of the line (y)
// numcol number of the col (x)
begin
  // Algo scan cols before lines then its better to organize memory by colomns first.
  if numline > iu_whsize.height - 1 then
    raise  IU_EOutOfBounds.Create(IU_ExceptionsMessages[IU_CurrentLang, K_IU_ExceptMSG_OutOfPixBounds])
  else if numcol > iu_whsize.width - 1 then
    raise  IU_EOutOfBounds.Create(IU_ExceptionsMessages[IU_CurrentLang, K_IU_ExceptMSG_OutOfPixBounds]);
  IU_LSeek_Pix := iu_whsize.height * numline + numcol ;
end;






// ***
// * Calc the physical position (x, y coordonates) of the first pixel of a column in a picture
// * Converting x, y coordonates into a physical position from the begining of the picture
// * Physical position is a move from the begining of the picture in bytes not in pixels
// *
// * @author : Pascal Lemaître
// *
// * in : size of the picture (width and height) iu_whsize
// * in : num of the column seeked
// * in : is there an alpha chanel in the picture iu_alpha
// *
// * return : The physical position from the begining of the pix of the first line (y) of the column seeked (x)
// ***
function IU_PSeek_Col (iu_whsize : T_IU_WHPixSize; numcol : T_IU_PosPix; iu_alpha:boolean) : T_IU_RealPosPix; inline;
// Return the physical position of a pix in memory or file (with size of a pixel it gives the addr of the col)
// numcol number of the col (x)
var
  Lpos : T_IU_PosPix;
begin
  if numcol > iu_whsize.width - 1 then
    raise  IU_EOutOfBounds.Create(IU_ExceptionsMessages[IU_CurrentLang, K_IU_ExceptMSG_OutOfPixBounds]);
  Lpos := IU_LSeek_Col(iu_whsize, numcol) ;
  if iu_alpha then
    IU_PSeek_Col := Lpos * sizeof(T_IU_AlphaColor32)
  else
    IU_PSeek_Col := Lpos * sizeof(T_IU_Color32);
end;






// ***
// * Calc the physical position (x, y coordonates) of the first pixel of a line and colum in a picture
// * Converting x, y coordonates into a logical position from the begining of the picture
// * Physical position is a move from the begining of the picture in bytes not in pixels
// *
// * @author : Pascal Lemaître
// *
// * in : size of the picture (width and height) iu_whsize
// * in : num of the line seeked
// * in : num of the column seeked
// * in : is there an alpha chanel in the picture iu_alpha
// *
// * return : The physical position from the begining of the pix of the line seeked (y) and the column seeked (x)
// ***
function IU_PSeek_Pix (iu_whsize : T_IU_WHPixSize; numline, numcol : T_IU_PosPix; iu_alpha:boolean) : T_IU_RealPosPix; inline;
// Return the physical position of a pix in memory or file (with size of a pixel it gives the addr of the pix)
// numline number of the line (y)
// numcol number of the col (x)
var
  Lpos : T_IU_PosPix;
begin
  if numcol > iu_whsize.width - 1 then
    raise  IU_EOutOfBounds.Create(IU_ExceptionsMessages[IU_CurrentLang, K_IU_ExceptMSG_OutOfPixBounds])
  else if numline > iu_whsize.height - 1 then
    raise  IU_EOutOfBounds.Create(IU_ExceptionsMessages[IU_CurrentLang, K_IU_ExceptMSG_OutOfPixBounds]);
  Lpos := IU_LSeek_pix(iu_whsize, numline, numcol) ;
  if iu_alpha then
    IU_PSeek_Pix := Lpos * sizeof(T_IU_AlphaColor32)
  else
    IU_PSeek_Pix := Lpos * sizeof(T_IU_Color32);
end;

end.

