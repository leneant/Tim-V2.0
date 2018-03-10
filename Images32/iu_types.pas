unit IU_Types;

{$mode objfpc}{$H+}
// ***
// * Unit provides all needed types and globals var and const for 32 bytes per chanels pictures
// * Creation Date : 2017 September
// *
// * Version : 0.3
// * Version Date : 2017 December
// * Version Contributors : Pascal Lemaître
// *
// * @authors : Pascal Lemaître
// *
// * @see :
// *
// *
// * Team : TIm (Traitement d'Images)
// *
// * v0.2 adding key code for tab key in KeyDown event
// * V0.3 Adapt size of frame according to installed RAM on computer
// *
// * 2017
// ***

interface

uses
  Classes, SysUtils
  ;

const
  // 256 values (0 to 255) are projected on 2^64 values
  // => 2^8 values are projected on 2^64 values
  // Then 2^8 . x = 2^32 <=> x = 2^(32-8) <=> x = 2^24
  K_IU_ConvertCoef8_32 = $FFFFFF;

  // Maximum value for a 32 bit chanal of color (including alpha chanel)
  K_Max32PixelColorValue = $FFFFFFFF;

  // Max windows picture size = 52 MPixels 4 chanels and 4 bytes by chanel
  // It is the same as MemoryFrame (see IU_Frames unit) for all threads
  // ***
  // * Modified V0.3
  // K_IU_MaxMemorySize = $32000000; // equal to 800 Mb
  K_IU_MaxMemorySize = $9000000 ; // 9 Mpix for 1GBytes of RAM
  // ***

  // Max temporary files used by Tim
  K_IU_MaxTempFiles = 10;

  // Max threads
  K_IU_MaxThreads = 4;
  // Each thread has its own memoryframe
  // Memory size results in K_IU_MaxMemorySize div K_IU_MaxThreads
  // This value is initializing when unit is loaded (see the bottom of implementation section)
  // But each threads work on source frame (read only) and target frame (write only)
  // Then total of memory needed is around 1.5 GBytes.

  // Source vs target frames
  K_IU_Source_Frames = 0;
  K_IU_Temporary_Frame = 1; // Needed for some algo like background extraction and application
  K_IU_Target_Frames = 2;

  // Background signal extraction max window
  K_IU_BKGRDSigExtract_MaxWindow = 700;
  // Need to fix the max value of window to 700 in the algo of background signal extraction
  // In Tim V1.x this max was reached for pix with 10 Mega pixels of width.
  // In the new version this value of 700 will not have to over pass even if the pix size is bigger.
  // See the bottom of this unit for explanation of the value


  // Recovery area follows a function like an logarithm (it is not a logarithm).
  // For pix of 32768 pixels un max size (width) the recovery area is K_IU_BKGRDSigExtract_MaxWindow pixels
  // To define recovery area for preview pixeils we use an function (see. IU_GeneralUtils Units).
  // This function takes the max of recovery area, a flex point where the delta of the curves become significantly low
  // And an adjustment of the initial delta of the curve.
  // For the model of this curve see the file RecoveryAreaModel.ods at the root of the project
  K_IU_RecoveryAreaCurveFlexPoint = 3000.0;
  K_IU_RecoveryAreaCurveCoef = 1.0;
  // Size if preview pix (x pixels * x pixels max)
  K_IU_PreviewMaxSize = 1200;

  // Recovery area beetwen access frames (see IU_Frames unit)
  K_IU_RecoveryArea = K_IU_BKGRDSigExtract_MaxWindow div 2;
  // x pixels needed beetwen two frames for calc of target picture
  // Align with the windows use for background signal extraction
  // See the bottom of this unit for explanation of the value

  // Preview pix declarations
  K_IU_OriginPix = 0;
  K_IU_PreviewPix = 1;

  // Max coord of a pixel in x (row) or y (line)
  K_MaxPix = 32767;

  // Min coord of a pixel in x (row) or y (line)
  K_MinPix = 0;

  // ***
  // * Add v0.2
  // *
  // KeyBoard Key
  K_IU_Key_TAB = 9;
  // ***

type
  // Luminence curves array
  T_IU_MyTCurves = array [0..255] of longint;
  // normally luminence can have a value on 32 bits. But the conversion from 8 bits to 32 bits
  // leave many value without pixels. Then for readibility a reduction on 256 values is better.

  // max of x or y pixels in a picture
  T_IU_MaxPix = smallint; // 32767 MAX

  // Logical pix position in memory or in a file
  T_IU_PosPix = longword;

  // Physical pix position in memory or in a file
  T_IU_RealPosPix = Qword;

  // Size of buffer
  T_IU_BufSize = T_IU_RealPosPix;

  // Allocated size in file or memory
  T_IU_Size = T_IU_PosPix;

  // Max pix position for x and y axis.
  T_IU_WHPixPosition = packed record
    width, height : T_IU_MaxPix;   // 32767 * 32767 pix max
  end;

  // Size of the pix
  T_IU_WHPixSize = T_IU_WHPixPosition;

  // 32 bits per chanel image colors declarations
  T_IU_AColorChanel32 = dword ;

  T_IU_Color32 = packed record
    R, G, B : T_IU_AColorChanel32;
  end;

  T_IU_AlphaColor32 = packed record
    Color : T_IU_Color32;
    alpha : T_IU_AColorChanel32;
  end;

  // 8 bits per chanel image colors declarations
  T_IU_AColorChanel8 = byte ;
  T_IU_Color8 = packed record
    R, G, B : T_IU_AColorChanel8;
  end;

  T_IU_AlphaColor8 = packed record
    Color : T_IU_Color8;
    alpha : T_IU_AColorChanel8;
  end;

  // 32 bits pixel addr in memory
  T_IU_Color32_Addr = ^T_IU_Color32 ;

  // 32 bits alpha pixel addr un memory
  T_IU_AlphaColor32_Addr = ^T_IU_AlphaColor32;

  // Picture data
  T_IU_PictData = packed record
    case alpha : boolean of
      false : (IU_V_PixData : T_IU_Color32);
      true  : (IU_V_AlphaPixData : T_IU_AlphaColor32);
    end ;

  // Pointer on T_IUPictData Use to point on memory area of picture's pixels
  T_IU_PPictData = ^T_IU_PictData ;

  //Window memory Picture type
  // all pixels of the picture are not loaded in memory
  // just a window on picture area is loaded
  // Window start at iu_x and iu_y
  // Window size is iu_width and iu_height
  T_IU_WindowPicture = packed record
    iu_alpha : boolean ;   // is pix with alpha chanel : true yes, false no
    iu_whposition : T_IU_WHPixPosition ; // start line and col position of the windows from the 0,0 position of the pix
    iu_whsize : T_IU_WHPixSize ; // width and height of the window
    iu_windowdata : T_IU_PPictData; // Pointer on the memory area of the windows
  end;

  //Temprary picture file
  // all pixels of a picture are written on disk in temporaries files
  // The access of record is a direct access.
  // A windows on an area is loaded into memory (see T_IU_WindowPicture)
  T_IU_TemporaryPictureFile = record
    iu_temporaryFileName : ansistring ;
    iu_alpha : boolean ;
    iu_whsize : T_IU_WHPixSize ;
  end;

  // Curves of image luminance
  IU_MyTCurves = Array[0..256] of longint;

  // Frame type source vs target frame
  T_IU_TypeOfFrame = K_IU_Source_Frames..K_IU_Target_Frames;

  // Frame index of Threads
  T_IU_FrameThreadIndex = 1..K_IU_MaxThreads;

  // Origin or Preview pix scalar for index of arrays
  T_IU_PixType = K_IU_OriginPix..K_IU_PreviewPix;


  // Global frame index descriptor
  T_IU_FrameIndex = record
    _iu_type : T_IU_TypeOfFrame;
    _iu_index : T_IU_FrameThreadIndex;
  end;

  var
    V_IU_tempDirectory :  ansistring ;
    V_IU_MaxInWindowSize, V_IU_MaxOutWindowSize :  T_IU_Size;
    V_IU_MaxInPreviewSize, V_IU_MaxOutPreviewSize : T_IU_Size;


  // ***
  // * Calculate de recovery area for an access frame (see unit IU_Frames) based on the max of width or height size.
  // *
  // * @author : Pascal Lemaître
  // *
  // * in : max of the width height dimension of a picture
  // *
  // * return : the size in pixels of the recovery area
  // *
  // ***
  function IU_BackGroudExtractionArea (size : T_IU_MaxPix) : T_IU_MaxPix;
  // function return size of recovery area needed for a pix with max size = size
  // see the file RecoveryAreaModel.ods at the root of the project.

implementation

// ***
// * Calculate de recovery area for an access frame (see unit IU_Frames) based on the max of width or height size.
// *
// * @author : Pascal Lemaître
// *
// * in : max of the width height dimension of a picture
// *
// * return : the size in pixels of the recovery area
// *
// ***
function IU_BackGroudExtractionArea (size : T_IU_MaxPix) : T_IU_MaxPix;
// function return size of recovery area needed for a pix with max size = size
// see the file RecoveryAreaModel.ods at the root of the project.
  var _linearAdjust, _ret : real;
  begin
    _linearAdjust := ((32768 * K_IU_RecoveryAreaCurveCoef) + K_IU_RecoveryAreaCurveFlexPoint) / 32768;
    _ret := ((size / (size * K_IU_RecoveryAreaCurveCoef +  K_IU_RecoveryAreaCurveFlexPoint)) * _linearAdjust * K_IU_BKGRDSigExtract_MaxWindow);
    IU_BackGroudExtractionArea := trunc(_ret) + 1 ;
    // Div by MaxThread because there are MaxThreads to work on a pix and there are one frame for one thread
  end;



// ***
// * Globals var initialization when program start up
// *
// * @author : Pascal Lemaître
// *
// * Auto adaptative to the target os (windows or linux) only at compilation
// *
// ***
begin
  {$ifdef Windows}
  // Under windows tempdir is in the user directory
  // but no standardization then userdir is the temporary dir
  V_IU_tempDirectory := ParamStr(0);
  {$else}
  // Unider linux temporary dir is standardized to /tmp
  V_IU_tempDirectory := '/tmp';
  {$endif}

  // Init of frame's memory size
  // ***
  // * Delete V0.3 move to IU_Frames init section
  {
  V_IU_MaxOutWindowSize := trunc (K_IU_MaxMemorySize * IU_getAdaptationRAMCoef / K_IU_MaxThreads);
  V_IU_MaxInWindowSize :=  V_IU_MaxOutWindowSize + (K_IU_BKGRDSigExtract_MaxWindow * 32 * 10000) ;
  // RecoveryArea is the half of the windows for background signal extraction.
  //
  // For background luminance alorithmes we need to have enought recovery area beetwen frames
  // It needs a temporary picture too for luminance mask (this pix is write and read pix)
  // By default in Tim V1.x the zone was 700 pixels max then in fact with this value we need
  // 700 * 16 * 32768 Bytes more for each frame (memory frame if we don't want a swap for on column)
  // It's around 350 MBytes more for each memory frame.
  // The best of camera is 52 Mpix. Each frame is sized to accept this size. It take aroud 800 Mb.
  // Tim manage 3 different types of pix (source, temporary and target) Then we need 3 * 800 Mb = 1.8 Gb.
  // For recovery area, the max for a 52 Mpix is around 14000 pixels for width and 10000 for height.
  // Then we can get 10000 * 4 * 4 * 700 as a max value (near 110 Mb) by frame.
  // 8 frames need recovery area (in and temporary). Only 4 need 110 Mb more and 4 need 55 Mb more
  //                  (left and right frame don't need 700 pixel of recovery area. 350 are enought)
  // The sum for three pix without recovery area is 800 Mb * 3 (pix) = 1.8 Gb
  // We must add recovery area :
  // 4 * 110 Mb + 4 * 55Mb = 4 * 165 Mb = 660 Mb.
  // Then we will use around 1.8 Gb + 0.65 Gb. It's around 2.5 Gb.

  // Memory frame allocation for previews pix
  V_IU_MaxInPreviewSize := trunc((K_IU_PreviewMaxSize * (K_IU_PreviewMaxSize + IU_BackGroudExtractionArea (K_IU_PreviewMaxSize)))  * 4 * 4 / K_IU_MaxThreads)+1;
  V_IU_MaxOutPreviewSize := trunc((K_IU_PreviewMaxSize * K_IU_PreviewMaxSize) * 4 * 4 / K_IU_MaxThreads)+1;
  // 4 * 4 Four channels, 4 bytes by channel

  // Then we need to add theses of the precedent allocation.
  // With FlexPoint at 3000 and coef at 1 (see constants declarations)
  // The area for a preview is 219 pixels (see the file RecoveryAreaModel at the root of the project)
  // Then we have 2 pix with 1200 * (1200 + 219) * 4 * 4 bytes and one with 1200 * 1200 * 4 * 4 bytes (four chanels of 4 Bytes)
  // We have 52 M bytes (0.05 Gb) more for source and temporary frames and 22 M bytes (0.02 Gb) for target frame.
  // The we need 2.5 Gb + 0.05 Gb + 0.02 Gb = 2.57 Gb.
  // Now, normaly, PC have 4 Gb. Some one (low powered) have only 3 Gb.
  // We can assume that Tim need 4 Gb to work correctly.
  }
  // ***
end.

