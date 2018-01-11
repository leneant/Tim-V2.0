unit IU_ExifUtils;
// ***
// * Unit defining controls objects like progress bar, scroll bars, tracks bars...
// * Creation date : 2018 January
// *
// * Version : 0.1
// * Version Date : 2018 January
// * Version contributors : Pascal Lemaître
// *
// * @authors : Pascal Lemaître
// *
// * @see :
// *
// * Team : TIm (Traitement d'IMages)
// *
// * 2017-2018
// ***


{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Process ;

const
  // ***
  // Exif data to display
  IU_K_Exif_APN              = 'APN';
  IU_K_Exif_Artist           = 'Artist';
  IU_K_Exif_Copyright        = 'Copyright';
  IU_K_Exif_ShootDate        = 'ShootDate';
  IU_K_Exif_Orientation      = 'Orientation';
  IU_K_Exif_ISO              = 'ISO';
  IU_K_Exif_Aperture         = 'Aperture';
  IU_K_Exif_Speed            = 'Speed';
  IU_K_Exif_Focal            = 'Focal';
  IU_K_Exif_ColorSpace       = 'ColorSpace';
  IU_K_Exif_Flash            = 'Flash';

type
  // ***
  // * Type of exif record
  IU_T_Exif = record
    Exif : string;
    value : string;
  end;

  // ***
  // * List of exif data
  IU_T_ExifArray = array of IU_T_Exif;

// ***
// * Get specifics Exif informations from a photo
// *
// * Creator : Pascal Lemaître
// *
// * in : filename of the photo (global path)
// *
// * return : list of exif (exif name and exif data)
// *
function getExif (filename : string) : IU_T_ExifArray;

implementation

const
  // ***
  // Exif ID of exiv2
  CameraModel             = 'Exif.Image.Model';
  Artist                  = 'Exif.Image.Artist';
  Copyright               = 'Exif.Image.Copyright';
  ShootDate               = 'Exif.Photo.DateTimeOriginal';
  PhotoOrientation        = 'Exif.Image.Orientation';
  ISO                     = 'Exif.Photo.ISOSpeedRatings';
  Aperture                = 'Exif.Photo.FNumber';
  Speed                   = 'Exif.Photo.ShutterSpeedValue';
  Focal                   = 'Exif.Photo.FocalLength';
  ColorSpace              = 'Exif.Photo.ColorSpace';
  Flash                   = 'Exif.Photo.Flash';


// ***
// * execute exiv2 with right parameters and filename
// *
// * Creator : Pascal Lemaître
// *
// * in : filename (global path) of the photo
// *
// * out : List of string as output of exiv2
// *
procedure loadExif (filename : string ; var AStringList : TStringList);
var
  AProcess: TProcess;

begin
  // exec command
    // This is where our program starts to run
    // Now we will create the TProcess object, and
    // assign it to the var AProcess.
    AProcess := TProcess.Create(nil);

    // Tell the new AProcess what the command to execute is.
    AProcess.PipeBufferSize := 2048;
    {$ifdef windows}
    AProcess.Executable := getcurrentdir + '\exiv2.exe';
    AProcess.Parameters.Add('-K');
    AProcess.Parameters.Add(CameraModel);
    AProcess.Parameters.Add('-K');
    AProcess.Parameters.Add(Artist);
    AProcess.Parameters.Add('-K');
    AProcess.Parameters.Add(CopyRight);
    AProcess.Parameters.Add('-K');
    AProcess.Parameters.Add(ShootDate);
    AProcess.Parameters.Add('-K');
    AProcess.Parameters.Add(PhotoOrientation);
    AProcess.Parameters.Add('-K');
    AProcess.Parameters.Add(ISO);
    AProcess.Parameters.Add('-K');
    AProcess.Parameters.Add(Aperture);
    AProcess.Parameters.Add('-K');
    AProcess.Parameters.Add(Speed);
    AProcess.Parameters.Add('-K');
    AProcess.Parameters.Add(Focal);
    AProcess.Parameters.Add('-K');
    AProcess.Parameters.Add(ColorSpace);
    AProcess.Parameters.Add('-K');
    AProcess.Parameters.Add(Flash);
    {$else}
    AProcess.Executable := 'exiv2';
    AProcess.Parameters.Add('-g');
    AProcess.Parameters.Add(CameraModel);
    AProcess.Parameters.Add('-g');
    AProcess.Parameters.Add(Artist);
    AProcess.Parameters.Add('-g');
    AProcess.Parameters.Add(CopyRight);
    AProcess.Parameters.Add('-g');
    AProcess.Parameters.Add(ShootDate);
    AProcess.Parameters.Add('-g');
    AProcess.Parameters.Add(PhotoOrientation);
    AProcess.Parameters.Add('-g');
    AProcess.Parameters.Add(ISO);
    AProcess.Parameters.Add('-g');
    AProcess.Parameters.Add(Aperture);
    AProcess.Parameters.Add('-g');
    AProcess.Parameters.Add(Speed);
    AProcess.Parameters.Add('-g');
    AProcess.Parameters.Add(Focal);
    AProcess.Parameters.Add('-g');
    AProcess.Parameters.Add(ColorSpace);
    AProcess.Parameters.Add('-g');
    AProcess.Parameters.Add(Flash);
    {$endif}
    AProcess.Parameters.Add(filename);

    // We will define an option for when the program
    // is run. This option will make sure that our program
    // does not continue until the program we will launch
    // has stopped running. Also now we will tell it that
    // we want to read the output of the file.
    AProcess.Options :=  [poWaitOnExit, poUsePipes, poNoConsole];
    // Now that AProcess knows what the commandline is it can be run.
    AProcess.Execute;

    // After AProcess has finished, the rest of the program will be executed.
    // Now read the output of the program we just ran into a TStringList.
    AStringList.Clear;
    AStringList.LoadFromStream(AProcess.Output);

    // Save the output to a file and clean up the TStringList.

    // Now that the output from the process is processed, it can be freed.
    AProcess.Free;
end;

// ***
// * extracting exif ID from an exiv2 output line
// *
// * Creator : Pascal Lemaître
// *
// * in : exiv2 output line
// *
// * return : Exif ID or empty string
// *
function extractExif(line : string) : string;
var
  _return, _test : string;
begin
    // Camera Model
    _test := leftstr(line, length(CameraModel) + 1);
    if (_test = CameraModel + ' ') then _return := IU_K_Exif_APN else begin
      // Artist
      _test := leftstr(line, length(Artist) + 1);
      if (_test = Artist + ' ') then _return := IU_K_Exif_Artist else begin
        // Copyright
        _test := leftstr(line, length(Copyright) + 1);
        if (_test = Copyright + ' ') then _return := IU_K_Exif_Copyright else begin
          // ShootDate
          _test := leftstr(line, length(ShootDate) + 1);
          if (_test = ShootDate + ' ') then _return := IU_K_Exif_ShootDate else begin
            // PhotoOrientation
            _test := leftstr(line, length(ShootDate) + 1) ;
            if (_test = PhotoOrientation + ' ') then _return := IU_K_Exif_Orientation else begin
              // ISO
              _test := leftstr(line, length(ISO) + 1) ;
              if (_test = ISO + ' ') then _return := IU_K_Exif_ISO else begin
                // Aperture
                _test := leftstr(line, length(Aperture) + 1) ;
                if (_test = Aperture + ' ') then _return := IU_K_Exif_Aperture else begin
                  // shoot speed
                  _test := leftstr(line, length(Speed) + 1);
                  if (_test = Speed + ' ') then _return := IU_K_Exif_Speed else begin
                    // Focal
                    _test := leftstr(line, length(Focal) + 1);
                    if (_test = Focal + ' ') then _return := IU_K_Exif_Focal else begin
                      // Color Space
                      _test := leftstr(line, length(ColorSpace) + 1);
                      if (_test = ColorSpace + ' ') then _return := IU_K_Exif_ColorSpace else begin
                        _test := leftstr(line, length(PhotoOrientation) + 1);
                        if (_test = PhotoOrientation + ' ') then _return := IU_K_Exif_Orientation else begin
                          _test := leftstr(line, length(Flash) + 1);
                          if (_test = Flash + ' ') then _return := IU_K_Exif_Flash else
                            _return := '';
                        end;
                      end;
                    end;
                  end;
                end;
              end;
            end;
          end;
        end;
      end;
    end;
    extractExif := _return;
end;

// ***
// * extract exif value from an exiv2 output line
// *
// * Creator : Pascal Lemaître
// *
// * in : exiv2 output line
// *
// * return : exif value or empty string
// *
function extractExifValue(line : string) : string;
var
  i : integer ;
  space : integer ;
  isspace : boolean;
  _return : string;
  curcar : char;
begin
    _return := '';
    space := 0;
    isspace :=false;
    for i := 1 to length(line) do begin
      curcar := line[i];
      if space = 3 then _return := _return + string(curcar) else
        if curcar = char(32) then begin
          if not isspace then begin
            inc(space);
            isspace := true;
          end;
        end else isspace := false;
    end;
    extractExifValue := _return;
end;

// ***
// * Get specifics Exif informations from a photo
// *
// * Creator : Pascal Lemaître
// *
// * in : filename of the photo (global path)
// *
// * return : list of exif (exif name and exif data)
// *
function getExif (filename : string) : IU_T_ExifArray;
var
  AStringList : TStringList;
  _return : IU_T_ExifArray;
  i, _count : integer;
  ExifID, ExifValue : string;
begin
    _count := 0;
    AStringList := TStringList.Create;
    loadExif(filename, AStringList);
    for i := 0 to AStringList.Count - 1 do begin
        ExifID := extractExif (AStringList[i]);
        if exifID <> '' then begin
           ExifValue:=extractExifValue(AStringList[i]);
           inc(_count);
           SetLength(_return, _count);
           _return[_count-1].Exif:=ExifID;
           _return[_count-1].value:=ExifValue;
        end;
    end;
    AStringList.free;
    getExif := _return;
end;

end.

