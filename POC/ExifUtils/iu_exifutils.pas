unit IU_ExifUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils
  {$ifdef linux}
  ,unix
  {$else}
  ,ShellApi, windows,Process
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
  {
  ret :integer;
  _currentstdout : THandle;
  _redirstdout : THandle;
  s : ansistring;
  }
  AProcess: TProcess;
  AStringList: TStringList;

  {$endif}
  _return : IU_T_StringArray;
begin
  // exec command
  {$ifdef linux}
  s := fpSystem('exiv2 -g Exif ' + filename + ' > test.txt');
  {$else}

  // This is where our program starts to run
    // Now we will create the TProcess object, and
    // assign it to the var AProcess.
    AProcess := TProcess.Create(nil);

    // Tell the new AProcess what the command to execute is.
    AProcess.Executable := getcurrentdir + '\exiv2.exe';

    AProcess.Parameters.Add('-g');
    AProcess.Parameters.Add('Exif.Photo');
    AProcess.Parameters.Add(filename);

    // We will define an option for when the program
    // is run. This option will make sure that our program
    // does not continue until the program we will launch
    // has stopped running. Also now we will tell it that
    // we want to read the output of the file.
    //AProcess.Options :=  [poWaitOnExit, poUsePipes, poNoConsole];
     AProcess.Options :=  [poWaitOnExit];
    // Now that AProcess knows what the commandline is it can be run.
    AProcess.Execute;

    // After AProcess has finished, the rest of the program will be executed.

    // Now read the output of the program we just ran into a TStringList.
    AStringList := TStringList.Create;
    AStringList.LoadFromStream(AProcess.Output);

    // Save the output to a file and clean up the TStringList.
    AStringList.SaveToFile(getcurrentdir + '\test.txt');
    AStringList.Free;

    // Now that the output from the process is processed, it can be freed.
    AProcess.Free;
    {$endif}
end;

end.

