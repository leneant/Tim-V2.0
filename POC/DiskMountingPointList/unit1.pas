unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  IU_GeneralUtils, unix;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    ListBox1: TListBox;
    procedure Button1Click(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

function isDiskMount(line : string) : boolean;
var i : integer;
  _detect : string;
  _return : boolean;
begin
  if line[1] <> '/' then begin
    _return := false ;

  end else begin
    _detect := '';
     for i := 1 to 5 do begin
        _detect := _detect + line[i];
     end;
     if _detect = '/dev/' then _return := true else _return := false;
  end;
  isDiskMount := _return;
end;

function filteringMountingLine(line : string) : string;
var i : integer;
  _return : string;
  _spaces : integer;
begin
  _return := '';
  _spaces := 0;
  for i := 1 to length(line) do begin
    if line[i] = ' ' then inc(_spaces);
    if _spaces < 2 then _return := _return + line[i];
  end;
  filteringMountingLine := _return;
end;

function filteringMountingPoint(line : string) : string;
var i : integer;
  _return : string;
  _ok : boolean;
begin
  _ok := false;
  _return := '';
  for i := 1 to length(line) do begin
     if line[i] = ' ' then
       _ok := true else begin
         if _ok then
           _return := _return + line[i];
     end;
  end;
  filteringMountingPoint := _return;
end;

function isSSD(mountpoint : string) : boolean;
var
  _tempdir : string;
  _File : TextFile;
  _inputLine : string;
  i : integer;
  _return : boolean;
begin
  // getting temporaty dir
  _tempdir := GetTempDir;
  // exec linux command
  if fpSystem('lsblk -o ROTA,MOUNTPOINT,RM | grep '''+ mountpoint + '  ''>' + _tempdir + 'tim.txt') <> 127 then begin
    // open file
    assignFile(_File, _tempdir + '/tim.txt');
    reset(_File);
    if not eof(_File) then  readln(_File, _inputLine)
    else                    _inputLine := '';
    CloseFile(_File);
    if _inputLine <> '' then begin
      i := 1;
      while ((_inputLine[i] =  ' ') and (i < Length(_inputLine))) do inc(i);
      if i < length(_inputLine) then _return := '0' = _inputLine[i];
    end else _return := false;
  end;
  DeleteFile(_tempdir + 'tim.txt');
  isSSD := _return;
end;

{$R *.lfm}

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
var
  _File : TextFile ;
  _line : string;
  _currentDir : string;
  _mounting, sizetxt, freetxt : string;
  _Item : string;
  _free, _size : int64;
  _ssd : string;
begin
  _currentDir := getCurrentDir;
  // Opening mounts in /procs
  assignFile (_File, '/proc/mounts');
  reset(_File);
  readln (_File, _Line);
  while not eof(_File) do begin
    if isDiskMount(_line) then begin
      _mounting := filteringMountingPoint(FilteringMountingLine(_line));
      chdir(_mounting);
      if isSSD(_mounting) then _ssd := 'SSD' else _ssd := '   ';
      IU_getCurrentDriveSizes(_size, _free);
      sizetxt := IU_realToString(_size/1024/1024/1024,2);
      freetxt := IU_realToString(_free/1024/1024/1024,2);
      _Item := concat ('Mounting Point : ', _mounting,' -> ', _ssd + ' : Total size : ', sizetxt, ' Go, Free : ', freetxt, ' Go');
      ListBox1.Items.Add(_Item);
    end;
    readln(_File, _line);
  end;
  closeFile(_File);
  chdir(_currentDir);
end;

end.

