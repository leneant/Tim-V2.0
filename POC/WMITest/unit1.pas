unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Variants,IU_GeneralUtils;



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

{$R *.lfm}



{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
var
  _driveslist : IU_T_SystemDrivesLists;
  i : integer ;
  _dsize, _dfree : int64;
  _currentdir : string;
  _fsize, _ffree : string;
begin
  ListBox1.Clear;
  _driveslist := getHDDMountingPointsDescriptions;
  _currentdir := GetCurrentDir;
  for i := Low(_drivesList) to High(_drivesList) do begin
      chdir (_drivesList[i].Entry);
      IU_getCurrentDriveSizes(_dsize, _dfree);
      _fsize := IU_realToString(_dsize/1024/1024/1024,2);
      _ffree := IU_realToString(_dfree/1024/1024/1024,2);
      ListBox1.Items.Add(_driveslist[i].Entry + ' ' + _drivesList[i].SystemFileType +', Total size ' + _fsize + ' Gb, Free room : ' + _ffree + ' Gb');
  end;
  chdir(_currentdir);
end;


end.

