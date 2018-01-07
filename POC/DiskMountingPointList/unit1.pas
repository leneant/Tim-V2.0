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


{$R *.lfm}

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
var
  _currentDir : string;
  sizetxt, freetxt : string;
  _free, _size : int64;
  _ssd : string;
  _mountingPointsDesc : IU_T_SystemDrivesLists ;
  i : integer ;
  _Item : string;
begin
  _currentDir := getCurrentDir;
  // Opening mounts in /procs
  _mountingPointsDesc := getHDDMountingPointsDescriptions ;
  for i := Low(_mountingPointsDesc) to High(_mountingPointsDesc) do begin
      chdir(_mountingPointsDesc[i].Entry);
      IU_getCurrentDriveSizes(_size, _free);
      sizetxt := IU_realToString(_size/1024/1024/1024,2);
      freetxt := IU_realToString(_free/1024/1024/1024,2);
      if _mountingPointsDesc[i].SSD then _ssd := 'SSD' else _ssd := '   ';
      _Item := concat ('Mounting Point : ', _mountingPointsDesc[i].Entry,' -> Systeme File : ', _mountingPointsDesc[i].SystemFileType, ', ', _ssd + ', Total size : ', sizetxt, ' Go, Free, ', freetxt, ' Go');
      ListBox1.Items.Add(_Item);
    end;
  chdir(_currentDir);
end;

end.

