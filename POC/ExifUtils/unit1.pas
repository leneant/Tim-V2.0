unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls, IU_ExifUtils;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Memo1: TMemo;
    OpenDialog1: TOpenDialog;
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
var _return : IU_T_ExifArray;
  i : integer;
begin
  if Opendialog1.Execute then begin
    _return := getExif (OpenDialog1.FileName);
    Form1.Memo1.Clear;
    for i := Low(_return) to High(_return) do begin
      Form1.Memo1.Append(_return[i].Exif + ' = ' + _return[i].value);
    end;
  end;
end;

end.

