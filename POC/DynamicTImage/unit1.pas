unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  Form1: TForm1;
  images : array [0..5] of TImage;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
var i : integer;
begin
  for i := 0 to 5 do begin
    images[i] := TImage.Create(Form1);
    images[i].Parent:=Form1;
    Images[i].Width:=200;
    Images[i].height := 50;
    Images[i].Left := 10;
    Images[i].Top := i * 50 + 70;
    Images[i].Canvas.Brush.Color:=rgbtocolor(i*20, i*15, i*10);
    Images[i].Canvas.FillRect(0,0,images[i].width,images[i].height);
  end;
end;

end.

