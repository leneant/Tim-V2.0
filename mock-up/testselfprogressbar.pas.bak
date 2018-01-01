unit TestSelfProgressBar;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls;

type

  { TForm1 }
  TProgressArray = record
    _picto : array [1..10] of TShape;
    _caption : TLabel;
  end;

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Label1: TLabel;
    Shape1: TShape;
    Shape10: TShape;
    Shape2: TShape;
    Shape3: TShape;
    Shape4: TShape;
    Shape5: TShape;
    Shape6: TShape;
    Shape7: TShape;
    Shape8: TShape;
    Shape9: TShape;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  Form1: TForm1;
  Shapes : TProgressArray;

implementation

{$R *.lfm}

{ TForm1 }
procedure _drawprogresse (var _img : TProgressArray ; _percent : integer);
var
  i, _progressWidth : integer ;
begin
  // Calc progress width
  _progressWidth := trunc(abs(_percent) / 10);
  // Drawing rectangle
  for i := 1 to 10 do
     if i <= _progressWidth then _img._picto[i].Brush.Color:=rgbtocolor($77,$77,$77);
  _img._caption.caption:=inttostr(abs(_percent))+'%';
end;

procedure TForm1.Button1Click(Sender: TObject);
var i : integer;
begin

  for i := 0 to 100 do begin
     _drawprogresse (Shapes, i);
     Application.ProcessMessages;
     sleep(100);
  end;
end;

procedure TForm1.Button2Click(Sender: TObject);
var i : integer;
begin
  Shapes._caption.caption := '0%';
  for i := 1 to 10 do
    Shapes._picto[i].Brush.Color:=rgbtocolor($44,$44,$44);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Shapes._picto[1] := Form1.Shape1;
  Shapes._picto[2] := Form1.Shape2;
  Shapes._picto[3] := Form1.Shape3;
  Shapes._picto[4] := Form1.Shape4;
  Shapes._picto[5] := Form1.Shape5;
  Shapes._picto[6] := Form1.Shape6;
  Shapes._picto[7] := Form1.Shape7;
  Shapes._picto[8] := Form1.Shape8;
  Shapes._picto[9] := Form1.Shape9;
  Shapes._picto[10] := Form1.Shape10;
  Shapes._caption := Form1.Label1;

end;

end.

