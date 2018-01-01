unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, wincontrol2, wincontrol3, winsand;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
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
begin
  Form3.hide;
  W_winsand.GroupBox1.Parent := Form2;
  Form2.show;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  Form2.hide;
  W_winsand.GroupBox1.Parent := Form3;
  Form3.show;
end;

end.

