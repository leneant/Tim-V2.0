unit winsand;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls;

type

  { TW_WinSand }

  TW_WinSand = class(TForm)
    GroupBox1: TGroupBox;
    Image1: TImage;
    Image10: TImage;
    Image11: TImage;
    Image12: TImage;
    Image13: TImage;
    Image14: TImage;
    Image15: TImage;
    Image2: TImage;
    Image3: TImage;
    Image4: TImage;
    Image5: TImage;
    Image6: TImage;
    Image7: TImage;
    Image8: TImage;
    Image9: TImage;
    procedure FormCreate(Sender: TObject);
    procedure Image1MouseEnter(Sender: TObject);
    procedure Image1Click(Sender: TObject);
    procedure Image11Click(Sender: TObject);
    procedure Image11MouseEnter(Sender: TObject);
    procedure Image11MouseLeave(Sender: TObject);
    procedure Image1MouseLeave(Sender: TObject);
    procedure Image6Click(Sender: TObject);
    procedure Image6MouseEnter(Sender: TObject);
    procedure Image6MouseLeave(Sender: TObject);
  private
    { private declarations }
    procedure Select1;
    procedure Highlight1;
    procedure unHighLight1;

    Procedure Select2;
    procedure Highlight2;
    procedure unHighLight2;

    Procedure Select3;
    procedure Highlight3;
    procedure unHighLight3;

    procedure unselectAll;
  public
    { public declarations }
  end;

var
  W_WinSand: TW_WinSand;


implementation

{$R *.lfm}

{ TW_WinSand }

procedure TW_WinSand.FormCreate(Sender: TObject);
begin
end;


procedure TW_WinSand.Select1;
begin
  unSelectAll;
  Image1.Picture.Bitmap := Image3.Picture.Bitmap;
  Image5.Picture.Bitmap := Image1.Picture.Bitmap;
end;

procedure TW_WinSand.Highlight1;
begin
    Image1.Picture.Bitmap := Image2.Picture.Bitmap;
end;

procedure TW_WinSand.unHighLight1;
begin
  Image1.Picture.Bitmap := Image5.Picture.Bitmap;
end;

Procedure TW_WinSand.Select2;
begin
  unSelectAll;

  Image6.Picture.Bitmap := Image8.Picture.Bitmap;
  Image10.Picture.Bitmap := Image6.Picture.Bitmap;
end;

procedure TW_WinSand.Highlight2;
begin
  Image6.Picture.Bitmap := Image7.Picture.Bitmap;
end;

procedure TW_WinSand.unHighLight2;
begin
  Image6.Picture.Bitmap := Image10.Picture.Bitmap;
end;

Procedure TW_WinSand.Select3;
begin
    unselectAll;

    Image11.Picture.Bitmap := Image13.Picture.Bitmap;
    Image15.Picture.Bitmap := Image11.Picture.Bitmap;
end;

procedure TW_WinSand.Highlight3;
begin
  Image11.Picture.Bitmap := Image12.Picture.Bitmap;
end;

procedure TW_WinSand.unHighLight3;
begin
  Image11.Picture.Bitmap := Image15.Picture.Bitmap;
end;

procedure TW_WinSand.unselectAll;
begin
  Image1.Picture.Bitmap := Image4.Picture.Bitmap;
  Image5.Picture.Bitmap := Image4.Picture.Bitmap;

  Image6.Picture.Bitmap:=Image9.Picture.Bitmap;
  Image10.Picture.Bitmap:=Image9.Picture.Bitmap;

  Image11.Picture.Bitmap := Image14.Picture.Bitmap;
  Image15.Picture.Bitmap := Image14.Picture.Bitmap;
end;

procedure TW_WinSand.Image1MouseEnter(Sender: TObject);
begin
  HighLight1;
end;



procedure TW_WinSand.Image1Click(Sender: TObject);
begin
  Select1;
end;

procedure TW_WinSand.Image11Click(Sender: TObject);
begin
  Select3 ;
end;



procedure TW_WinSand.Image11MouseEnter(Sender: TObject);
begin
  HighLight3;
end;



procedure TW_WinSand.Image11MouseLeave(Sender: TObject);
begin
  unHighLight3;
end;

procedure TW_WinSand.Image1MouseLeave(Sender: TObject);
begin
  unHighLight1;
end;


procedure TW_WinSand.Image6Click(Sender: TObject);
begin
  Select2;
end;

procedure TW_WinSand.Image6MouseEnter(Sender: TObject);
begin
  highLight2;
end;



procedure TW_WinSand.Image6MouseLeave(Sender: TObject);
begin
  unHighLight2;
end;


end.

