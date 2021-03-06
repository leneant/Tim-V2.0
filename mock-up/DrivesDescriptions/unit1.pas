unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, IU_GeneralUtils, IU_TimControls, math, IU_I18N_Messages;

type

  { TForm1 }

  TForm1 = class(TForm)
    Image1: TImage;
    HDD: TImage;
    SSD: TImage;
    Panel1: TPanel;
    procedure FormChangeBounds(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormPaint(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    _onScrollChange : T_IU_TB_onChangePTR;
  end;

  T_IU_DriveDescription = record
    Drive : TLabel;
    FS : TLabel;
    SIZE : TLabel;
    FREE : TLabel;
    DDType : TImage;
  end;

  T_IU_DrivesDescriptions = array of T_IU_DriveDescription;

var
  Form1: TForm1;
  Drives : IU_T_SystemDrivesLists;
  DrivesDescriptors :  T_IU_DrivesDescriptions;
  scrollBar : T_IU_ScrollBar;

implementation

var
  _xmin : integer;

{$R *.lfm}

Procedure _scroll(ID : word ; _currentValue : extended);
var k : integer ;
  i, _ymax : integer ;
begin
  _ymax := High(DrivesDescriptors)* 40 + 20 + DrivesDescriptors[High(DrivesDescriptors)].FREE.Height + 10 - form1.Panel1.height;
  k := round((1-scrollbar._GetValue)*_ymax);
  for i := Low(DrivesDescriptors) to High(DrivesDescriptors) do begin
    DrivesDescriptors[i].Drive.Top := i*40 + 5 - k ;
    DrivesDescriptors[i].FS.Top := i*40 + 20 - k ;
    DrivesDescriptors[i].DDType.Top := i*40 + 7  - k;
    DrivesDescriptors[i].SIZE.Top := i*40 + 4 - k ;
    DrivesDescriptors[i].FREE.Top := i*40 + 20 - k;
  end;
end;


{ TForm1 }


procedure TForm1.FormCreate(Sender: TObject);
var i,j : integer;
    _size, _free : int64;
    _fsize, _ffree : string;
    _curdir : string;
    _maxy : integer;
begin
  form1.Color:=V_IU_DefaultBG;
  Drives := getHDDMountingPointsDescriptions;
  i := High(Drives) - Low(Drives) + 1;
  SetLength(DrivesDescriptors, i);
  _curdir := getCurrentDir;
  _maxy := 0;
  for j := Low(DrivesDescriptors) to High(DrivesDescriptors) do begin
    DrivesDescriptors[j].Drive := TLabel.Create(Form1.Panel1);
    DrivesDescriptors[j].Drive.Name := 'LabelEntry' + inttostr(j);
    DrivesDescriptors[j].Drive.Parent := Form1.Panel1;
    DrivesDescriptors[j].Drive.Top := j*40 + 5 ;
    DrivesDescriptors[j].Drive.Left := 5;
    DrivesDescriptors[j].Drive.Font.Size:=9;
    {$ifdef windows}
    DrivesDescriptors[j].Drive.Caption := Drives[Low(Drives) - Low(DrivesDescriptors)+j].Entry;
    {$else}
    DrivesDescriptors[j].Drive.Caption := Drives[Low(Drives) - Low(DrivesDescriptors)+j].Entry + ' : ';
    {$endif}
    DrivesDescriptors[j].Drive.Font.Color:=V_IU_PB_TextColor;
    DrivesDescriptors[j].FS := TLabel.Create(Form1.Panel1);
    DrivesDescriptors[j].FS.Name := 'LabelFS' + inttostr(j);
    DrivesDescriptors[j].FS.Parent := Form1.Panel1;
    DrivesDescriptors[j].FS.Top := j*40 + 20 ;
    DrivesDescriptors[j].FS.Left := 10;
    DrivesDescriptors[j].FS.Font.Size:=9;
    DrivesDescriptors[j].FS.Caption := Drives[Low(Drives) - Low(DrivesDescriptors)+j].SystemFileType ;
    DrivesDescriptors[j].FS.Font.Color:=V_IU_PB_TextColor;
    DrivesDescriptors[j].DDType := TImage.Create(Form1.Panel1);
    DrivesDescriptors[j].DDType.Name := 'ImageHDD' + inttostr(j);
    DrivesDescriptors[j].DDType.Parent := Form1.Panel1;
    DrivesDescriptors[j].DDType.Top := j*40 + 7 ;
    DrivesDescriptors[j].DDType.Left := Form1.Panel1.Width - 42;
    DrivesDescriptors[j].DDType.Width := 72;
    DrivesDescriptors[j].DDType.Height := 36;
    if Drives[Low(Drives) - Low(DrivesDescriptors)+j].SSD then
      DrivesDescriptors[j].DDType.Picture := SSD.Picture
    else
      DrivesDescriptors[j].DDType.Picture := HDD.Picture;
    {$ifdef windows}
    chdir(DrivesDescriptors[j].Drive.Caption);
    {$else}
    chdir(LeftSTR(DrivesDescriptors[j].Drive.Caption, length(DrivesDescriptors[j].Drive.Caption)-3));
    {$endif}
    IU_getCurrentDriveSizes(_size, _free);
    _fsize := IU_realToString(_size/1024/1024/1024, 2);
    _ffree := IU_realToString(_free/1024/1024/1024, 2);
    DrivesDescriptors[j].SIZE := TLabel.Create(Form1.Panel1);
    DrivesDescriptors[j].SIZE.Name := 'LabelSIZE' + inttostr(j);
    DrivesDescriptors[j].SIZE.Parent := Form1.Panel1;
    DrivesDescriptors[j].SIZE.Top := j*40 + 4 ;
    DrivesDescriptors[j].SIZE.Font.Size:=9;
    DrivesDescriptors[j].SIZE.Caption := IU_HI_Messages[IU_CurrentLang,K_IU_HIMSG_Size] + ' : ' + _fsize + ' ' + IU_HI_Messages[IU_CurrentLang,K_IU_HIMSG_GB];
    IU_HI_Messages[K_IU_I18N_FRENCH,K_IU_HIMSG_Free] := 'Couleur du texte';
    DrivesDescriptors[j].SIZE.Alignment:=taRightJustify;
    DrivesDescriptors[j].SIZE.Font.Color:=V_IU_PB_TextColor;
    DrivesDescriptors[j].FREE := TLabel.Create(Form1.Panel1);
    DrivesDescriptors[j].FREE.Name := 'LabelFREE' + inttostr(j);
    DrivesDescriptors[j].FREE.Parent := Form1.Panel1;
    DrivesDescriptors[j].FREE.Top := j*40 + 20 ;
    DrivesDescriptors[j].FREE.Font.Size:=9;
    DrivesDescriptors[j].FREE.Caption := IU_HI_Messages[IU_CurrentLang,K_IU_HIMSG_Free] + ' : ' + _ffree + ' ' + IU_HI_Messages[IU_CurrentLang,K_IU_HIMSG_GB];
    DrivesDescriptors[j].FREE.Font.Color:=V_IU_PB_TextColor;
    DrivesDescriptors[j].FREE.Alignment:=taRightJustify;
    _maxy := DrivesDescriptors[j].FREE.Top + DrivesDescriptors[j].FREE.Height + 5;
  end;
  scrollBar := T_IU_ScrollBar.Create(iu_tb_cursordirection_standard, _maxy - form1.Panel1.height, Image1);
  scrollBar.onChange:= @_scroll;
  //
  if _maxy > Panel1.Height then scrollBar.visible :=true
  // else Image1.visible := false;
  else scrollBar.visible := false;
  scrollBar._setValue(0);
  scrollBar._draw;
  Form1.Refresh;
  chdir(_curdir);
end;

procedure TForm1.FormPaint(Sender: TObject);
var i : integer;
  a,b,c : integer;
begin
  _xmin := 0;
  for i := Low(DrivesDescriptors) to High(DrivesDescriptors) do begin
    a := max(DrivesDescriptors[i].Drive.Width,DrivesDescriptors[i].FS.Width);
    b := max(DrivesDescriptors[i].SIZE.width,DrivesDescriptors[i].FREE.width);
    c := DrivesDescriptors[i].DDType.width;
    _xmin := max(_xmin, a+b+c+20+5+Image1.width+5);
    DrivesDescriptors[i].SIZE.Left := DrivesDescriptors[i].DDType.Left - DrivesDescriptors[i].SIZE.Width - 5 ;
    DrivesDescriptors[i].FREE.Left := DrivesDescriptors[i].DDType.Left - DrivesDescriptors[i].FREE.Width - 5 ;
  end;
  form1.Constraints.MinWidth:=_xmin;
end;

procedure TForm1.FormChangeBounds(Sender: TObject);
var i : integer;
begin
  form1.Panel1.Width:=form1.Width - 30 - Image1.width;
  for i := Low(DrivesDescriptors) to High(DrivesDescriptors) do begin
    DrivesDescriptors[i].Drive.Left := 5;
    DrivesDescriptors[i].FS.Left := 10;
    DrivesDescriptors[i].DDType.Left := Form1.Panel1.Width - 42;
    DrivesDescriptors[i].SIZE.Left := DrivesDescriptors[i].DDType.Left - DrivesDescriptors[i].SIZE.Width - 5 ;
    DrivesDescriptors[i].FREE.Left := DrivesDescriptors[i].DDType.Left - DrivesDescriptors[i].FREE.Width - 5 ;
    Image1.Left := form1.Panel1.Left + form1.Panel1.width + 1;
  end;
end;

end.

