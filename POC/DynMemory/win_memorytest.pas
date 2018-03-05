unit win_memorytest;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls
  , IU_GeneralUtils
  , IU_FramesManagement
  , math
  ;

type

  { TMemoryTest }

  TMemoryTest = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    Label1: TLabel;
    Label10: TLabel;
    LBL_CopyTestValue: TLabel;
    LBL_Test: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    LBL_MemSize: TLabel;
    LBL_MPix: TLabel;
    Label9: TLabel;
    LBL_Hauteur: TLabel;
    LBL_Largeur: TLabel;
    LBL_Resolution: TLabel;
    LBL_AllocMemory: TLabel;
    LBL_CoefMem: TLabel;
    LBL_InstalledMemory: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Label3Click(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  MemoryTest: TMemoryTest;
  frame, framecopy : IU_T_Frame;

implementation

{$R *.lfm}

{ TMemoryTest }

procedure TMemoryTest.Label3Click(Sender: TObject);
begin

end;

procedure TMemoryTest.FormCreate(Sender: TObject);
var
  InstalledMemory : QWord;
  CoefForAlloc : real;
  AllocatingSize : real;
  C9 : real;
begin
  InstalledMemory := IU_getInstalledRAM; // G Bytes
  CoefForAlloc := IU_getAdaptationRAMCoefWithoutAlpha;
  AllocatingSize := (71*(InstalledMemory/1024/1024/1024)-30);
  C9 :=  AllocatingSize / 3 / 4;
  LBL_InstalledMemory.Caption := IU_RealToString(InstalledMemory/1024/1024/1024, 2) + ' Gb';
  LBL_CoefMem.Caption := IU_RealToString(CoefForAlloc, 4);
  LBL_Resolution.Caption := IU_RealToString(C9,2) + ' Mp';
  LBL_AllocMemory.Caption := IU_RealToString(AllocatingSize,2) + ' Mb';
end;

procedure TMemoryTest.Button1Click(Sender: TObject);
var
  _width, _height : integer;
  _size : real;
  _sqrsize : real;
  _ram : real;
begin
  // Size of frame in Mpix

  _ram := IU_getInstalledRAM/1024/1024/1024; // G Bytes
  _size := (71*_ram-30)/3/4 *1024*1024; // Pix
  _sqrsize := sqrt(_size);  // Pix
  _height := trunc(_sqrsize); // Pix
  if (_height > 32767) or (_height < 0) then _height := 32767 ; // Max size of a pix dimension
  _width := _height;
  IU_AllocFrame(frame, _width, _height, 0,0, char(1));
  LBL_Largeur.Caption := inttostr(_width);
  LBL_Hauteur.Caption := inttostr(_height);
  LBL_MPix.Caption := IU_RealToString(_width*_height/1024/1024,2) + ' MPix';
  LBL_MemSize.Caption := IU_RealToString(_width*_height*3*4/1024/1024,2) + ' Mb';
end;

procedure TMemoryTest.Button2Click(Sender: TObject);
begin
  IU_ReleaseFrame (frame);
  LBL_Largeur.Caption := '0';
  LBL_Hauteur.Caption := '0';
  LBL_MPix.Caption := '0';
  LBL_MemSize.Caption := '0';
end;

procedure TMemoryTest.Button3Click(Sender: TObject);
begin
  IU_FrameSave(frame, '/home/pascal/Temps/test.frame');
end;

procedure TMemoryTest.Button4Click(Sender: TObject);
begin
  IU_FrameLoad(frame, '/home/pascal/Temps/test.frame');
  LBL_Largeur.Caption := inttostr(frame._descriptor.Width);
  LBL_Hauteur.Caption := inttostr(frame._descriptor.Height);
  LBL_MPix.Caption := IU_RealToString(frame._descriptor.Height*frame._descriptor.Width/1024/1024,2) + ' MPix';
  LBL_MemSize.Caption := IU_RealToString(frame._descriptor.Height*frame._descriptor.Width*3*4/1024/1024,2) + ' Mb';
end;

procedure TMemoryTest.Button5Click(Sender: TObject);
var i : dword;
  _value : qword;
begin
  LBL_Test.Caption := 'Running sum of each RGB values for all pixels !';
  Application.ProcessMessages;
  _value := 0;
  for i := 0 to frame._descriptor.Height*frame._descriptor.Width - 1 do
    _value := _value + frame._pixels[i].B+frame._pixels[i].R+frame._pixels[i].G;
  LBL_Test.Caption := inttostr(_value);
end;

procedure TMemoryTest.Button6Click(Sender: TObject);
var i : dword;
  _value : qword;
begin
  IU_CopyFrame(frame, framecopy);
  LBL_CopyTestValue.Caption := 'Running sum of each RGB values for all pixels !';
  Application.ProcessMessages;
  _value := 0;
  for i := 0 to framecopy._descriptor.Height*framecopy._descriptor.Width - 1 do
    _value := _value + framecopy._pixels[i].B+framecopy._pixels[i].R+framecopy._pixels[i].G;
  LBL_CopyTestValue.Caption := inttostr(_value);
end;

end.

