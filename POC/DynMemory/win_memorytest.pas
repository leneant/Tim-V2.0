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
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
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
    procedure FormCreate(Sender: TObject);
    procedure Label3Click(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  MemoryTest: TMemoryTest;
  frame : IU_Frame;

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
  InstalledMemory := IU_getInstalledRAM;
  CoefForAlloc := IU_getAdaptationRAMCoefWithoutAlpha;
  C9 := 8*InstalledMemory/1024/1024/1024*CoefForAlloc;
  AllocatingSize := ((C9*1024*1024+700)*3*4/1024/1024/1024)*3+0.14;
  LBL_InstalledMemory.Caption := IU_RealToString(InstalledMemory/1024/1024/1024, 2) + ' Gb';
  LBL_CoefMem.Caption := IU_RealToString(CoefForAlloc, 4);
  LBL_Resolution.Caption := IU_RealToString(C9,2) + ' Mp';
  LBL_AllocMemory.Caption := IU_RealToString(AllocatingSize,2) + ' Gb';
end;

procedure TMemoryTest.Button1Click(Sender: TObject);
var
  _width, _height : integer;
  _size : QWord;
  _sqrsize : real;
  _h,_w : real;
begin
  // Size of frame in Mpix
  _size := trunc((8*IU_getInstalledRAM*IU_getAdaptationRAMCoefWithoutAlpha+700*700)/1024/4);
  _sqrsize := sqrt(_size);
  _height := trunc(_sqrsize);
  if (_height > 32767) or (_height < 0) then _height := 32768 ; // Max size of a pix dimension
  _width := _height;
  IU_AllocFrame(frame, _width, _height, 0,0);
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

end.

