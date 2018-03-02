unit win_memorytest;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  IU_GeneralUtils
  ;

type

  { TMemoryTest }

  TMemoryTest = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    LBL_Resolution: TLabel;
    LBL_AllocMemory: TLabel;
    LBL_CoefMem: TLabel;
    LBL_InstalledMemory: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure Label3Click(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  MemoryTest: TMemoryTest;

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
  AllocatingSize := ((C9*1024*1024+700)*3*4/1024/1024/1024)*3+0.07;
  LBL_InstalledMemory.Caption := IU_RealToString(InstalledMemory/1024/1024/1024, 2) + ' Gb';
  LBL_CoefMem.Caption := IU_RealToString(CoefForAlloc, 4);
  LBL_Resolution.Caption := IU_RealToString(C9,2) + ' Mp';
  LBL_AllocMemory.Caption := IU_RealToString(AllocatingSize,2) + ' Gb';
end;

end.

