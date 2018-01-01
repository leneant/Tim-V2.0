unit MainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  IU_Types, IU_ImagesUtils, IU_Frames, IU_TemporaryPixFiles, IU_GeneralUtils,
  IU_CriticalSections;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Edit1: TEdit;
    Edit2: TEdit;
    Edit3: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
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

var
  V_32color : T_IU_AColorChanel32;

procedure TForm1.Button1Click(Sender: TObject);
var
  in_color : T_IU_AColorChanel8 ;
  out_color : T_IU_AColorChanel32 ;
  out_colorTXT : string ;
begin
  in_color := T_IU_AColorChanel8(StrToInt(Edit1.text));
  IU_ConvertColorChanel8to32(in_color, out_color);
  V_32color := out_color;
  out_colorTXT := intToStr(out_color);
  Edit2.text := out_colorTXT;
end;

procedure TForm1.Button2Click(Sender: TObject);
var
  out_color : T_IU_AColorChanel8 ;
  in_color : T_IU_AColorChanel32 ;
  out_colorTXT : string ;
begin
  in_color := V_32color;
  IU_ConvertColorChanel32to8(in_color, out_color);
  out_colorTXT := IntToStr(out_color);
  Edit3.text := out_colorTXT;
end;

procedure TForm1.Button3Click(Sender: TObject);
var _memalloc1, _memalloc2 : real;
begin
  IU_AllocMemoryFrames;
  _memalloc1 := ((((2 * V_IU_MaxInWindowSize) + V_IU_MaxOutWindowSize) * K_IU_MaxThreads) / 1024 / 1024 / 1024);
  _memalloc2 := ((((2 * V_IU_MaxInPreviewSize) + V_IU_MaxOutPreviewSize) * K_IU_MaxThreads) / 1024 / 1024 / 1024);
  Label4.Caption := 'Origin = ' + FormatFloat('#.0000',_memalloc1) + ' Go, Preview = ' + FormatFloat('#0.0000',_memalloc2) + ' Go, Total = ' + FormatFloat('#0.0000',_memalloc1 + _memalloc2) + ' Go' ;
end;

procedure TForm1.Button4Click(Sender: TObject);
begin
  IU_CleanMemoryFrames;
  Label4.Caption := 'Memory released !';
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  text1, text2 : ansistring;
begin
  // text1 := V_IU_tempDirectory + '/TimPix-' + FormatDateTime('yyyy-mm-dd-hh-mm-ss-zzz',Now) + FloatToStr(random) + '-temp.tim';
  // text2 := V_IU_tempDirectory + '/TimPix-' + FormatDateTime('yyyy-mm-dd-hh-mm-ss-zzz',Now) + FloatToStr(random) + '-temp.tim';
  text1 := IU_Get_TemporaryFileName;
  text2 := IU_Get_TemporaryFileName;
  Label2.caption := text1;
  Label3.caption := text2;
  IU_Init_CriticalSections;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  IU_CleanMemoryFrames;
  IU_Release_CriticalSections;
end;

begin
  V_32color := $0;
end.

