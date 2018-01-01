unit unit1manuelcrationofprogressbar;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, IU_TimControls, Types, LCLType, IU_GeneralUtils
  {$ifdef linux}
  ,linux, BaseUnix, unixtype
  {$endif}
  ;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Edit1: TEdit;
    Edit2: TEdit;
    Edit3: TEdit;
    GroupBox1: TGroupBox;
    Image1: TImage;
    Image2: TImage;
    Image3: TImage;
    Image4: TImage;
    Label1: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    procedure Bevel1ChangeBounds(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormChangeBounds(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure GroupBox1ChangeBounds(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    Progress : T_IU_ProgressBar;
    Tb, Tc, Td, Te : T_IU_TrackBar;
  end;

var
  Form1: TForm1;
  RAM : qword;
  RAM_Coef : real;


implementation

{$R *.lfm}

procedure CallBackTB1(ID : word ; _value : extended);
begin
  form1.Label5.Caption:=IU_realToString(_value , 0);
end;

procedure CallBackTB2(ID : word ; _value : extended);
begin
  form1.Label6.Caption:=IU_realToString(_value, 0);
end;

procedure CallBackTB3(ID : word ; _value : extended);
begin
  form1.Label7.Caption:=IU_realToString(_value, 0);
end;

procedure CallBackTB4(ID : word ; _value : extended);
begin
  form1.Label8.Caption:=IU_realToString(_value, 0);
end;


{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  // Getting RAM on PC
  RAM := IU_getInstalledRAM ;
  // Getting alloc coef
  RAM_coef := IU_getAdaptationRAMCoef ;
  Label10.Caption := IU_realToString(RAM/1024/1024,2);
  Label12.Caption := IU_realToString(RAM_Coef,3);
  Progress := T_IU_ProgressBar.Create(TWinControl(GroupBox1));
  Tb := T_IU_TrackBar.Create(iu_tb_cursordirection_inverted,20000,Image1,Edit1);
  Tb._setNumberOfDecimalPlace(2);
  Tb._setStartingValue(-100);
  Tc := T_IU_TrackBar.Create(iu_tb_cursordirection_standard,400000,Image2,Edit2);
  Tc._setStartingValue(-50000);
  Td := T_IU_TrackBar.Create(iu_tb_cursordirection_standard,20000,Image3);
  Td._setNumberOfDecimalPlace(2);
  Td._setStartingValue(-100);
  Te := T_IU_TrackBar.Create(iu_tb_cursordirection_inverted,20000,Image4,Edit3);
  Te._setNumberOfDecimalPlace(2);
  Te._setStartingValue(-100);
  Tb.onChange:=@CallBackTB1;
  Tc.onChange:=@CallBackTB2;
  Td.onChange:=@CallBackTB3;
  Te.onChange:=@CallBackTB4;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  Progress.Release;
  Tb.Release;
  Tc.Release;
end;

procedure TForm1.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState
  );
begin
  Tb._setValueFromKeyboard(Key);
  Tc._setValueFromKeyBoard(Key);
  Td._setValueFromKeyBoard(Key);
  Te._setValueFromKeyBoard(Key);
end;

procedure TForm1.GroupBox1ChangeBounds(Sender: TObject);
begin
    Progress._resize(TWinControl(GroupBox1));
end;


procedure TForm1.Button1Click(Sender: TObject);
var i : integer;
begin
  i := 0;
  while i <= 1000 do begin
    Progress._setPercent(i/10);
    Progress._draw;
    sleep(20);
    inc(i,1);
  end;
  Progress._setPercent(100);
  Progress._draw;

end;

procedure TForm1.Bevel1ChangeBounds(Sender: TObject);
begin
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  Progress._init;
end;

procedure TForm1.FormChangeBounds(Sender: TObject);
begin
  GroupBox1.Left:=48;
  GroupBox1.Width:=Form1.Width-96;
  GroupBox1.Height := Form1.Height - 150;
  Image1.Left := 48;
  Image1.Width := Form1.Width - 96;
  Image2.Left := 48;
  Image2.Width := Form1.Width - 96;
  Image2.Top := GroupBox1.Top + GroupBox1.Height+1;
  Image3.Height := Image2.Top + Image2.Height - Image1.Top;
  Image4.Height := Image2.Top + Image2.Height - Image1.Top;
  Image4.Left := GroupBox1.Left+GroupBox1.Width+15;
  Edit2.Top := Image2.Top+Image2.Height+3;
  Edit3.Top := Edit2.Top;
  Edit3.Left := Image4.Left+Image4.Width - Edit3.Width;
  Button1.Top := Form1.Height - 5 - Button1.Height;
  Button2.Top := Form1.Height - 5 - Button2.Height;
end;

end.

