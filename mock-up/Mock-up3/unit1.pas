unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, RTTICtrls, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls, Buttons, IU_TimControls, IU_Types;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Edit1: TEdit;
    GroupBox1: TGroupBox;
    Image1: TImage;
    Image2: TImage;
    Image3: TImage;
    Image4: TImage;
    Image5: TImage;
    Image6: TImage;
    Image7: TImage;
    Image8: TImage;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure Edit1Change(Sender: TObject);
    procedure Edit1Click(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormChangeBounds(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure FormPaint(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    _onScrollChange : T_IU_TB_onChangePTR;
  end;

var
  Form1: TForm1;

  _trackBar :  T_IU_TrackBar;
  _scrollBar : T_IU_ScrollBar;
  _progressBar : T_IU_ProgressBar;
  _checkbox : T_IU_CheckBox;
  _radioButton : T_IU_RadioButton;
  _radioButton2 : T_IU_RadioButton;
  _radioButton3 : T_IU_RadioButton;

  // ***
  // * Add IU_TimControls V0.10
  _comA : T_IU_RadioButton;
  _comB : T_IU_RadioButton;
  // ***

  BaseY : array[1..16] of integer;

implementation

var
  LocalObjectQueue : T_IU_ControlsQueue;
  LocalGroup : T_IU_RB_Group;
  // ***
  // * Add IU_TimControls V0.10
  LocalCom : T_IU_RB_Group;
  // ***

{$R *.lfm}

// ****
// *
// * Scroll call back
// *
// ****
// ***
Procedure _scroll(ID : word ; _currentValue : extended);
var k : extended ;
begin
  k := _currentValue/5000 - 1; // Scale factor of 2 from scrollBar interval then the middle is at 1 not at 0.5 (0.5 * 2 = 1)
  Form1.Edit1.Top := round(BaseY[1] + Form1.Height * k);
  Form1.Image1.Top := round(BaseY[2] + Form1.Height * k);
  Form1.GroupBox1.Top := round(BaseY[3] + Form1.Height * k);
  Form1.Button1.Top := round(BaseY[4] + Form1.Height * k);
  Form1.Label1.Top := round(BaseY[5] + Form1.Height * k);
  Form1.Image3.Top := round(BaseY[6] + Form1.Height * k);
  Form1.Label2.Top := round(BaseY[7] + Form1.Height * k);
  Form1.Image4.Top := round(BaseY[8] + Form1.Height * k);
  Form1.Label3.Top := round(BaseY[9] + Form1.Height * k);
  Form1.Image5.Top := round(BaseY[10] + Form1.Height * k);
  Form1.Label4.Top := round(BaseY[11] + Form1.Height * k);
  Form1.Image6.Top := round(BaseY[12] + Form1.Height * k);
  Form1.Image7.Top := round(BaseY[13] + Form1.Height * k);
  Form1.Image8.Top := round(BaseY[14] + Form1.Height * k);
  Form1.Label5.Top := round(BaseY[15] + Form1.Height * k);
  Form1.Label6.Top := round(BaseY[16] + Form1.Height * k);
end;

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  _trackBar := T_IU_TrackBar.Create(iu_tb_cursordirection_standard,10000,Image1,Edit1);
  // ***
  // * Add IU_TimControls V0.9
  _trackBar.setHint('Contrôle de saisie simulé !');
  _trackBar.activateHint;
  // *
  // * End Added IU_TimControls V0.9
  _scrollBar := T_IU_ScrollBar.Create(iu_tb_cursordirection_standard, 10000, Image2);
  // ***
  // * Add IU_TimControls V0.9
  _scrollbar.setHint('Démonstration barre de défilement !');
  _scrollbar.activateHint;
  // *
  // * End Added IU_TimControls V0.9
  _progressBar := T_IU_ProgressBar.Create(TWinControl(GroupBox1));
  // ***
  // * Add IU_TimControls V0.9
  _progressBar.setHint('Barre de progression simulée !');
  _progressBar.activateHint;
  // *
  // * End Added IU_TimControls V0.9
  _checkbox := T_IU_CheckBox.Create(Image3, Label1);
  // ***
  // * Add IU_TimControls V0.9
  _checkbox.setHint('Démonstration case à cocher !');
  _checkbox.activateHint;
  // *
  // * End Added IU_TimControls V0.9
  _radiobutton := T_IU_RadioButton.Create(Image4, Label2);
  // ***
  // * Add IU_TimControls V0.9
  _radiobutton.setHint('Démonstration premier radio bouton !');
  _radiobutton.activateHint;
  // *
  // * End Added IU_TimControls V0.9
  _radioButton2 := T_IU_RadioButton.Create(Image5, Label3);
  // ***
  // * Add IU_TimControls V0.9
  _radiobutton2.setHint('Démonstration deuxième radio bouton !');
  _radiobutton2.activateHint;
  // *
  // * End Added IU_TimControls V0.9
  _radioButton3 := T_IU_RadioButton.Create(Image6, Label4);

  // ***
  // * Add IU_TimControls V0.10
  _comA := T_IU_RadioButton.Create(Image7, Label5);
  _comB := T_IU_RadioButton.Create(Image8, Label6);
  _comA.setUnselect(false);
  _comB.setUnselect(false);
  _comA.setHint('Définir la position A (par exemple visualiser l''image source) !');
  _comA.activateHint;
  _comA.setValue(true);
  _comB.setHint('Définir la position B (par exemple visualiser l''image de prévisualisation des traitements) !');
  _comB.activateHint;
  // ***


  // ***
  // * Add IU_TimControls V0.9
  _radiobutton3.setHint('Démonstration 3ème radio bouton !');
  _radiobutton3.activateHint;
  // *
  // * End Added IU_TimControls V0.9
  _progressBar._init;
  _scrollBar._init;

  BaseY[1] := Edit1.Top;
  BaseY[2] := Image1.Top;
  BaseY[3] := GroupBox1.Top;
  BaseY[4] := Button1.Top;
  BaseY[5]:= Label1.Top;
  BaseY[6] := Image3.Top;
  BaseY[7]:= Label2.Top;
  BaseY[8] := Image4.Top;
  BaseY[9]:= Label3.Top;
  BaseY[10] := Image5.Top;
  BaseY[11]:= Label4.Top;
  BaseY[12] := Image6.Top;
  BaseY[13]:= Image7.Top;
  BaseY[14] := Image8.Top;
  BaseY[15]:= Label5.Top;
  BaseY[16] := Label6.Top;
  _scrollBar.onChange:= @_scroll;
  LocalObjectQueue := T_IU_ControlsQueue.Create(10);
  LocalObjectQueue.Add(form1.GetControlIndex(Edit1));             // 1- Adding the edit box of 1st TB
  LocalObjectQueue.Add(_trackbar._getID);                         // 2- Adding the trackbar
  LocalObjectQueue.Add(_checkbox._getID);                         // 3- Adding the first CB
  LocalObjectQueue.Add(_comA._getID);                      // 4- Adding the second CB
  LocalObjectQueue.Add(_comB._getID);
  LocalObjectQueue.Add(_radioButton._getID);                      // 4- Adding the second CB
  LocalObjectQueue.Add(_radioButton2._getID);
  LocalObjectQueue.Add(_radioButton3._getID);
  LocalObjectQueue.Add(form1.GetControlIndex(Button1));           // 5- Adding the button command
  LocalObjectQueue.Add(_scrollbar._getID);                        // 6- And finally adding the scroll bar
  LocalGroup := T_IU_RB_Group.Create(3); // only 2 RB
  LocalGroup.Add(@LocalGroup, @_radioButton);
  LocalGroup.Add(@LocalGroup, @_radioButton2);
  LocalGroup.Add(@LocalGroup, @_radioButton3);

  // ***
  // * Add IU_TimControls V0.10
  LocalCom := T_IU_RB_Group.Create(2); // only 2 RB
  LocalCom.Add(@LocalCom, @_comA);
  LocalCom.Add(@LocalCom, @_comB);
  // ***


end;

procedure TForm1.FormDestroy(Sender: TObject);
begin

  LocalGroup.Destroy;
  // Release all
  LocalObjectQueue.Destroy;


  _trackbar.Destroy;
  _checkbox.Destroy;
  _radioButton.Destroy;
  _radioButton2.Destroy;
  _radioButton3.Destroy;
  _scrollbar.Destroy;
end;

procedure TForm1.Button1Click(Sender: TObject);
var i : integer;
begin
  LocalObjectQueue.setCurrent(form1.GetControlIndex(form1.Button1));
  LocalObjectQueue.setFocus(TForm(Form1));
  for i := 0 to 10000 do begin
    _progressBar._setPercent(i/100);
    _progressBar._draw;
    sleep(2);
    Application.ProcessMessages;
  end;
end;


procedure TForm1.Edit1Change(Sender: TObject);
begin

end;

procedure TForm1.Edit1Click(Sender: TObject);
begin
  LocalObjectQueue.setCurrent(form1.GetControlIndex(form1.Edit1));
  LocalObjectQueue.setFocus(TForm(Form1));
end;

procedure TForm1.FormActivate(Sender: TObject);
begin

end;

procedure TForm1.FormChangeBounds(Sender: TObject);
begin
  Image2.Height := Form1.Height - 10;
end;

procedure TForm1.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState
  );
begin
  if Key = K_IU_Key_TAB then begin
    Key := word(0);
    if ssShift in Shift then LocalObjectQueue.MovePrevious
    else LocalObjectQueue.MoveNext;
    LocalObjectQueue.setFocus(TForm(Form1));
  end else begin
    _trackBar._setValueFromKeyboard(Key);
    _scrollBar._setValueFromKeyboard(Key);
    _checkbox._setValueFromKeyboard(key);
    _radioButton._setValueFromKeyboard(key);
    _radioButton2._setValueFromKeyboard(key);
    _radioButton3._setValueFromKeyboard(key);
    _comA._setValueFromKeyBoard(key);
    _comB._setValueFromKeyBoard(key);
  end;
end;

procedure TForm1.FormKeyPress(Sender: TObject; var Key: char);
begin
end;

procedure TForm1.FormPaint(Sender: TObject);
begin
end;

procedure TForm1.FormShow(Sender: TObject);
begin
  LocalObjectQueue.Rewind;
  LocalObjectQueue.setFocus(TForm(Form1));
end;


end.

