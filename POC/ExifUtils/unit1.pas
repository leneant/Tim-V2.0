unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls, IU_ExifUtils, IU_I18N_Messages;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Memo1: TMemo;
    OpenDialog1: TOpenDialog;
    procedure Button1Click(Sender: TObject);
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
var _return : IU_T_ExifArray;
  i : integer;
  _value : string;
begin
  IU_CurrentLang := K_IU_I18N_FRENCH;
  if Opendialog1.Execute then begin
    _return := getExif (OpenDialog1.FileName);
    Form1.Memo1.Clear;
    for i := Low(_return) to High(_return) do begin
      if _return[i].value = 'No' then _value := IU_HI_Messages[IU_CurrentLang,K_IU_HIMSG_No]
      else if _return[i].value = 'Yes' then _value := IU_HI_Messages[IU_CurrentLang,K_IU_HIMSG_Yes]
      else if _return[i].value = ' top, left' then _value := IU_HI_Messages[IU_CurrentLang,K_IU_HIMSG_TopLeft]
      else if _return[i].value = ' right, top' then _value := IU_HI_Messages[IU_CurrentLang,K_IU_HIMSG_RightTop]
      else if _return[i].value = ' bottom, right' then _value := IU_HI_Messages[IU_CurrentLang,K_IU_HIMSG_BottomRight]
      else if _return[i].value = ' left, bottom' then _value := IU_HI_Messages[IU_CurrentLang,K_IU_HIMSG_LeftBottom]
      else _value := _return[i].value;
      Form1.Memo1.Append(_return[i].Exif + ' = ' + _value);
    end;
  end;
end;

end.

