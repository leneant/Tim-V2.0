unit IU_importfile;
// ***
// * Unit provides an import file windows
// * Creation Date : 2017 December
// *
// * Version : 0.8
// * Version Date : 2018 January
// * Version Contributors : Pascal Lemaître
// *
// * Version 0.8 : Changing geolocalisation of components to set space for current directory indicator
// * Version 0.7 : Add splitter position in windows properties
// * Version 0.6 : Add color personnalization popupmenu
// * Version 0.5 : Add list invert font color command
// * Version 0.4 : Add default colors from IU_TimControls
// * Version 0.3 : Add change for CentOS. Try to control BG color of ListBox when there are not empty
// * Version 0.2 : Adding management of horizontals scrollbars
// * Version 0.1 : Creation
// *
// * @authors : Pascal Lemaître
// *
// * @see :
// *
// *
// * Team : TIm (Traitement d'Images)
// *
// * 2017-2018
// ***
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  StdCtrls, ExtDlgs, ShellCtrls, IU_GeneralUtils, BGRABitmap, BGRABitmapTypes, IU_strechutils, LCLProc
  // ***
  // * Add v0.3 IU_I18N_Messages
  , IU_I18N_Messages, LCLType, ExtCtrls, PairSplitter, Menus
  // ***

  // ***
  // * Add v0.4
  , IU_TimControls
  // ***

  // ***
  // * Add v0.8
  , IU_ExifUtils
  // ***
  ;

type

  // ***
  // * Add v0.6
  T_ImportFile_Colors = record
    WindowsColor : TColor;
    TextColor : TColor;
    ListColor : TColor;
    ListSelectedColor : TColor;
    ListTextColor : TColor;
    DropDownColor : TColor;
    DropDownSelectedColor : TColor;
    DropDownTextColor : TColor;
    ButtonColor : TColor;
    ButtonSelectedColor : TColor;
  end;

  T_ImportFile_Bounds = record
    x,y,widht,height : integer;
    // ***
    // * Add v0.7
    split1, split2 : integer;
    // ***
  end;

  T_ImportFile_Properties = record
    bounds : T_ImportFile_Bounds;
    colors : T_ImportFile_Colors;
  end;

  // *
  // * End Add V0.6
  // ***

  { TImportFile }

  TImportFile = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    ColorDialog1: TColorDialog;
    ComboBox1: TComboBox;
    Image1: TImage;
    Label1: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    Label18: TLabel;
    Label19: TLabel;
    Label2: TLabel;
    Label20: TLabel;
    Label21: TLabel;
    Label22: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    ListBox1: TListBox;
    ListBox2: TListBox;
    Menu11: TMenuItem;
    Menu01: TMenuItem;
    Menu10: TMenuItem;
    Menu2: TMenuItem;
    Menu3: TMenuItem;
    Menu4: TMenuItem;
    Menu5: TMenuItem;
    Menu6: TMenuItem;
    Menu07: TMenuItem;
    Menu8: TMenuItem;
    Menu9: TMenuItem;
    Panel1: TPanel;
    PopupMenu1: TPopupMenu;
    Shape1: TShape;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    procedure Button1Click(Sender: TObject);
    procedure Button1Enter(Sender: TObject);
    procedure Button1Exit(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button2Enter(Sender: TObject);
    procedure Button2Exit(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button5Enter(Sender: TObject);
    procedure Button5Exit(Sender: TObject);
    procedure Button6Enter(Sender: TObject);
    procedure Button6Exit(Sender: TObject);
    procedure Button7Click(Sender: TObject);
    procedure Button7Enter(Sender: TObject);
    procedure Button7Exit(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
    procedure ComboBox1Enter(Sender: TObject);
    procedure ComboBox1Exit(Sender: TObject);
    procedure ComboBox1KeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormChangeBounds(Sender: TObject);
    procedure FormClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ListBox1Click(Sender: TObject);
    procedure ListBox1DblClick(Sender: TObject);
    procedure ListBox1Enter(Sender: TObject);
    procedure ListBox1Exit(Sender: TObject);
    procedure ListBox1KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState
      );
    procedure ListBox2Click(Sender: TObject);
    procedure ListBox2Enter(Sender: TObject);
    procedure ListBox2Exit(Sender: TObject);
    procedure ListBox2KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState
      );
    procedure Menu10Click(Sender: TObject);
    procedure Menu11Click(Sender: TObject);
    procedure Menu01Click(Sender: TObject);
    procedure Menu2Click(Sender: TObject);
    procedure Menu3Click(Sender: TObject);
    procedure Menu4Click(Sender: TObject);
    procedure Menu5Click(Sender: TObject);
    procedure Menu6Click(Sender: TObject);
    procedure Menu07Click(Sender: TObject);
    procedure Menu8Click(Sender: TObject);
    procedure Menu9Click(Sender: TObject);
    procedure Splitter1Moved(Sender: TObject);
    procedure Splitter2Moved(Sender: TObject);
  private
    { private declarations }
    bmp1 : TBGRABitmap;
    SelectedFile : widestring;
  public
    { public declarations }
    // ***
    // * Add v0.4 IU_I18N_Messages
    procedure I18N ;
    // *
    // * End Add v0.4 IU_I18N_Messages
    // ***
  end;

var
  ImportFile: TImportFile;
  // ***
  // * Del v0.6
  // fontColorInverted : boolean;
  // ***

  // ***
  // * Add v0.6
  WindowsColors : T_ImportFile_Colors;
  ImportFile_Properties : T_ImportFile_Properties;
  // ***


  // ***
  // * Add V1.5 IU_GeneralUtils
  filesList : IU_T_FilesList;
  dirsList : IU_T_DirsList;
  // *
  // * End Add V1.5 IU_GeneralUtils
  // ***


implementation

{$R *.lfm}
// ***
// * Add V1.2 IU_GeneralUtils
// *
{$ifdef Windows}
var
  WinDrives : T_WindowsListDrives ;
{$endif}
// *
// * End Add V1.2 IU_GeneralUtils
// ***

// ***
// * Add v0.6
// ***
// * Setting default colors (has defined by programmers
// *
// * @author : Pascal Lemaitre
// *
procedure initDefaultColors;
begin
  with  WindowsColors do begin
    WindowsColor := V_IU_TB_DefaultBG;
    TextColor := rgbtocolor(V_IU_Box_TextColorHigh.red, V_IU_Box_TextColorHigh.green, V_IU_Box_TextColorHigh.blue);
    ListColor := rgbtocolor(V_IU_Box_BGColor.red,V_IU_Box_BGColor.green,V_IU_Box_BGColor.blue);
    ListSelectedColor := rgbtocolor(V_IU_SB_DefaultBGColor.red,V_IU_SB_DefaultBGColor.green,V_IU_SB_DefaultBGColor.blue);
    ListTextColor := rgbtocolor(V_IU_Box_TextColorHigh.red, V_IU_Box_TextColorHigh.green, V_IU_Box_TextColorHigh.blue);
    DropDownColor := rgbtocolor(V_IU_Box_BGColor.red,V_IU_Box_BGColor.green,V_IU_Box_BGColor.blue);
    DropDownSelectedColor := rgbtocolor(V_IU_SB_DefaultBGColor.red,V_IU_SB_DefaultBGColor.green,V_IU_SB_DefaultBGColor.blue);
    DropDownTextColor := rgbtocolor(V_IU_Box_TextColorHigh.red, V_IU_Box_TextColorHigh.green, V_IU_Box_TextColorHigh.blue);
    ButtonColor := rgbtocolor(V_IU_Button_unfocused.red, V_IU_Button_unfocused.green, V_IU_Button_unfocused.blue);
    ButtonSelectedColor := rgbtocolor(V_IU_Button_focused.red, V_IU_Button_focused.green, V_IU_Button_focused.blue);
  end;
end;
// *
// * End Add v0.6
// ***

// ***
// * Add v0.4
// ***
// * Setting colors to all components of the windows
// *
// * @author : Pascal Lemaitre
// *
procedure setColors;
var listFontColor : TColor;
begin
  ImportFile.Color:=WindowsColors.WindowsColor;
  with ImportFile do begin
    // Setting default background colors
    with WindowsColors do begin
      // ***
      // * Add v0.8
      Panel1.Color := WindowsColor;
      Label11.Color := WindowsColor;
      Label11.Font.Color := TextColor;
      // ***
      Shape1.Color:=WindowsColor;
      Label1.Color:=WindowsColor;
      Label1.Font.Color:=TextColor;
      Label2.Color:=WindowsColor;
      Label2.Font.Color:=TextColor;
      Label3.Color:=WindowsColor;
      Label3.Font.Color:=TextColor;
      Label4.Color:=WindowsColor;
      Label4.Font.Color:=TextColor;
      Label5.Color:=WindowsColor;
      Label5.Font.Color:=TextColor;
      Label6.Color:=WindowsColor;
      Label6.Font.Color:=TextColor;
      Label7.Color:=WindowsColor;
      Label7.Font.Color:=TextColor;
      Label8.Color:=WindowsColor;
      Label8.Font.Color:=TextColor;
      Label9.Color:=WindowsColor;
      Label9.Font.Color:=TextColor;
      Label10.Color:=WindowsColor;
      Label10.Font.Color:=TextColor;
      if Button1.Focused then Button1.Color := ButtonSelectedColor else
      Button1.Color:=ButtonColor;
      if Button2.Focused then Button2.Color := ButtonSelectedColor else
      Button2.Color:=ButtonColor;
      if Button5.Focused then Button5.Color := ButtonSelectedColor else
      Button5.Color:=ButtonColor;
      if Button6.Focused then Button6.Color := ButtonSelectedColor else
      Button6.Color:=ButtonColor;
      if ListBox1.focused then ListBox1.Color := ListSelectedColor else
      ListBox1.Color:=ListColor;
      ListBox1.Font.Color:=listTextColor;
      if ListBox2.focused then ListBox2.Color := ListSelectedColor else
      ListBox2.Color:=ListColor;
      ListBox2.Font.Color:=listTextColor;
      Splitter1.Color:=WindowsColor;
      Splitter1.Brush.Color:=WindowsColor;
      Splitter2.Color:=WindowsColor;
      Splitter2.Brush.Color:=WindowsColor;
      if ComboBox1.Focused then ComboBox1.Color := ButtonSelectedColor else
      ComboBox1.Color:=DropDownColor;
      ComboBox1.Font.Color:=DropDownTextColor;
      Image1.Color:=WindowsColor;
      Image1.Canvas.Brush.Color:=WindowsColor;
      Image1.Canvas.Pen.Color:=WindowsColor;
      Image1.Canvas.FillRect(0,0,Image1.Width, Image1.Height);
      Image1.Refresh;
    end;
  end;
end;
// *
// * End Add v0.4
// ***

// ***
// * Add v0.4 IU_I18N_Messages
// ***
// * Translate all text in the form
// *
// * @author : Pascal Lemaitre
// *
procedure TImportFile.I18N ;
var
  dsize, dfree : int64;
  _coef : int64;
  _scale : string;
  _index : integer;
begin
  ImportFile.Caption := IU_HI_Messages[IU_CurrentLang,K_IU_HIMSG_ImportFileWindowTitle];
  ImportFile.Label5.Caption := IU_HI_Messages[IU_CurrentLang,K_IU_HIMSG_Dir];
  ImportFile.Label6.Caption := IU_HI_Messages[IU_CurrentLang,K_IU_HIMSG_Files];
  ImportFile.Label1.Caption := IU_HI_Messages[IU_CurrentLang,K_IU_HIMSG_DiskSize];
  ImportFile.Label3.Caption := IU_HI_Messages[IU_CurrentLang,K_IU_HIMSG_DiskFree];
  ImportFile.Label8.Caption := IU_HI_Messages[IU_CurrentLang,K_IU_HIMSG_FilesTypes];
  // Adding files types filters
  _index := ImportFile.ComboBox1.ItemIndex;
  ImportFile.ComboBox1.Clear;
  ImportFile.ComboBox1.Items.Add(IU_HI_Messages[IU_CurrentLang,K_IU_HIMSG_AllFiles]);
  ImportFile.ComboBox1.Items.Add(IU_HI_Messages[IU_CurrentLang,K_IU_HIMSG_JpegFiles]);
  ImportFile.ComboBox1.Items.Add(IU_HI_Messages[IU_CurrentLang,K_IU_HIMSG_BmpFiles]);
  ImportFile.ComboBox1.Items.Add(IU_HI_Messages[IU_CurrentLang,K_IU_HIMSG_TiffFiles]);
  if _index = - 1 then ImportFile.ComboBox1.ItemIndex := 0 else ImportFile.ComboBox1.ItemIndex:=_index;
  // Disk Size update
  IU_getCurrentDriveSizes(dsize, dfree);
  if dsize < 1024*1024*1024*1024 then begin
    _coef := 1024*1024*1024;
    _scale := IU_HI_Messages[IU_CurrentLang,K_IU_HIMSG_GB];
  end else begin
    _coef := 1024*1024*1024*1024 ;
    _scale := IU_HI_Messages[IU_CurrentLang,K_IU_HIMSG_TB];
  end ;
  Label2.Caption:=IU_realToString(dsize/_coef, 2) + ' ' + _scale;
  Label4.Caption:=IU_realToString(dfree/_coef, 2) + ' ' + _scale;
  // Commands
  Button5.Caption := IU_HI_Messages[IU_CurrentLang,K_IU_HIMSG_ImportCommand];
  Button6.Caption := IU_HI_Messages[IU_CurrentLang,K_IU_HIMSG_CancelCommand];

  // ***
  // * Add v0.6
  Menu01.Caption := IU_HI_Messages[IU_CurrentLang,K_IU_HIMSG_setWindowsColor];
  Menu2.Caption := IU_HI_Messages[IU_CurrentLang,K_IU_HIMSG_setTextColor];
  Menu3.Caption := IU_HI_Messages[IU_CurrentLang,K_IU_HIMSG_setListColor];
  Menu10.Caption := IU_HI_Messages[IU_CurrentLang,K_IU_HIMSG_setListSelectedColor];
  Menu6.Caption := IU_HI_Messages[IU_CurrentLang,K_IU_HIMSG_setListTextColor];
  Menu4.Caption := IU_HI_Messages[IU_CurrentLang,K_IU_HIMSG_setDropDownColor];
  Menu9.Caption := IU_HI_Messages[IU_CurrentLang,K_IU_HIMSG_setDropDownSelectedColor];
  Menu07.Caption := IU_HI_Messages[IU_CurrentLang,K_IU_HIMSG_setDropDownTextColor];
  Menu5.Caption := IU_HI_Messages[IU_CurrentLang,K_IU_HIMSG_setButtonColor];
  Menu8.Caption := IU_HI_Messages[IU_CurrentLang,K_IU_HIMSG_setButtonSelectedColor];
  Menu11.Caption := IU_HI_Messages[IU_CurrentLang,K_IU_HIMSG_setStandardColors];
  // *
  // * End Add v0.6
  // ***

  // ***
  // * Add v0.8
  Label11.Caption := IU_HI_Messages[IU_CurrentLang,K_IU_HIMSG_CurrentDir];
  // ***
end;

// *
// * End Add v0.4 IU_I18N_Messages
// ***

{ TImportFile }


// ***
// * Add V1.5 IU_GeneralUtils
procedure TImportFile.Button1Click(Sender: TObject);
var i : integer;
  _directory : string;
begin
  dirsList.revertReadOrder;
  dirsList.rewind;
  ListBox1.Clear;
  {$ifdef Windows}
  WinDrives := IU_ListWindowsDrives;
  for i := 1 to 26 do begin
    if WinDrives[i].exist then begin
      ListBox1.Items.Add(WinDrives[i].drive + ':\');
      if length(WinDrives[i].drive + ':\') * 10 > ListBox1.ScrollWidth then ListBox1.ScrollWidth := length(WinDrives[i].drive + ':\') * 10;
    end ;
  end;
  _directory := getCurrentDir + '\';
  {$else}
  _directory := getCurrentDir + '/';
  {$endif}
  // ListBox1.Items.Add(_directory);
  for i := 1 to dirsList.getItemsNumbers - 1 do begin
    ListBox1.Items.Add(dirsList.getText);
    if length(dirsList.getText) * 10 > ListBox1.ScrollWidth then ListBox1.ScrollWidth := length(dirsList.getText) * 10;
    dirsList.next;
  end;
  ListBox1.Items.Add(dirsList.getText);
  if length(dirsList.getText) * 10 > ListBox1.ScrollWidth then ListBox1.ScrollWidth := length(dirsList.getText) * 10;
  // ***
  // * Modified V0.6
  ListBox1.Color:=WindowsColors.ListColor;
  // ***
end;

procedure TImportFile.Button1Enter(Sender: TObject);
begin
  // ***
  // * Modified v0.6
    Button1.Color:=WindowsColors.ButtonSelectedColor;
  ImportFile.Refresh;
  // ***
end;

procedure TImportFile.Button1Exit(Sender: TObject);
begin
  // ***
  // * Modified v0.6
  Button1.Color:=WindowsColors.ButtonColor;
  // ***
end;


procedure TImportFile.Button2Click(Sender: TObject);
var i : integer;
  _directory : string;
begin
  filesList.revertReadOrder;
  filesList.rewind;
  ListBox2.Clear;
  for i := 1 to filesList.getItemsNumbers - 1 do begin
    ListBox2.Items.Add(filesList.getText);
    if length(filesList.getText) * 10 > ListBox2.ScrollWidth then ListBox2.ScrollWidth := length(filesList.getText) * 10;
    filesList.next;
  end;
  ListBox2.Items.Add(filesList.getText);
  if length(filesList.getText) * 10 > ListBox2.ScrollWidth then ListBox2.ScrollWidth := length(filesList.getText) * 10;
  // ***
  // * Modified v0.6
  ListBox2.Color:=WindowsColors.ListColor;
  // *
  // * End Add V0.6
  // ***
end;

procedure TImportFile.Button2Enter(Sender: TObject);
begin
  // ***
  // * Modified v0.6
  button2.Color:=WindowsColors.ButtonSelectedColor;
  // ***
end;

procedure TImportFile.Button2Exit(Sender: TObject);
begin
  // ***
  // * Modified v0.6
  button2.color:=WindowsColors.ButtonColor;
  // ***
end;

procedure TImportFile.Button3Click(Sender: TObject);
begin
  IU_CurrentLang:=K_IU_I18N_FRENCH;
  I18N;
end;

procedure TImportFile.Button4Click(Sender: TObject);
begin
  IU_CurrentLang:=K_IU_I18N_ENGLISH;
  I18N;
end;

procedure TImportFile.Button5Click(Sender: TObject);
var R : integer;
begin
  if SelectedFile <>'' then
    R := MessageDlg(IU_HI_Messages[IU_CurrentLang,K_IU_HIMSG_WarningBoxTitle],SelectedFile,mtWarning,[mbOk],0);
end;

procedure TImportFile.Button5Enter(Sender: TObject);
begin
  // ***
  // * Modified v0.6
  button5.color:=WindowsColors.ButtonSelectedColor;
  // ***
end;

procedure TImportFile.Button5Exit(Sender: TObject);
begin
  // ***
  // * Modified v0.6
  button5.color:=WindowsColors.ButtonColor;
  // ***
end;

procedure TImportFile.Button6Enter(Sender: TObject);
begin
  // ***
  // * Modified v0.6
  button6.color:=WindowsColors.ButtonSelectedColor;
  // ***
end;

procedure TImportFile.Button6Exit(Sender: TObject);
begin
  // ***
  // * Modified v0.6
  button6.color:=WindowsColors.ButtonColor;
  // ***
end;

procedure TImportFile.Button7Click(Sender: TObject);
begin
  // ***
  // * Deleted v0.6
  //  fontColorInverted := not fontColorInverted;
  //  setColors;
  // ***
end;

procedure TImportFile.Button7Enter(Sender: TObject);
begin
  // ***
  // * Del v0.6
  // button7.color:=rgbtocolor(V_IU_Button_focused.red, V_IU_Button_focused.green, V_IU_Button_focused.blue);
  // ***
end;

procedure TImportFile.Button7Exit(Sender: TObject);
begin
  // ***
  // Modified v0.6
  button6.color:=WindowsColors.ButtonColor;
  // ***
end;

procedure TImportFile.ComboBox1Change(Sender: TObject);
begin
  ListBox1DblClick(Sender);
end;

procedure TImportFile.ComboBox1Enter(Sender: TObject);
begin
  // ***
  // * Modified v0.6
  ComboBox1.color:=WindowsColors.DropDownSelectedColor;
  // ***
end;

procedure TImportFile.ComboBox1Exit(Sender: TObject);
begin
  // ***
  // * Modified v0.6
  ComboBox1.Color := WindowsColors.DropDownColor;
  // ***
end;

procedure TImportFile.ComboBox1KeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  // ***
  // * Modified V0.6
  Combobox1.Color:=WindowsColors.DropDownSelectedColor;
  // ***
  // ***
  // To resolve Bug in focus management with combobox.
  // Even if tabstop gives combo , listbox2, button2 the
  // default sequence of the focus looped like that combobox, button2, combobox, button2 ....
  // Then we must manage ourself the sequence of the focus.
  // Catched the tab and shift-tab (keydown event) to give the focus to the listbox2 or to button1
  if Key = Word(9) then
    if (ssShift in Shift) then Button1.setFocus
    else ListBox2.setFocus;
  key:=word(0);
end;

// *
// * End Add V1.5 IU_GeneralUtils
// ***

procedure TImportFile.FormChangeBounds(Sender: TObject);
begin
  ListBox1.Height := ImportFile.Height - ListBox1.Top - 2;
  ListBox2.Height := ImportFile.Height - ListBox2.Top - 2;
  Splitter1.Height := ImportFile.Height;
  Splitter2.Height := ImportFile.Height;
  Button5.Top := ImportFile.Height - Button5.Height - 5;
end;

procedure TImportFile.FormClick(Sender: TObject);
begin

end;


// ***
// * Add V1.5 IU_GeneralUtils
procedure TImportFile.FormCreate(Sender: TObject);
var
  R: Integer;
  SearchRec: TSearchRec;
  _directory : string;
  dsize, dfree : int64;
  _coef : int64;
  _scale : string;
begin
  // ***
  // * Add v0.6
  // Trying to load defaults properties
  try
    readProperties('ImportFile', ImportFile_Properties, sizeof(ImportFile_Properties));
    WindowsColors := ImportFile_Properties.colors;
    ImportFile.Top := ImportFile_Properties.bounds.y;
    ImportFile.Left := ImportFile_Properties.bounds.x;
    ImportFile.Width := ImportFile_Properties.bounds.widht;
    ImportFile.Height := ImportFile_Properties.bounds.height;
    ImportFile.Splitter2.Left := ImportFile_Properties.bounds.Split2;
    ImportFile.Splitter1.Left := ImportFile_Properties.bounds.split1;
    // ***
    // * Add v0.7 Windows Corrections
    ImportFile.Splitter1Moved(Sender);
    ImportFile.FormChangeBounds(Sender);
    // ***
  Except
    initDefaultColors;
  end;
  // * End Add v0.6
  // ***
  setColors;
  Shape1.Top := Image1.Top-1;
  Shape1.Left := Image1.Left - 1;
  Shape1.Width := Image1.Width + 2;
  shape1.Height := Image1.Height + 2;
  SelectedFile := ''; // no file selected
  dirsList := IU_T_DirsList.Create;
  filesList := IU_T_FilesList.Create;
  I18N;
  IU_getCurrentDriveSizes(dsize, dfree);
  if dsize < 1024*1024*1024*1024 then begin
    _coef := 1024*1024*1024;
    _scale := IU_HI_Messages[IU_CurrentLang,K_IU_HIMSG_GB];
  end else begin
    _coef := 1024*1024*1024*1024 ;
    _scale := IU_HI_Messages[IU_CurrentLang,K_IU_HIMSG_TB];
  end ;
  Label2.Caption:=IU_realToString(dsize/_coef, 2) + ' ' + _scale;
  Label4.Caption:=IU_realToString(dfree/_coef, 2) + ' ' + _scale;
  {$ifdef Windows}
  WinDrives := IU_ListWindowsDrives;
  for R := 1 to 26 do begin
    if WinDrives[R].exist then begin
      ListBox1.Items.Add(WinDrives[R].drive + ':\');
      if length(WinDrives[R].drive + ':\') * 10 > ListBox1.ScrollWidth then ListBox1.ScrollWidth := length(WinDrives[R].drive + ':\') * 10;
    end ;
  end;
  _directory := getCurrentDir + '\';
  {$else}
  _directory := getCurrentDir + '/';
  {$endif}
  // ListBox1.Items.Add(_directory);
  // ***
  // * Modified v0.8
  // * Del ' : ' at the end of the path
  Label7.Caption := _directory;
  // ***
  // create dirs list
  dirsList.initDirectoriesList;
  // create files list
  filesList.addFilesInList('*.*');
  // Files list
  for R := 1 to filesList.getItemsNumbers do begin
    // No current item selected then must start with next for selecting the first
    filesList.next;
    ListBox2.Items.Add(filesList.getText);
    if length(filesList.getText) * 10 > ListBox2.ScrollWidth then ListBox2.ScrollWidth := length(filesList.getText) * 10;
  end;
  // ***
  // * Modified v0.6
  if ListBox2.Focused then ListBox2.Color := WindowsColors.ListSelectedColor else
    ListBox2.Color := WindowsColors.ListColor;
  // *
  // * End Modified V0.3
  // ***
  // Dir list
  for R := 1 to dirsList.getItemsNumbers do begin
    // No current item selected then must start with next for selecting the first
    dirsList.next;
    ListBox1.Items.Add(dirsList.getText);
    if length(dirsList.getText) * 10 > ListBox1.ScrollWidth then ListBox1.ScrollWidth := length(dirsList.getText) * 10;
  end;
  // ***
  // * Modified v0.6
  if ListBox1.Focused then ListBox1.Color := WindowsColors.ListSelectedColor else
    ListBox1.Color := WindowsColors.ListColor;
  // *
  // * End Modified V0.6
  // ***
end;
// *
// * End Add V1.5 IU_GeneralUtils
// ***

procedure TImportFile.FormDestroy(Sender: TObject);
begin
  // ***
  // * Add v0.6
  // Trying to load defaults properties
  ImportFile_Properties.colors := WindowsColors;
  ImportFile_Properties.bounds.y := ImportFile.Top;
  ImportFile_Properties.bounds.x := ImportFile.Left;
  ImportFile_Properties.bounds.widht := ImportFile.Width;
  ImportFile_Properties.bounds.height := ImportFile.Height;
  // ***
  // * Add v0.7
  ImportFile_Properties.bounds.split1 := ImportFile.Splitter1.Left;
  ImportFile_Properties.bounds.split2 := ImportFile.Splitter2.Left;
  // ***
  try
    writeProperties('ImportFile', ImportFile_Properties, sizeof(ImportFile_Properties));
  finally
  // * End Add v0.6
  // ***
    dirsList.Destroy;
    filesList.Destroy;
  // ***
  // * Add v0.6
  end;
  // * End Add v0.6
  // ***
end;

procedure TImportFile.FormMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
end;

procedure TImportFile.ListBox1Click(Sender: TObject);
begin
  // ***
  // * Add v0.6
  ListBox1.Color := WindowsColors.ListSelectedColor;
  // ***
end;

// ***
// * Add V1.5 IU_GeneralUtils
procedure TImportFile.ListBox1DblClick(Sender: TObject);
var
  R: Integer;
  SearchRec: TSearchRec;
  _directory, _olddir : string;
  dsize, dfree : int64;
  _coef : int64;
  _scale : string;
begin
  ListBox1.ScrollWidth := ListBox1.Width;
  ListBox2.ScrollWidth := ListBox2.Width;
  _olddir := getCurrentDir;
  _directory := ListBox1.GetSelectedText;
  ListBox1.Clear;
  ListBox2.Clear;
  // reseting internal lists
  filesList.reset;
  dirsList.reset;
  try
    ChDir(_directory);
  except
    R := MessageDlg(IU_HI_Messages[IU_CurrentLang,K_IU_HIMSG_WarningBoxTitle],IU_HI_Messages[IU_CurrentLang,K_IU_HIMSG_UnreachedDrive],mtWarning,[mbOk],0);
    _directory := _olddir;
  end;
  IU_getCurrentDriveSizes(dsize, dfree);
  if dsize < 1024*1024*1024*1024 then begin
    _coef := 1024*1024*1024;
    _scale := IU_HI_Messages[IU_CurrentLang,K_IU_HIMSG_GB];
  end else begin
    _coef := 1024*1024*1024*1024 ;
    _scale := IU_HI_Messages[IU_CurrentLang,K_IU_HIMSG_TB];
  end ;
  Label2.Caption:=IU_realToString(dsize/_coef, 2) + ' ' +  _scale;
  Label4.Caption:=IU_realToString(dfree/_coef, 2) + ' ' + _scale;
  {$ifdef Windows}
  for R := 1 to 26 do begin
    if WinDrives[R].exist then begin
      ListBox1.Items.Add(WinDrives[R].drive + ':\');
      if length(WinDrives[R].drive + ':\' )* 10 > ListBox1.ScrollWidth then ListBox1.ScrollWidth := length(WinDrives[R].drive + ':\') * 10;
    end ;
  end;
  _directory := getCurrentDir + '\';
  {$else}
  _directory := getCurrentDir + '/';
  {$endif}
  // ListBox1.Items.Add(_directory);
  // ***
  // * Modified v0.8
  // * Dell + ' :' after _directory
  Label7.Caption := _directory;
  // ***
  // init dirs list
  dirsList.initDirectoriesList;
  // init files list
  if ImportFile.ComboBox1.Items[ComboBox1.ItemIndex]=IU_HI_Messages[IU_CurrentLang,K_IU_HIMSG_AllFiles] then
    filesList.addFilesInList('*.*')
  else if ImportFile.ComboBox1.Items[ComboBox1.ItemIndex]=IU_HI_Messages[IU_CurrentLang,K_IU_HIMSG_JpegFiles] then begin
    filesList.addFilesInList('*.jpg');
    filesList.addFilesInList('*.JPG');
    filesList.addFilesInList('*.jpeg');
    filesList.addFilesInList('*.JPEG');
  end else if ImportFile.ComboBox1.Items[ComboBox1.ItemIndex]=IU_HI_Messages[IU_CurrentLang,K_IU_HIMSG_BmpFiles] then begin
    filesList.addFilesInList('*.bmp');
    filesList.addFilesInList('*.BMP');
    filesList.addFilesInList('*.png');
    filesList.addFilesInList('*.PNG');
  end else if ImportFile.ComboBox1.Items[ComboBox1.ItemIndex]=IU_HI_Messages[IU_CurrentLang,K_IU_HIMSG_TiffFiles] then begin
    filesList.addFilesInList('*.tif');
    filesList.addFilesInList('*.TIF');
    filesList.addFilesInList('*.tiff');
    filesList.addFilesInList('*.TIFF');
  end;
   // Files list
   for R := 1 to filesList.getItemsNumbers do begin
     // No current item selected then must start with next for selecting the first
     filesList.next;
     ListBox2.Items.Add(filesList.getText);
     if length(filesList.getText) * 10 > ListBox2.ScrollWidth then ListBox2.ScrollWidth := length(filesList.getText) * 10;
   end;
   // ***
   // * Modified v0.6
   if ListBox2.Focused then ListBox2.Color := WindowsColors.ListSelectedColor else
     ListBox2.Color := WindowsColors.ListColor;
   // *
   // * End Modified V0.6
   // ***
   // Dir list
   for R := 1 to dirsList.getItemsNumbers do begin
     // No current item selected then must start with next for selecting the first
     dirsList.next;
     ListBox1.Items.Add(dirsList.getText);
     if length(dirsList.getText) * 10 > ListBox1.ScrollWidth then ListBox1.ScrollWidth := length(dirsList.getText) * 10;
   end;
   // ***
   // * Modified v0.6
   if ListBox1.Focused then ListBox1.Color := WindowsColors.ListSelectedColor else
     ListBox1.Color := WindowsColors.ListColor;
   // *
   // * End Modified V0.6
   // ***
end;

procedure TImportFile.ListBox1Enter(Sender: TObject);
begin
  // ***
  // * Modified V0.6
  ListBox1.Color:=WindowsColors.ListSelectedColor;
  // ***
end;

procedure TImportFile.ListBox1Exit(Sender: TObject);
begin
  // ***
  // * Modified v0.6
  ListBox1.Color:=WindowsColors.ListColor;
  // ***
end;

procedure TImportFile.ListBox1KeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  // ***
  // * Modified V0.6
  ListBox1.Color:=WindowsColors.ListSelectedColor;
  // ***
  if Key = Word(13) then
    ListBox1DblClick(Sender);
end;

procedure TImportFile.ListBox2Click(Sender: TObject);
var stretched : TBGRABitmap;
  _width, _height, x, y, i : integer;
  _file, _dir : string;
  _error : boolean;
  R : integer;
  // ***
  // * Add v0.8
  _exifsList : IU_T_ExifArray ;
  _value : string;
  // ***
begin
  ListBox2.Color:=WindowsColors.ListSelectedColor;
  ListBox2.Repaint;
  ListBox2.Refresh;
  if (ListBox2.Count > 0) and (ListBox2.ItemIndex > -1) then begin
    screen.cursor := crHourGlass;
    _error := false;
    bmp1 := TBGRABitmap.Create;
    {$ifdef windows}
    _dir := GetCurrentDir + '\';
    {$else}
    _dir := GetCurrentDir + '/';
    {$endif}
    _file := ListBox2.Items[ListBox2.ItemIndex] ;
    try
      bmp1.LoadFromFile(_dir + _file);
    Except
      on  EAccessViolation do begin
        Image1.Canvas.AutoRedraw:=true;
        // ***
        // * Modified v0.6
        Image1.Canvas.Pen.Color:=WindowsColors.WindowsColor;
        Image1.Canvas.Brush.Color:=WindowsColors.WindowsColor;
        // *
        // * End modified v0.6
        // ***
        Image1.Canvas.FillRect(0,0,Image1.Width, Image1.Height);
        Image1.Refresh;
        SelectedFile :=  '';
        Label9.Caption := IU_HI_Messages[IU_CurrentLang,K_IU_HIMSG_LoadingFileError];
        Label10.Caption := '';
        _error := true;
        screen.cursor := crDefault;
         R := MessageDlg(IU_HI_Messages[IU_CurrentLang,K_IU_HIMSG_WarningBoxTitle],IU_HI_Messages[IU_CurrentLang,K_IU_HIMSG_LoadingFileError],mtWarning,[mbOk],0);
      end;
      else begin
        screen.cursor := crDefault;
        _error := true;
      end;
    end;
    if not _error then begin
      StrechUtils_StrechLimits(bmp1.Width, bmp1.Height, Image1.Width, Image1.Height, _width, _height);
      x := (Image1.Width - _width) div 2;
      y := (Image1.Height - _height) div 2;
      stretched := bmp1.Resample(_width, _Height, rmSimpleStretch) as TBGRABitmap;
      stretched.Draw(Image1.Canvas, x, y, True);                           //affiche la BGRABitmap sur la fenêtre
      Image1.Canvas.AutoRedraw:=true;
      // ***
      // * Modified v0.6
      Image1.Canvas.Pen.Color:=WindowsColors.WindowsColor;
      Image1.Canvas.Brush.Color:=WindowsColors.WindowsColor;
      // *
      // * End modified v0.6
      // ***
      if x > 0 then begin
        Image1.Canvas.FillRect(0,0,x, Image1.Height);
        Image1.Canvas.FillRect(Image1.Width - x, 0, Image1.Width, Image1.Height);
      end else if y > 0 then begin
        Image1.Canvas.FillRect(0,0,image1.Width, y);
        Image1.Canvas.FillRect(0,Image1.height-y, Image1.Width, Image1.Height);
      end;
      SelectedFile :=  utf8toutf16(_dir + _file);
      Label9.Caption := _file;
      Label10.Caption := inttostr(bmp1.Width) + 'x' + inttostr(bmp1.height);

      // ***
      // * Add v0.8
      try
        _exifsList := getExif (SelectedFile);
        // Clearing exif data
        Label12.Caption := '';
        Label13.Caption := '';
        Label14.Caption := '';
        Label15.Caption := '';
        Label16.Caption := '';
        Label17.Caption := '';
        Label18.Caption := '';
        Label19.Caption := '';
        Label20.Caption := '';
        Label21.Caption := '';
        Label22.Caption := '';
        // FullFill exifs values
        for i := Low(_exifsList) to High(_exifsList) do begin
          // 1 APN - Label12
          if _exifsList[i].Exif = IU_K_Exif_APN then
             Label12.Caption := IU_HI_Messages[IU_CurrentLang,K_IU_HIMSG_APN] + _exifsList[i].value
          else
          // 2 Orientation - Label13
          if _exifsList[i].Exif = IU_K_Exif_Orientation then begin
            _value := _exifsList[i].value;
            if _value = ' top, left' then begin
              Label13.Caption := IU_HI_Messages[IU_CurrentLang,K_IU_HIMSG_PhotoOrientation] + ' ' + IU_HI_Messages[IU_CurrentLang,K_IU_HIMSG_TopLeft] ;
            end else begin
              if _value = ' right, top' then begin
                Label13.Caption := IU_HI_Messages[IU_CurrentLang,K_IU_HIMSG_PhotoOrientation] + ' ' +   IU_HI_Messages[IU_CurrentLang,K_IU_HIMSG_RightTop];
              end else begin
                if _value = ' bottom, right' then begin
                  Label13.Caption := IU_HI_Messages[IU_CurrentLang,K_IU_HIMSG_PhotoOrientation] + ' ' +  IU_HI_Messages[IU_CurrentLang,K_IU_HIMSG_BottomRight];
                end else begin
                  if _value = ' left, bottom' then begin
                    Label13.Caption := IU_HI_Messages[IU_CurrentLang,K_IU_HIMSG_PhotoOrientation] +  ' ' + IU_HI_Messages[IU_CurrentLang,K_IU_HIMSG_LeftBottom];
                  end else begin
                    Label13.Caption := IU_HI_Messages[IU_CurrentLang,K_IU_HIMSG_PhotoOrientation] + ' ' + IU_HI_Messages[IU_CurrentLang,K_IU_HIMSG_PhotoUnknowOrientation];
                  end;
                end;
              end;
            end;
          end else
          // 3 Aperture - Label14
          if _exifsList[i].Exif = IU_K_Exif_Aperture then
             Label14.Caption := IU_HI_Messages[IU_CurrentLang,K_IU_HIMSG_PhotoAperture]  + _exifsList[i].value
          else
          // 4 Speed - Label 15
          if _exifsList[i].Exif = IU_K_Exif_Speed then
             Label15.Caption := IU_HI_Messages[IU_CurrentLang,K_IU_HIMSG_PhotoSpeed] + _exifsList[i].value
          else
          // 5 ISO - Label 16
          if _exifsList[i].Exif = IU_K_Exif_ISO then
             Label16.Caption :=  IU_HI_Messages[IU_CurrentLang,K_IU_HIMSG_PhotoISO] + _exifsList[i].value
          else
          // 6 Flash - Label 17
          if _exifsList[i].Exif = IU_K_Exif_Flash then begin
            if _exifsList[i].value = 'No' then Label17.Caption := IU_HI_Messages[IU_CurrentLang,K_IU_HIMSG_PhotoFlash]  + ' ' + IU_HI_Messages[IU_CurrentLang,K_IU_HIMSG_No]
            else Label17.Caption := IU_HI_Messages[IU_CurrentLang,K_IU_HIMSG_PhotoFlash] + ' ' + IU_HI_Messages[IU_CurrentLang,K_IU_HIMSG_Yes];
          end else
          // 7 Focal - Label 18
          if _exifsList[i].Exif = IU_K_Exif_Focal then
            Label18.Caption := IU_HI_Messages[IU_CurrentLang,K_IU_HIMSG_PhotoFocal] + _exifsList[i].value
          else
          // 8 Color Space - Label 19
          if _exifsList[i].Exif = IU_K_Exif_ColorSpace then
            Label19.Caption := IU_HI_Messages[IU_CurrentLang,K_IU_HIMSG_PhotoColorSpace] +  _exifsList[i].value
          else
          // 9 Shoot Date - Label 20
          if _exifsList[i].Exif = IU_K_Exif_ShootDate then
            Label20.Caption := IU_HI_Messages[IU_CurrentLang,K_IU_HIMSG_PhotoShootDate]  +  _exifsList[i].value
          else
          // 10 Artist - Label 21
          if _exifsList[i].Exif = IU_K_Exif_Artist then
            Label21.Caption := IU_HI_Messages[IU_CurrentLang,K_IU_HIMSG_PhotoArtist] +  _exifsList[i].value
          else
          // 11 Copyrights
          if _exifsList[i].Exif = IU_K_Exif_Copyright then
            Label22.Caption := IU_HI_Messages[IU_CurrentLang,K_IU_HIMSG_PhotoCopyrights] +  _exifsList[i].value
        end;
      finally
        Image1.Refresh;
        stretched.free;
        screen.cursor := crDefault;
      end;
      // *
      // * End Add v0.8
      // ***
    end;
    bmp1.Free;
  end;
end;

procedure TImportFile.ListBox2Enter(Sender: TObject);
begin
  // ***
  // * Modified v0.6
  ListBox2.Color:=WindowsColors.ListSelectedColor;
  // ***
end;

procedure TImportFile.ListBox2Exit(Sender: TObject);
begin
  // ***
  // * Modified v0.6
  ListBox2.Color:=WindowsColors.ListColor;
  // ***
end;

procedure TImportFile.ListBox2KeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  // ***
  // * Modified V0.6
  ListBox2.Color:=WindowsColors.ListSelectedColor;
  // ***
end;

procedure TImportFile.Menu10Click(Sender: TObject);
begin
  // ***
  // * Add v0.6
  ImportFile.PopupMenu1.Close;
  if ColorDialog1.Execute then begin
    WindowsColors.ListSelectedColor := ColorDialog1.Color;
    setColors;
  end;
  // ***
end;

procedure TImportFile.Menu11Click(Sender: TObject);
begin
  // ***
  // * Add v0.6
  initDefaultColors;
  setColors;
  // ***
end;

procedure TImportFile.Menu01Click(Sender: TObject);
begin
  // ***
  // * Add v0.6
  ImportFile.PopupMenu1.Close;
  if ColorDialog1.Execute then begin
    WindowsColors.WindowsColor := ColorDialog1.Color;
    setColors;
  end;
  // ***
end;

procedure TImportFile.Menu2Click(Sender: TObject);
begin
  // ***
  // * Add v0.6
  ImportFile.PopupMenu1.Close;
  if ColorDialog1.Execute then begin
    WindowsColors.TextColor := ColorDialog1.Color;
    setColors;
  end;
  // ***
end;

procedure TImportFile.Menu3Click(Sender: TObject);
begin
  // ***
  // * Add v0.6
  ImportFile.PopupMenu1.Close;
  if ColorDialog1.Execute then begin
    WindowsColors.ListColor := ColorDialog1.Color;
    setColors;
  end;
  // ***
end;

procedure TImportFile.Menu4Click(Sender: TObject);
begin
  // ***
  // * Add v0.6
  ImportFile.PopupMenu1.Close;
  if ColorDialog1.Execute then begin
    WindowsColors.DropDownColor := ColorDialog1.Color;
    setColors;
  end;
  // ***
end;

procedure TImportFile.Menu5Click(Sender: TObject);
begin
  // ***
  // * Add v0.6
  ImportFile.PopupMenu1.Close;
  if ColorDialog1.Execute then begin
    WindowsColors.ButtonColor := ColorDialog1.Color;
    setColors;
  end;
  // ***
end;

procedure TImportFile.Menu6Click(Sender: TObject);
begin
  // ***
  // * Add v0.6
  ImportFile.PopupMenu1.Close;
  if ColorDialog1.Execute then begin
    WindowsColors.ListTextColor := ColorDialog1.Color;
    setColors;
  end;
  // ***
end;

procedure TImportFile.Menu07Click(Sender: TObject);
begin
  // ***
  // * Add v0.6
  ImportFile.PopupMenu1.Close;
  if ColorDialog1.Execute then begin
    WindowsColors.DropDownTextColor := ColorDialog1.Color;
    setColors;
  end;
  // ***
end;

procedure TImportFile.Menu8Click(Sender: TObject);
begin
  // ***
  // * Add v0.6
  ImportFile.PopupMenu1.Close;
  if ColorDialog1.Execute then begin
    WindowsColors.ButtonSelectedColor := ColorDialog1.Color;
    setColors;
  end;
  // ***
end;

procedure TImportFile.Menu9Click(Sender: TObject);
begin
  // ***
  // * Add v0.6
  ImportFile.PopupMenu1.Close;
  if ColorDialog1.Execute then begin
    WindowsColors.DropDownSelectedColor := ColorDialog1.Color;
    setColors;
  end;
  // ***
end;

procedure TImportFile.Splitter1Moved(Sender: TObject);
begin
  if Splitter1.Left < 250 then Splitter1.Left := 250;
  // ***
  // * Modified v0.8
  // * Changing limits os splitter2 to 320 after splitter1
  if Splitter2.Left - Splitter1.Left - Splitter1.Width < 320 then  Splitter2.Left := Splitter1.Left + Splitter1.Width + 320;
  // ***
  // ***
  // * Add v0.8
  // * Panel1 added in v0.8
  Panel1.Width := Splitter2.Left - 1 ;
  // ***
  // ***
  // * Modified v0.8
  // * Text positionned after the reverse list button
  Label7.Width := Splitter2.Left - Label7.Left - 2 ;
  // ***
  ListBox1.Width := Splitter1.Left - 2;
  ListBox2.Left := Splitter1.Left + Splitter1.Width+2;
  ListBox2.Width := Splitter2.left - 4 - (Splitter1.Left + Splitter1.Width);
  Button2.Left := Splitter1.Left + Splitter1.Width + 1 ;
  Label6.Left := Splitter1.Left + Splitter1.Width + 3 ;
  Label8.Left := Button2.Left + Button2.Width + 5 ;
  ComboBox1.Left := Button2.Left + Button2.Width + 131 ;
  Button5.Left := Splitter2.Left + Splitter2.Width + 5;
  Button6.Left := Splitter2.Left + Splitter2.Width + 5;
  Image1.Left := Splitter2.Left+Splitter2.Width + 5;
  Shape1.Left := Splitter2.Left+Splitter2.Width + 4;
  Label9.Left := Splitter2.Left+Splitter2.Width + 5;
  Label10.Left := Splitter2.Left+Splitter2.Width + 5;
  Label12.Left := Splitter2.Left+Splitter2.Width + 5;
  Label13.Left := Splitter2.Left+Splitter2.Width + 5;
  Label14.Left := Splitter2.Left+Splitter2.Width + 5;
  Label15.Left := Splitter2.Left+Splitter2.Width + 5;
  Label16.Left := Splitter2.Left+Splitter2.Width + 5;
  Label17.Left := Splitter2.Left+Splitter2.Width + 5;
  Label18.Left := Splitter2.Left+Splitter2.Width + 5;
  Label19.Left := Splitter2.Left+Splitter2.Width + 5;
  Label20.Left := Splitter2.Left+Splitter2.Width + 5;
  Label21.Left := Splitter2.Left+Splitter2.Width + 5;
  Label22.Left := Splitter2.Left+Splitter2.Width + 5;
end;

procedure TImportFile.Splitter2Moved(Sender: TObject);
begin
  Splitter1Moved(Sender);
end;

// *
// * End Add V1.5 IU_GeneralUtils
// ***

end.

