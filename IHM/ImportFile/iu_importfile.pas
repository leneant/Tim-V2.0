unit IU_importfile;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  StdCtrls, ExtDlgs, ShellCtrls, IU_GeneralUtils, BGRABitmap, BGRABitmapTypes, IU_strechutils, LCLProc
  // ***
  // * Add v0.3 IU_I18N_Messages
  , IU_I18N_Messages, LCLType, ExtCtrls, PairSplitter
  // ***
  ;

type

  { TImportFile }

  TImportFile = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    ComboBox1: TComboBox;
    Image1: TImage;
    Label1: TLabel;
    Label10: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    ListBox1: TListBox;
    ListBox2: TListBox;
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
    procedure ComboBox1Change(Sender: TObject);
    procedure ComboBox1Enter(Sender: TObject);
    procedure ComboBox1Exit(Sender: TObject);
    procedure ComboBox1KeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormChangeBounds(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ListBox1Click(Sender: TObject);
    procedure ListBox1DblClick(Sender: TObject);
    procedure ListBox1Enter(Sender: TObject);
    procedure ListBox1Exit(Sender: TObject);
    procedure ListBox1KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState
      );
    procedure ListBox2Click(Sender: TObject);
    procedure ListBox2Enter(Sender: TObject);
    procedure ListBox2Exit(Sender: TObject);
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
// * Add v0.4 IU_I18N_Messages
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
  if length(_directory) * 10 > ListBox1.ScrollWidth then ListBox1.ScrollWidth := length(_directory) * 10;
  for i := 1 to dirsList.getItemsNumbers - 1 do begin
    ListBox1.Items.Add(dirsList.getText);
    dirsList.next;
  end;
  ListBox1.Items.Add(dirsList.getText);
  if length(dirsList.getText) * 10 > ListBox1.ScrollWidth then ListBox1.ScrollWidth := length(dirsList.getText) * 10;
end;

procedure TImportFile.Button1Enter(Sender: TObject);
begin
  button1.Color:=rgbtocolor($50,$50,$50);
end;

procedure TImportFile.Button1Exit(Sender: TObject);
begin
  button1.Color:=rgbtocolor($2C,$2C,$2C);
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
end;

procedure TImportFile.Button2Enter(Sender: TObject);
begin
  button2.Color:=rgbtocolor($50,$50,$50);
end;

procedure TImportFile.Button2Exit(Sender: TObject);
begin
  button2.color:=rgbtocolor($2c,$2c,$2c);
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
  button5.color:=rgbtocolor($50,$50,$50);
end;

procedure TImportFile.Button5Exit(Sender: TObject);
begin
  button5.color:=rgbtocolor($2c,$2c,$2c);
end;

procedure TImportFile.Button6Enter(Sender: TObject);
begin
  button6.color:=rgbtocolor($50,$50,$50);
end;

procedure TImportFile.Button6Exit(Sender: TObject);
begin
  button6.color:=rgbtocolor($2c,$2c,$2c);
end;

procedure TImportFile.ComboBox1Change(Sender: TObject);
begin
  ListBox1DblClick(Sender);
end;

procedure TImportFile.ComboBox1Enter(Sender: TObject);
begin
  ComboBox1.color:=rgbtocolor($50,$50,$50);
end;

procedure TImportFile.ComboBox1Exit(Sender: TObject);
begin
  ComboBox1.Color := rgbtocolor($2c,$2c,$2c);
end;

procedure TImportFile.ComboBox1KeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
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
  Label7.Caption := _directory + ' :';
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
  // Dir list
  for R := 1 to dirsList.getItemsNumbers do begin
    // No current item selected then must start with next for selecting the first
    dirsList.next;
    ListBox1.Items.Add(dirsList.getText);
    if length(dirsList.getText) * 10 > ListBox1.ScrollWidth then ListBox1.ScrollWidth := length(dirsList.getText) * 10;
  end;
end;
// *
// * End Add V1.5 IU_GeneralUtils
// ***

procedure TImportFile.FormDestroy(Sender: TObject);
begin
  dirsList.Destroy;
  filesList.Destroy;
end;

procedure TImportFile.ListBox1Click(Sender: TObject);
begin

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
  Label7.Caption := _directory + ' :';
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
   // Dir list
   for R := 1 to dirsList.getItemsNumbers do begin
     // No current item selected then must start with next for selecting the first
     dirsList.next;
     ListBox1.Items.Add(dirsList.getText);
     if length(dirsList.getText) * 10 > ListBox1.ScrollWidth then ListBox1.ScrollWidth := length(dirsList.getText) * 10;
   end;

end;

procedure TImportFile.ListBox1Enter(Sender: TObject);
begin
  ListBox1.Color:=rgbtocolor($30,$30,$30);
end;

procedure TImportFile.ListBox1Exit(Sender: TObject);
begin
    ListBox1.Color:=rgbtocolor($2b,$29,$29);
end;

procedure TImportFile.ListBox1KeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = Word(13) then
    ListBox1DblClick(Sender);
end;

procedure TImportFile.ListBox2Click(Sender: TObject);
var stretched : TBGRABitmap;
  _width, _height, x, y : integer;
  _file, _dir : string;
  _error : boolean;
  R : integer;
begin
  ListBox2.Color:=rgbtocolor($30,$30,$30);
  ListBox2.Repaint;
  ListBox2.Refresh;
  if ListBox2.Count > 0 then begin
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
        Image1.Canvas.Pen.Color:=rgbtocolor($2c,$2c,$2c);
        Image1.Canvas.Brush.Color:=rgbtocolor($2c,$2c,$2c);
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
      Image1.Canvas.Pen.Color:=rgbtocolor($2c,$2c,$2c);
      Image1.Canvas.Brush.Color:=rgbtocolor($2c,$2c,$2c);
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
      Image1.Refresh;
      stretched.free;
      screen.cursor := crDefault;
    end;
    bmp1.Free;
  end;
end;

procedure TImportFile.ListBox2Enter(Sender: TObject);
begin
    ListBox2.Color:=rgbtocolor($30,$30,$30);
end;

procedure TImportFile.ListBox2Exit(Sender: TObject);
begin
    ListBox2.Color:=rgbtocolor($2b,$29,$29);
end;

procedure TImportFile.Splitter1Moved(Sender: TObject);
begin
  if Splitter1.Left < 250 then Splitter1.Left := 250;
  if Splitter2.Left - Splitter1.Left - Splitter1.Width < 300 then  Splitter2.Left := Splitter1.Left + Splitter1.Width + 300;
  ListBox1.Width := Splitter1.Left - 2;
  ListBox2.Left := Splitter1.Left + Splitter1.Width;
  ListBox2.Width := Splitter2.left - 2 - (Splitter1.Left + Splitter1.Width);
  Button2.Left := Splitter1.Left + Splitter1.Width + 1 ;
  Label6.Left := Splitter1.Left + Splitter1.Width + 3 ;
  Label8.Left := Splitter1.Left + Splitter1.Width + 3 ;
  ComboBox1.Left := Splitter1.Left + Splitter1.Width + 126 ;
  Button5.Left := Splitter2.Left + Splitter2.Width + 5;
  Button6.Left := Splitter2.Left + Splitter2.Width + 5;
  Image1.Left := Splitter2.Left+Splitter2.Width + 5;
  Label9.Left := Splitter2.Left+Splitter2.Width + 5;
  Label10.Left := Splitter2.Left+Splitter2.Width + 5;
end;

procedure TImportFile.Splitter2Moved(Sender: TObject);
begin
  Splitter1Moved(Sender);
end;

// *
// * End Add V1.5 IU_GeneralUtils
// ***

end.

