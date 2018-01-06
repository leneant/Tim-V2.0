unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Windows,Variants,ActiveX,JwaWbemCli,IU_GeneralUtils;



type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    ListBox1: TListBox;
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


const
RPC_C_AUTHN_LEVEL_DEFAULT = 0;
RPC_C_IMP_LEVEL_IMPERSONATE = 3;
RPC_C_AUTHN_WINNT = 10;
RPC_C_AUTHZ_NONE = 0;
RPC_C_AUTHN_LEVEL_CALL = 3;
EOAC_NONE = 0;

procedure wmi_LOGICALDISK;
  const
  strLocale    = '';
  strUser      = '';
  strPassword  = '';
  strNetworkResource = 'root\cimv2';
  strAuthority       = '';
  WQL                = 'SELECT DeviceID,FileSystem FROM Win32_logicaldisk where (DriveType=''3'')';
  var
  FWbemLocator         : IWbemLocator;
  FWbemServices        : IWbemServices;
  FUnsecuredApartment  : IUnsecuredApartment;
  ppEnum               : IEnumWbemClassObject;
  apObjects            : IWbemClassObject;
  puReturned           : ULONG;
  pVal1, pVal2         : OleVariant;
  pType                : Integer;
  plFlavor             : Integer;
  Succeed              : HRESULT;

  var
  _currentDir : string;
  _dirs : string;
  _dsize, _free : int64;
  _fsize, _ffree : string;

  begin
  // Set general COM security levels --------------------------
  // Note: If you are using Windows 2000, you need to specify -
  // the default authentication credentials for a user by using
  // a SOLE_AUTHENTICATION_LIST structure in the pAuthList ----
  // parameter of CoInitializeSecurity ------------------------
  if Failed(CoInitializeSecurity(nil, -1, nil, nil, RPC_C_AUTHN_LEVEL_DEFAULT, RPC_C_IMP_LEVEL_IMPERSONATE, nil, EOAC_NONE, nil)) then Exit;
  // Obtain the initial locator to WMI -------------------------
  if Succeeded(CoCreateInstance(CLSID_WbemLocator, nil, CLSCTX_INPROC_SERVER, IID_IWbemLocator, FWbemLocator)) then
  try
    // Connect to WMI through the IWbemLocator::ConnectServer method
    if Succeeded(FWbemLocator.ConnectServer(strNetworkResource, strUser, strPassword, strLocale,  WBEM_FLAG_CONNECT_USE_MAX_WAIT, strAuthority, nil, FWbemServices)) then
    try
      // Set security levels on the proxy -------------------------
      if Failed(CoSetProxyBlanket(FWbemServices, RPC_C_AUTHN_WINNT, RPC_C_AUTHZ_NONE, nil, RPC_C_AUTHN_LEVEL_CALL, RPC_C_IMP_LEVEL_IMPERSONATE, nil, EOAC_NONE)) then Exit;
      if Succeeded(CoCreateInstance(CLSID_UnsecuredApartment, nil, CLSCTX_LOCAL_SERVER, IID_IUnsecuredApartment, FUnsecuredApartment)) then
      try
        // Use the IWbemServices pointer to make requests of WMI
        //Succeed := FWbemServices.ExecQuery('WQL', WQL, WBEM_FLAG_FORWARD_ONLY OR WBEM_FLAG_RETURN_IMMEDIATELY, nil, ppEnum);
        Succeed := FWbemServices.ExecQuery('WQL', WQL, WBEM_FLAG_FORWARD_ONLY, nil, ppEnum);
        if Succeeded(Succeed) then
        begin
           _currentdir := getCurrentDir;
           // Get the data from the query
           while (ppEnum.Next(WBEM_INFINITE, 1, apObjects, puReturned)=0) do
           begin
             apObjects.Get('DeviceID', 0, pVal1, pType, plFlavor);
             apObjects.Get('FileSystem', 0, pVal2, pType, plFlavor);
             chdir(pVal1);
             _dsize := disksize(0);
             _free := diskfree(0);
             _fsize := IU_realToString(_dsize/1024/1024/1024,2) + ' Go';
             _ffree := IU_realToString(_free/1024/1024/1024,2) + ' Go';
             form1.ListBox1.Items.Add(pVal1 + ' -> ' + pVal2 + ' ' + _fsize + ', ' + _ffree);
             VarClear(pVal1);
             VarClear(pVal2);
           end;
           chdir(_currentdir);
        end
        else
        form1.ListBox1.Items.Add(Format('Error executing WQL sentence %x',[Succeed]));
      finally
        FUnsecuredApartment := nil;
      end;
    finally
      FWbemServices := nil;
    end;
  finally
    FWbemLocator := nil;
  end;
end;

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
begin
// Initialize COM. ------------------------------------------
//if Succeeded(CoInitializeEx(nil, COINIT_MULTITHREADED)) then
try
  wmi_LOGICALDISK;;
finally
//  CoUninitialize();
end;

end;

end.

