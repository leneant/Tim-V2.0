program TestImportFile;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, IU_importfile, IU_GeneralUtils, IU_I18N_Messages, IU_Exceptions,
  IU_Types, IU_strechutils, IU_TimControls
  { you can add units after this };

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TImportFile, ImportFile);
  Application.Run;
end.

