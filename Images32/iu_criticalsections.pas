unit IU_CriticalSections;

{$mode objfpc}{$H+}
// ***
// * Unit provides criticals sections declarations and initialisation for multi threading
// * Creation Date : 2017 September
// *
// * Version : 0.1
// * Version Date : 2017 November
// * Version Contributors : Pascal Lemaître
// *
// * @authors : Pascal Lemaître
// *
// * @see :
// *
// *
// * Team : TIm (Traitement d'Images)
// *
// * 2017
// ***

interface

uses
  Classes, SysUtils;

var
  IU_RegisteringTempFileCriticalSection : TRTLCriticalSection; // Declare object
  // InitCriticalSection(IU_RegisteringTempFileCriticalSection); must be call when program is strating
  // DoneCriticalSection((IU_RegisteringTempFileCriticalSection); must be call when program is ending

  IU_MemoryFrameDeclarationCriticalSection : TRTLCriticalSection; // Declare object
  // InitCriticalSection(IU_MemoryFrameDeclarationCriticalSection); must be call when program is strating
  // DoneCriticalSection(IU_MemoryFrameDeclarationCriticalSection); must be call when program is ending

  // Critical section for threading management
  IU_Thread_CriticalSection0 : TRTLCriticalSection; // Declare object
  IU_Thread_CriticalSection1 : TRTLCriticalSection; // Declare object
  IU_Thread_CriticalSection2 : TRTLCriticalSection; // Declare object
  IU_Thread_CriticalSection3 : TRTLCriticalSection; // Declare object
  IU_Thread_CriticalSectionThreadID_read : TRTLCriticalSection; // Declare object


  procedure IU_Init_CriticalSections ;
  procedure IU_Release_CriticalSections ;

implementation

procedure IU_Init_CriticalSections ;
begin
  InitCriticalSection(IU_RegisteringTempFileCriticalSection);
  InitCriticalSection(IU_MemoryFrameDeclarationCriticalSection);

  InitCriticalSection(IU_Thread_CriticalSection0);
  InitCriticalSection(IU_Thread_CriticalSection1);
  InitCriticalSection(IU_Thread_CriticalSection2);
  InitCriticalSection(IU_Thread_CriticalSection3);
  InitCriticalSection(IU_Thread_CriticalSectionThreadID_read);

end;

procedure IU_Release_CriticalSections ;
begin
  DoneCriticalSection(IU_RegisteringTempFileCriticalSection);
  DoneCriticalSection(IU_MemoryFrameDeclarationCriticalSection);

  DoneCriticalSection(IU_Thread_CriticalSection0);
  DoneCriticalSection(IU_Thread_CriticalSection1);
  DoneCriticalSection(IU_Thread_CriticalSection2);
  DoneCriticalSection(IU_Thread_CriticalSection3);
  DoneCriticalSection(IU_Thread_CriticalSectionThreadID_read);

end;

end.

