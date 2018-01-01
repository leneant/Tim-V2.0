unit U_MyThread_Test;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, global, sectionsCritiques, forms;


function ThreadExecute : integer;

implementation

function ThreadExecute : integer;
var i : qword ;
  threadid : TThreadID;
  stopme : boolean; // pour stopper la thread
  _criticalsection : ^TRTLCriticalSection;
  _threadcontrol : ^T_ThreadControl;
begin
  // getting thread ID
  // With windows in thread GetThreadID function doesn't work
  // Then we need to set the right ThreadID ourself
  // Waiting for caller finished to init value (do nothing just wait)
  EnterCriticalSection(MyCriticalSection);
  // just wait
  LeaveCriticalSection(MyCriticalSection);

  // Reading thread ID and ack when it read
  EnterCriticalSection(MyCriticalSectionThreadID_read);
  threadid := CreatedThreadID;
  threadid_read := true;
  LeaveCriticalSection(MyCriticalSectionThreadID_read);

//  threadid := GetThreadID ; // doesn't work under windows 10
  try
    EnterCriticalSection(MyCriticalSection);
    if (Threadcontrol[0].thread = threadid) then begin
      _threadcontrol := @Threadcontrol[0];
      _criticalsection := @MyCriticalSection0;
    end else if (Threadcontrol[1].thread = threadid) then begin
      _threadcontrol := @Threadcontrol[1];
      _criticalsection := @MyCriticalSection1;
    end else if (Threadcontrol[2].thread = threadid) then begin
       _threadcontrol := @Threadcontrol[2];
       _criticalsection := @MyCriticalSection2;
    end else begin
       _threadcontrol := @Threadcontrol[3];
       _criticalsection := @MyCriticalSection3;
    end;
    with _threadcontrol^ do begin
      ThreadMSG := 'Thread starting...';
    end ;
  finally
    LeaveCriticalSection(MyCriticalSection);
  end;
  for i := 0 to qword($FFFFFFFFFFFFFFFF) do
  begin
    try
      EnterCriticalSection(_criticalsection^);
      with _threadcontrol^ do
        begin
          stopme := stopthread;
          ThreadMSG := inttostr (i);
        end;
    finally
      LeaveCriticalSection(_criticalsection^);
    end;
    if stopme then Break;
  end;
  try
    EnterCriticalSection(_criticalsection^);
    with _threadcontrol^ do
      begin
        ThreadMSG := 'Thread stop...';
        running := false;
      end;
  finally
    LeaveCriticalSection(_criticalsection^);
  end;
  ThreadExecute := 0;
  EndThread(0);
end;

end.

