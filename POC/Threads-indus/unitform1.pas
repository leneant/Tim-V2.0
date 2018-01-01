unit unitForm1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  global,
  IU_CriticalSections, IU_Threads_Utils;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button10: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    Button7: TButton;
    Button8: TButton;
    Button9: TButton;
    Label1: TLabel;
    Label2: TLabel;
    LT2msg: TLabel;
    LT1msg: TLabel;
    LT3msg: TLabel;
    LT4msg: TLabel;
    procedure Button10Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
    procedure Button8Click(Sender: TObject);
    procedure Button9Click(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Label2Click(Sender: TObject);
  private
    { private declarations }
    pause0, pause1, pause2, pause3 : boolean;
  public
    { public declarations }
  end;

procedure ThreadExecute(PThreadCtrl : pointer) ;

var
  Form1: TForm1;
  boucle : boolean;


  Threadcontrol : array [0..3] of T_IU_ThreadControl;


  thread1ID, thread2ID, thread3ID, thread4ID : TThreadID;



implementation

{$R *.lfm}


{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
var i : integer;
begin
  // Initialization of thread context
  IU_Init_Threads_Utils;

  // Init threads control descriptors
  Threadcontrol[0].running := false;
  Threadcontrol[1].running := false;
  Threadcontrol[2].running := false;
  Threadcontrol[3].running := false;
  Threadcontrol[0].pause := false;
  Threadcontrol[1].pause := false;
  Threadcontrol[2].pause := false;
  Threadcontrol[3].pause := false;
  Threadcontrol[0].stopthread := false;
  Threadcontrol[1].stopthread := false;
  Threadcontrol[2].stopthread := false;
  Threadcontrol[3].stopthread := false;
{
  InitCriticalSection(IU_Thread_CriticalSection);
  InitCriticalSection(IU_Thread_CriticalSection0); // Create critical section
  InitCriticalSection(IU_Thread_CriticalSection1); // Create critical section
  InitCriticalSection(IU_Thread_CriticalSection2); // Create critical section
  InitCriticalSection(IU_Thread_CriticalSection3); // Create critical section

  InitCriticalSection(IU_Thread_CriticalSectionThreadID_read); // Create critical section

  pause0 := false;
  pause1 := false;
  pause2 := false;
  pause3 := false;
}
end;

procedure TForm1.Button1Click(Sender: TObject);
var msg : string;
  var go : boolean;
begin
  if not ((Threadcontrol[0].running and Threadcontrol[1].running) and
  (Threadcontrol[2].running and Threadcontrol[3].running)) then begin
    Threadcontrol[0].StopThread := false;
    Threadcontrol[1].StopThread := false;
    Threadcontrol[2].StopThread := false;
    Threadcontrol[3].StopThread := false;
    Threadcontrol[0].running := true;
    Threadcontrol[1].running := true;
    Threadcontrol[2].running := true;
    Threadcontrol[3].running := true;
    boucle := ((Threadcontrol[0].running and Threadcontrol[1].running) and
              (Threadcontrol[2].running and Threadcontrol[3].running));

    // Which procedure should be executed in thread ?
    ThreadExec := TThreadFunc(@ThreadExecute);

    // Creating Thread 1
    thread1ID := IU_CreateThread(Threadcontrol[0]);
    button3.enabled := true;

    // Creating Thread 2
    thread2ID := IU_CreateThread(Threadcontrol[1]);
    button4.enabled := true;

    // Creating Thread 3
    thread3ID := IU_CreateThread(Threadcontrol[2]);
    button5.enabled := true;

    // Creating Thead 5
    thread4ID := IU_CreateThread(Threadcontrol[3]);
    button6.enabled := true;
    Label1.caption := 'Threads démarrées...';
    pause0 := false;
    pause1 := false;
    pause2 := false;
    pause3 := false;
    while boucle do
      begin
        // ---> Getting Threads messages
        try
           IU_PrepareAccessingSharedData(ThreadControl[0]);
        finally
          boucle := Threadcontrol[0].running ;
          msg := Threadcontrol[0].ThreadMSG;
          Lt1msg.caption := msg;
          IU_EndAccessingSharedData(Threadcontrol[0]);
        end;
        try
          IU_PrepareAccessingSharedData(ThreadControl[1]);
        finally
          boucle := Threadcontrol[1].running ;
          msg := Threadcontrol[1].ThreadMSG;
          Lt2msg.caption := msg;
          IU_EndAccessingSharedData(Threadcontrol[1]);
        end;
        try
           IU_PrepareAccessingSharedData(ThreadControl[2]);
        finally
          boucle := Threadcontrol[2].running ;
          msg := Threadcontrol[2].ThreadMSG;
          Lt3msg.caption := msg;
          IU_EndAccessingSharedData(Threadcontrol[2]);
        end;
        try
          IU_PrepareAccessingSharedData(ThreadControl[3]);
        finally
          boucle := Threadcontrol[3].running ;
          msg := Threadcontrol[3].ThreadMSG;
          Lt4msg.caption := msg;
          IU_EndAccessingSharedData(Threadcontrol[3]);
        end;
        // <---- Getting Threads Messages
        Application.ProcessMessages;
        sleep(100);
      end;
    Label1.caption := 'Threads terminées...';
    // Destroying all Threads
    IU_DestroyThread(Threadcontrol[0]);
    IU_DestroyThread(Threadcontrol[1]);
    IU_DestroyThread(Threadcontrol[2]);
    IU_DestroyThread(Threadcontrol[3]);
  end;
end;

procedure TForm1.Button10Click(Sender: TObject);
begin
  // Resume Thread 4
  IU_ResumeThread(Threadcontrol[3]);
  button6.enabled := true;
  button10.enabled := false;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
{
  try
     // to stop a thread we need to have an executing thread (not suspended)
     IU_PrepareAccessingStateAndCommand(Threadcontrol[0]);
     if Threadcontrol[0].pause then
     // Waking up the thread
     IU_ResumeThread(ThreadControl[0]);
  finally
    // Send command through the thread control descriptor
     Threadcontrol[0].stopThread := true;
     // end command was sent
     IU_EndAccessingStateAndCommand(ThreadControl[0]);
  end;
  try
     // to stop a thread we need to have an executing thread (not suspended)
     IU_PrepareAccessingStateAndCommand(Threadcontrol[1]);
     if Threadcontrol[1].pause then
     // Waking up the thread
     IU_ResumeThread(ThreadControl[1]);
  finally
    // Send command through the thread control descriptor
     Threadcontrol[1].stopThread := true;
     // end command was sent
     IU_EndAccessingStateAndCommand(ThreadControl[1]);
  end;
  try
     // to stop a thread we need to have an executing thread (not suspended)
     IU_PrepareAccessingStateAndCommand(Threadcontrol[2]);
     if Threadcontrol[2].pause then
     // Waking up the thread
     IU_ResumeThread(ThreadControl[2]);
  finally
    // Send command through the thread control descriptor
     Threadcontrol[2].stopThread := true;
     // end command was sent
     IU_EndAccessingStateAndCommand(ThreadControl[2]);
  end;
  try
     // to stop a thread we need to have an executing thread (not suspended)
     IU_PrepareAccessingStateAndCommand(Threadcontrol[3]);
     if Threadcontrol[3].pause then
     // Waking up the thread
     IU_ResumeThread(ThreadControl[3]);
  finally
    // Send command through the thread control descriptor
     Threadcontrol[3].stopThread := true;
     // end command was sent
     IU_EndAccessingStateAndCommand(ThreadControl[3]);
  end;
}
   IU_StopThread(ThreadControl[0]);
   IU_StopThread(ThreadControl[1]);
   IU_StopThread(ThreadControl[2]);
   IU_StopThread(ThreadControl[3]);

   Label1.caption := 'Threads Stoppées...';
   button3.enabled := false;
   button4.enabled := false;
   button5.enabled := false;
   button6.enabled := false;
   button7.enabled := false;
   button8.enabled := false;
   button9.enabled := false;
   button10.enabled := false;

end;

procedure TForm1.Button3Click(Sender: TObject);
var Thread:TThreadID;
begin
  // Suspend thread 1
  IU_SuspendThread(Threadcontrol[0]);
  button3.enabled := false;
  button7.enabled := true;
end;

procedure TForm1.Button4Click(Sender: TObject);
begin
  // Suspend thread 2
  IU_SuspendThread(Threadcontrol[1]);
  button4.enabled := false;
  button8.enabled := true;
end;

procedure TForm1.Button5Click(Sender: TObject);
begin
  // Suspend thread 3
  IU_SuspendThread(Threadcontrol[2]);
  button5.enabled := false;
  button9.enabled := true;
end;

procedure TForm1.Button6Click(Sender: TObject);
begin
  // Suspend thread 4
  IU_SuspendThread(Threadcontrol[3]);
  button6.enabled := false;
  button10.enabled := true;
end;

procedure TForm1.Button7Click(Sender: TObject);
begin
  // Resume Thread 1
  IU_ResumeThread(Threadcontrol[0]);
  button3.enabled := true;
  button7.enabled := false;
end;

procedure TForm1.Button8Click(Sender: TObject);
begin
  // Resume Thread 2
  IU_ResumeThread(Threadcontrol[1]);
  button4.enabled := true;
  button8.enabled := false;
end;

procedure TForm1.Button9Click(Sender: TObject);
begin
  // Resume Thread 3
  IU_ResumeThread(Threadcontrol[2]);
  button5.enabled := true;
  button9.enabled := false;
end;

procedure TForm1.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  try
    IU_StopThread(ThreadControl[0]);
    IU_StopThread(ThreadControl[1]);
    IU_StopThread(ThreadControl[2]);
    IU_StopThread(ThreadControl[3]);
  finally
  end;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  // Destroying threads unit contex
  IU_Close_Threads_Utils;
end;

procedure TForm1.Label2Click(Sender: TObject);
begin

end;

procedure ThreadExecute(PThreadCtrl : pointer) ;
var i : qword ;
  threadid : TThreadID;
  stopme : boolean; // pour stopper la thread
  _threadcontrol : PT_IU_ThreadControl;
begin
  // getting thread ID
  // With windows in thread GetThreadID function doesn't work
  // Waiting for caller finished to init value (do nothing just wait)
  // Getting thread control descriptor
  _threadcontrol := PT_IU_ThreadControl(PThreadCtrl);
  IU_ThreadInit(_threadcontrol^);
  for i := 0 to qword($FFFFFFFFFFFFFFFF) do
  begin
    // Pause the thread ?
    try
      IU_SuspendingThread(_threadcontrol^);
    finally
    end;
    // Read if command was sent to the thread
    try
      IU_PrepareAccessingStateAndCommand(_threadcontrol^);
      stopme := _threadcontrol^.stopthread;
    finally
      IU_EndAccessingStateAndCommand(_threadcontrol^);
    end;
    // Sending count message
    try
      IU_PrepareAccessingSharedData(_threadcontrol^);
      _threadcontrol^.ThreadMSG := inttostr (i);
    finally
      IU_EndAccessingSharedData(_threadcontrol^);
    end;
    if stopme then Break;
  end;
  // Sending message to the main thread
  try
    IU_PrepareAccessingSharedData(_threadcontrol^);
    _threadcontrol^.ThreadMSG := 'Thread stop...';
    _threadcontrol^.running := false;
  finally
    IU_EndAccessingSharedData(_threadcontrol^);
  end;
  // end of thread exec
  IU_ThreadEnding(_threadcontrol^);
end;


end.

