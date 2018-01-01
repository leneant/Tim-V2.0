unit IU_Threads_Utils;

{$mode objfpc}{$H+}
// ***
// * Unit provides multi threading utils for creating, controling and destroying threads
// * Creation Date : 2017 November
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
  Classes, SysUtils, global, iu_criticalsections, forms;

type
  T_IU_ThreadControl = record
    thread : TThreadID; // Internal ID of the Thread
    ThreadMSG : string; // Message between thread and the world
    stopthread : boolean; // Stop (kill) thread command
    // states of the thread
    running : boolean; // is thread is running (not paused)
    pause : boolean; // is thread is paused
    _criticalsectionsuspend : ^TRTLCriticalSection; // for suspending thread
    _criticalsection : ^TRTLCriticalSection; // for accessing to data
    _criticalsectionstate : ^TRTLCriticalSection; // for accessing to state of the thread
                                                  // and for send a command
  end;

  PT_IU_ThreadControl = ^T_IU_ThreadControl;

var

  // With windows in thread GetThreadID function doesn't work
  // Then we need to set the right ThreadID ourself
  CreatedThreadID : TThreadID;

  IU_Thread_CriticalSection : TRTLCriticalSection; // for thread init

  // Must be intialized before call thread creator
  ThreadExec : TThreadFunc;

  // ***
  // * Procedure IU_Init_Threads_Utils Initializing all internals var for threads management
  // *
  // * @authors : Pascal Lemaître
  // *
  // *
  // * This function must be call before using units
  // *
  // ***
  Procedure IU_Init_Threads_Utils;

  // ***
  // * Procedure IU_Close_Threads_Utils release all internals var for threads management
  // *
  // * @authors : Pascal Lemaître
  // *
  // *
  // * This function must be call before program ending
  // *
  // ***
  Procedure IU_Close_Threads_Utils;


  // ***
  // * Function IU_CreateThread creating a new thread and int its control descriptor
  // *
  // * @authors : Pascal Lemaître
  // *
  // *
  // * in/out thread control descriptor structure
  // * in : address of procedur to execute in thread. Procedure must be like procedure-name (threadcontrol : pointer);
  // *                                                with threadcontrol a pointer on Thread control descripteur
  // *
  // * return : thread ID
  // *
  // * This function must be call by the thread who wants to create a new thread
  // *
  // ***
  function IU_CreateThread(var ThreadCtrl : T_IU_ThreadControl) : TThreadID;

  // ***
  // * Procedure IU_InitThread initialize the new created thread
  // *
  // * @authors : Pascal Lemaître
  // *
  // *
  // * in/out thread control descriptor structure
  // *
  // * This procedure must be call first by the procedure executed in the new created thread
  // *
  // ***
  procedure IU_ThreadInit(var ThreadCtrl : T_IU_ThreadControl);

  // ***
  // * Procedure IU_ThreadEnding Finish a thread execution
  // *
  // * @authors : Pascal Lemaître
  // *
  // *
  // * in/out thread control descriptor structure
  // *
  // * This procedure must be call last by the procedure executed in the new created thread
  // * when the procedure end its prossessing
  // *
  // ***
  procedure IU_ThreadEnding(var ThreadCtrl : T_IU_ThreadControl);

  // ***
  // * Procedure IU_DestroyThread Destroy thread and its descriptor (only if thread execution is over)
  // *
  // * @authors : Pascal Lemaître
  // *
  // *
  // * in/out thread control descriptor structure
  // *
  // * This procedure must be call by thread was created the thread stopped (not paused)
  // *
  // ***
  procedure IU_DestroyThread(var ThreadCtrl : T_IU_ThreadControl);

  // ***
  // * Procedure IU_SuspendThread temporay suspend the execution of the thread
  // *
  // * @authors : Pascal Lemaître
  // *
  // *
  // * in/out thread control descriptor structure
  // *
  // * This procedure must be call by thread was created the thread to suspend
  // *
  // ***
  procedure IU_SuspendThread(var ThreadCtrl : T_IU_ThreadControl);

  // ***
  // * Procedure IU_ResumeThread restart a temporay suspended thread
  // *
  // * @authors : Pascal Lemaître
  // *
  // *
  // * in/out thread control descriptor structure
  // *
  // * This procedure must be call by thread was created and suspended the thread
  // *
  // ***
  procedure IU_ResumeThread(var ThreadCtrl : T_IU_ThreadControl);

  // ***
  // * Procedure IU_SuspendingThread temporay suspend the execution of the thread
  // *
  // * @authors : Pascal Lemaître
  // *
  // *
  // * in/out thread control descriptor structure
  // *
  // * This procedure must be call by thread created for react to suspend and resume commands
  // *
  // ***
  procedure IU_SuspendingThread(var ThreadCtrl : T_IU_ThreadControl);

  // ***
  // * Procedure IU_SuspendingThread temporay suspend the execution of the thread
  // *
  // * @authors : Pascal Lemaître
  // *
  // *
  // * in/out thread control descriptor structure
  // *
  // * This procedure must be call by thread creator for stopping a thread child
  // *
  // ***
  procedure IU_StopThread(var ThreadCtrl : T_IU_ThreadControl);


  // ***
  // * Procedure IU_PrepareAccessingSharedData concurrent write/read protection
  // *
  // * @authors : Pascal Lemaître
  // *
  // *
  // * in/out thread control descriptor structure
  // *
  // * This procedure must be call by thread was created and the thread created
  // *
  // ***
  procedure IU_PrepareAccessingSharedData(var ThreadCtrl : T_IU_ThreadControl);

  // ***
  // * Procedure IU_PrepareAccessingSharedData concurrent write/read protection
  // *
  // * @authors : Pascal Lemaître
  // *
  // *
  // * in/out thread control descriptor structure
  // *
  // * This procedure must be call by thread was created and the thread created
  // *
  // ***
  procedure IU_EndAccessingSharedData(var ThreadCtrl : T_IU_ThreadControl);

  // ***
  // * Procedure IU_PrepareAccessingStateAndCommand concurrent write/read protection
  // *
  // * @authors : Pascal Lemaître
  // *
  // *
  // * in/out thread control descriptor structure
  // *
  // * This procedure must be call by thread was created and the thread created
  // *
  // ***
  procedure IU_PrepareAccessingStateAndCommand(var ThreadCtrl : T_IU_ThreadControl);

  // ***
  // * Procedure IU_PrepareAccessingStateAndCommand concurrent write/read protection
  // *
  // * @authors : Pascal Lemaître
  // *
  // *
  // * in/out thread control descriptor structure
  // *
  // * This procedure must be call by thread was created and the thread created
  // *
  // ***
  procedure IU_EndAccessingStateAndCommand(var ThreadCtrl : T_IU_ThreadControl);





implementation

var
  threadid_read : boolean;

// ***
// * Procedure IU_Init_Threads_Utils Initializing all internals var for threads management
// *
// * @authors : Pascal Lemaître
// *
// *
// * This function must be call before using units
// *
// ***
Procedure IU_Init_Threads_Utils;
begin
   InitCriticalSection(IU_Thread_CriticalSection);
end;

// ***
// * Procedure IU_Close_Threads_Utils release all internals var for threads management
// *
// * @authors : Pascal Lemaître
// *
// *
// * This function must be call before program ending
// *
// ***
Procedure IU_Close_Threads_Utils;
begin
   DoneCriticalSection(IU_Thread_CriticalSection);
end;

// ***
// * Function IU_CreateThread creating a new thread and int its control descriptor
// *
// * @authors : Pascal Lemaître
// *
// *
// * in/out thread control descriptor structure
// * in : address of procedur to execute in thread. Procedure must be like procedure-name (threadcontrol : pointer);
// *                                                with threadcontrol a pointer on Thread control descripteur
// *
// * return : thread ID
// *
// * This function must be call by the thread who wants to create a new thread
// *
// ***
function IU_CreateThread(var ThreadCtrl : T_IU_ThreadControl) : TThreadID;
var _thread_id : TThreadID;
begin
  // With windows in thread GetThreadID function doesn't work
  // Then we need to set the right ThreadID ourself
  // Paused the new thread until init var will be set
  EnterCriticalSection(IU_Thread_CriticalSection);
  threadid_read := false;
  _thread_id := BeginThread(ThreadExec, pointer(@ThreadCtrl));
  // Init Thread ctrl descriptor
  ThreadCtrl.stopthread:=false;
  ThreadCtrl.running:=true;
  ThreadCtrl.thread:=_thread_id;
  // Creating a dedicated critical section for the new thread
  new(ThreadCtrl._criticalsection);
  new(ThreadCtrl._criticalsectionsuspend);
  new(ThreadCtrl._criticalsectionstate);
  // Init new critical sections
  InitCriticalSection(ThreadCtrl._criticalsectionsuspend^);
  InitCriticalSection(ThreadCtrl._criticalsectionstate^);
  InitCriticalSection(ThreadCtrl._criticalsection^);
  // Leave critical section -> thread can resume and read its thread id
  IU_CreateThread := _thread_id;
  LeaveCriticalSection(IU_Thread_CriticalSection);

end;

// ***
// * Function IU_InitThread initialize the new created thread
// *
// * @authors : Pascal Lemaître
// *
// *
// * in/out thread control descriptor structure
// *
// * This function must be call first by the procedure executed in the new created thread
// *
// ***
procedure IU_ThreadInit(var ThreadCtrl : T_IU_ThreadControl);
begin
  // Waiting for caller finished to init value (do nothing just wait)
  EnterCriticalSection(IU_Thread_CriticalSection);
  // just wait
  LeaveCriticalSection(IU_Thread_CriticalSection);
end;

// ***
// * Function IU_ThreadEnding Finish a thread execution
// *
// * @authors : Pascal Lemaître
// *
// *
// * in/out thread control descriptor structure
// *
// * This function must be call last by the procedure executed in the new created thread
// * when the procedure end its prossessing
// *
// ***
procedure IU_ThreadEnding(var ThreadCtrl : T_IU_ThreadControl);
begin
  if ThreadCtrl._criticalsectionstate <> nil then
    EnterCriticalSection(ThreadCtrl._criticalsectionstate^);
  ThreadCtrl.running:=false;
  if ThreadCtrl._criticalsectionstate <> nil then
    LeaveCriticalSection(ThreadCtrl._criticalsectionstate^);
  EndThread(0);
end;

// ***
// * Function IU_DestroyThread Destroy thread and its descriptor (only if thread execution is over)
// *
// * @authors : Pascal Lemaître
// *
// *
// * in/out thread control descriptor structure
// *
// * This function must be call by thread was created the thread stopped (not paused)
// *
// ***
procedure IU_DestroyThread(var ThreadCtrl : T_IU_ThreadControl);
var stopdone : boolean;
begin
  // Killing Thread
  // KillThread(ThreadCtrl.thread);

  // Destroying thread controler
  // free critical section dedicated to the thread

  DoneCriticalSection(ThreadCtrl._criticalsection^);
  DoneCriticalSection(ThreadCtrl._criticalsectionstate^);
  DoneCriticalSection(ThreadCtrl._criticalsectionsuspend^);
  dispose(ThreadCtrl._criticalsection);
  dispose(ThreadCtrl._criticalsectionstate);
  dispose(ThreadCtrl._criticalsectionsuspend);
  ThreadCtrl._criticalsection:=nil;
  ThreadCtrl._criticalsectionstate:=nil;
  ThreadCtrl._criticalsectionsuspend:=nil;
  // Setting thread ID to 0
  ThreadCtrl.thread:=TThreadID(0);

end;

// ***
// * Function IU_SuspendThread temporay suspend the execution of the thread
// *
// * @authors : Pascal Lemaître
// *
// *
// * in/out thread control descriptor structure
// *
// * This function must be call by thread was created the thread to suspend
// *
// ***
procedure IU_SuspendThread(var ThreadCtrl : T_IU_ThreadControl);
begin
  // Accessing to the state of the thread control descriptor
  if ThreadCtrl._criticalsectionstate <> nil then
    EnterCriticalSection(ThreadCtrl._criticalsectionstate^);
  // setting state to suspend
  ThreadCtrl.pause:=true;
  // exiting of state update section
  if ThreadCtrl._criticalsectionstate <> nil then
    LeaveCriticalSection(ThreadCtrl._criticalsectionstate^);
  // Stopping Thread : entering in criticalsectionsuspend
  if ThreadCtrl._criticalsectionsuspend <> nil then
    EnterCriticalSection(ThreadCtrl._criticalsectionsuspend^);
end;

// ***
// * Function IU_ResumeThread restart a temporay suspended thread
// *
// * @authors : Pascal Lemaître
// *
// *
// * in/out thread control descriptor structure
// *
// * This function must be call by thread was created and suspended the thread
// *
// ***
procedure IU_ResumeThread(var ThreadCtrl : T_IU_ThreadControl);
begin
  // Resume thread with leaving critical section
  if ThreadCtrl._criticalsectionsuspend <> nil then
    LeaveCriticalSection(ThreadCtrl._criticalsectionsuspend^);
  // Updating state of the thread
  // Accessing to the state of the thread control descriptor
  if ThreadCtrl._criticalsectionstate <> nil then
    EnterCriticalSection(ThreadCtrl._criticalsectionstate^);
  // setting state to suspend
  ThreadCtrl.pause:=false;
  // exiting of state update section
  if ThreadCtrl._criticalsectionstate <> nil then
    LeaveCriticalSection(ThreadCtrl._criticalsectionstate^);
end;

// ***
// * Procedure IU_SuspendingThread temporay suspend the execution of the thread
// *
// * @authors : Pascal Lemaître
// *
// *
// * in/out thread control descriptor structure
// *
// * This procedure must be call by thread creator for stopping a thread child
// *
// ***
procedure IU_StopThread(var ThreadCtrl : T_IU_ThreadControl);
begin
  try
     // to stop a thread we need to have an executing thread (not suspended)
     IU_PrepareAccessingStateAndCommand(ThreadCtrl);
     if ThreadCtrl.pause then
     // Waking up the thread
     IU_ResumeThread(ThreadCtrl);
  finally
    // Send command through the thread control descriptor
     ThreadCtrl.stopThread := true;
     // end command was sent
     IU_EndAccessingStateAndCommand(ThreadCtrl);
  end;
end;

// ***
// * Procedure IU_SuspendingThread temporay suspend the execution of the thread
// *
// * @authors : Pascal Lemaître
// *
// *
// * in/out thread control descriptor structure
// *
// * This procedure must be call by thread created for react to suspend and resume commands
// *
// ***
procedure IU_SuspendingThread(var ThreadCtrl : T_IU_ThreadControl);
begin
  // Accessing to the state of the thread control descriptor
  if ThreadCtrl._criticalsectionsuspend <> nil then begin
    EnterCriticalSection(ThreadCtrl._criticalsectionsuspend^);
    // nothing to do
    LeaveCriticalSection(ThreadCtrl._criticalsectionsuspend^);
  end;
end;

// ***
// * Procedure IU_PrepareAccessingSharedData concurrent write/read protection
// *
// * @authors : Pascal Lemaître
// *
// *
// * in/out thread control descriptor structure
// *
// * This procedure must be call by thread was created and the thread created
// *
// ***
procedure IU_PrepareAccessingSharedData(var ThreadCtrl : T_IU_ThreadControl);
begin
  if ThreadCtrl._criticalsection <> nil then
    EnterCriticalSection(ThreadCtrl._criticalsection^);
end;

// ***
// * Procedure IU_PrepareAccessingSharedData concurrent write/read protection
// *
// * @authors : Pascal Lemaître
// *
// *
// * in/out thread control descriptor structure
// *
// * This procedure must be call by thread was created and the thread created
// *
// ***
procedure IU_EndAccessingSharedData(var ThreadCtrl : T_IU_ThreadControl);
begin
  if ThreadCtrl._criticalsection <> nil then
    LeaveCriticalSection(ThreadCtrl._criticalsection^);
end;

// ***
// * Procedure IU_PrepareAccessingStateAndCommand concurrent write/read protection
// *
// * @authors : Pascal Lemaître
// *
// *
// * in/out thread control descriptor structure
// *
// * This procedure must be call by thread was created and the thread created
// *
// ***
procedure IU_PrepareAccessingStateAndCommand(var ThreadCtrl : T_IU_ThreadControl);
begin
  if ThreadCtrl._criticalsectionstate <> nil then
    EnterCriticalSection(ThreadCtrl._criticalsectionstate^);
end;

// ***
// * Procedure IU_PrepareAccessingStateAndCommand concurrent write/read protection
// *
// * @authors : Pascal Lemaître
// *
// *
// * in/out thread control descriptor structure
// *
// * This procedure must be call by thread was created and the thread created
// *
// ***
procedure IU_EndAccessingStateAndCommand(var ThreadCtrl : T_IU_ThreadControl);
begin
  if ThreadCtrl._criticalsectionstate <> nil then
    LeaveCriticalSection(ThreadCtrl._criticalsectionstate^);
end;


end.

