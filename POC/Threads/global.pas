unit global;

{$mode objfpc}{$H+}

interface

uses
  LCLType, Classes, SysUtils, stdctrls;


type
  T_ThreadControl = record
    thread : TThreadID;
    ThreadMSG : string;
    running : boolean;
    stopthread : boolean;
  end;


var
  Threadcontrol : array [0..3] of T_ThreadControl;


  thread1ID, thread2ID, thread3ID, thread4ID : TThreadID;

  // With windows in thread GetThreadID function doesn't work
  // Then we need to set the right ThreadID ourself
  CreatedThreadID : TThreadID;

  threadid_read : boolean;



implementation


end.

