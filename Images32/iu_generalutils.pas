unit IU_GeneralUtils;
// ***
// * Unit provides generals utils services (adapted from TIm V1.x branch)
// * Creation Date : 2017 September
// *
// * Version : 1.7
// * Version Date : 2018 January
// * Version Contributors : Pascal Lemaître
// *
// *
// * Version 1.7 : Procedure to save and read user properties for windows
// * Version 1.6 : Testing equality in file name because under windows x.tiff = x.TIFF
// *               Then test was added to insert only once the file in the list
// * Version 1.5 : Adding drives list and files list object
// * Version 1.4 : Adding double chained list of text
// * Version 1.3 : Adding size of current disk and free room on it (for windows and linux)
// * Version 1.2 : For windows Adding a min function with real args and windows drive list
// * Version 1.1 : Adding coef adaptation function according to installed RAM
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

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IU_Types, math
  {$ifdef Linux}
  ,linux, BaseUnix, unixtype
  {$endif}
  {$ifdef windows}
  ,windows
  {$endif}

  // ***
  // * Add V1.4
  , IU_I18N_Messages, IU_Exceptions
  // *
  // * End Add V1.4
  // ***
  ;

type

// ***
// * Add V1.2
{$ifdef Windows}
  T_WindowsDrive = record
    drive : char;
    exist : boolean;
  end;

  T_WindowsListDrives = array [1..26] of T_WindowsDrive;
{$endif}
// *
// * End Add V1.2
// ***

// ***
// * Add V1.4
// *
  // pointer declaration
  IU_T_StringDoubleLinkPointer = ^IU_T_StringDoubleLink;
  // real declaration
  IU_T_StringDoubleLink = record
    _previous, _next : IU_T_StringDoubleLinkPointer ;
    _caption : string;
  end;

  // Double chained list of string
  IU_T_StringDoubleChainedList = class(TObject)
    private
      _start : IU_T_StringDoubleLinkPointer ;    // Anchor of the list
      _end   : IU_T_StringDoubleLinkPointer ;    // Anchor of the list
      _current : IU_T_StringDoubleLinkPointer ;  // Current selected item
      _saved : IU_T_StringDoubleLinkPointer ;    // Saved current for transactions
      _savedReadOrder : boolean;                 // Saved current readOrder
      _count : integer ;                         // Number of items in the list
      _defaultReadDirection : boolean;           // Read direction (default from begining to end)
      _sorted : boolean;
    public
      constructor Create(sort : boolean);        // constructor
                                                 // if sort = true then sort in alphabetic order
                                                 // else last in first out
      destructor Destroy;                        // destructor
      procedure reset;                           // Clear all items in the list
      procedure rewind;                          // Go to the first item recorded in the list
      procedure next;                            // Move to next record in the list
      procedure previous;                        // Move to the previous record in the list
      procedure saveCurrent;                     // Save current item for transactionnal manipulations
      procedure restoreCurrent;                  // Restore the cutrrent saved after transactionnal manipulations
      function getText : string;                 // Get text from the current record
      procedure add(_value : string);            // Add a new record in the list
// ***
// * Add V1.5
      procedure addFirst(_value : string) ;      // Add a new record at the begining of the list (ordered list or not)
// ***
      function getItemsNumbers : integer; // return the number of items in the list
      procedure revertReadOrder;          // change reading direction
                                          //   by default next move to next item from the anchor of the list
                                          //   in this case revertorder change the reading direction from the end to the anchor
                                          //   an other revertorder reset the default direction and so on...
      function isStandardReadOrder : boolean;       // Return true if is the standard read order else return false
      procedure addChain(_chain : IU_T_StringDoubleChainedList) ; // concat an other chain


  end;
// *
// * End Add V1.4
// ***

// ***
// * Add V1.5
  IU_T_DirsList = class(IU_T_StringDoubleChainedList)
    public
      constructor Create ; // Constructor
      procedure initDirectoriesList ; // Fullfill the list with the directories form the current dir
  end;

  IU_T_FilesList = class(IU_T_StringDoubleChainedList)
    public
      constructor Create ; // Constructor
      procedure addFilesInList(patern : string) ; // Adding new files with specific pattern in the list
  end;
// *
// * End Add V1.5
// ***

// ***
// * Extract filename from a complete Path (Path + Filename).
// *
// * @author : Pascal Lemaître
// *
// * in : path with filename path
// *
// * return : filename
// *
// ***
function IU_nameFromPath(path : string) : string;
// function retourn the name of the string from a complete path/file name


// ***
// * Extract path from a complete Path (Path + Filename).
// *
// * @author : Pascal Lemaître
// *
// * in : path with filename path
// *
// * return : path without filename
// *
// ***
function IU_pathFromPath(path : string) : string;
// function return the path of the directory from a complete path/file name



// ***
// * Transform a real into a string with a fixed number of decimals.
// *
// * @author : Pascal Lemaître
// *
// * in : value to convert x
// * in : number of decimals wanted
// *
// * return : real expressed in characters string
// *
// ***
function IU_realToString(x : real; nbdecimal : integer) : string;
// function return a string expression of an real with fixed decimals



// ***
// * Make approximation on points from a TCurves (for histrograms curves).
// *
// * @author : Pascal Lemaître
// *
// * in : curves courbeIn
// * in : coef (weight) of central point x
// * in : left border for approximation a
// * in : right border for approximation b
// *
// * return : approximation value (point in the middle of a and b borders)
// *
// ***
function IU_Approx(courbeIn : T_IU_MyTCurves; x, a, b : integer) : integer;
// function  return an approximation value of a new point around one and its nearest points
//   useful for luminance curves

// ***
// * Return the RAM installed on the computer
// *
// * @author : Pascal Lemaître
// *
// * return : total of RAM used and not used
// *
// ***
function IU_getInstalledRAM : qword;
// function return a qword (size of total RAM)

// ***
// * Return coef adaptation for allocated memory according to installed RAM
// *
// * @author : Pascal Lemaître
// *
// * return : adaptation coef
// *
// ***
function IU_getAdaptationRAMCoef : real;
// function return a real has an allocation memory coef to adapt size of frame according to installed RAM

// ***
// * Add V1.2
// *
// ***
// * Return min value between 2 reals
// *
// * @author : Pascal Lemaître
// *
// * return : min coef
// *
// ***
function IU_min(a, b : real) : real;
// function return a real min between two reals
// *
// * End Add V1.2
// ***

// ***
// * Add V1.2
{$ifdef Windows}
// ***
// * create an array of 26 drives state for windows
// *
// * @author : Pascal Lemaître
// *
// * return a T_WindowsListDrives
// *
function IU_ListWindowsDrives : T_WindowsListDrives ;
{$endif}
// *
// * End Add V1.2
// ***


// ***
// * Add V1.3
// *
// ***
// * Getting total size and free room of current drive
// *
// * @author : Pascal Lemaître
// *
// * out : _size (total size of the disk from the current selected dir)
// * out : _free (free room on the disk from the current selected dir)
// *
procedure IU_getCurrentDriveSizes(var _size, _free : int64);
// *
// * End Add V1.3
// ***


// ***
// * Add v1.6
// *
// ***
// * Saving in user home dir and sub properties Tim dir the properties for a windows
// *
// @author : Pascal Lemaitre
// *
// * in : windowsname : the name of the windows
// * in : propertiesstructure : pointer on properties data for the windows
// * in : size of the properties strucure
// *
procedure writeProperties (windowsname : string ; propertiesStructure : Pchar ; _strucsize : integer);

// ***
// * Saving in user home dir and sub properties Tim dir the properties for a windows
// *
// @author : Pascal Lemaitre
// *
// * in : windowsname : the name of the windows
// * out : propertiesstructure : pointer on properties data for the windows
// * in : size of the properties strucure
// *
procedure readProperties (windowsname : string ; propertiesStructure : Pchar ; _strucsize : integer);
// * End Add v1.6
// ***

implementation

// ***
// * Extract filename from a complete Path (Path + Filename).
// *
// * @author : Pascal Lemaître
// *
// * in : path with filename path
// *
// * return : filename
// *
// ***
function IU_nameFromPath(path : string) : string;
var i, position, taille : integer;
  car : char;
begin
  {$IFDEF Windows}
  car := '\';
  {$ELSE}
  car := '/';
  {$ENDIF}
  position := 0;
  taille := Length(path);
  for i:= taille - 1 downto 0 do
    begin
        if path[i] = car then begin
          position := i+1;
          break;
        end
    end;
  IU_nameFromPath := copy(path, position, taille - position + 1);
end;



// ***
// * Extract path from a complete Path (Path + Filename).
// *
// * @author : Pascal Lemaître
// *
// * in : path with filename path
// *
// * return : path without filename
// *
// ***
function IU_pathFromPath(path : string) : string;
var i, position, taille : integer;
  car : char;
begin
  {$IFDEF Windows}
  car := '\';
  {$ELSE}
  car :='/';
  {$ENDIF}
  position := 0;
  taille := Length(path);
  for i:= taille - 1 downto 0 do
    begin
        if path[i] = car then begin
          position := i;
          break;
        end
    end;
  IU_pathFromPath := copy(path, 1, position);
end;


// ***
// * Transform a real into a string with a fixed number of decimals.
// *
// * @author : Pascal Lemaître
// *
// * in : value to convert x
// * in : number of decimals wanted
// *
// * return : real expressed in characters string
// *
// ***
function IU_realToString(x : real; nbdecimal : integer) : string;
var entier, dec,i : integer;
  decimal : real;
  ret, ret2 : string;
  neg : boolean;
begin
  if x < 0 then begin
    x := -x;
    neg := true;
  end else neg := false;
  entier := trunc(x);
  decimal := x - entier;
  dec := round(decimal * power(10, nbdecimal));
  str(entier, ret);
  str(dec, ret2);
  if nbDecimal > 0 then begin
    for i := length(ret2) to nbdecimal - 1 do
        ret2 := concat('0',ret2);
    ret := concat(ret, '.');
    ret := concat(ret, ret2);
  end;
  if neg then ret := concat('-', ret);
  IU_realToString := ret;

end;

// ***
// * Transform a real into a string with a fixed number of decimals.
// *
// * @author : Pascal Lemaître
// *
// * in : value to convert x
// * in : number of decimals wanted
// *
// * return : real expressed in characters string
// *
// ***
function IU_realToString(x : extended; nbdecimal : integer) : string;
var entier, dec,i : integer;
  decimal : real;
  ret, ret2 : string;
  neg : boolean;
begin
  if x < 0 then begin
    x := -x;
    neg := true;
  end else neg := false;
  entier := trunc(x);
  decimal := x - entier;
  dec := round(decimal * power(10, nbdecimal));
  str(entier, ret);
  str(dec, ret2);
  if nbdecimal > 0 then begin
    for i := length(ret2) to nbdecimal - 1 do
      ret2 := concat('0',ret2);
    ret := concat(ret, '.');
    ret := concat(ret, ret2);
  end;
  if neg then ret := concat('-', ret);
  IU_realToString := ret;

end;


// ***
// * Make approximation on points from a TCurves (for histrograms curves).
// *
// * @author : Pascal Lemaître
// *
// * in : curves courbeIn
// * in : coef (weight) of central point x
// * in : left border for approximation a
// * in : right border for approximation b
// *
// * return : approximation value (point in the middle of a and b borders)
// *
// ***
function IU_Approx(courbeIn : T_IU_MyTCurves; x, a, b : integer) : integer;
var
  i, j : integer;
  r, d ,diviseur : real;
begin
    j := round(x * a / b);
    r := 0;
    d := 0;
    for i:=j to j + b - a do begin
      diviseur := 0.2 * abs(i-x)+1;
      r := r + (courbeIn[i] / diviseur);
      d := d + (1 / diviseur);
    end;
    IU_Approx := round (r/d);
end;

// ***
// * Return the RAM installed on the computer
// *
// * @author : Pascal Lemaître
// *
// * return : total of RAM used and not used
// *
// ***
function IU_getInstalledRAM : qword;
// function return a int64 (size of total RAM)
var ret : qword;
  {$ifdef Linux}
  Info : TSysInfo;
  {$endif}
  {$ifdef Windows}
  s : TMemoryStatus ;
  {$endif}
begin
  {$ifdef Linux}
  SysInfo(@Info);
  ret := Info.totalram;
  {$endif}
  {$ifdef Windows}
  ret := 0;
  s.dwLength := SizeOf(s);
  GlobalMemoryStatus(s);
  ret :=s.dwTotalPhys;
  {$endif}
  IU_GetInstalledRAM := ret;
end;

// ***
// * Return coef adaptation for allocated memory according to installed RAM
// *
// * @author : Pascal Lemaître
// *
// * return : adaptation coef
// *
// ***
function IU_getAdaptationRAMCoef : real;
// function return a real has an allocation memory coef to adapt size of frame according to installed RAM
const
  _K = 0.04;
var
  RAM : real;
  coef : real;
begin
  RAM := IU_getInstalledRAM / 1024 / 1024 / 1024 ;
  coef := real(IU_min(2.05, 1+(RAM*_K)));
  IU_getAdaptationRAMCoef := coef;
end;

// ***
// * Add V1.2
// *
// ***
// * Return min value between 2 reals
// *
// * @author : Pascal Lemaître
// *
// * return : min coef
// *
// ***
function IU_min(a, b : real) : real;
// function return a real min between two reals
begin
  if a < b then IU_min := a else IU_min := b;
end;
// *
// * End Add V1.2
// ***

// ***
// * Add V1.2
{$ifdef Windows}
// ***
// * create an array of 26 drives state for windows
// *
// * @author : Pascal Lemaître
// *
// * return a T_WindowsListDrives
// *
function IU_ListWindowsDrives : T_WindowsListDrives ;
var i : integer ;
  Drives :  T_WindowsListDrives ;
  bits : dword;
begin
  bits := GetLogicalDrives();
  for i := 1 to 26 do begin
    Drives[i].drive:=chr(i+64);
    Drives[i].exist:=bits AND 1 = 1 ;
    bits := bits shr 1;
  end;
  IU_ListWindowsDrives := Drives;
end;
{$endif}
// *
// * End Add V1.2
// ***


// ***
// * Add V1.3
// *
// ***
// * Getting total size and free room of current drive
// *
// * @author : Pascal Lemaître
// *
// * out : _size (total size of the disk from the current selected dir)
// * out : _free (free room on the disk from the current selected dir)
// *
procedure IU_getCurrentDriveSizes(var _size, _free : int64);
begin
  _size := disksize(0);
  _free := diskfree(0);
end;
// *
// * End Add V1.3
// ***


// ***
// * Add V1.4
// *
// ***
// * Creator of a double chained list of string
// *
// * @author : Pascal Lemaître
// *
// * in : sort : true add in the list are sorted, false add are made at the begining of the list
// *
constructor IU_T_StringDoubleChainedList.Create(sort : boolean); // constructor
                                                                 // if sort = true then sort in alphabetic order
                                                                 // else last in first out
begin
  self._start:=nil;
  self._end:=nil;
  self._current:=nil;
  self._saved:=nil;
  self._defaultReadDirection:=true;
  self._count:=0; // No item in the list
  self._sorted:=sort;
end;


// ***
// * Destructor of a double chained list of string
// *
// * @author : Pascal Lemaître
// *
destructor IU_T_StringDoubleChainedList.Destroy;                 // destructor
begin
  // calling reset for releasing all items of the list
  self.reset;
  inherited;
end;

// ***
// * Reseting to an empty list of string
// *
// * @author : Pascal Lemaître
// *
// * Deleting all items in the list but list still exists
// *
procedure IU_T_StringDoubleChainedList.reset;                    // Clear all items in the list
var
  pt : IU_T_StringDoubleLinkPointer;
begin
  // Loop for free all items of the list
  pt := self._start;
  while pt <> nil do begin
    self._start := pt^._next;
    dispose(pt);
    pt := self._start;
  end;
  // updating list anchor properties
  self._start := nil;
  self._end := nil;
  self._current:= nil;
  self._saved:= nil;
  self._defaultReadDirection:=true;
  self._count := 0;
end;

// ***
// * Rewind to the begining of the list if default readOrder or rewind to the end of the list if not default readOrder
// *
// * @author : Pascal Lemaître
// *
procedure IU_T_StringDoubleChainedList.rewind;                   // Go to the first item recorded in the list
begin
  // rewind depends on defaulReadDirection
  // if it is true then rewind go to the start item
  // if it is false then rewind go to the las item
  if self._defaultReadDirection then
    self._current:=self._start
  else self._current:=self._end;
end;

// ***
// * Move to the next item in the list if default readOrder or previous if not defaut readOrder
// *
// * @author : Pascal Lemaître
// *
procedure IU_T_StringDoubleChainedList.next;                     // Move to next record in the list
begin
  // next depends on defaultReadDirection
  // if it is false then next is the previous (inverted reading direction)
  // if it is true then next is the next (standard reading direction)
  // if there is no next item then an exception is raised and no change in selected item
  // if there is no item in the list then an execption is raised
  if self._current=nil then begin
    // may be it is the first access of list item ?
    // then next select the start item if defaultReadDirection is true
    // or select the end item if defaultReadDirection if false
    // But if there is no item in the list an exception is raised
    if self._start = nil then begin
      // raised an exception
      raise IU_EStringDoubleChainedList_NoItem.Create(IU_ExceptionsMessages[IU_CurrentLang, K_IU_ExceptMSG_StringDoubleChainedList_NoItem]);
    end else if self._defaultReadDirection then
      self._current:=self._start else
      self._current:=self._end;
  end else begin
    // if defaultReadDirection is true then chain on next else chain on previous
    if self._defaultReadDirection then begin
      // testing if there is a next item. If no an exception is raised
      if self._current^._next = nil then begin
        // raised an exception
        raise IU_EStringDoubleChainedList_NoMoreItem.Create(IU_ExceptionsMessages[IU_CurrentLang, K_IU_ExceptMSG_StringDoubleChainedList_NoMoreItem]);
      end else // chain on next
        self._current := self._current^._next;
    end else begin
      // testing if there is a previous item. If no an exception is raised
      if self._current^._previous = nil then begin
        // raised an exception
        raise IU_EStringDoubleChainedList_NoMoreItem.Create(IU_ExceptionsMessages[IU_CurrentLang, K_IU_ExceptMSG_StringDoubleChainedList_NoMoreItem]);
      end else // chain on previous
        self._current := self._current^._previous;
    end;
  end;
end;

// ***
// * Move to previous item in the list if defautl readOrder or next if not default readOrder
// *
// * @author : Pascal Lemaître
// *
procedure IU_T_StringDoubleChainedList.previous;                 // Move to the previous record in the list
begin
  // previous depends on defaultReadDirection
  // if it is false then previous is the next (inverted reading direction)
  // if it is true then previous is the previous (standard reading direction)
  // if there is no next item then an exception is raised and no change in selected item
  // if there is no item in the list then an execption is raised
  if self._current=nil then begin
    // may be it is the first access of list item ?
    // then next select the start item if defaultReadDirection is true
    // or select the end item if defaultReadDirection if false
    // But if there is no item in the list an exception is raised
    if self._start = nil then begin
      // raised an exception
      raise IU_EStringDoubleChainedList_NoItem.Create(IU_ExceptionsMessages[IU_CurrentLang, K_IU_ExceptMSG_StringDoubleChainedList_NoItem]);
    end else if self._defaultReadDirection then
      self._current:=self._start else
      self._current:=self._end;
  end else begin
    // if defaultReadDirection is true then chain on next else chain on previous
    if self._defaultReadDirection then begin
      // testing if there is a next item. If no an exception is raised
      if self._current^._previous = nil then begin
        // raised an exception
        raise IU_EStringDoubleChainedList_NoMoreItem.Create(IU_ExceptionsMessages[IU_CurrentLang, K_IU_ExceptMSG_StringDoubleChainedList_NoMoreItem]);
      end else // chain on next
        self._current := self._current^._previous;
    end else begin
      // testing if there is a previous item. If no an exception is raised
      if self._current^._next = nil then begin
        // raised an exception
        raise IU_EStringDoubleChainedList_NoMoreItem.Create(IU_ExceptionsMessages[IU_CurrentLang, K_IU_ExceptMSG_StringDoubleChainedList_NoMoreItem]);
      end else // chain on previous
        self._current := self._current^._next;
    end;
  end;
end;

// ***
// * Saving the current item of the list for a futur restoration
// *
// * @author : Pascal Lemaître
// *
procedure IU_T_StringDoubleChainedList.saveCurrent;              // Save current item for transactionnal manipulations
begin
  self._saved:=self._current;
  self._savedReadOrder:=self._defaultReadDirection;
end;

// ***
// * Restoring a previous saved current item
// *
// * @author : Pascal Lemaître
// *
// * if no previous saved then restore set current to nil (deselect)
// * In this case a next or previous call must be done before to get a text item
procedure IU_T_StringDoubleChainedList.restoreCurrent;           // Restore the current saved after transactionnal manipulations
begin
  self._current := self._saved;
  self._defaultReadDirection:=self._savedReadOrder;
end;

// ***
// * Getting the text of the current item of the list
// *
// * @author : Pascal Lemaître
// *
// * Exception will be raised if there is no item in the list
// *
function IU_T_StringDoubleChainedList.getText : string;          // Get text from the current record
begin
  // if no item selected then an error is raised (an item must be selected before use next or previous method)
  // if list is empty then an other error is raised
  // else the string of the current item is returned
  if self._start=nil then
      // an exception is raised
  else if self._current=nil then
      // an exception is raised
  else // the string is returned
      try
        getText := self._current^._caption;
      Except
        // raised an exception
        raise IU_EStringDoubleChainedList_AccessError.Create(IU_ExceptionsMessages[IU_CurrentLang, K_IU_ExceptMSG_StringDoubleChainedList_AccessError]);
      end;
end;

// ***
// * Adding a new string value in the list
// *
// * @author : Pascal Lemaître
// *
// * in : string to add
// *
// * if list is created with order flag then the add is done with alphabetic order (no distinction between lowrcase and upercase
// *    else add is done at the begining of the list
// *
procedure IU_T_StringDoubleChainedList.add(_value : string);     // Add a new record in the list
var
  pt, searchpt : IU_T_StringDoubleLinkPointer;
  nilerror : boolean;
  captioncomp : string;
  inserted : boolean;
begin
  // saved policy when no more memory availlable
  nilerror := ReturnNilIfGrowHeapFails;
  // set policy to return nil and no raising exception
  ReturnNilIfGrowHeapFails := true ;
  // trying to create a new item
  new(pt);
  if pt = nil then begin
    // reset policy to its old value
    ReturnNilIfGrowHeapFails := nilerror ;

    // raise an exception
    raise IU_EStringDoubleChainedList_NoMoreAvailableMemory.Create(IU_ExceptionsMessages[IU_CurrentLang,K_IU_ExceptMSG_StringDoubleChainedList_NoMoreAvailableMemory]);
  end;
  // set string
  try
    pt^._caption:=_value;
  Except
    // pt must be released
    try
      Dispose(pt);
    finally
      // reset policy to its old value
      ReturnNilIfGrowHeapFails := nilerror ;
      // memory access error on an existing item ?
      // raised an exception
      raise IU_EStringDoubleChainedList_AccessError.Create(IU_ExceptionsMessages[IU_CurrentLang, K_IU_ExceptMSG_StringDoubleChainedList_CreateAccessError]);
    end;
  end;
  // Searching where add the item in the list
  searchpt := self._start;
  // if searchpt = nil then this is the first add
  if searchpt = nil then begin
    self._start:=pt;
    self._end:=pt;
    pt^._next:=nil;
    pt^._previous:=nil;
    // ***
    // * Add V1.6
    inc(self._count);
    // ***
  end else begin
    inserted := false;
    while searchpt <> nil do begin
      // trying to get caption from the list item
      try
        captioncomp := searchpt^._caption;
      Except
        // pt must be released
        try
        Dispose(pt);

        finally
          // reset policy to its old value
          ReturnNilIfGrowHeapFails := nilerror ;
          // memory access error on an existing item ?
          // raised an exception
          raise IU_EStringDoubleChainedList_AccessError.Create(IU_ExceptionsMessages[IU_CurrentLang, K_IU_ExceptMSG_StringDoubleChainedList_CreateAccessError]);
        end;
      end;
      // case insenstitive comparison
      // CompareText return 0 if s1 < s2 then not (CompareText(s1,s2) < 0) test if s1 = s2 and s1 > s2
      // if it is an unsorted list then the add is made at the begining of the list
      if ((CompareText(_value, captioncomp) < 0) or (not self._sorted)) then begin // adding item here
        pt^._next:=searchpt;
        pt^._previous:=searchpt^._previous;
        if searchpt^._previous <> nil then
            // link previous to new item
            try
              searchpt^._previous^._next:=pt;

            Except
              // pt must be released
              try
                Dispose(pt);

              finally
                // reset policy to its old value
                ReturnNilIfGrowHeapFails := nilerror ;
                // memory access error on an existing item ?
                // raised an exception
                raise IU_EStringDoubleChainedList_AccessError.Create(IU_ExceptionsMessages[IU_CurrentLang, K_IU_ExceptMSG_StringDoubleChainedList_CreateAccessError]);
              end;
            end;
        searchpt^._previous := pt;
        // trying to access to the next item (searchpt._next)
        // Case 1 it is the last (no next)
        // Case 3 searchpt = self._start then it is added in first position
        // Case 4 => exception when trying to access searchpt^._next^._previous => Nothing to do (release pt)
        // trying to access to searchpt^._next^._previous
        if pt^._next = nil then begin // case 1
          self._end:=pt;
        end;
        if searchpt = self._start then begin // case 3
          self._start:=pt;
        end;
        searchpt := nil;
        inserted := true;
        // ***
        // * Add V1.6
        inc(self._count);
        // ***
      end else
      // ***
      // * Add v1.6
      begin
        if CompareText(_value, captioncomp) = 0 then begin
          searchpt := nil;
          inserted := true;
          // exiting without insertion then no inc of _count
        end else
      // *
      // * End Add V1.6
      // ***
        searchpt := searchpt^._next;
      end;
    end;
    // Case when pt was not inserted (because it is the higher value of text)
    // Then it becomes the last item
    if not inserted then begin
      try
        self._end^._next:=pt;
      except
        // pt must be released
        try
          Dispose(pt);

        finally
          // reset policy to its old value
          ReturnNilIfGrowHeapFails := nilerror ;
          // memory access error on an existing item ?
          // raised an exception
          raise IU_EStringDoubleChainedList_AccessError.Create(IU_ExceptionsMessages[IU_CurrentLang, K_IU_ExceptMSG_StringDoubleChainedList_CreateAccessError]);
        end;
      end;
      pt^._previous:=self._end;
      pt^._next := nil;
      self._end := pt;
      // ***
      // * Add V1.6
      inc(self._count);
      // ***
    end;
  end;
  // ***
  // * Del V1.6
  {
  // if no exception then item was added
  inc(self._count);
  }
  // ***
  // reset policy to its old value
  ReturnNilIfGrowHeapFails := nilerror ;
end;

// ***
// * Add V1.5
// ***
// * Adding a new string value in the list
// *
// * @author : Pascal Lemaître
// *
// * in : string to add
// *
// * if list is created with order flag or not the string is added at the begining of the list
// *
procedure IU_T_StringDoubleChainedList.addFirst(_value : string) ;      // Add a new record at the begining of the list (ordered list or not)
var
  pt :  IU_T_StringDoubleLinkPointer;
  nilerror : boolean;
begin
  // setting allocation error (nil returned not raise an error)
  nilerror := ReturnNilIfGrowHeapFails;
  ReturnNilIfGrowHeapFails := true;
  // Trying to allocate new item
  new(pt);
  if pt = nil then begin
    // Can't allocate new item
    // restore the old allocation error managment
    ReturnNilIfGrowHeapFails := nilerror;
    // Raise an execption
    // raise an exception
    raise IU_EStringDoubleChainedList_NoMoreAvailableMemory.Create(IU_ExceptionsMessages[IU_CurrentLang,K_IU_ExceptMSG_StringDoubleChainedList_NoMoreAvailableMemory]);
  end else begin
    // Trying to access the new item
    try
      pt^._next:=self._start;
      pt^._previous:=nil;
      pt^._caption:=_value;
    Except
      // unknow error memory was allocated, but item is unrechable ?
      // restore the old allocation error managment
      ReturnNilIfGrowHeapFails := nilerror;
      // trying to free new item
      try
        Dispose(pt);
      finally
        // reset policy to its old value
        ReturnNilIfGrowHeapFails := nilerror ;
        // memory access error on an existing item ?
        // raised an exception
        raise IU_EStringDoubleChainedList_AccessError.Create(IU_ExceptionsMessages[IU_CurrentLang, K_IU_ExceptMSG_StringDoubleChainedList_CreateAccessError]);
      end;
    end;
  if self._start <> nil then begin
    // Link with old first item
    try
      self._start^._previous:=pt;
    Except
      // unknow error memory was allocated, but item is unrechable ?
      // restore the old allocation error managment
      ReturnNilIfGrowHeapFails := nilerror;
      // trying to free new item
      try
        Dispose(pt);
      finally
        // reset policy to its old value
        ReturnNilIfGrowHeapFails := nilerror ;
        // memory access error on an existing item ?
        // raised an exception
        raise IU_EStringDoubleChainedList_AccessError.Create(IU_ExceptionsMessages[IU_CurrentLang, K_IU_ExceptMSG_StringDoubleChainedList_CreateAccessError]);
      end;
    end;
    self._start:=pt;
  end else begin
    // list was empty
    self._start:=pt;
    self._end := pt;
  end;
  inc(self._count);
  // restore the old allocation error managment
  ReturnNilIfGrowHeapFails := nilerror;
  end;
end;
// *
// * End Add V1.5
// ***


// ***
// * Return the number of items in the list
// *
// * @author : Pascal Lemaître
// *
// * return : Total number of items in the list
// *
function IU_T_StringDoubleChainedList.getItemsNumbers : integer; // return the number of items in the list
begin
  getItemsNumbers := self._count;
end;

// ***
// * Reverting the current read order of the list
// *
// * @author : Pascal Lemaître
// *
// * When the list is created the read order is from the begining to the end
// * First call of this procedure make the read order from the end to the begining
// * Another call change the read order from the begining to the end
// * And so on
// *
procedure IU_T_StringDoubleChainedList.revertReadOrder;          // change reading direction
                                                                 //   by default next move to next item from the anchor of the list
                                                                 //   in this case revertorder change the reading direction from the end to the anchor
                                                                 //   an other revertorder reset the default direction and so on...
begin
  self._defaultReadDirection:= not self._defaultReadDirection;
end;

// ***
// * Return the read Direction
// *
// * @author : Pascal Lemaître
// *
// * return true of is the standard reading direction else retrun false
// *
function IU_T_StringDoubleChainedList.isStandardReadOrder : boolean;       // Return true if is the standard read order else return false
begin
  isStandardReadOrder := self._defaultReadDirection;
end;


// ***
// * Adding (copy) an other list at the begining of this list (no destructive)
// *
// * @author : Pascal Lemaître
// *
// * in : other double chained list of string
// *
// * The list added is not destroyed. It still exists
// *
procedure IU_T_StringDoubleChainedList.addChain(_chain : IU_T_StringDoubleChainedList) ; // concat an other chain
var i : integer;
  pt : IU_T_StringDoubleLinkPointer;
  nilerror : boolean;
begin
  // if no item in _chain => Nothing to do
  if _chain._count > 0 then begin
    // saved policy when no more memory availlable
    nilerror := ReturnNilIfGrowHeapFails;
    // set policy to return nil and no raising exception
    ReturnNilIfGrowHeapFails := true ;
    // before to copy the _chain list items, its current selected item must be saved
    // and it must be restored after the add
    _chain.saveCurrent;
    // set the readOrder into the reverted direction
    //   Adding from the last to the start item because each item is added at the begining of the list
    if _chain.isStandardReadOrder then _chain.revertReadOrder;
    // rewind the chain to the last item
    _chain.rewind;
    for i := 1 to _chain._count do begin
      new (pt) ;
      // raised an exception if allocation failed (pt = nil)
      if pt=nil then begin
          // reset policy to its old value
          ReturnNilIfGrowHeapFails := nilerror ;

          // restore previous _chain status
          _chain.restoreCurrent;

          // raise an exception
          raise IU_EStringDoubleChainedList_NoMoreAvailableMemory.Create(IU_ExceptionsMessages[IU_CurrentLang,K_IU_ExceptMSG_StringDoubleChainedList_NoMoreAvailableMemory]);
      end;
      pt^._next:=self._start;
      pt^._previous := nil;
      // trying to copy string. if error procedure is stopped item is not added
      // but we don't know how many items have been copied
      try
        pt^._caption:=_chain.getText;
      except
        // pt must be released
        dispose(pt);
        // reset policy to its old value
        ReturnNilIfGrowHeapFails := nilerror ;
        // restore previous _chain status
        _chain.restoreCurrent;

        // memory access error on an existing item ?
        // raised an exception
        raise IU_EStringDoubleChainedList_AccessError.Create(IU_ExceptionsMessages[IU_CurrentLang, K_IU_ExceptMSG_StringDoubleChainedList_CopyAccessError]);
      end;
      // trying to chain the previous first item to the new one. if error procedure is stopped nothing change
      // but we don't know how many items have been copied
      try
        pt^._next^._previous:=pt;
      except
        // reset policy to its old value
        ReturnNilIfGrowHeapFails := nilerror ;
        // memory access error on an existing item ?
        // pt must be released
        dispose(pt);
        // restore previous _chain status
        _chain.restoreCurrent;

        // raised an exception
        raise IU_EStringDoubleChainedList_AccessError.Create(IU_ExceptionsMessages[IU_CurrentLang, K_IU_ExceptMSG_StringDoubleChainedList_CopyAccessError]);
      end;
      // chaining start to the new copied item
      self._start:=pt;
      // increment the number of item in the list
      inc(self._count);
    end;
    // restore the _chain list
    _chain.restoreCurrent;
    // reset policy to its old value
    ReturnNilIfGrowHeapFails := nilerror ;
  end;
end;
// *
// * End Add V1.4
// ***

// ***
// * Add V1.5
// ***
// * Directories List creator (always ordered)
// *
// * @author : Pascal Lemaître
// *
constructor IU_T_DirsList.Create ; // Constructor
begin
  inherited Create(true);
end;

procedure IU_T_DirsList.initDirectoriesList ; // Fullfill the list with the directories form the current dir
var
  R : integer;
  _directory : string;
  SearchRec: TSearchRec;
begin
  {$ifdef windows}
  _directory := GetCurrentDir + '\';
  {$else}
  _directory := GetCurrentDir + '/';
  {$endif}
  R := FindFirst(_directory + '*', faAnyFile, SearchRec);
  try
    while R = 0 do
    begin
      {$ifdef windows}
      if (SearchRec.Attr and 16) > 0 then
      {$else}
      if (SearchRec.Attr and $10) > 0 then
      {$endif}
        if (SearchRec.Attr and faHidden) = 0 then
          self.add(SearchRec.Name);
      R := FindNext(SearchRec);
    end;
  finally
    {$ifdef windows}
    sysutils.FindClose(SearchRec);
    {$else}
    FindClose(SearchRec);
    {$endif}
  end;
  // Add user home directory
  self.addFirst(GetUserDir);
  // Add / under linux or \ under windows
  {$ifdef windows}
  self.add('\');
  {$else}
  self.add('/');
  {$endif}
end;

constructor IU_T_FilesList.Create ; // Constructor
begin
  inherited Create(true);
end;

procedure IU_T_FilesList.addFilesInList(patern : string) ; // Adding new files with specific pattern in the list
var
  R : integer;
  _directory : string;
  SearchRec: TSearchRec;
begin
  {$ifdef windows}
  _directory := GetCurrentDir + '\' ;
  {$else}
  _directory := GetCurrentDir + '/';
  {$endif}
  R := FindFirst(_directory + patern, faAnyFile, SearchRec);
  try
    while R = 0 do
    begin
      {$ifdef windows}
      if (SearchRec.Attr and not 16) = SearchRec.Attr then
      {$else}
      if (SearchRec.Attr and not $10) = SearchRec.Attr then
      {$endif}
        if (SearchRec.Attr and faHidden) = 0 then
          self.add(SearchRec.Name);
      R := FindNext(SearchRec);
    end;
  finally
    {$ifdef windows}
    sysutils.FindClose(SearchRec);
    {$else}
    FindClose(SearchRec);
    {$endif}
  end;
end;
// *
// * End Add V1.5
// ***

// ***
// * Add v1.6
// *
// ***
// * Saving in user home dir and sub properties Tim dir the properties for a windows
// *
// @author : Pascal Lemaitre
// *
// * in : windowsname : the name of the windows
// * in : propertiesstructure : pointer on properties data for the windows
// * in : size of the properties strucure
// *
procedure writeProperties (windowsname : string ; propertiesStructure : Pchar ; _strucsize : integer);
var
  userhome : widestring;
  timpropertiesdir : widestring;
  filename : widestring;
  _File : File;
begin
  // 1 - getting user home
  userhome := GetUserDir;
  {$ifdef windows}
  timpropertiesdir := userhome + 'tim.ini';
  {$else}
  timpropertiesdir := userhome + '.tim';
  {$endif}
  // 1 - test if directory already exists
  if not directoryExists(timpropertiesdir) then begin
    // trying to creat dir
    if not createdir(timpropertiesdir) then
      raise IU_EFileError.Create(IU_ExceptionsMessages[IU_CurrentLang, K_IU_ExceptMSG_MkDirError]);
  end ;
  // 2 - calc filename
  {$ifdef windows}
  filename := timpropertiesdir + '\' + windowsname + '.ini' ;
  {$else}
  filename := timpropertiesdir + '/.' + windowsname + '.rc';
  {$endif}
  // 3 - trying to open file
  try
    assign(_File, filename);
    rewrite(_File);
  except
    raise IU_EFileError.Create(IU_ExceptionsMessages[IU_CurrentLang, K_IU_ExceptMSG_CreateFileError]);
  end;
  // 5 - trying to read record
  try
    blockwrite(_File, propertiesStructure^, _strucsize);
  except
    // if error closing file and raise an error
    try
      close(_File);
    finally
      raise IU_EFileError.Create(IU_ExceptionsMessages[IU_CurrentLang, K_IU_ExceptMSG_FileWriteError]);
    end;
  end;
  // 6 - close file
  try
    close(_File);
  finally
  end;
end;

// ***
// * Saving in user home dir and sub properties Tim dir the properties for a windows
// *
// @author : Pascal Lemaitre
// *
// * in : windowsname : the name of the windows
// * out : propertiesstructure : pointer on properties data for the windows
// * in : size of the properties strucure
// *
procedure readProperties (windowsname : string ; propertiesStructure : Pchar ; _strucsize : integer);
var
  userhome : widestring;
  timpropertiesdir : widestring;
  filename : widestring;
  _File : File;
begin
  // 1 - getting user home
  userhome := GetUserDir;
  {$ifdef windows}
  timpropertiesdir := userhome + 'tim.ini';
  {$else}
  timpropertiesdir := userhome + '.tim';
  {$endif}
  // 1 - test if directory already exists
  if directoryExists(timpropertiesdir) then begin
    // 2 - calc filename
    {$ifdef windows}
    filename := timpropertiesdir + '\' + windowsname + '.ini' ;
    {$else}
    filename := timpropertiesdir + '/.' + windowsname + '.rc';
    {$endif}
    // 3 - testing if file exists
    if fileexists(filename) then begin
      // 4 - trying to open file
      try
        assign(_File, filename);
        reset(_File);
      except
        raise IU_EFileError.Create(IU_ExceptionsMessages[IU_CurrentLang, K_IU_ExceptMSG_FileReadError]);
      end;
      // 5 - trying to read record
      try
        blockread(_File, propertiesStructure^, _strucsize);
      except
        // if error closing file and raise an error
        try
          close(_File);
        finally
          raise IU_EFileError.Create(IU_ExceptionsMessages[IU_CurrentLang, K_IU_ExceptMSG_FileReadError]);
        end;
      end;
      // 6 - close file
      try
        close(_File);
      finally
      end;
    end else raise IU_EFileError.Create(IU_ExceptionsMessages[IU_CurrentLang, K_IU_ExceptMSG_FileNotExists]);
  end else raise IU_EFileError.Create(IU_ExceptionsMessages[IU_CurrentLang, K_IU_ExceptMSG_DirNotExists]);
end;

// * End Add v1.6
// ***

end.



