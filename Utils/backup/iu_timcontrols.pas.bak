unit IU_TimControls;

{$mode objfpc}{$H+}
// ***
// * Unit defining controls objects like progress bar, scroll bars, tracks bars...
// * Creation date : 2017 November
// *
// * Version : 0.13
// * Version Date : 2018 January
// * Version contributors : Pascal Lemaître
// *
// * V0.13 : Adding default colors for buttons
// *
// * V0.12 : Adding autofocus on the parent form of personnal controls
// *
// * V0.11 : Update focus management. Now when mouse leave a control without going on other focus, the control stay selected
// *
// * V0.10 : Add noselection flag on radio button. If noselection is set to true then
// *         a radio button can be de selected else it can't
// *
// * V0.9 : Add Hint commands on personnal controls
// *
// * V0.8 : Add comments on how to use controls in a interface section
// *
// * V0.7 : Creation of checkbox and radio buton Caption alignement
// *
// * V0.6 : Adding groups for RB (Radio Buttons)
// *
// * V0.5 : Adding Radio button (not group of radio button only RB)
// *
// * V0.4 : Adding queue for manage focus order between interactives graphics controls
// *
// * V0.3 : Adding a parent class for ID management.
// *        Impacting only Track Bar, Scroll Bar and Check Box
// *
// * @authors : Pascal Lemaître
// *
// * @see :
// *
// * Team : TIm (Traitement d'IMages)
// *
// * 2017-2018
// ***

interface

uses
  Classes, SysUtils, Forms, ExtCtrls, Graphics, StdCtrls, Controls,
  IU_GeneralUtils, Math,
  // ***
  // * Add V0.4
  // *
  IU_Exceptions, IU_I18N_Messages
  // ***
  ;

const
  // ***
  // * Personnal progress bar object
  // *
  // * @author : Pascal Lemaître
  // *
  // * Constant for defining a parametring apparence of progress bar
  // *
  // ***
  K_IU_PB_NbIndicators = 10 ; // Number of indicators of progression
  {$IFDEF Windows}
  K_IU_PB_DiffTopTextTopPB = -7; // top difference between text label and shapes
  {$ELSE}
  K_IU_PB_DiffTopTextTopPB = -5; // top difference between text label and shapes
  {$ENDIF}
  K_IU_PB_ShapeHeight = 10; // Height of shape
  K_IU_PB_TextHeight = 10 ; // Height of the text
  K_IU_PB_MinShapeWidth = 5 ; // Minimal width size for a shape
  K_IU_PB_WidthMarging = 10; // marging to right and left border
  K_IU_Space = 2 ; // space between shapes
  K_IU_TextSep = 5 ; // separation between percent text indicator and first shape
  // *
  // ***

  // ***
  // * Personnal track bar object
  // *
  // * @author : Pascal Lemaitre
  // *
  // * Constant for defining and parametring apparence and comportment of track bar
  // *
  // ***
  // Spline function is :
  //   (a.(x.Base)^3 + b.(x.Base)^2+c.(x.Base)+d)/SCale
  // Width :
  K_IU_TB_SplineBase = 1.0 ; // Base of x variation for cubic spline function
  K_IU_TB_a = -0.7 ;         // a coef for x^3 in cubic spline function
  K_IU_TB_b = 2.5 ;          // b coef for x^2 in cubic spline function
  K_IU_TB_c = -3 ;           // c coef for x in cubic spline function
  K_IU_TB_d = 1.2 ;          // d constant in cubic spline function
  K_IU_TB_Scale = 1.2 ;      // scale coef for set y in [0..1] for x in [-1..1]
  // *
  // * Constants for curves design
  K_IU_TB_LineWidth = 1;     // Width for a line
  K_IU_TB_LineSepWidth = 1;  // Separation width between two lines
  K_IU_TB_CursorScale = 0.75; // Apparence of the cursor's curve
  K_IU_TB_EditTextSize = 10; // Text size in Edit box for TB value
  K_IU_TB_ScaleWidth = 3 ; // Size of a scale line
  K_IU_TB_ScaleSep = 20 ; // Size betwseen two scale line
  // *
  // * Constants for keyboard managment
  K_IU_LittleLeft = 37 ;
  K_IU_LittleRight = 39 ;
  K_IU_LittleUp = 38 ;
  K_IU_LittleDown = 40;
  K_IU_BigUp = 33;
  K_IU_BigDown = 34;
  // *
  // * Constant for scoll bar highlight
  K_IU_ScrollBar_HighLight = $10;
  // *
  // * Constant for radio or check bo
  _PI = 3.14116;


  // ***
  // * Box text size
  K_IU_BoxTextSize = 10;
  // *
  // ***

  // ***
  // Comment added in V0.8
  // Under windows and some linux garphical env marging when there is no caption
  // has set like there is a caption. And on some graphicals env marging change
  // Then some constants are needed with differents value under windows and linux
  {$IFDEF Windows}
  // * Top margin of TGroupBox
  K_IU_TGPBX_TopMarging = 25;
  {$ELSE}
  K_IU_TGPBX_TopMarging = 0;
  // ***
  // * Add V0.7
  K_IU_TGPBX_TopMargingNotEmptyCaption = 16;
  // *
  // * End Add V0.7
  // ***
  {$ENDIF}

type
  // ***
  // * Comment added in V0.8
  // * Progress bar is dynamicaly created with TShape and TLabel
  // * Tshapes are the graphical indicators
  // * TLabel is the percent value of the pregression
  // * Progress indicators change colors when progress is near 100%
  // * Changing color are defined in the initializing section of the unit
  T_IU_Progress_Line = record
    _picto : array [1..K_IU_PB_NbIndicators] of TShape ;
    _caption : TLabel;
  end;
  T_IU_Progress_Line_Color = record
    red, green, blue : byte;
  end;


    // ***
    // * Comment added in V0.8
    // * A queue is needed to manage the focus on controls.
    // * This manager take the control of tab and shift tab key for changing the
    // * controls focus.
    // * If controls are registred in the queue as an Object a sigsev error occure
    // * at the execution. Then the queue can only recording the index of controls
    // * Each win control are registring in the parent form in an array of control
    // * The index of the array is an integer
    // * Each personnal control has an ID. The ID is an word.
    // * The queue get wincontrol ID and Personnal control ID in a variant record
    // * The queue manager is a dynamical array of BaseQueueRecord
    T_IU_BaseQueueRecord = record
      _twincontrol : boolean ;
      case boolean of
        true : (wcontrol : integer);
        false : (IU_Control : word);
    end;

    // ***
    // * Comment added in V0.8
    // * The queue manager alloc a dynamic array of BaseQueueRecord with dynamic memory allocation
    // * Then the queue manager need to have a pointer on BaseQueueRecord
    T_IU_BaseQueueRecordPTR = ^T_IU_BaseQueueRecord;

    // ***
    // * Personnal Base class for queued controls
    // *
    // * @author : Pascal Lemaître
    // *
    // * Queue manager for set the focus on interactives controls
    // *
    // ***
    // ***
    // * Comment added in V0.8
    // * Control queue manager register all controls (win control and personnal control) in a form
    // * It manage the focus with tab and shift tab
    // * All controls managed by it must be registered in a parent form with the Add method
    // * The add order define the order of the focus
    // * At the creation of the queue manager the number of controls must be know (for dynamic memeory allocation)
    // * When a win control get the focus with mouse event the Update method must be call with the index of the focus in it parent form
    // * For setting the focus to the frst control in the queue rewind method can be call
    // * MoveNext set the next control in the queue to get the focus (but not give the focus to it)
    // * MovePrevious set the previous control on the queue to get teh focus (but not give the focus to it)
    // * setCurrent set the control on the queue for control whose catch the focus (with mouse event for exemple). Is used to synchronise queue with keyboard actions and mouse actions
    // * to give the focus on the selected control in the queue, the setFocus method must be call. form parameter is the form where the control is declared
    T_IU_ControlsQueue = class (TObject)
      private
        _NbObjects : word ; // Number of object to queue
        _TotalAdded : word ; // number of added objects may be less tanh NbObjects
        _queue : ^T_IU_BaseQueueRecord; // Anchor of the queue
        _currentID : word; // Current selected Control in the queue
        procedure synchronizeQueue ; // Synchronize with mouse event
      public
        constructor Create (NbObjects : word);
        destructor Destroy ;

        procedure Add(ObjectPTR : integer); // Adding a TWinControl Child (must be casted)
        procedure Add(ObjectPTR : word) ; // Adding a personnal control child (must be casted)

        procedure MoveNext ; // Move to next control in the queue
        procedure MovePrevious ; // Move to previous control in the queue

        procedure Update(id : integer) ; // When TWinControl focused
        procedure Rewind ; // When form is showed

        procedure setCurrent(id : integer); // set current control for TWinControl when it get the focus
        procedure setCurrent(id : word); // set current control for T_IU_BaseControlObject when it get the focus

        procedure setFocus (var form : TForm) ; // Set the focus on the current control in the queue (auto adapt to the control type)

    end;

    T_IU_ControlsQueuePTR = ^T_IU_ControlsQueue ;


  // ***
  // * Personnal Base class for command control personnal cursors
  // *
  // * @author : Pascal Lemaître
  // *
  // * This class is the ancestor for controls that user can interact with
  // *
  // ***
  // ***
  // * Comment added in V0.8
  // * The BaseControlObject is the base of presonnal control identification for selection and focus operations
  // * All personnals controls are specialization of this class
  // * for selecting a personnal control, the select methos must be call
  // * for unselecting a personnal control, the unselect method could be call, but calling a select method of an other control
  // * unselect to
  // * to know if a control is selected, the function isSelected return true if yes and false if no
  // *
  // * This class is used by queue control manager
  T_IU_BaseControlObject = class(TObject)
    _ID : word ; // Internal ID of the cursor
    procedure select ; // Select like set focus
    procedure unselect ; // unselect like defocus
    function isSelected : boolean ; // return if control is selected

    constructor Create ;  // constructor
    destructor Destroy ; // destructor
    public
      function _getID : word ; // return the ID
  end;

  //*
  // ***

  // ***
  // * Personnal progress bar object
  // *
  // * @author : Pascal Lemaître
  // *
  // * This is to show a work progression.
  // * Aspect under various versions of OS or Linux is diferent.
  // * It is necessary to define own progress bar if we want to have the same aspect on
  // * all targets systems
  // *
  // ***
  // ***
  // * Comment added in V0.8
  // * Progress bar is a personnal control that can't catch the focus
  // * It's the only personnal object that do not specialize BaseControlObject
  // * ProgressBar is include in a TWinControl like a TGroupBox
  // * The control automaticaly create a % caption and graphics indicators to show the progression
  // * It need to be initialized first
  // * To change the pregression status, setPercent method must be call with the percent value of the progression
  // *   percent value is an real then no integers values are accepted. Higher the precision is, smoother the progression is.
  // * When the parent wincontrol size change, then the resize method must be call to readapt the control to the new size of its parent
  T_IU_ProgressBar = class(TObject)
    private
      _percent : real;
      upLimit1, upLimit2, upLimit3 : integer;
      percentByindicator : extended;
    public
      _progressLine : T_IU_Progress_Line;

      procedure _init;
      procedure _draw;
      procedure _setPercent(_in_percent : real) ;
      procedure _resize (var form : TWinControl);
      constructor Create (var Form : TWinControl);
      destructor Release;

      // ***
      // Add V0.9
      // *
      procedure setHint(hintSTR : string); // set the hint text
      procedure activateHint ; // activate Hint display when mouse over the control
      procedure deactivateHint ; // deactivate Hint. Nothing display when mouse over the control
      // ***

  end;

  // ***
  // * Personnal track bar object
  // *
  // * @author : Pascal Lemaître
  // *
  // * This is to set a value in interval [start_value..end_Value].
  // * Aspect under various versions of OS or Linux is diferent.
  // * It is necessary to define own track bar if we want to have the same aspect on
  // * all targets systems
  // *
  // ***
  // * Types dedicated for tracks bars
  // ***
  // * Comment added on V0.8
  // * Track bar can have many direction and there cursors too.
  // * The direction of the track bar horizontal or vertiacal is given by the TImage object height and width proportion for drawing the cursor
  // * The cursor can be in the standard orientation (by default top orientation for horizontal TB and right for vertical TB)
  // * this standard orientation can be inverted.
  T_IU_TB_CursorDirection = (iu_tb_cursordirection_standard, iu_tb_cursordirection_inverted);
  T_IU_TB_Direction = (iu_tb_vertical, iu_tb_horizontal);

  // Call backs signs for bars
  T_IU_TB_onChange = Procedure (ID : word ; _currentValue : extended);
  T_IU_TB_onChangePTR = ^T_IU_TB_onChange;

  // Call Backs signs for radio or check box
  T_IU_Box_onChange = procedure (ID : word ; _currentSelected : boolean);
  T_IU_Box_onChangePTR = ^T_IU_Box_onChange;

  // ***
  // * Comment added in V0.8
  // * Track bar can be created with or without a TEdit box to show and get position.
  // * TB specialize BaseControlObject to manage the focus with keyboard action
  // * TB must be initialized before its first use with _init method
  // * TB can call a callback when the value of its cursor change (see upper the callback declaration)
  // * TB can be manipulated by mouse (mouse click and move and mouse wheel)
  // * TB can be manipulated with keybord (in this case the form where TB is in must have its keypreview property set to true
  // *    and its OnKeyDown event must call _setValueFromKeyboard method of the TB
  // * The internal interval of value, and display values are managed with dedicated methods (see bellow)
  // * When the size of the form where is the TB change, the method resize must be call
  // * All onEvent methods are automaticaly managed when the TB is created. Nothing to do except for keyboard management.
  T_IU_TrackBar = class(T_IU_BaseControlObject)
    private
      _direction : T_IU_TB_Direction ; // Orientation of the track bar horizontal or vertical
      _cursorOrientation : T_IU_TB_CursorDirection ; // standard bottom to top for horizonatl TB
                                                     // standard left to right for vertical TB
      _OutputInterval : int64 ; // Interval of values for output
      _InternalInterval : integer ; // Internal precision of the track bar must be width or height of the TB
                                    // Depending if it is a horizontal or a vertical TB
      _InternalHeight : integer ; // internal relative height of the TB
      _minDelta : extended; // Min delta of variation according to _OutputInterval and _width of the control
      _currentvalue : extended; // position of the cursor in area of the track bar
      _width : integer ; // Needed for set the number of position lines
                         // and to calculate _currentvalue on mouse mouve
      _height : integer ; // Needed for drawing
      Parent : TImage; // for drawing cursor
      Edition : TEdit ; // for writing and readin position into _OutputInterval references
      _sharpDelta, _largeDelta : extended ; // for cursor adjustments with keybord.
      _DisplayStartingValue : extended ; // First value displayed when cursor is on the left
      _DisplayNumBerOfDecimalsPlace : integer ; // How many decimal places should be displayed


      function convertGetingDisplayValue (_getValue : string) : extended ; // Converting get value from Edit box into _currentvalue
      function convertCurrentValueForDisplaying : string ; // Converting _currentvalue into displayed format

    public

      onChange : T_IU_TB_OnChange ; // Call back for TB when its value is changing

      procedure _init;
      procedure _draw;
      procedure _setValue(_mouse_position : integer) ;
      procedure _setValue(_value : extended);
      procedure _setValueInInterval(_value : int64);
      procedure _setInterval(_value : int64);
      procedure _setNumberOfDecimalPlace (_value : integer);
      procedure _setStartingValue (_value : extended);
      procedure _setValueOnWheelDelta(WheelDelta : integer);
      procedure _setValueFromKeyboard(var _key : word);
      function _getValue : extended;
      function _getInterval : extended;
      function _GetValueInInterval : extended;

      procedure _setSelectedTB;
      procedure _unsetSelectedTB;
      procedure _resize;
      constructor Create(cursorOrientation : T_IU_TB_CursorDirection ; interval : integer ; var form : TImage ; var edit : TEdit);
      constructor Create(cursorOrientation : T_IU_TB_CursorDirection ; interval : integer ; var form : TImage);
      destructor Release;

      // ***
      // Add V0.9
      // *
      procedure setHint(hintSTR : string); // set the hint text
      procedure activateHint ; // activate Hint display when mouse over the control
      procedure deactivateHint ; // deactivate Hint. Nothing display when mouse over the control
      // ***

      // ***
      // * procedures to response on events of visual components
      procedure EditKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
      procedure ParentMouseDown(Sender: TObject; Button: TMouseButton;
        Shift: TShiftState; X, Y: Integer);
      procedure ParentMouseEnter(Sender: TObject);
      procedure ParentMouseLeave(Sender: TObject);
      procedure ParentMouseMove(Sender: TObject; Shift: TShiftState; X,
        Y: Integer);
      procedure ParentMouseWheel(Sender: TObject; Shift: TShiftState;
        WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
      procedure ParentPaint(Sender: TObject);
      procedure ParentChangeBounds(Sender: TObject);
      // *
      // ***

  end;


  // ***
  // * Personnal scroll bar object
  // *
  // * @author : Pascal Lemaitre
  // *
  // * Constant for defining and parametring apparence and comportment of scroll bar
  // *
  // ***
  // ***
  // * Comment added in V0.8
  // * Scroll Bar specialize BaseControlObject to manage the focus with keyboard action
  // * SB must be initialized before its first use with _init method
  // * SB can call a callback when the value of its cursor change (see upper the callback declaration)
  // * SB can be manipulated by mouse (mouse click and move and mouse wheel)
  // * SB can be manipulated with keybord (in this case the form where SB is in must have its keypreview property set to true
  // *    and its OnKeyDown event must call _setValueFromKeyboard method of the SB
  // * The internal interval of value, and display values are managed with dedicated methods (see bellow)
  // * When the size of the form where is the SB change, the method resize must be call
  // * All onEvent methods are automaticaly managed when the SB is created. Nothing to do except for keyboard management.
  T_IU_ScrollBar = class(T_IU_BaseControlObject)
    private
      _direction : T_IU_TB_Direction ; // Orientation of the track bar horizontal or vertical
      _cursorOrientation : T_IU_TB_CursorDirection ; // standard bottom to top for horizonatl TB
                                                     // standard left to right for vertical TB
      _OutputInterval : int64 ; // Interval of values for output
      _InternalInterval : integer ; // Internal precision of the track bar must be width or height of the TB
                                    // Depending if it is a horizontal or a vertical TB
      _InternalHeight : integer ; // internal relative height of the TB
      _minDelta : extended; // Min delta of variation according to _OutputInterval and _width of the control
      _currentvalue : extended; // position of the cursor in area of the track bar
      _width : integer ; // Needed for set the number of position lines
                         // and to calculate _currentvalue on mouse mouve
      _height : integer ; // Needed for drawing
      Parent : TImage; // for drawing cursor
      _sharpDelta, _largeDelta : extended ; // for cursor adjustments with keybord.
      _DisplayStartingValue : extended ; // First value displayed when cursor is on the left
      _DisplayNumBerOfDecimalsPlace : integer ; // How many decimal places should be displayed

    public

      onChange : T_IU_TB_OnChange ; // Call back for TB when its value is changing

      procedure _init;
      procedure _draw;
      procedure _setValue(_mouse_position : integer) ;
      procedure _setValue(_value : extended);
      procedure _setValueInInterval(_value : int64);
      procedure _setInterval(_value : int64);
      procedure _setStartingValue (_value : extended);
      procedure _setValueOnWheelDelta(WheelDelta : integer);
      procedure _setValueFromKeyboard(var _key : word);
      function _getValue : extended;
      function _getInterval : extended;
      function _GetValueInInterval : extended;

      procedure _setSelectedTB;
      procedure _unsetSelectedTB;
      procedure _resize;
      constructor Create(cursorOrientation : T_IU_TB_CursorDirection ; interval : integer ; var form : TImage);
      destructor Release;

      // ***
      // Add V0.9
      // *
      procedure setHint(hintSTR : string); // set the hint text
      procedure activateHint ; // activate Hint display when mouse over the control
      procedure deactivateHint ; // deactivate Hint. Nothing display when mouse over the control
      // ***

      // ***
      // * procedures to response on events of visual components
      procedure ParentMouseDown(Sender: TObject; Button: TMouseButton;
        Shift: TShiftState; X, Y: Integer);
      procedure ParentMouseEnter(Sender: TObject);
      procedure ParentMouseLeave(Sender: TObject);
      procedure ParentMouseMove(Sender: TObject; Shift: TShiftState; X,
        Y: Integer);
      procedure ParentMouseWheel(Sender: TObject; Shift: TShiftState;
        WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
      procedure ParentPaint(Sender: TObject);
      procedure ParentChangeBounds(Sender: TObject);
      // *
      // ***

  end;


  // ***
  // * Personnal check box object
  // *
  // * @author : Pascal Lemaitre
  // *
  // ***
  // ***
  // * Comment added in V0.8
  // * Check Box specialize BaseControlObject to manage the focus with keyboard action
  // * CB is auto initialized at its creation
  // * CB can call a callback when its value change (see upper the callback declaration)
  // * CB can be manipulated by mouse (mouse click)
  // * CB can be manipulated with keybord (in this case the form where CB is in must have its keypreview property set to true
  // *    and its OnKeyDown event must call _setValueFromKeyboard method of the CB
  // * When the size of the form where the CB is change, then the form itself must manage the new position of the CB
  // * All onEvent methods are automaticaly managed when the CB is created. Nothing to do except for keyboard management.
  T_IU_CheckBox = class(T_IU_BaseControlObject)
    private
      _box : TImage ; // for drawing check box state
      _caption : TLabel ; // for caption and adding all event ans setting styles

      checked : boolean ; // check box checked or not

    public
      onChange : T_IU_Box_onChange; // onChange Call back

      procedure setValue(_selected : boolean);
      function getValue : boolean;
      procedure _setValueFromKeyboard(var _key : word);

      constructor Create(var _check : TImage ; var _label : TLabel);
      destructor Release;

      procedure _draw;

      // ***
      // Add V0.9
      // *
      procedure setHint(hintSTR : string); // set the hint text
      procedure activateHint ; // activate Hint display when mouse over the control
      procedure deactivateHint ; // deactivate Hint. Nothing display when mouse over the control
      // ***

      // ***
      // * procedures to response on events of visual components
      procedure MouseDown(Sender: TObject; Button: TMouseButton;
        Shift: TShiftState; X, Y: Integer);
      procedure MouseEnter(Sender: TObject);
      procedure MouseLeave(Sender: TObject);
      procedure Paint(Sender: TObject);
      // *
      // ***
  end;


  T_IU_RB_GroupPTR = ^T_IU_RB_Group;
  // ***
  // * Personnal radio button object
  // *
  // * @author : Pascal Lemaitre
  // *
  // ***
  // ***
  // * Comment added in V0.8
  // * Radio Button specialize BaseControlObject to manage the focus with keyboard action
  // * RB is auto initialized at its creation
  // * RB can be included into a RB group. Then when a RB is checked in a group the last RB checked is automaticaly unchecked
  // * RB can call a callback when its value change (see upper the callback declaration)
  // * RB can be manipulated by mouse (mouse click)
  // * RB can be manipulated with keybord (in this case the form where RB is in must have its keypreview property set to true
  // *    and its OnKeyDown event must call _setValueFromKeyboard method of the RB
  // * When the size of the form where the RB is change, then the form itself must manage the new position of the RB
  // * All onEvent methods are automaticaly managed when the RB is created. Nothing to do except for keyboard management.
  T_IU_RadioButton = class(T_IU_BaseControlObject)
    private
      _box : TImage ; // for drawing check box state
      _caption : TLabel ; // for caption and adding all event ans setting styles

      checked : boolean ; // check box checked or not

      GroupPTR : T_IU_RB_GroupPTR; // Need for calling group method when RB is checked

      // ***
      // * Add V0.10
      canUnselect : boolean; // if true radio button can be unselect else not
      // ***

    public
      onChange : T_IU_Box_onChange; // onChange Call back

      procedure setValue(_selected : boolean);
      function getValue : boolean;
      procedure _setValueFromKeyboard(var _key : word);

      constructor Create(var _check : TImage ; var _label : TLabel);
      destructor Release;

      procedure _draw;

      // ***
      // Add V0.9
      // *
      procedure setHint(hintSTR : string); // set the hint text
      procedure activateHint ; // activate Hint display when mouse over the control
      procedure deactivateHint ; // deactivate Hint. Nothing display when mouse over the control
      // ***

      // ***
      // Add v0.10
      procedure setUnselect(_status : boolean) ; // true radio button can be unselected with mouse click or with keyboard action
                                                 // false radio button can be unselected with mouse click ou keyboard action
      // ***

      // ***
      // * procedures to response on events of visual components
      procedure MouseDown(Sender: TObject; Button: TMouseButton;
        Shift: TShiftState; X, Y: Integer);
      procedure MouseEnter(Sender: TObject);
      procedure MouseLeave(Sender: TObject);
      procedure Paint(Sender: TObject);
      // *
      // ***

  end;


  T_IU_RadioButtonPTR = ^T_IU_RadioButton ;

  // ***
  // * Personnal Base class for groups of RB
  // *
  // * @author : Pascal Lemaître
  // *
  // * Group manager for manage checked coherence beetwen RB in the group
  // *
  // ***
  // ***
  // * Comment added in V0.8
  // * Radio Button Group can't get focus then it specialize TObject
  // * RBG is a set of RB. When a RB in the group is checked, the previous checked is autoaticaly unchecked
  // * RB must be added in a RBG with the Add method
  // * Nothing else to do.
  // * Update of the RB in the RBG is automaticaly made.
  T_IU_RB_Group = class (TObject)
    private
      _NbObjects : word ; // Number of object to queue
      _TotalAdded : word ; // number of added objects may be less tanh NbObjects
      _RB : ^T_IU_RadioButtonPTR; // Anchor of the RadioButton included in the group

    public
      constructor Create(nbRB : word); // Creator of the group. Need to know max number of RB in the group
      destructor Destroy;

      procedure Add(Group : T_IU_RB_GroupPTR ; RBPTR : T_IU_RadioButtonPTR); // add a new Radio Button in the group

      procedure updateRB(Group : T_IU_RB_GroupPTR ; RBCaller : T_IU_RadioButtonPTR); // need to set of RB in the group except the caller if it set to checked

  end;


  var
      // ***
      // * Comment added in V0.8
      // * All colors var decalaration made bellow are intitalizated in the begin end. section of the unit
      // * It is the best way to personnalize colors apparance of the personnals controls

      V_IU_DefaultBG : TColor ; // default color BG color
      V_IU_PB_DefaultBGColor : T_IU_Progress_Line_Color ; // Default color of the shape (not activated)
      V_IU_PB_ActivateBG : T_IU_Progress_Line_Color ; // activated color of the shape
      V_IU_PB_DistantEndBG : T_IU_Progress_Line_Color ; // activated color when end is always distant
      V_IU_PB_NearEndBG : T_IU_Progress_Line_Color ; // activated color when end is near
      V_IU_PB_EndBG : T_IU_Progress_Line_Color ; // acivated color when end
      V_IU_PB_TextColor : TColor ; // Color of text
      V_IU_TB_DefaultBG : TColor ; // Color of unactived curve for track Bar
      V_IU_TB_ActivatedCurve : TColor ; // Color for activated cirve for ftrack bar
      V_IU_TB_TextColor : TColor ; // Color of the text into the Edit box of the Track Bar
      V_IU_TB_TextBGColor : TColor ; // Back Ground Color of Edit box of the track bar
      V_IU_TB_SelectedActiviatedCurve : TColor ; // Color of the cursor of the activated curve
      V_IU_TB_ScaleColor : TColor ; // Color of a scale line
      V_IU_TB_unselectedScaleColor : TColor ; // Color of a scale line for unselected Track bar

      V_IU_PB_CoefHihgLight_Shape : real ; // Light of each shape is variant with percent value
                                           // A shape cover an interval of percent 4% for example
                                           // then when 1% is reached lum of shape is low
                                           // and when 4% is reached lum of shape is max


      V_IU_SB_DefaultBGColor : T_IU_Progress_Line_Color ; // ScrollBar default indicators colors

      V_IU_SB_CursorColor : T_IU_Progress_Line_Color ; // Default color of scroll bar cursor
      V_IU_SB_CursorColorHigh : T_IU_Progress_Line_Color ; // HighLight color of scroll bar cursor

      V_IU_Box_BGColor : T_IU_Progress_Line_Color ;           // back ground color of check part for CB
      V_IU_Box_DefaultColor : T_IU_Progress_Line_Color ;      // default color for unchecked
      V_IU_Box_ColorHigh : T_IU_Progress_Line_Color;          // highlight color for unchecked
      V_IU_Box_Color_Checked : T_IU_Progress_Line_Color;      // default color for checked
      V_IU_Box_Color_CheckedHigh : T_IU_Progress_Line_Color;  // highlight color for checked
      V_IU_Box_TextColorDefault : T_IU_Progress_Line_Color;   // Default text color
      V_IU_Box_TextColorHigh : T_IU_Progress_Line_Color;      // higlight text color

      // ***
      // * Add v0.13
      // *
      V_IU_Button_unfocused : T_IU_Progress_Line_Color;
      V_IU_Button_focused : T_IU_Progress_Line_Color;
      // *
      // * End Add v0.13
      // ***


implementation

// ***
// * Unit internal variables
// *

var

    _IU_TimControls_indexID : word ; // Index of personnal control created must be init to 0 when program starts
    _IU_TimControls_selectedID : word ; // Current ID of the selected control must be init to 0 when program starts

// ***
// * Personnal Base class for command control personnal cursors : constructor
// *
// * @author : Pascal Lemaître
// *
// * Init internal _ID of the control
// *
// ***
 constructor T_IU_BaseControlObject.Create ;
begin
  inherited;
  inc (_IU_TimControls_indexID);
  _ID := _IU_TimControls_indexID;
end;

// ***
// * Personnal Base class for command control personnal cursors : select (setFocus)
// *
// * @author : Pascal Lemaître
// *
// * Select control as active control like setFocus
// *
// ***
procedure T_IU_BaseControlObject.select ; // Select like set focus
begin
  _IU_TimControls_selectedID := _ID;
end;

// ***
// * Personnal Base class for command control personnal cursors : unselect (unFocus)
// *
// * @author : Pascal Lemaître
// *
// * unSelect control as active control like unFocus
// *
// ***
procedure T_IU_BaseControlObject.unselect ; // unSelect like unfocus
begin
  _IU_TimControls_selectedID := 0;
end;


// ***
// * Personnal Base class for command control personnal cursors : isSelected (like has the focus ?)
// *
// * @author : Pascal Lemaître
// *
// * return true if has the focus then return false
// *
// ***
function T_IU_BaseControlObject.isSelected : boolean ; // return if control is selected
begin
  isSelected := _ID = _IU_TimControls_selectedID;
end;

// ***
// * Personnal Base class for command control personnal cursors : destructor
// *
// * @author : Pascal Lemaître
// *
// ***
destructor T_IU_BaseControlObject.Destroy ; // destructor
begin
  inherited;
end;


// ***
// * Personnal Base class for command control personnal cursors : getId of control
// *
// * @author : Pascal Lemaître
// *
// * return the ID of the control
// *
// ***
function T_IU_BaseControlObject._getID : word ; // return the ID
begin
  _getID := _ID;
end;


// ****************************************************

// ***
// * Personnal bars objects : internal spline function
// *
// * @author : Pascal Lemaître
// *
// * Personal track bars and scroll bars has a particular form.
// * Position of the seted value is represented by a curve like a gaussian curve
// * spline calculate the curve
// *
// ***
function splineBars (x : extended) : extended ;
var k, ret : extended;
begin
  // definition set of spline is [-1..1]
  k := abs(x / K_IU_TB_SplineBase);
  if k > 1 then ret := 0
  else ret := ((k*k*k*K_IU_TB_a) + (k*k*K_IU_TB_b) + (k*K_IU_TB_c) + K_IU_TB_d) / K_IU_TB_scale;
  splineBars := ret;
end;


// *****************************************


// ***
// * Personnal progress bar object constructor
// *
// * @author : Pascal Lemaître
// *
// * This constructor creat the object and add it to a form and init all values
// *
// * in out : the form which contains the progress bar
// *
// ***
constructor T_IU_ProgressBar.Create (var Form : TWinControl);
var i : integer;
  _textsize, _startTextPos, _shapesize, _startShapePos : integer;
  _topText, _topShapes : integer;
begin
  // init private var
  percentByindicator :=  100 / K_IU_PB_NbIndicators ;
  upLimit1 := round(0.80 * K_IU_PB_NbIndicators);
  upLimit2 := round(0.87 * K_IU_PB_NbIndicators);
  upLimit3 := round(0.95 * K_IU_PB_NbIndicators);
  // Defining the width of each shape
  // There are K_IU_PB_NbIndicators shapes
  // each must be separated by some pixels
  // The size of the text should be enought to write 100% without recovering the first shape
  // 1- text size
  // 4 digits of 12 pixels width + K_IU_Space of separation
  _textsize := 4 * 14 ;
  // 2- Shape size
  // K_IU_PB_NbIndicators shapes
  // 10 pixels of marging to the left and the right of the form
  // 5 pixels between each shapes
  _shapesize := trunc((Form.Width - _textsize - K_IU_PB_WidthMarging * 2 - K_IU_Space * K_IU_PB_NbIndicators) / K_IU_PB_NbIndicators);
  // Test if it is smaller than the minimal width
  if _shapesize < K_IU_PB_MinShapeWidth + K_IU_Space then _shapesize := K_IU_PB_MinShapeWidth ;
  // Defining text position from the left border of the form
  _startTextPos := 10;
  // Definig the first shape position from the left border of the form
  _startShapePos := _startTextPos + K_IU_TextSep ;
  // 3- Top of shapes and text
  // ***
  // * Add V0.7
  {$IFDEF Windows}
  _TopShapes := (Form.Height - K_IU_PB_ShapeHeight - K_IU_TGPBX_TopMarging) div 2;
  _TopText := (Form.Height - K_IU_PB_TextHeight + K_IU_PB_DiffTopTextTopPB - K_IU_TGPBX_TopMarging) div 2;
  {$ELSE}
  if Form.Caption <> '' then begin
    _TopShapes := (Form.Height - K_IU_PB_ShapeHeight - K_IU_TGPBX_TopMarging - K_IU_TGPBX_TopMargingNotEmptyCaption) div 2 ;
    _TopText := (Form.Height - K_IU_PB_TextHeight + K_IU_PB_DiffTopTextTopPB - K_IU_TGPBX_TopMarging - K_IU_TGPBX_TopMargingNotEmptyCaption) div 2;
  end
  else begin
    _TopShapes := (Form.Height - K_IU_PB_ShapeHeight - K_IU_TGPBX_TopMarging) div 2;
    _TopText := (Form.Height - K_IU_PB_TextHeight + K_IU_PB_DiffTopTextTopPB - K_IU_TGPBX_TopMarging) div 2;
  end;
  {$ENDIF}
  // *
  // * End Add V0.7
  // ***
  // ***
  // * Del V0.7
  // *
  // _TopShapes := (Form.Height - K_IU_PB_ShapeHeight - K_IU_TGPBX_TopMarging) div 2;
  // _TopText := (Form.Height - K_IU_PB_TextHeight + K_IU_PB_DiffTopTextTopPB - K_IU_TGPBX_TopMarging) div 2; //_TopShapes + K_IU_PB_DiffTopTextTopPB ;
  // *
  // * End Del V0.7
  // ***
  // testing if is a covering between text and shapes (then width of the form is to small)
  // in this case, first shape position start after the text and K_IU_Space pixels of separation
  if _startTextPos + _textSize + K_IU_Space > _startShapePos then _StartShapePos := _startTextPos + _textSize + 3;
  // Loop of objects creation
  for i := 1 to K_IU_PB_NbIndicators do begin
    // 1- Shape creation
    _progressLine._picto[i]:= TShape.Create(form);
    _progressLine._picto[i].Parent := form;
    // 2- Defining the default color of the shape
    _progressLine._picto[i].Brush.Color:=rgbtocolor(V_IU_PB_DefaultBGColor.red,V_IU_PB_DefaultBGColor.green,V_IU_PB_DefaultBGColor.blue);
    // 3- Setting position of the shape in the form
    _progressLine._picto[i].Left:=_startShapePos;
    _progressLine._picto[i].Top:= _TopShapes;
    // 4- inc the pos for the next shape
    _startShapePos := _startShapePos + _shapesize + K_IU_Space;
    // 6- set size of the shape
    _progressLine._picto[i].Height:= K_IU_PB_ShapeHeight;
    _progressLine._picto[i].Width:= _shapesize;
    // 7- set visible propertie to true
    _progressLine._picto[i].Visible:=true;
  end;
  // Define text size and position
  _progressLine._caption := TLabel.Create(form);
  _progressLine._caption.Parent := form;
  _progressLine._caption.Font.Color:=V_IU_PB_TextColor;
  _progressLine._caption.Font.Size:=K_IU_PB_TextHeight;
  _progressLine._Caption.Font.Bold := true;
  _progressLine._caption.AutoSize:=false;
  _progressLine._caption.Width:=_textsize;
  _progressLine._caption.Height:=K_IU_PB_TextHeight+4;
  _progressLine._caption.Alignment:= taRightJustify;
  _progressLine._Caption.Top := _TopText-1;
  _progressLine._Caption.Left := K_IU_PB_WidthMarging;
  _progressLine._caption.Visible:=true;
  _progressLine._caption.Caption:='0%';
  // ***
  // * Add V0.9
  self.deactivateHint;
  // ***
end;

// ***
// * Personnal progress bar object resize
// *
// * @author : Pascal Lemaître
// *
// * This procedure resize the progress bar accoring to form in parameter
// *
// ***
procedure T_IU_ProgressBar._resize (var Form : TWinControl);
var i : integer;
  _textsize, _startTextPos, _shapesize, _startShapePos : extended;
  _topText, _topShapes : extended;
begin
  // Defining the width of each shape
  // There are K_IU_PB_NbIndicators shapes
  // each must be separated by some pixels
  // The size of the text should be enought to write 100% without recovering the first shape
  // 1- text size
  // 4 digits of 12 pixels width + K_IU_Space of separation
  _textsize := 4 * 14 ;
  // 2- Shape size
  // K_IU_PB_NbIndicators shapes
  // 10 pixels of marging to the left and the right of the form
  // 5 pixels between each shapes
  _shapesize := (Form.Width - _textsize - K_IU_PB_WidthMarging * 2 - K_IU_Space * K_IU_PB_NbIndicators) / K_IU_PB_NbIndicators;
  // Test if it is smaller than the minimal width
  if round(_shapesize) < K_IU_PB_MinShapeWidth + K_IU_Space then _shapesize := K_IU_PB_MinShapeWidth ;
  // Defining text position from the left border of the form
  _startTextPos := 10;
  // Definig the first shape position from the left border of the form
  _startShapePos := _StartTextPos + K_IU_TextSep ;
  // 3- Top of shapes and text
  _TopShapes := (Form.Height - K_IU_PB_ShapeHeight - K_IU_TGPBX_TopMarging) / 2;
  _TopText := (Form.Height - K_IU_PB_TextHeight + K_IU_PB_DiffTopTextTopPB - K_IU_TGPBX_TopMarging) / 2; // _TopShapes + K_IU_PB_DiffTopTextTopPB ;
  // testing if is a covering between text and shapes (then width of the form is to small)
  // in this case, first shape position start after the text and K_IU_Space pixels of separation
  if _startTextPos + _textSize + K_IU_Space > _startShapePos then _StartShapePos := _startTextPos + _textSize + K_IU_TextSep;
  // Loop of objects creation
  for i := 1 to K_IU_PB_NbIndicators do begin
    // 1- Setting position of the shape in the form
    _progressLine._picto[i].Left:=round(_startShapePos);
    _progressLine._picto[i].Top:= round(_TopShapes);
    // 2- inc the pos for the next shape
    _startShapePos := _startShapePos + _shapesize + K_IU_Space;
    // 3- set size of the shape
    _progressLine._picto[i].Height:= K_IU_PB_ShapeHeight;
    _progressLine._picto[i].Width:= round(_shapesize);
    // 4- set visible propertie to true
    _progressLine._picto[i].Visible:=true;
  end;
  // Define text size and position
  _progressLine._Caption.Left := K_IU_PB_WidthMarging;
  _progressLine._Caption.Top:= round(_TopText);
end;


// ***
// * Personnal progress bar object init method
// *
// * @author : Pascal Lemaître
// *
// * This method init progress bar to 0 (nothing is done)
// *
// ***
procedure T_IU_ProgressBar._init;
begin
  self._percent :=0;
  self._draw;
end ;

// ***
// * Personnal progress bar object draw method
// *
// * @author : Pascal Lemaître
// *
// * This method draw the progress bar. Hilight the progress indicators and write the percent
// *
// ***
procedure T_IU_ProgressBar._draw;
var i, k : integer ;
  lumintensity : real;
  luminence : real;
  R,G,B : byte;
  coef : real;
begin
  luminence :=  (V_IU_PB_ActivateBG.red + V_IU_PB_ActivateBG.green + V_IU_PB_ActivateBG.blue) / (3*$FF);
  for i := 1 to K_IU_PB_NbIndicators do begin
    k := trunc(i * percentByIndicator);
      lumintensity := (self._percent- trunc((i-1)* percentByIndicator)) / self.percentByindicator ;
      if lumintensity > 1 then coef := 1 else
        if lumintensity <= 0 then coef := 0 else coef := lumintensity;
          if i < upLimit1 then begin
            if self._percent > k then begin
              R := V_IU_PB_ActivateBG.red;
              G := V_IU_PB_ActivateBG.green;
              B := V_IU_PB_ActivateBG.blue;
            end else begin
              R := byte(trunc(V_IU_PB_ActivateBG.red * coef));
              G := byte(trunc(V_IU_PB_ActivateBG.green * coef));
              B := byte(trunc(V_IU_PB_ActivateBG.blue * coef));
            end;
          end else if i < upLimit2 then begin
            if self._percent > k then begin
              R := V_IU_PB_DistantEndBG.red;
              G := V_IU_PB_DistantEndBG.green;
              B := V_IU_PB_DistantEndBG.blue;
            end else begin
              R := byte(trunc(V_IU_PB_DistantEndBG.red * coef));
              G := byte(trunc(V_IU_PB_DistantEndBG.green * coef));
              B := byte(trunc(V_IU_PB_DistantEndBG.blue * coef));
            end;
            end else if i < upLimit3 then begin
              if self._percent > k then begin
                R := V_IU_PB_NearEndBG.red;
                G := V_IU_PB_NearEndBG.green;
                B := V_IU_PB_NearEndBG.blue;
              end else begin
                R := byte(trunc(V_IU_PB_NearEndBG.red * coef));
                G := byte(trunc(V_IU_PB_NearEndBG.green * coef));
                B := byte(trunc(V_IU_PB_NearEndBG.blue * coef));
              end;
            end else begin
              R := byte(trunc(V_IU_PB_EndBG.red * coef));
              G := byte(trunc(V_IU_PB_EndBG.green * coef));
              B := byte(trunc(V_IU_PB_EndBG.blue * coef));
            end ;
    self._progressLine._picto[i].Brush.Color:=rgbtocolor(R,G,B);
  end;
  self._progressLine._caption.Caption:=inttostr(trunc(self._percent))+'%';
  Application.ProcessMessages;
end ;

// ***
// * Personnal progress bar object draw method
// *
// * @author : Pascal Lemaître
// *
// * This method set the percent value. Percent must be in [0..100]
// *
// ***
procedure T_IU_ProgressBar._setPercent(_in_percent : real) ;
begin
  if _in_percent < 0 then _in_percent := 0 else
    if _in_percent > 100 then _in_percent := 100;
  self._percent := _in_percent;
end ;


// ***
// * Personnal progress bar object destructor
// *
// * @author : Pascal Lemaître
// *
// ***
destructor T_IU_ProgressBar.Release;
var i : integer;
begin
  for i := 1 to K_IU_PB_NbIndicators do
    with self._progressLine do
        _picto[i].Destroy;
  self._progressLine._caption.Destroy;
  inherited;
end;

// ***
// * Add V0.9
// *
// ***
// * Personnal progress bar : set hint message
// *
// * @author : Pascal Lemaître
// *
// * in : hint message
// *
// ***
procedure T_IU_ProgressBar.setHint(hintSTR : string); // set the hint text
var i : integer;
begin
  for i := 1 to K_IU_PB_NbIndicators do
    with self._progressLine do begin
        _picto[i].Hint:=hintSTR;
    end;
  self._progressLine._caption.Hint:=hintSTR;
  self._progressLine._caption.Parent.Hint:=hintSTR ;
end;

// ***
// * Personnal progress bar : activate hint message
// *
// * @author : Pascal Lemaître
// *
// ***
procedure T_IU_ProgressBar.activateHint ; // activate Hint display when mouse over the control
var i : integer;
begin
  for i := 1 to K_IU_PB_NbIndicators do
    with self._progressLine do begin
        _picto[i].ShowHint:=true;
    end;
  self._progressLine._caption.ShowHint:=true;
  self._progressLine._caption.Parent.ShowHint:=true ;
end;

// ***
// * Personnal progress bar : deactivate hint message
// *
// * @author : Pascal Lemaître
// *
// ***
procedure T_IU_ProgressBar.deactivateHint ; // deactivate Hint. Nothing display when mouse over the control
var i : integer;
begin
  for i := 1 to K_IU_PB_NbIndicators do
    with self._progressLine do begin
        _picto[i].ShowHint:=false;
    end;
  self._progressLine._caption.ShowHint:=false;
  self._progressLine._caption.Parent.ShowHint:=false ;
end;

// *
// * End Add V0.9
// ***

// ***************************

// ***
// * Personnal bars object : _Translate coord with bar and cursor orientation
// *
// * @author : Pascal Lemaître
// *
// * Internal procedure to translate relative coord into real coord
// *                   real coord depends on track bar orientation (horizontal or vertical)
// *                   and cursor orientation (standard or inverted)
// *
// * in : relative coord (xr, yr)
// * out : real coord (x, y)
// *
// ***
procedure _translateXY (xr, yr : integer ; var x, y : integer ;
                         _height, _width : integer ;
                         _direction : T_IU_TB_Direction ;
                         _cursorOrientation : T_IU_TB_CursorDirection) inline ;
var
  _startWidth, _startHeigh, _endWidth, _endHeight : integer ; // for relative drawing vertical/horizontal TB
                                                              // and standard cursor direction or inverted direction
  _baseWidth, _directionWidth : integer ;          // for relative drawing vertical/horizontal TB
                                                   // and standard cursor direction or inverted direction
  _baseHeight, _directionHeight : integer ;        // for relative drawing vertical/horizontal TB
                                                   // and standard cursor direction or inverted direction
  _x, _y : integer ; // coordonate according to orientation of TB and cursor direction for intermediates calcs

begin
  // defining bases for relatives coordonates of the TB
  if _cursorOrientation=iu_tb_cursordirection_standard then begin
    if _direction = iu_tb_horizontal then begin
      // ***
      // * horizontal TB with standard cursor orientation align bottom to top
      // *
      _baseWidth := 0;
      _directionWidth := 1;
      _baseHeight := _height-1;
      _directionHeight := -1;
      // *
      // ***
    end else begin
      // ***
      // *
      // * vertical TB with standard cursor orientation align left to right
      // *
      _baseWidth := _height-1;
      _directionWidth := -1;
      _baseHeight := 0 ;
      _directionHeight :=1;
      // *
      // ***
    end;
  end else begin
    if _direction = iu_tb_horizontal then begin
      // ***
      // * horizontal TB with inverted cursor orientation align top to bottom
      // *
      _baseWidth := 0;
      _directionWidth := 1;
      _baseHeight := 0;
      _directionHeight := 1;
      // *
      // ***
    end else begin
      // ***
      // vertical TB with inverted cursor orientation align right to left
      _baseWidth := _height-1;
      _directionWidth := -1;
      _baseHeight := _width-1 ;
      _directionHeight :=-1
      // *
      // ***
    end;
  end;
  // Transalte relative coord according to cursor orientation
  _x := xr*_directionWidth + _baseWidth;
  _y := yr*_directionHeight + _baseHeight;
  // Translate in real TB coordonate (depending on TB orientation - swapping or not _x and _y)
  if _direction = iu_tb_horizontal then begin
    x := _x;
    y := _y;
  end else begin
    x := _y;
    y := _x;
  end;
end;


// ***
// * Personnal track bar object : _init procedure
// *
// * @author : Pascal Lemaître
// *
// * Init values of track bar set current value to _startvalue
// * and redraw the curve
// *
// ***
procedure T_IU_TrackBar._init;
begin
  if self.Parent.width >= self.Parent.height then begin
    self._InternalInterval:=self.Parent.Width;
    self._direction:=iu_tb_horizontal;
  end else begin
    self._InternalInterval:=self.Parent.Height;
    self._direction:=iu_tb_vertical;
  end;
  self._width := self.Parent.Width;
  self._height := self.Parent.Height;
  self._currentvalue:=0.5; // mid in the [0..1] interval
end ;


// ***
// * Personnal track bar object : _redraw procedure
// *
// * @author : Pascal Lemaître
// *
// * Draw the curve of the track bar
// *
// ***
procedure T_IU_TrackBar._draw;
var i : integer ;
  coefScale : extended ; // Conversion factor between width of the track bar and it's interval
  nbLines : integer ; // A line is 2 pixels width and 1 pixels separate each line
  coefPosX : extended ; // To define the start position of a line
  cursorScale : extended ; // Size of the cursor (depends of the numbers of lines
                       // Spline is according to 100 Lines
  targetPos : extended;
  targetValue : extended ;
  lineHeight : integer;
  x : extended ; //  delta with _currentvalue for spline call
  scalenumber : integer ; // How many line of scale to draw
  scalewidth : integer ; // ScaleWidth for center it on reference line
  scalemarging : integer ; // shift from the left to centered scale lines
  scalePosFactor : integer ; // Factor to define line position
  _x, _y : integer ; // real pixel coord


begin
  // erasing top line
  // Black Line
  self.Parent.Canvas.Pen.Color:=clBlack;
  _translateXY (1,self._InternalHeight-1,_x,_y, self._height, self._width, self._direction, self._cursorOrientation);
  self.Parent.Canvas.MoveTo(_x,_y);
  _translateXY (self._InternalInterval-1,self._InternalHeight-1, _x, _y, self._height, self._width, self._direction, self._cursorOrientation);
  self.Parent.Canvas.LineTo(_x,_y);
  nbLines := self._InternalInterval div (K_IU_TB_LineWidth + K_IU_TB_LineSepWidth); // Defining the numbers of the lines for drawing the curve
  coefPosX := (self._InternalInterval - 3)/(nbLines - 1) ; // Defining the translation to have the position of the curve in the track bar
  coefScale := coefPosX;
  cursorScale := K_IU_TB_CursorScale * 1 / coefScale;
  for i := 0 to nbLines - 1 do begin
    // calc the position of the bar in the shape
    targetPos := i * coefPosX +1;
    // From x position of the bar transform it into a value in [_startvalue.._endvalue]
    targetValue := i * coefScale;
    // getting the y value of the line (apply spline fonction)
    x := ((self._currentvalue * (self._InternalInterval - 2) - targetvalue)*cursorScale)/10.0;
    if abs(x)>1 then lineHeight := 1 else lineHeight := round(self._InternalHeight * splineBars(x));
    if lineHeight < 0 then lineHeight :=0;
    // Drawing the line
    if isSelected then
      self.Parent.Canvas.Pen.Color := V_IU_TB_SelectedActiviatedCurve
    else
        self.Parent.Canvas.Pen.Color:=V_IU_TB_ActivatedCurve;
    _translateXY(trunc(targetPos), 1, _x,_y, self._height, self._width, self._direction, self._cursorOrientation);
    self.Parent.Canvas.MoveTo(_x,_y);
    _translateXY(trunc(targetPos),lineHeight-1,_x,_y, self._height, self._width, self._direction, self._cursorOrientation);
    self.Parent.Canvas.LineTo(_x,_y);
    if self.Edition <> nil then
      self.Edition.Text := self.convertCurrentValueForDisplaying;
  end;

  // Drawing a pseudo scale
  // How many line ?
  // scalenumber := self._width div (K_IU_TB_ScaleWidth + K_IU_TB_ScaleSep);
  scalenumber := self._InternalInterval div (K_IU_TB_ScaleWidth + K_IU_TB_ScaleSep);
  scalewidth := scalenumber * (K_IU_TB_ScaleWidth + K_IU_TB_ScaleSep);
  // scalemarging := (self._width - scalewidth) div 2;
  scalemarging := (self._InternalInterval - scalewidth) div 2;
  // drawing scale lines
  self.Parent.Canvas.Pen.Style:=psSolid;
  self.Parent.Canvas.Pen.Width:=1;
  if isSelected then
    self.Parent.Canvas.Pen.Color := V_IU_TB_ScaleColor
  else
      self.Parent.Canvas.Pen.Color:=V_IU_TB_unselectedScaleColor;
  scalePosFactor := K_IU_TB_ScaleWidth + K_IU_TB_ScaleSep;
  for i := 0 to scalenumber do begin
      // self.Parent.Canvas.MoveTo(i*scalePosFactor + scalemarging, 0);
      _translateXY(i*scalePosFactor + scalemarging, self._InternalHeight-1,_x,_y, self._height, self._width, self._direction, self._cursorOrientation);
      self.Parent.Canvas.MoveTo(_x,_y);
      // self.Parent.Canvas.LineTo(i*scalePosFactor+scalemarging+K_IU_TB_ScaleWidth,0);
      _translateXY(i*scalePosFactor+scalemarging+K_IU_TB_ScaleWidth,self._InternalHeight-1,_x,_y, self._height, self._width, self._direction, self._cursorOrientation);
      self.Parent.Canvas.LineTo(_x,_y);
  end;
  // Drawing base of cursor
  self.Parent.Canvas.Pen.Color:=V_IU_TB_DefaultBG;
  _translateXY (1,0,_x,_y, self._height, self._width, self._direction, self._cursorOrientation);
  self.Parent.Canvas.MoveTo(_x,_y);
  _translateXY (self._InternalInterval-1,0, _x, _y, self._height, self._width, self._direction, self._cursorOrientation);
  self.Parent.Canvas.LineTo(_x,_y);
  // Drawing left Border
  self.Parent.Canvas.Pen.Color:=V_IU_TB_DefaultBG;
  _translateXY (1,0,_x,_y, self._height, self._width, self._direction, self._cursorOrientation);
  self.Parent.Canvas.MoveTo(_x,_y);
  _translateXY (1,self._internalHeight, _x, _y, self._height, self._width, self._direction, self._cursorOrientation);
  self.Parent.Canvas.LineTo(_x,_y);
  // Drawing right Border
  self.Parent.Canvas.Pen.Color:=V_IU_TB_DefaultBG;
  _translateXY (self._InternalInterval,0,_x,_y, self._height, self._width, self._direction, self._cursorOrientation);
  self.Parent.Canvas.MoveTo(_x,_y);
  _translateXY (self._InternalInterval,self._internalHeight,_x,_y, self._height, self._width, self._direction, self._cursorOrientation);
  self.Parent.Canvas.LineTo(_x,_y);
end;


// ***
// * Personnal track bar object : _set the current value selected with the mouse
// *
// * @author : Pascal Lemaître
// *
// * Get the mouse position and translate it in a value between _startvalue and _endvalue
// *
// ***
procedure T_IU_TrackBar._setValue(_mouse_position : integer) ;
begin
  if (_mouse_position > 0) and (_mouse_position < self._InternalInterval) then begin
    // 1- convertir x coordonates in value according to [0..1] interval
    _currentvalue := _mouse_position / (self._InternalInterval - 3);
    if _currentvalue > 1 then _currentvalue := 1
    else if _currentvalue < 0 then _currentvalue :=0;
    // 2- redraw the track bar cursor
    self._draw;
    if self.onChange <> nil then
      self.onChange(_getID, round(self._currentvalue*self._OutputInterval));
  end;
end;


// ***
// * Personnal track bar object : _change the current value on mouse wheel action
// *
// * @author : Pascal Lemaître
// *
// ***
procedure T_IU_TrackBar._setValueOnWheelDelta(WheelDelta : integer) ;
var _value, _interval, _inc : extended;
begin
  _value := self._getValue;
  _interval := self._getInterval;
  if WheelDelta > 0 then _inc := 1 / _interval else _inc := -1/_interval;
  self._setValue(_value + _inc);
  self._draw;
  if self.onChange <> nil then
    self.onChange(_getID, round(self._currentvalue*self._OutputInterval));
end;


// ***
// * Personnal track bar object : _set the current value selected with a value
// *
// * @author : Pascal Lemaître
// *
// * Get the mouse position and translate it in a value between _startvalue and _endvalue
// *
// ***
procedure T_IU_TrackBar._setValue(_value : extended) ;
begin
  if _value < 0 then _value := 0 else if _value > 1 then _value := 1;
  self._currentvalue:=_value;
  self._draw;
  if self.onChange <> nil then
    self.onChange(_getID, round(self._currentvalue*self._OutputInterval));
end;


// ***
// * Personnal track bar object : Set new value from keyboard action
// *
// * @author : Pascal Lemaître
// *
// * return the ID of the selected TB
// *
// ***
procedure T_IU_TrackBar._setValueFromKeyboard(var _key : word);
var oldvalue : extended ;
begin
  // 1- Testing if it is the selected track bar
  // ***
  if isSelected then
  // ***
    begin
      oldvalue := self._currentvalue;
      // 2- Testing which key is pressed
      if _key = word(40) then // decrease
        self._currentvalue:=self._currentvalue-self._sharpDelta
      else if _key = word(37) then // fine decrease
        self._currentvalue := self._currentvalue-self._minDelta
      else if _key = word(38) then // increase
        self._currentvalue :=self._currentvalue+self._sharpDelta
      else if _key = word(39) then // fine increase
        self._currentvalue:=self._currentvalue+self._minDelta
      else if (_key = word(34)) then // big decrease
        self._currentvalue := self._currentvalue - self._largeDelta
      else if (_key = word(33)) then // big increase
        self._currentvalue:=self._currentvalue+self._largeDelta;
      if self._currentvalue < 0 then self._currentvalue:=0
      else if self._currentvalue >1 then self._currentvalue := 1;
      self._draw;
      if oldvalue <> self._currentvalue then
        _key := word(0);
      if self.onChange <> nil then
        self.onChange(_getID, round(self._currentvalue*self._OutputInterval));
    end;
end;

// ***
// * Personnal track bar object : _get the current value of position
// *
// * @author : Pascal Lemaître
// *
// * Get the mouse position and translate it in a value between _startvalue and _endvalue
// *
// ***
function T_IU_TrackBar._getValue: extended ;
begin
  _getValue := self._currentvalue;
end;


// ***
// * Personnal track bar object : _getInterval Get the size of the track Bar
// *
// * @author : Pascal Lemaître
// *
// *
// ***
function T_IU_TrackBar._getInterval: extended ;
begin
  _getInterval := self.Parent.width;
end;


// ***
// * Personnal track bar object : _resize the curve when Parent shape size change (must be call)
// *
// * @author : Pascal Lemaître
// *
// * Redraw the curve according to the new size of the parent's Shape
// *
// ***
procedure T_IU_TrackBar._resize;
begin
  if self._direction=iu_tb_horizontal then begin
    self._InternalInterval:=self.Parent.Width;
    self._InternalHeight:=self.Parent.Height;
  end
  else begin
    self._InternalHeight:=self.Parent.Width;
    self._InternalInterval:=self.Parent.Height;
  end;
  self._width := self.Parent.Width;
  self._height := self.Parent.Height;
  self._sharpDelta:=1/self._InternalInterval;
  self._largeDelta:=self._sharpDelta * 10;
  if self._OutputInterval <> 0 then self._minDelta:= abs(1 / self._OutputInterval)
  else self._minDelta:=0.0;
  self._draw;
end;


// ***
// * Personnal track bar object : Creator with edit box
// *
// * @author : Pascal Lemaître
// *
// * in : Parent Shape where curve will be draw
// *
// ***
constructor T_IU_TrackBar.Create(cursorOrientation : T_IU_TB_CursorDirection ; interval : integer ; var form : TImage ; var edit : TEdit);
begin
  inherited Create;
  Parent := form;
  Parent.OnMouseDown:=@ParentMouseDown;
  Parent.OnMouseEnter:=@ParentMouseEnter;
  Parent.OnMouseLeave:=@ParentMouseLeave;
  Parent.OnMouseMove:=@ParentMouseMove;
  Parent.OnMouseWheel:=@ParentMouseWheel;
  Parent.OnPaint:=@ParentPaint;
  Parent.OnChangeBounds:=@ParentChangeBounds;
  self.Edition := edit;
  self.onChange := nil;
  self._OutputInterval:=interval;
  self._cursorOrientation:=cursorOrientation;
  Parent.Picture.Bitmap.Canvas.Brush.Color:=V_IU_TB_DefaultBG;
  self._init;
  self._resize;
  self._draw;
  self.Edition.NumbersOnly:=true;
  self.Edition.Color:=V_IU_TB_DefaultBG;
  self.Edition.Font.Color:=V_IU_TB_TextColor;
  self.Edition.Font.Size:=K_IU_TB_EditTextSize;
  self._DisplayNumBerOfDecimalsPlace:=0;
  self._DisplayStartingValue:=0;
  self.Edition.Text := self.convertCurrentValueForDisplaying;
  self.Edition.OnKeyDown:=@self.EditKeyDown;
  // ***
  // * Add V0.9
  self.deactivateHint;
  // ***
end;


// ***
// * Personnal track bar object : Creator without edit box
// *
// * @author : Pascal Lemaître
// *
// * in : Parent Shape where curve will be draw
// *
// ***
constructor T_IU_TrackBar.Create(cursorOrientation : T_IU_TB_CursorDirection ; interval : integer ; var form : TImage);
begin
  inherited Create;
  Parent := form;
  Parent.OnMouseDown:=@ParentMouseDown;
  Parent.OnMouseEnter:=@ParentMouseEnter;
  Parent.OnMouseLeave:=@ParentMouseLeave;
  Parent.OnMouseMove:=@ParentMouseMove;
  Parent.OnMouseWheel:=@ParentMouseWheel;
  Parent.OnPaint:=@ParentPaint;
  Parent.OnChangeBounds:=@ParentChangeBounds;
  self.Edition := nil;
  self.onChange:= nil;
  self._OutputInterval:=interval;
  self._cursorOrientation:=cursorOrientation;
  Parent.Picture.Bitmap.Canvas.Brush.Color:=V_IU_TB_DefaultBG;
  self._init;
  self._resize;
  self._draw;
  self._DisplayNumBerOfDecimalsPlace:=0;
  self._DisplayStartingValue:=0;
  // ***
  // * Add V0.9
  self.deactivateHint;
  // ***
end;

// ***
// * Personnal track bar object : Destructor
// *
// * @author : Pascal Lemaître
// *
// ***
destructor T_IU_TrackBar.Release;
begin
  inherited;
end;


// * Personnal track bar object : Set selected Track Bar
// *
// * @author : Pascal Lemaître
// *
// * return the ID of the TB
// *
// ***
procedure T_IU_TrackBar._setSelectedTB;
begin
  select ;
  self._draw;
  // ***
  // * Add V0.12
  self.Parent.Parent.SetFocus; // Set the focus to the form where the control is
                               // else keyboard action can not be captured if form has not the focus
  // ***
  // ***
  // Add V0.11
  // Send paint event on the main form
  self.Parent.Parent.Refresh;
  self.Parent.Parent.Repaint;
  // ***
end;


// * Personnal track bar object : Unset selected Track Bar
// *
// * @author : Pascal Lemaître
// *
// * return the ID of the TB
// *
// ***
procedure T_IU_TrackBar._unsetSelectedTB;
begin
  unselect;
  self._draw;
end;



// * Personnal track bar object : Validate a keyboard enter for track bar position
// *
// * @author : Pascal Lemaître
// *
// ***
procedure T_IU_TrackBar.EditKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  input : extended;
begin
  input :=-1;
  if Key = word(13) then begin // return pressed value validated
    input := self.convertGetingDisplayValue(self.Edition.Text);
    self._currentvalue:=input;
    self._draw;
    if self.onChange <> nil then
      self.onChange(_getID, round(self._currentvalue*self._OutputInterval));
  end;
end;


// * Personnal track bar object : Converting get value from Edit box into _currentvalue
// *
// * @author : Pascal Lemaître
// *
// * in : value getted from user as a string _getValue
// *
// * return : new _currentvalue
// *
// ***
function T_IU_TrackBar.convertGetingDisplayValue (_getValue : string) : extended ;
var _inter : extended ; // intermediate var for transformation
  _error : boolean; // error detected
  _return : extended ; // return value
begin
  _error := false;
  // 1- try to transform text into extended
  try
    _inter := strtofloat(_getValue);
  except
    // error in transformation
    // current value is returned
    _error := true;
    _return := self._currentvalue;
  end;
  // 2- try to take only number of decimals place and shift value according to start value to be displayed
  if not _error then begin
    try
      _inter := (_inter - self._DisplayStartingValue) * power(10, self._DisplayNumBerOfDecimalsPlace) ;
      _return := round(_inter);
    except
      // error in transformation
      // current value is returned
      _error := true;
      _return := self._currentvalue;
    end;
    // 3- change value in _currentvalue reference from interval reference
    if not _error then
      if _return < 0 then _return := 0
      else if _return > self._OutputInterval then _return := self._OutputInterval;
      _return := _return / self._OutputInterval;
  end;
  convertGetingDisplayValue := _return;
end;



// * Personnal track bar object : Converting _currentvalue into displayed format
// *
// * @author : Pascal Lemaître
// *
// * return : value to display as a string
// *
// ***
function T_IU_TrackBar.convertCurrentValueForDisplaying : string ;
var
  _return : string;
begin
  // two case
  //   -1 : no decimal
  //   -2 : decimals
  // 1- Case of decimal
  if self._DisplayNumBerOfDecimalsPlace > 0 then begin
    _return := IU_realToString((self._currentvalue*self._OutputInterval)/power(10,self._DisplayNumBerOfDecimalsPlace)+self._DisplayStartingValue,self._DisplayNumBerOfDecimalsPlace);
  end else begin
  // 2- Case of no decimal
    _return:=inttostr(round(self._currentvalue*self._OutputInterval+self._DisplayStartingValue));
  end;
  convertCurrentValueForDisplaying:=_return;
end;


// * Personnal track bar object : set value in interval reference
// *
// * @author : Pascal Lemaître
// *
// * in : value to set in interval reference
// *
// ***
procedure T_IU_TrackBar._setValueInInterval(_value : int64);
begin
  // 1- testing and adapting bounds if needed
  if _value < 0 then _value := 0
  else if _value > self._OutputInterval then _value := self._OutputInterval;
  // 2- converting into the currentvalue track bar internal representation
  self._currentvalue:=_value / self._OutputInterval;
  // 3- Change value displaying in Edit box
  if self.Edition <> nil then
    self.Edition.Text:=convertCurrentValueForDisplaying;
end;


// * Personnal track bar object : Define a new interval value for track bar
// *
// * @author : Pascal Lemaître
// *
// * in : interval value
// *
// ***
procedure T_IU_TrackBar._setInterval(_value : int64);
begin
  // interval is always > 0 if = 0 then replace it by 1
  _value := abs(_value);
  if _value = 0 then _value := 1;
  self._OutputInterval:=_value;
  // No need to adapt _currentvalue because _currentvalue is relative to the track bar width
  // Changing the interval has no effect on the _currentvalue
  // it has just effect on internal _currentvalue representation into client representation
  if self.Edition <> nil then
    self.Edition.Text:=convertCurrentValueForDisplaying;
end;


// * Personnal track bar object : Defining the number of decimal place for displaying track bar value
// *
// * @author : Pascal Lemaître
// *
// * in : number of decimal place
// *
// ***
procedure T_IU_TrackBar._setNumberOfDecimalPlace (_value : integer);
begin
  // must be positive and can be 0
  self._DisplayNumBerOfDecimalsPlace:=abs(_value);
  // has an effect on current value displayed
  if self.Edition <> nil then
    self.Edition.Text:=convertCurrentValueForDisplaying;
end;


// * Personnal track bar object : Defining a starting (base) value for displayin track bar value
// *                              Track bar value is in [0..Interval] but for display
// *                              value should be [startvalue..Interval+startvalue]
// *
// * @author : Pascal Lemaître
// *
// * in : base value for displaying track bar value adjusted to a new start value <> from 0
// *
// ***
procedure T_IU_TrackBar._setStartingValue (_value : extended);
begin
  self._DisplayStartingValue:=_value;
  // has effect on displayed value in Edit Box
  if self.Edition <> nil then
    self.Edition.Text:=convertCurrentValueForDisplaying;
end;


// * Personnal track bar object : Return the track value into the Display interval
// *                              Display interval is defined by
// *                              _DisplayStartingValue = Start value in the left of the track bar
// *                              _OutputInterval = number of values from _DisplayStartingValue
// *                              _Return value is in the display reference not in track bar reference
// *
// * @author : Pascal Lemaître
// *
// * return : current value in the display references
// *
// ***
function T_IU_TrackBar._GetValueInInterval : extended;
var _return : extended;
begin
  _return := (self._currentvalue*self._OutputInterval) / power(10,self._DisplayNumBerOfDecimalsPlace) + self._DisplayStartingValue;
  _GetValueInInterval := _return;
end;


// * Personnal track bar object : Mouse down event manager
// *
// * @author : Pascal Lemaître
// *
// * in : Object where trigered the event Sender
// * in : Button states of mouse boutons
// * in : Shift states ctrl, shift ... keys
// * in : Coordonates of the mouse when it is clicked
// *
// ***
procedure T_IU_TrackBar.ParentMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if ssLeft in Shift then
    if self._direction = iu_tb_horizontal then self._setValue(x)
    else self._setValue(self._InternalInterval - y);
end;


// * Personnal track bar object : Mouse enter event manager
// *
// * @author : Pascal Lemaître
// *
// * in : Object where trigered the event Sender
// *
// ***
procedure T_IU_TrackBar.ParentMouseEnter(Sender: TObject);
begin
  self._setSelectedTB;
  // ***
  // * Add V0.12
  self.Parent.Parent.SetFocus; // Set the focus to the form where the control is
                               // else keyboard action can not be captured if form has not the focus
  // ***
  // ***
  // Add V0.11
  // Send paint event on the main form
  self.Parent.Parent.Refresh;
  self.Parent.Parent.Repaint;
  // ***

end;


// * Personnal track bar object : Mouse leave event manager
// *
// * @author : Pascal Lemaître
// *
// * in : Object where trigered the event Sender
// *
// ***
procedure T_IU_TrackBar.ParentMouseLeave(Sender: TObject);
begin
  // ***
  // * Del V0.11
  // self._unsetSelectedTB;
  // ***
end;


// * Personnal track bar object : Mouse move event manager
// *
// * @author : Pascal Lemaître
// *
// * in : Object where trigered the event Sender
// *
// ***
procedure T_IU_TrackBar.ParentMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  self.select;
  if ssLeft in Shift then begin
    if self._direction = iu_tb_horizontal then self._setValue(x)
    else self._setValue(self._InternalInterval -  y);
  end;
  // ***
  // * Add V0.12
  self.Parent.Parent.SetFocus; // Set the focus to the form where the control is
                               // else keyboard action can not be captured if form has not the focus
  // ***
  // ***
  // Add V0.11
  // Send paint event on the main form
  self.Parent.Parent.Refresh;
  self.Parent.Parent.Repaint;
  // ***
end;


// * Personnal track bar object : Paint event manager
// *
// * @author : Pascal Lemaître
// *
// * in : Object where trigered the event Sender
// *
// ***
procedure T_IU_TrackBar.ParentPaint(Sender: TObject);
begin
  self._draw;
end;


// * Personnal track bar object : Change bounds event manager
// *
// * @author : Pascal Lemaître
// *
// * in : Object where trigered the event Sender
// *
// ***
procedure T_IU_TrackBar.ParentChangeBounds(Sender: TObject);
begin
  self._resize;
end;


// * Personnal track bar object : Parent mouse wheel event manager
// *
// * @author : Pascal Lemaître
// *
// * in : Object where trigered the event Sender
// *
// ***
procedure T_IU_TrackBar.ParentMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  self._setValueOnWheelDelta(WheelDelta);
end;


// ***
// Add V0.9
// *
// ***
// * Personnal track bar object : set hint message
// *
// * @author : Pascal Lemaître
// *
// * in : hint message
// *
// ***
procedure T_IU_TrackBar.setHint(hintSTR : string); // set the hint text
begin
  self.Parent.Hint:=hintSTR;
  if self.Edition <> nil then self.Edition.Hint:=hintSTR;
end;

// ***
// * Personnal track bar object : activate hint message
// *
// * @author : Pascal Lemaître
// *
// ***
procedure T_IU_TrackBar.activateHint ; // activate Hint display when mouse over the control
begin
  self.Parent.ShowHint:=true;
  if self.Edition <> nil then self.Edition.ShowHint:=true;
end;

// ***
// * Personnal track bar object : deactivate hint message
// *
// * @author : Pascal Lemaître
// *
// ***
procedure T_IU_TrackBar.deactivateHint ; // deactivate Hint. Nothing display when mouse over the control
begin
  self.Parent.ShowHint:=false;
  if self.Edition <> nil then self.Edition.ShowHint:=false;
end;

// ***


// **************************


// * Personnal scroll bar object : Parent mouse wheel event manager
// *
// * @author : Pascal Lemaître
// *
// * in : Object where trigered the event Sender
// *
// ***
procedure T_IU_ScrollBar._draw;
var i : integer ;
  coefScale : extended ; // Conversion factor between width of the track bar and it's interval
  nbLines : integer ; // A line is 2 pixels width and 1 pixels separate each line
  coefPosX : extended ; // To define the start position of a line
  cursorScale : extended ; // Size of the cursor (depends of the numbers of lines
                       // Spline is according to 100 Lines
  targetPos : extended;
  targetValue : extended ;
  lineHeight : integer;
  x, _coefcol : extended ; //  delta with _currentvalue for spline call
  scalenumber : integer ; // How many line of scale to draw
  scalewidth : integer ; // ScaleWidth for center it on reference line
  scalemarging : integer ; // shift from the left to centered scale lines
  scalePosFactor : integer ; // Factor to define line position
  _x, _y : integer ; // real pixel coord


begin
  nbLines := self._InternalInterval div (K_IU_TB_LineWidth + K_IU_TB_LineSepWidth); // Defining the numbers of the lines for drawing the curve
  coefPosX := (self._InternalInterval - 3)/(nbLines - 1) ; // Defining the translation to have the position of the curve in the track bar
  coefScale := coefPosX;
  cursorScale := K_IU_TB_CursorScale * 1 / coefScale;
  for i := 0 to nbLines - 2 do begin
    // calc the position of the bar in the shape
    targetPos := i * coefPosX +1;
    // From x position of the bar transform it into a value in [_startvalue.._endvalue]
    targetValue := i * coefScale;
    // getting the y value of the line (apply spline fonction)
    x := ((self._currentvalue * (self._InternalInterval - 2) - targetvalue)*cursorScale)/20.0;
    if abs(x)>1 then _coefcol := 0 else _coefcol :=  splineBars(x);
    if _coefcol < 0 then _coefcol := 0 else if _coefcol > 1 then _coefcol := 1;
    // Drawing the line
    if isSelected then
      self.Parent.Canvas.Pen.Color := rgbtocolor(trunc(_coefcol*V_IU_SB_CursorColorHigh.red+(1-_coefcol)*V_IU_SB_DefaultBGColor.red+K_IU_ScrollBar_HighLight) ,
                                                 trunc(_coefcol*V_IU_SB_CursorColorHigh.green+(1-_coefcol)*V_IU_SB_DefaultBGColor.green+K_IU_ScrollBar_HighLight),
                                                 trunc(_coefcol*V_IU_SB_CursorColorHigh.blue+(1-_coefcol)*V_IU_SB_DefaultBGColor.blue+K_IU_ScrollBar_HighLight))
    else
      self.Parent.Canvas.Pen.Color := rgbtocolor(trunc((_coefcol*V_IU_SB_CursorColor.red+(1-_coefcol)*V_IU_SB_DefaultBGColor.red)) ,
                                                 trunc((_coefcol*V_IU_SB_CursorColor.green+(1-_coefcol)*V_IU_SB_DefaultBGColor.green)),
                                                 trunc((_coefcol*V_IU_SB_CursorColor.blue+(1-_coefcol)*V_IU_SB_DefaultBGColor.blue)));

    _translateXY(trunc(targetPos), 1, _x,_y, self._height, self._width-1, self._direction, self._cursorOrientation);
    self.Parent.Canvas.MoveTo(_x,_y);
    _translateXY(trunc(targetPos), self._InternalHeight-1, _x, _y, self._height, self._width-1, self._direction, self._cursorOrientation);
    self.Parent.Canvas.LineTo(_x,_y);
  end;

  // Drawing base of cursor
  self.Parent.Canvas.Pen.Color:=V_IU_DefaultBG;
  _translateXY (1,0,_x,_y, self._height, self._width, self._direction, self._cursorOrientation);
  self.Parent.Canvas.MoveTo(_x,_y);
  _translateXY (self._InternalInterval-1,0, _x, _y, self._height, self._width, self._direction, self._cursorOrientation);
  self.Parent.Canvas.LineTo(_x,_y);
  // Drawing left Border
  self.Parent.Canvas.Pen.Color:=V_IU_DefaultBG;
  _translateXY (1,0,_x,_y, self._height, self._width-1, self._direction, self._cursorOrientation);
  self.Parent.Canvas.MoveTo(_x,_y);
  _translateXY (1,self._internalHeight, _x, _y, self._height, self._width-1, self._direction, self._cursorOrientation);
  self.Parent.Canvas.LineTo(_x,_y);
  // Drawing right Border
  self.Parent.Canvas.Pen.Color:=V_IU_DefaultBG;
  _translateXY (self._InternalInterval-1,0,_x,_y, self._height, self._width-1, self._direction, self._cursorOrientation);
  self.Parent.Canvas.MoveTo(_x,_y);
  _translateXY (self._InternalInterval-1,self._internalHeight,_x,_y, self._height, self._width-1, self._direction, self._cursorOrientation);
  self.Parent.Canvas.LineTo(_x,_y);
  // Drawing border
  self.Parent.Canvas.Pen.Color:=V_IU_DefaultBG;
  _translateXY (0,0,_x,_y, self._height, self._width, self._direction, self._cursorOrientation);
  self.Parent.Canvas.MoveTo(_x,_y);
  _translateXY (self._InternalInterval,0,_x,_y, self._height, self._width, self._direction, self._cursorOrientation);
  self.Parent.Canvas.LineTo(_x,_y);
  _translateXY (self._InternalInterval,self._internalHeight,_x,_y, self._height, self._width, self._direction, self._cursorOrientation);
  self.Parent.Canvas.LineTo(_x,_y);
  _translateXY (0,self._internalHeight,_x,_y, self._height, self._width, self._direction, self._cursorOrientation);
  self.Parent.Canvas.LineTo(_x,_y);
  _translateXY (0,0,_x,_y, self._height, self._width, self._direction, self._cursorOrientation);
  self.Parent.Canvas.LineTo(_x,_y);
end;



// ***
// * Personnal scroll bar object : _init procedure
// *
// * @author : Pascal Lemaître
// *
// * Init values of track bar set current value to _startvalue
// * and redraw the curve
// *
// ***
procedure T_IU_ScrollBar._init;
begin
  if self.Parent.width >= self.Parent.height then begin
    self._InternalInterval:=self.Parent.Width;
    self._direction:=iu_tb_horizontal;
  end else begin
    self._InternalInterval:=self.Parent.Height;
    self._direction:=iu_tb_vertical;
  end;
  self._width := self.Parent.Width;
  self._height := self.Parent.Height;
  self._currentvalue:=01; // mid in the [0..1] interval
end ;



// ***
// * Personnal scroll bar object : _set the current value selected with the mouse
// *
// * @author : Pascal Lemaître
// *
// * Get the mouse position and translate it in a value between _startvalue and _endvalue
// *
// ***
procedure T_IU_ScrollBar._setValue(_mouse_position : integer) ;
begin
  if (_mouse_position > 0) and (_mouse_position < self._InternalInterval) then begin
    // 1- convertir x coordonates in value according to [0..1] interval
    _currentvalue := _mouse_position / (self._InternalInterval - 3);
    if _currentvalue > 1 then _currentvalue := 1
    else if _currentvalue < 0 then _currentvalue :=0;
    // 2- redraw the track bar cursor
    self._draw;
    if self.onChange <> nil then
      self.onChange(_getID, round(self._currentvalue*self._OutputInterval));
  end;
end;


// ***
// * Personnal scroll bar object : _change the current value on mouse wheel action
// *
// * @author : Pascal Lemaître
// *
// ***
procedure T_IU_ScrollBar._setValueOnWheelDelta(WheelDelta : integer) ;
var _value, _interval, _inc : extended;
begin
  _value := self._getValue;
  _interval := self._getInterval;
  if WheelDelta > 0 then _inc := 1 / _interval else _inc := -1/_interval;
  self._setValue(_value + _inc);
  self._draw;
  if self.onChange <> nil then
    self.onChange(_getID, round(self._currentvalue*self._OutputInterval));
end;


// ***
// * Personnal scroll bar object : _set the current value selected with a value
// *
// * @author : Pascal Lemaître
// *
// * Get the mouse position and translate it in a value between _startvalue and _endvalue
// *
// ***
procedure T_IU_ScrollBar._setValue(_value : extended) ;
begin
  if _value < 0 then _value := 0 else if _value > 1 then _value := 1;
  self._currentvalue:=_value;
  self._draw;
  if self.onChange <> nil then
    self.onChange(_getID, round(self._currentvalue*self._OutputInterval));
end;


// * Personnal scroll bar object : Set new value from keyboard action
// *
// * @author : Pascal Lemaître
// *
// * return the ID of the selected TB
// *
// ***
procedure T_IU_ScrollBar._setValueFromKeyboard(var _key : word);
var oldvalue : extended;
begin
  // 1- Testing if it is the selected track bar
  if isSelected then
    begin
      oldvalue := self._currentvalue;
      // 2- Testing which key is pressed
      if _key = word(40) then // decrease
        self._currentvalue:=self._currentvalue-self._sharpDelta
      else if _key = word(37) then // fine decrease
        self._currentvalue := self._currentvalue-self._minDelta
      else if _key = word(38) then // increase
        self._currentvalue :=self._currentvalue+self._sharpDelta
      else if _key = word(39) then // fine increase
        self._currentvalue:=self._currentvalue+self._minDelta
      else if (_key = word(34)) then // big decrease
        self._currentvalue := self._currentvalue - self._largeDelta
      else if (_key = word(33)) then // big increase
        self._currentvalue:=self._currentvalue+self._largeDelta;
      if self._currentvalue < 0 then self._currentvalue:=0
      else if self._currentvalue >1 then self._currentvalue := 1;
      self._draw;
      if oldvalue <> self._currentvalue then
        _key := word(0);
      if self.onChange <> nil then
        self.onChange(_getID, round(self._currentvalue*self._OutputInterval));
    end;
end;



// ***
// * Personnal scroll bar object : _get the current value of position
// *
// * @author : Pascal Lemaître
// *
// * Get the mouse position and translate it in a value between _startvalue and _endvalue
// *
// ***
function T_IU_ScrollBar._getValue: extended ;
begin
  _getValue := self._currentvalue;
end;


// ***
// * Personnal scroll bar object : _getInterval Get the size of the track Bar
// *
// * @author : Pascal Lemaître
// *
// *
// ***
function T_IU_ScrollBar._getInterval: extended ;
begin
  _getInterval := self.Parent.width;
end;


// ***
// * Personnal scroll bar object : _resize the curve when Parent shape size change (must be call)
// *
// * @author : Pascal Lemaître
// *
// * Redraw the curve according to the new size of the parent's Shape
// *
// ***
procedure T_IU_ScrollBar._resize;
begin
  if self._direction=iu_tb_horizontal then begin
    self._InternalInterval:=self.Parent.Width;
    self._InternalHeight:=self.Parent.Height;
  end
  else begin
    self._InternalHeight:=self.Parent.Width;
    self._InternalInterval:=self.Parent.Height;
  end;
  self._width := self.Parent.Width;
  self._height := self.Parent.Height;
  self._sharpDelta:=1/self._InternalInterval;
  self._largeDelta:=self._sharpDelta * 10;
  if self._OutputInterval <> 0 then self._minDelta:= abs(1 / self._OutputInterval)
  else self._minDelta:=0.0;
  self._draw;
end;



// ***
// * Personnal scroll bar object : Creator without edit box
// *
// * @author : Pascal Lemaître
// *
// * in : Parent Shape where curve will be draw
// *
// ***
constructor T_IU_ScrollBar.Create(cursorOrientation : T_IU_TB_CursorDirection ; interval : integer ; var form : TImage);
begin
  inherited Create;
  Parent := form;
  Parent.OnMouseDown:=@ParentMouseDown;
  Parent.OnMouseEnter:=@ParentMouseEnter;
  Parent.OnMouseLeave:=@ParentMouseLeave;
  Parent.OnMouseMove:=@ParentMouseMove;
  Parent.OnMouseWheel:=@ParentMouseWheel;
  Parent.OnPaint:=@ParentPaint;
  Parent.OnChangeBounds:=@ParentChangeBounds;
  self.onChange:= nil;
  self._OutputInterval:=interval;
  self._cursorOrientation:=cursorOrientation;
  Parent.Picture.Bitmap.Canvas.Brush.Color:=V_IU_TB_DefaultBG;
  self._init;
  self._resize;
  self._draw;
  self._DisplayNumBerOfDecimalsPlace:=0;
  self._DisplayStartingValue:=0;
  // ***
  // * Add V0.9
  self.deactivateHint;
  // ***
end;

// ***
// * Personnal scroll bar object : Destructor
// *
// * @author : Pascal Lemaître
// *
// ***
destructor T_IU_ScrollBar.Release;
begin
  inherited;
end;


// * Personnal scroll bar object : Set selected Track Bar
// *
// * @author : Pascal Lemaître
// *
// * return the ID of the TB
// *
// ***
procedure T_IU_ScrollBar._setSelectedTB;
begin
  select;
  self._draw;
  // ***
  // * Add V0.12
  self.Parent.Parent.SetFocus; // Set the focus to the form where the control is
                               // else keyboard action can not be captured if form has not the focus
  // ***
  // ***
  // Add V0.11
  // Send paint event on the main form
  self.Parent.Parent.Refresh;
  self.Parent.Parent.Repaint;
  // ***
end;


// * Personnal scroll bar object : Unset selected Track Bar
// *
// * @author : Pascal Lemaître
// *
// * return the ID of the TB
// *
// ***
procedure T_IU_ScrollBar._unsetSelectedTB;
begin
  unselect;
  self._draw;
end;


// * Personnal scroll bar object : set value in interval reference
// *
// * @author : Pascal Lemaître
// *
// * in : value to set in interval reference
// *
// ***
procedure T_IU_ScrollBar._setValueInInterval(_value : int64);
begin
  // 1- testing and adapting bounds if needed
  if _value < 0 then _value := 0
  else if _value > self._OutputInterval then _value := self._OutputInterval;
  // 2- converting into the currentvalue track bar internal representation
  self._currentvalue:=_value / self._OutputInterval;
  // 3- Change value displaying in Edit box
end;


// * Personnal scroll bar object : Define a new interval value for track bar
// *
// * @author : Pascal Lemaître
// *
// * in : interval value
// *
// ***
procedure T_IU_ScrollBar._setInterval(_value : int64);
begin
  // interval is always > 0 if = 0 then replace it by 1
  _value := abs(_value);
  if _value = 0 then _value := 1;
  self._OutputInterval:=_value;
  // No need to adapt _currentvalue because _currentvalue is relative to the track bar width
  // Changing the interval has no effect on the _currentvalue
  // it has just effect on internal _currentvalue representation into client representation
end;



// * Personnal scroll bar object : Defining a starting (base) value for displayin track bar value
// *                              Track bar value is in [0..Interval] but for display
// *                              value should be [startvalue..Interval+startvalue]
// *
// * @author : Pascal Lemaître
// *
// * in : base value for displaying track bar value adjusted to a new start value <> from 0
// *
// ***
procedure T_IU_ScrollBar._setStartingValue (_value : extended);
begin
  self._DisplayStartingValue:=_value;
end;


// * Personnal scroll bar object : Return the track value into the Display interval
// *                              Display interval is defined by
// *                              _DisplayStartingValue = Start value in the left of the track bar
// *                              _OutputInterval = number of values from _DisplayStartingValue
// *                              _Return value is in the display reference not in track bar reference
// *
// * @author : Pascal Lemaître
// *
// * return : current value in the display references
// *
// ***
function T_IU_ScrollBar._GetValueInInterval : extended;
var _return : extended;
begin
  _return := (self._currentvalue*self._OutputInterval) / power(10,self._DisplayNumBerOfDecimalsPlace) + self._DisplayStartingValue;
end;


// * Personnal scroll bar object : Mouse down event manager
// *
// * @author : Pascal Lemaître
// *
// * in : Object where trigered the event Sender
// * in : Button states of mouse boutons
// * in : Shift states ctrl, shift ... keys
// * in : Coordonates of the mouse when it is clicked
// *
// ***
procedure T_IU_ScrollBar.ParentMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if ssLeft in Shift then begin
    self.select;
    if self._direction = iu_tb_horizontal then self._setValue(x)
    else self._setValue(self._InternalInterval - y);
  end;
end;


// * Personnal scroll bar object : Mouse enter event manager
// *
// * @author : Pascal Lemaître
// *
// * in : Object where trigered the event Sender
// *
// ***
procedure T_IU_ScrollBar.ParentMouseEnter(Sender: TObject);
begin
  self._setSelectedTB;
  // ***
  // * Add V0.12
  self.Parent.Parent.SetFocus; // Set the focus to the form where the control is
                               // else keyboard action can not be captured if form has not the focus
  // ***
  // ***
  // Add V0.11
  // Send paint event on the main form
  self.Parent.Parent.Refresh;
  self.Parent.Parent.Repaint;
  // ***

end;


// * Personnal track bar object : Mouse leave event manager
// *
// * @author : Pascal Lemaître
// *
// * in : Object where trigered the event Sender
// *
// ***
procedure T_IU_ScrollBar.ParentMouseLeave(Sender: TObject);
begin
// ***
// * Del V0.11
//  self._unsetSelectedTB;
// ***
end;


// * Personnal scroll bar object : Mouse move event manager
// *
// * @author : Pascal Lemaître
// *
// * in : Object where trigered the event Sender
// *
// ***
procedure T_IU_ScrollBar.ParentMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  self.select;
  if ssLeft in Shift then begin
    if self._direction = iu_tb_horizontal then self._setValue(x)
    else self._setValue(self._InternalInterval -  y);
  end;
  // ***
  // * Add V0.12
  self.Parent.Parent.SetFocus; // Set the focus to the form where the control is
                               // else keyboard action can not be captured if form has not the focus
  // ***
  // ***
  // Add V0.11
  // Send paint event on the main form
  self.Parent.Parent.Refresh;
  self.Parent.Parent.Repaint;
  // ***
end;


// * Personnal scroll bar object : Paint event manager
// *
// * @author : Pascal Lemaître
// *
// * in : Object where trigered the event Sender
// *
// ***
procedure T_IU_ScrollBar.ParentPaint(Sender: TObject);
begin
  self._draw;
end;


// * Personnal scroll bar object : Change bounds event manager
// *
// * @author : Pascal Lemaître
// *
// * in : Object where trigered the event Sender
// *
// ***
procedure T_IU_ScrollBar.ParentChangeBounds(Sender: TObject);
begin
  self._resize;
end;


// * Personnal scroll bar object : Parent mouse wheel event manager
// *
// * @author : Pascal Lemaître
// *
// * in : Object where trigered the event Sender
// *
// ***
procedure T_IU_ScrollBar.ParentMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  self._setValueOnWheelDelta(WheelDelta);
end;

// ***
// Add V0.9
// *
// ***
// * Personnal scroll bar object : Set hint message
// *
// * @author : Pascal Lemaître
// *
// * in : Hint message
// *
// ***
procedure T_IU_ScrollBar.setHint(hintSTR : string); // set the hint text
begin
  self.Parent.Hint:=hintSTR;
end;

// ***
// * Personnal scroll bar object : Activate hint message
// *
// * @author : Pascal Lemaître
// *
// ***
procedure T_IU_ScrollBar.activateHint ; // activate Hint display when mouse over the control
begin
  self.Parent.ShowHint:=true;
end;

// ***
// * Personnal scroll bar object : Deactivate hint message
// *
// * @author : Pascal Lemaître
// *
// ***
procedure T_IU_ScrollBar.deactivateHint ; // deactivate Hint. Nothing display when mouse over the control
begin
  self.Parent.ShowHint:=false;
end;

// ***


// ***************************

// ***
// * Personnal check box object
// *
// * @author : Pascal Lemaitre
// *
// * Class creator
// *
// * in : Image where check cursor will be draw
// * in : Label of the check box (need for events management)
// *
// ***
constructor T_IU_CheckBox.Create(var _check : TImage ; var _label : TLabel);
begin
  inherited Create;
  _box := _check;
  _box.Canvas.Brush.Color:=V_IU_DefaultBG;
  _caption := _label;
  self._caption.Font.Size:=K_IU_BoxTextSize;
  self.checked:=false;
  self.onChange := nil;
  _box.OnMouseDown:=@self.MouseDown;
  _caption.OnMouseDown:=@self.MouseDown;
  _box.OnMouseEnter:=@self.MouseEnter;
  _caption.OnMouseEnter:=@self.MouseEnter;
  _box.OnMouseLeave:=@self.MouseLeave;
  _caption.OnMouseLeave:=@self.MouseLeave;
  _box.OnPaint:=@Paint;
  // ***
  // * Add V0.9
  self.deactivateHint;
  // ***
  // ***
  // * Add V0.7
  _label.Top := _check.Top + (_check.Height - _label.Height);
  // *
  // * End Add V0.7
  // ***
  self._draw;
end;

// ***
// * Personnal check box object
// *
// * @author : Pascal Lemaitre
// *
// * Class destructor
// *
// ***
destructor T_IU_CheckBox.Release;
begin
  inherited;
end;

// ***
// * Personnal check box object
// *
// * @author : Pascal Lemaitre
// *
// * set the value of the check box (checked or not)
// *
// * in : checked value (true check, false uncheck)
// *
// * if parameter value is the same as the internal checked value nothing done
// * else internal checked is updated, and the control is redrawed and if there is a call back it is executed.
// *
// ***
procedure T_IU_CheckBox.setValue(_selected : boolean);
begin
  // If no change then no event
  if self.checked <> _selected then begin
    self.checked := _selected;
    self._draw;
    // if no call back no procedure call
    if self.onChange <> nil then
      onChange(self._ID, self.checked);
  end;
end;


// ***
// * Personnal check box object
// *
// * @author : Pascal Lemaitre
// *
// * get the value of the check box (checked or not)
// *
// * return true if checked else return false
// *
// ***
function T_IU_CheckBox.getValue : boolean;
begin
  getValue := self.checked;
end;


// ***
// * Personnal check box object
// *
// * @author : Pascal Lemaitre
// *
// * Changing value from keyboard action
// *
// * in/out : key pressed from the keybord
// *
// ***
procedure T_IU_CheckBox._setValueFromKeyboard(var _key : word);
begin
  // is the right CB ?
  if isSelected then begin
    // is return key or space key ?
    if (_key = word(13)) or (_key = word(32)) then begin
      self.setValue(not(self.getValue)); // redraw is made by setValue procedure, Call back exec is made by setValue
      _key := word(0); // delete key because event has been processed
    end;
  end;
end;


// ***
// * Personnal check box object : drawing (paint it)
// *
// * @author : Pascal Lemaitre
// *
// ***
procedure T_IU_CheckBox._draw;
var i, k, marge, nb_lines, y : integer ;
begin
  // Drawing selected or unselected check box
  k := self._box.Height div 4;
  marge := k;
  // pen definition for drawing
  // drawing selected symbol
  self._box.Canvas.Pen.Color:=rgbtocolor(V_IU_Box_BGColor.red,
                                         V_IU_Box_BGColor.green,
                                         V_IU_Box_BGColor.blue);
  for i := 0 to self._box.Height do begin
    self._box.Canvas.moveto(0,i);
    self._box.Canvas.lineto(self._box.Width,i);
  end;


  for i := 0 to k - 1 do begin
    if isSelected then
      if self.checked then
        _box.Canvas.pen.Color:=rgbtocolor(V_IU_Box_Color_CheckedHigh.red,
                                          V_IU_Box_Color_CheckedHigh.green,
                                          V_IU_Box_Color_CheckedHigh.blue)
      else
        _box.Canvas.pen.Color:=rgbtocolor(V_IU_Box_ColorHigh.red,
                                          V_IU_Box_ColorHigh.green,
                                          V_IU_Box_ColorHigh.blue)
    else
      if self.checked then
        _box.Canvas.pen.Color:=rgbtocolor(V_IU_Box_Color_Checked.red,
                                          V_IU_Box_Color_Checked.green,
                                          V_IU_Box_Color_Checked.blue)
      else
        _box.Canvas.pen.Color:=rgbtocolor(V_IU_Box_DefaultColor.red,
                                          V_IU_Box_DefaultColor.green,
                                          V_IU_Box_DefaultColor.blue);
    y := i*2+marge+1;
    self._box.Canvas.MoveTo(0,y);
    self._box.Canvas.LineTo(self._box.width,y);
  end;
  // drawing frame of the box
  self._box.Canvas.Pen.Color := clBlack;
  self._box.Canvas.MoveTo(0,0);
  self._box.Canvas.LineTo(self._box.Width-1, 0);
  self._box.Canvas.Pen.Color := rgbtocolor(V_IU_Box_ColorHigh.red,
                                           V_IU_Box_ColorHigh.green,
                                           V_IU_Box_ColorHigh.blue);
  self._box.Canvas.LineTo(self._box.Width-1, self._box.Height-1);
  self._box.Canvas.LineTo(0, self._box.Height-1);
  self._box.Canvas.Pen.Color := clBlack;
  self._box.Canvas.LineTo(0,0);

  // setting text color
  if not isSelected then
    self._caption.Font.Color:=rgbtocolor(V_IU_Box_TextColorDefault.red,
                                         V_IU_Box_TextColorDefault.green,
                                         V_IU_Box_TextColorDefault.blue)
  else
    self._caption.Font.Color:=rgbtocolor(V_IU_Box_TextColorHigh.red,
                                         V_IU_Box_TextColorHigh.green,
                                         V_IU_Box_TextColorHigh.blue);
end ;

// ***
// * Personnal check box object : Mouse Down event manager
// *
// * @author : Pascal Lemaitre
// *
// ***
procedure T_IU_CheckBox.MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  self.select;
  self.setValue(not(self.getValue));
  self._caption.Parent.Repaint;
  self._caption.Parent.Refresh;
  // ***
  // * Add V0.12
  self._box.Parent.SetFocus; // Set the focus to the form where the control is
                             // else keyboard action can not be captured if form has not the focus
  // ***
  // ***
  // Add V0.11
  // Send paint event on the main form
  self._box.Parent.Refresh;
  self._box.Parent.Repaint;
  // ***

end;

// ***
// * Personnal check box object : Mouse Enter event manager
// *
// * @author : Pascal Lemaitre
// *
// ***
procedure T_IU_CheckBox.MouseEnter(Sender: TObject);
begin
  select ;
  self._draw;
  // ***
  // * Add V0.12
  self._box.Parent.SetFocus; // Set the focus to the form where the control is
                             // else keyboard action can not be captured if form has not the focus
  // ***
  // ***
  // Add V0.11
  // Send paint event on the main form
  self._box.Parent.Refresh;
  self._box.Parent.Repaint;
  // ***

end;

// ***
// * Personnal check box object : Mouse Leave event manager
// *
// * @author : Pascal Lemaitre
// *
// ***
procedure T_IU_CheckBox.MouseLeave(Sender: TObject);
begin
// ***
// * Del V0.11
//  unselect;
//  self._draw;
// ***
end;

// ***
// * Personnal check box object : Paint event manager
// *
// * @author : Pascal Lemaitre
// *
// ***
procedure T_IU_CheckBox.Paint(Sender: TObject);
begin
  self._draw;
end;

// ***
// Add V0.9
// *
// ***
// * Personnal check box object : set hint message
// *
// * @author : Pascal Lemaître
// *
// * in : hint message
// *
// ***
procedure T_IU_CheckBox.setHint(hintSTR : string); // set the hint text
begin
  self._box.Hint:=hintSTR;
  self._caption.Hint:=hintSTR;
end;

// ***
// * Personnal check box object : activate hint message
// *
// * @author : Pascal Lemaître
// *
// ***
procedure T_IU_CheckBox.activateHint ; // activate Hint display when mouse over the control
begin
  self._box.ShowHint:=true;
  self._caption.ShowHint:=true;
end;

// ***
// * Personnal check box object : deactivate hint message
// *
// * @author : Pascal Lemaître
// *
// ***
procedure T_IU_CheckBox.deactivateHint ; // deactivate Hint. Nothing display when mouse over the control
begin
  self._box.ShowHint:=false;
  self._caption.ShowHint:=false;
end;

// ***

// ********************************


// ***
// * Personnal Base class for queued controls : constructor
// *
// * @author : Pascal Lemaître
// *
// * in : number of controls to add with add method
// *
// ***
constructor T_IU_ControlsQueue.Create (NbObjects : word);
begin
  inherited Create;
  self._NbObjects := 0;
  self._TotalAdded := 0;
  self._currentID := 0;
  self._queue := getmem(sizeof(T_IU_BaseQueueRecordPTR) * NbObjects);
  if self._queue = nil then // exception
    raise IU_EGraphicControlQueue_MemoryAllocationError.Create(IU_ExceptionsMessages[IU_CurrentLang, K_IU_ExceptMSG_ControlQueue_NoEnoughtMemoryError])
  else
    self._NbObjects := NbObjects;
end ;

// ***
// * Personnal Base class for queued controls : destructor
// *
// * @author : Pascal Lemaître
// *
// ***
destructor T_IU_ControlsQueue.Destroy ;
begin
  if self._queue<>nil then
    freemem (self._queue);
  self._NbObjects := 0;
  self._TotalAdded := 0;
  self._currentID := 0;
  inherited;
end;

// ***
// * Personnal Base class for queued controls : Add a TWinControl
// *
// * @author : Pascal Lemaître
// *
// * in : Addr of the TWinControl Instance
// *
// ***
procedure T_IU_ControlsQueue.Add(ObjectPTR : integer); // Adding a TWinControl Child (must be casted)
begin
   if self._queue = nil then // error
     raise IU_EGraphicControlQueue_AddError.Create(IU_ExceptionsMessages[IU_CurrentLang, K_IU_ExceptMSG_ControlQueue_AddError])
   else if self._TotalAdded = self._NbObjects then
     raise IU_EGraphicControlQueue_AddFullError.Create(IU_ExceptionsMessages[IU_CurrentLang, K_IU_ExceptMSG_ControlQueue_AddFullError])
   else begin
     (self._queue+self._TotalAdded)^._twincontrol:=true;
     (self._queue+self._TotalAdded)^.wcontrol := ObjectPTR;
     inc(self._TotalAdded);
   end;
end;

// ***
// * Personnal Base class for queued controls : Add a T_IU_BaseControl
// *
// * @author : Pascal Lemaître
// *
// * in : Addr of the T_IU_BaseControl Instance
// *
// ***
procedure T_IU_ControlsQueue.Add(ObjectPTR : word) ; // Adding a personnal control child (must be casted)
begin
   if self._queue = nil then // error
     raise IU_EGraphicControlQueue_AddError.Create(IU_ExceptionsMessages[IU_CurrentLang, K_IU_ExceptMSG_ControlQueue_AddError])
   else if self._TotalAdded = self._NbObjects then
     raise IU_EGraphicControlQueue_AddFullError.Create(IU_ExceptionsMessages[IU_CurrentLang, K_IU_ExceptMSG_ControlQueue_AddFullError])
   else begin
     self._queue[self._TotalAdded]._twincontrol:=false;
     self._queue[self._TotalAdded].IU_Control := ObjectPTR;
     inc(self._TotalAdded);
   end;
end;

// ***
// * Personnal Base class for queued controls : Move Next in the queue
// *
// * @author : Pascal Lemaître
// *
// * if end of the queue is reached then continues at the begining
// *
// ***
procedure T_IU_ControlsQueue.synchronizeQueue ; // Synchronize with mouse event
var i : word ;
  index : word ;
begin
  // looking for control index
  for i := 0 to self._TotalAdded - 1 do begin
    if not self._queue[i]._twincontrol then
      if self._queue[i].IU_Control = _IU_TimControls_selectedID then
        self._currentID := i;
  end;
end;

// ***
// * Personnal Base class for queued controls : Move Next in the queue
// *
// * @author : Pascal Lemaître
// *
// * if end of the queue is reached then continues at the begining
// *
// ***
procedure T_IU_ControlsQueue.MoveNext ; // Move to next control in the queue
begin
  if self._queue = nil then
    raise IU_EGraphicControlQueue_MoveError.Create(IU_ExceptionsMessages[IU_CurrentLang,K_IU_ExceptMSG_ControlQueue_MoveError])
  else begin
    self.synchronizeQueue;
    if self._currentID = self._TotalAdded - 1 then self._currentID:=0
    else inc(self._currentID);
  end;
end;

// ***
// * Personnal Base class for queued controls : Move Pevious in the queue
// *
// * @author : Pascal Lemaître
// *
// * if begining of the queue is reached then continues at the end
// *
// ***
procedure T_IU_ControlsQueue.MovePrevious ; // Move to previous control in the queue
begin
  if self._queue = nil then
    raise IU_EGraphicControlQueue_MoveError.Create(IU_ExceptionsMessages[IU_CurrentLang,K_IU_ExceptMSG_ControlQueue_MoveError])
  else begin
    self.synchronizeQueue;
    if self._currentID = 0 then self._currentID := self._TotalAdded - 1
    else self._currentID := self._currentID - 1;
  end;
end;

// ***
// * Personnal Base class for queued controls : Tries to set the focus on the current control
// *
// * @author : Pascal Lemaître
// *
// ***
procedure T_IU_ControlsQueue.setFocus (var form : TForm) ; // Set the focus on the current control in the queue (auto adapt to the control type)
begin
  if self._queue = nil then
    raise IU_EGraphicControlQueue_setFocusError.Create(IU_ExceptionsMessages[IU_CurrentLang,K_IU_ExceptMSG_ControlQueue_setFocusError])
  else if self._currentID >= self._TotalAdded then
    raise IU_EGraphicControlQueue_setFocusError.Create(IU_ExceptionsMessages[IU_CurrentLang,K_IU_ExceptMSG_ControlQueue_setFocusError])
  else begin
    try
      if self._queue[self._currentID]._twincontrol then begin
        form.ActiveControl := nil;
        TWinControl(Form.Controls[self._queue[self._currentID].wcontrol]).SetFocus;
        _IU_TimControls_selectedID := 0; // deselect own controls
      end else
      begin
        form.ActiveControl := nil;
        _IU_TimControls_selectedID := self._queue[self._currentID].IU_Control;
      end;
    except
      raise IU_EGraphicControlQueue_setFocusError.Create(IU_ExceptionsMessages[IU_CurrentLang,K_IU_ExceptMSG_ControlQueue_setFocusError]);
    end;
    // Refresh form
    form.invalidate;
    form.repaint;
    form.refresh;
  end;
end;

// ***
// * Personnal Base class for queued controls : Must be call when TWinControl get focus on mouse action
// *
// * @author : Pascal Lemaître
// *
// * in : Control index
// *
// ***
procedure T_IU_ControlsQueue.Update (id : integer); // When TWinControl focused
begin
  self.setCurrent(id);
  _IU_TimControls_selectedID := 0;
end;

// ***
// * Personnal Base class for queued controls : Must be call when TWinControl get focus on mouse action
// *
// * @author : Pascal Lemaître
// *
// ***
procedure T_IU_ControlsQueue.Rewind ; // When form is showed
begin
  if self._queue[0]._twincontrol then begin
    self.setCurrent(self._queue[0].wcontrol);
    _IU_TimControls_selectedID := 0;
  end else begin
    self.setCurrent(self._queue[0].IU_Control);
  end;
end;

// ***
// * Personnal Base class for queued controls : set current control for TWinControl when it get the focus
// *
// * @author : Pascal Lemaître
// *
// * in : Control index
// *
// ***
procedure T_IU_ControlsQueue.setCurrent(id : integer); // set current control for TWinControl when it get the focus
var i : word ;
  index : word ;
begin
  index := 0;
  // looking for control index
  for i := 0 to self._TotalAdded - 1 do begin
    if self._queue[i]._twincontrol then
      if self._queue[i].wcontrol = id then index := i;
  end;
  self._currentID:= index;
end;

// ***
// * Personnal Base class for queued controls : set current control for T_IU_BaseControlObject when it get the focus
// *
// * @author : Pascal Lemaître
// *
// * in : Control index
// *
// ***
procedure T_IU_ControlsQueue.setCurrent(id : word); // set current control for T_IU_BaseControlObject when it get the focus
var i : word ;
  index : word ;
begin
  index := 0;
  // looking for control index
  for i := 0 to self._TotalAdded - 1 do begin
    if not self._queue[i]._twincontrol then
      if self._queue[i].wcontrol = id then index := i;
  end;
  self._currentID:= index;
end ;


// ***************************

// ***
// * Personnal radio button object
// *
// * @author : Pascal Lemaitre
// *
// * Class creator
// *
// * in : Image where check cursor will be draw
// * in : Label of the check box (need for events management)
// *
// ***
constructor T_IU_RadioButton.Create(var _check : TImage ; var _label : TLabel);
begin
  inherited Create;
  self.GroupPTR:=nil;
  _box := _check;
  _box.Canvas.Brush.Color:=V_IU_DefaultBG;
  _caption := _label;
  self._caption.Font.Size:=K_IU_BoxTextSize;
  self.checked:=false;
  self.onChange := nil;
  _box.OnMouseDown:=@self.MouseDown;
  _caption.OnMouseDown:=@self.MouseDown;
  _box.OnMouseEnter:=@self.MouseEnter;
  _caption.OnMouseEnter:=@self.MouseEnter;
  _box.OnMouseLeave:=@self.MouseLeave;
  _caption.OnMouseLeave:=@self.MouseLeave;
  _box.OnPaint:=@Paint;
  // ***
  // * Add V0.9
  self.deactivateHint;
  // ***
  // ***
  // * Add V0.7
  _label.Top := _check.Top + (_check.Height - _label.Height);
  // *
  // * End Add V0.7
  // ***
  // ***
  // Add V0.10
  self.canUnselect:=true;
  // ***
  self._draw;
end;

// ***
// * Personnal radio button object
// *
// * @author : Pascal Lemaitre
// *
// * Class destructor
// *
// ***
destructor T_IU_RadioButton.Release;
begin
  inherited;
end;

// ***
// * Personnal radio button object
// *
// * @author : Pascal Lemaitre
// *
// * set the value of the radio button (checked or not)
// *
// * in : checked value (true check, false uncheck)
// *
// * if parameter value is the same as the internal checked value nothing done
// * else internal checked is updated, and the control is redrawed and if there is a call back it is executed.
// *
// ***
procedure T_IU_RadioButton.setValue(_selected : boolean);
begin
  // If no change then no event
  if self.checked <> _selected then begin
    if not self.checked then
      if self.GroupPTR <> nil then
        self.GroupPTR^.updateRB(self.GroupPTR, @self);
    self.checked := _selected;
    self._draw;
    // if no call back no procedure call
    if self.onChange <> nil then
      onChange(self._ID, self.checked);
  end ;
end;


// ***
// * Personnal radio button object
// *
// * @author : Pascal Lemaitre
// *
// * get the value of the radio button (checked or not)
// *
// * return true if checked else return false
// *
// ***
function T_IU_RadioButton.getValue : boolean;
begin
  getValue := self.checked;
end;


// ***
// * Personnal radio button object
// *
// * @author : Pascal Lemaitre
// *
// * Changing value from keyboard action
// *
// * in/out : key pressed from the keybord
// *
// ***
procedure T_IU_RadioButton._setValueFromKeyboard(var _key : word);
begin
  // is the right RB ?
  if isSelected then begin
    // is return key or space key ?
    if (_key = word(13)) or (_key = word(32)) then begin
      // ***
      // * Modified V0.10
      // self.setValue(not(self.getValue)); // redraw is made by setValue procedure, Call back exec is made by setValue
      self.setValue((not(self.getValue)) or (not(self.canUnselect))); // redraw is made by setValue procedure, Call back exec is made by setValue
      // ***
      _key := word(0); // delete key because event has been processed
    end;
  end;
end;


// ***
// * Personnal radio button object
// *
// * @author : Pascal Lemaitre
// *
// * drawing control
// *
// * in/out : key pressed from the keybord
// *
// ***
procedure T_IU_RadioButton._draw;
var i, k, marge, nb_lines, y, _width : integer ;
  len : extended ;
begin
  // drawing back ground
  // drawing selected symbol
  self._box.Canvas.Pen.Width:=1;
  self._box.Canvas.Pen.Color:=rgbtocolor(V_IU_Box_BGColor.red,
                                         V_IU_Box_BGColor.green,
                                         V_IU_Box_BGColor.blue);
  for i := 0 to self._box.Height do begin
    self._box.Canvas.moveto(0,i);
    self._box.Canvas.lineto(self._box.Width,i);
  end;
  // Drawing guideline
  k := self._box.Height div 4;
  marge := k;
  for i := 0 to k do begin
    _box.Canvas.pen.Color:=rgbtocolor(V_IU_Box_DefaultColor.red - K_IU_ScrollBar_HighLight,
                                      V_IU_Box_DefaultColor.green - K_IU_ScrollBar_HighLight,
                                      V_IU_Box_DefaultColor.blue - K_IU_ScrollBar_HighLight);
    y := i*2+marge;
    self._box.Canvas.MoveTo(0,y);
    self._box.Canvas.LineTo(self._box.width,y);
  end;
  // Drawing selected or unselected check box
  k := self._box.Height div 4;
  marge := k ;
  // pen definition for drawing
  self._box.Canvas.Pen.Width:=1;

  _width := min (self._box.Width, self._box.Height);

  for i := 0 to k - 1 do begin
    if isSelected then
      if self.checked then
        _box.Canvas.pen.Color:=rgbtocolor(V_IU_Box_Color_CheckedHigh.red,
                                          V_IU_Box_Color_CheckedHigh.green,
                                          V_IU_Box_Color_CheckedHigh.blue)
      else
        _box.Canvas.pen.Color:=rgbtocolor(V_IU_Box_ColorHigh.red,
                                          V_IU_Box_ColorHigh.green,
                                          V_IU_Box_ColorHigh.blue)
    else
      if self.checked then
        _box.Canvas.pen.Color:=rgbtocolor(V_IU_Box_Color_Checked.red,
                                          V_IU_Box_Color_Checked.green,
                                          V_IU_Box_Color_Checked.blue)
      else
        _box.Canvas.pen.Color:=rgbtocolor(V_IU_Box_DefaultColor.red,
                                          V_IU_Box_DefaultColor.green,
                                          V_IU_Box_DefaultColor.blue);
    y := i*2+marge+1;
    self._box.Canvas.MoveTo(self._box.width div 4,y);
    self._box.Canvas.LineTo(self._box.width - self._box.width div 4,y);
  end;
  // drawing frame of the box
  self._box.Canvas.Pen.Color := clBlack;
  self._box.Canvas.MoveTo(0,0);
  self._box.Canvas.LineTo(self._box.Width-1, 0);
  self._box.Canvas.Pen.Color := rgbtocolor(V_IU_Box_ColorHigh.red,
                                           V_IU_Box_ColorHigh.green,
                                           V_IU_Box_ColorHigh.blue);
  self._box.Canvas.LineTo(self._box.Width-1, self._box.Height-1);
  self._box.Canvas.LineTo(0, self._box.Height-1);
  self._box.Canvas.Pen.Color := clBlack;
  self._box.Canvas.LineTo(0,0);

  // setting text color
  if not isSelected then
    self._caption.Font.Color:=rgbtocolor(V_IU_Box_TextColorDefault.red,
                                         V_IU_Box_TextColorDefault.green,
                                         V_IU_Box_TextColorDefault.blue)
  else
    self._caption.Font.Color:=rgbtocolor(V_IU_Box_TextColorHigh.red,
                                         V_IU_Box_TextColorHigh.green,
                                         V_IU_Box_TextColorHigh.blue);
end ;

// ***
// * Personnal radio button object
// *
// * @author : Pascal Lemaitre
// *
// * Mouse Down event manager
// *
// ***
procedure T_IU_RadioButton.MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  self.select;
  // ***
  // * Modified V0.10
  // *
  // self.setValue(not(self.getValue));
  self.setValue((not(self.getValue) or not(self.canUnselect)));
  // ***
  // ***
  // * Add V0.12
  self._box.Parent.SetFocus; // Set the focus to the form where the control is
                             // else keyboard action can not be captured if form has not the focus
  // ***
  // ***
  // Add V0.11
  // Send paint event on the main form
  self._box.Parent.Refresh;
  self._box.Parent.Repaint;
  // ***
end;

// ***
// * Personnal radio button object
// *
// * @author : Pascal Lemaitre
// *
// * Mouse Enter event manager
// *
// ***
procedure T_IU_RadioButton.MouseEnter(Sender: TObject);
begin
  select ;
  self._draw;
  // ***
  // * Add V0.12
  self._box.Parent.setFocus; // Set the focus to the form where the control is
                                    // else keyboard action can not be captured if form has not the focus
  // ***
  // ***
  // Add V0.11
  // Send paint event on the main form
  self._box.Parent.Refresh;
  self._box.Parent.Repaint;
  // ***
end;

// ***
// * Personnal radio button object
// *
// * @author : Pascal Lemaitre
// *
// * Mouse Leave event manager
// *
// ***
procedure T_IU_RadioButton.MouseLeave(Sender: TObject);
begin
// ***
// * Del V0.11
//  unselect;
//  self._draw;
// ***
end;

// ***
// * Personnal radio button object
// *
// * @author : Pascal Lemaitre
// *
// * Paint event manager
// *
// ***
procedure T_IU_RadioButton.Paint(Sender: TObject);
begin
  self._draw;
end;

// ***
// Add V0.9
// *
// ***
// * Personnal radio button object : set hint message
// *
// * @author : Pascal Lemaître
// *
// * in : hint message
// *
// ***
procedure T_IU_RadioButton.setHint(hintSTR : string); // set the hint text
begin
  self._box.Hint:=hintSTR;
  self._caption.Hint:=hintSTR;
end;

// ***
// * Personnal radio button object : activate hint message
// *
// * @author : Pascal Lemaître
// *
// ***
procedure T_IU_RadioButton.activateHint ; // activate Hint display when mouse over the control
begin
  self._box.ShowHint:=true;
  self._caption.ShowHint:=true;
end;

// ***
// * Personnal radio button object : deactivate hint message
// *
// * @author : Pascal Lemaître
// *
// ***
procedure T_IU_RadioButton.deactivateHint ; // deactivate Hint. Nothing display when mouse over the control
begin
  self._box.ShowHint:=false;
  self._caption.ShowHint:=false;
end;

// ***

// ***
// Add v0.10
// ***
// * Personnal radio button object : deselected method
// *
// * @author : Pascal Lemaître
// *
// ***
procedure T_IU_RadioButton.setUnselect(_status : boolean) ; // true radio button can be unselected with mouse click or with keyboard action
                                                            // false radio button can be unselected with mouse click ou keyboard action
begin
  self.canUnselect:=_status;
end;

// *
// * End Add V0.10
// ***


// ****************************


// ***
// * Personnal Base class for groups of RB
// *
// * @author : Pascal Lemaître
// *
// * Group manager for manage checked coherence beetwen RB in the group
// *
// ***

// ***
// * Personnal Base class for groups of RB
// *
// * @author : Pascal Lemaître
// *
// * Constructor
// *
// ***
constructor T_IU_RB_Group.Create(nbRB : word); // Creator of the group. Need to know max number of RB in the group
begin
  inherited Create;
  self._NbObjects := 0;
  self._TotalAdded := 0;
  self._RB := getmem(sizeof(T_IU_RadioButtonPTR) * NbRB);
  if self._RB = nil then // exception
    raise IU_EGraphicControlRBGroup_MemoryAllocationError.Create(IU_ExceptionsMessages[IU_CurrentLang, K_IU_ExceptMSG_RBGroup_NoEnoughtMemoryError])
  else
    self._NbObjects := NbRB;
end;


// ***
// * Personnal Base class for groups of RB
// *
// * @author : Pascal Lemaître
// *
// * Destructor
// *
// ***
destructor T_IU_RB_Group.Destroy;
var i : word;
begin
  if self._RB <> nil then begin
    for i := 0 to self._TotalAdded - 1 do begin
      if self._RB[i] <> nil then
        try
          self._RB[i]^.GroupPTR:=nil;
        finally
          ;
        end;
    end;
    freemem (self._RB);
  end;
  self._NbObjects := 0;
  self._TotalAdded := 0;
  inherited ;
end;


// ***
// * Personnal Base class for groups of RB
// *
// * @author : Pascal Lemaître
// *
// * Add new RB in the group
// *
// ***
procedure T_IU_RB_Group.Add(Group : T_IU_RB_GroupPTR ; RBPTR : T_IU_RadioButtonPTR); // add a new Radio Button in the group
begin
  if self._RB = nil then // error
    raise IU_EGraphicControlRBGroup_AddError.Create(IU_ExceptionsMessages[IU_CurrentLang, K_IU_ExceptMSG_RBGroup_AddError])
  else if self._TotalAdded = self._NbObjects then
    raise IU_EGraphicControlRBGroup_AddFullError.Create(IU_ExceptionsMessages[IU_CurrentLang, K_IU_ExceptMSG_RBGroup_AddFullError])
  else begin
    self._RB[self._TotalAdded] := RBPTR;
    self._RB[self._TotalAdded]^.GroupPTR:=Group;
    inc(self._TotalAdded);
  end;
end;

// ***
// * Personnal Base class for groups of RB
// *
// * @author : Pascal Lemaître
// *
// * Updating all RB status (not checked) if needed
// *
// ***
procedure T_IU_RB_Group.updateRB(Group : T_IU_RB_GroupPTR ; RBCaller : T_IU_RadioButtonPTR); // need to set of RB in the group except the caller if it set to checked
var i : word;
  _callerID : word;
  local : T_IU_RadioButtonPTR;
begin
  if RBCaller <> nil then begin
      // Getting Caller ID
      _callerID := RBCaller^._getID;
      for i := 0 to self._TotalAdded - 1 do begin
        // Getting Current ID
        local := Group^._RB[i];
        // Checking ID
        if local^._getID <> _callerID then begin
          // checking if RB[i] is checked or not because we don't need to exec the call back when a value doesn't change
          if self._RB[i]^.checked then // here we can change the state to unchecked
            self._RB[i]^.setValue(false); // Need to call the method because the method can exec the right call back
        end;
      end;
  end;

end;




begin
      // ***
      // *
      // * starts init
      // *
      // ***
      _IU_TimControls_indexID := 0;
      _IU_TimControls_selectedID := 0;

      // init some var
      ReturnNilIfGrowHeapFails := true ; // According to IU_Frames unit
      // if getmem fail then nil pointer is returned

      V_IU_DefaultBG := rgbtocolor($44,$44,$44); // default BG color
      with V_IU_PB_DefaultBGColor do begin // default BG color of the shape
        red := $0;
        green := $0;
        blue := $0;
      end ;
      with V_IU_PB_ActivateBG do begin // activated color of the shape
        red := $70;
        green := $70;
        blue := $70;
      end ;
      with V_IU_PB_DistantEndBG do begin // activated color when end is near
        red := $3F;
        green := $80;
        blue :=$3F;
      end;
      with V_IU_PB_NearEndBG  do begin // activated color when end is near
        red := $35;
        green := $95;
        blue := $35;
      end;
      with V_IU_PB_EndBG do begin // acivated color when end
        red := $11;
        green := $C9;
        blue := $11;
      end;
      V_IU_PB_TextColor := rgbtocolor($CC,$CC,$CC); // Color of text

      V_IU_PB_CoefHihgLight_Shape := 100 / K_IU_PB_NbIndicators; // calc here then not to do every time

      V_IU_TB_DefaultBG := rgbtocolor($44,$44,$44) ; // Color of unactived curve for track Bar
      V_IU_TB_ActivatedCurve := rgbtocolor($88,$88,$88) ; // Color for activated cirve for ftrack bar
      V_IU_TB_TextColor := rgbtocolor($CC,$CC,$CC) ; // Color of the text into the Edit box of the Track Bar
      V_IU_TB_TextBGColor := rgbtocolor($44,$44,$44) ; // Back Ground Color of Edit box of the track bar
      V_IU_TB_SelectedActiviatedCurve := rgbtocolor($99,$99,$CF) ; // Color of the cursor of the activated curve
      V_IU_TB_ScaleColor := rgbtocolor($7F,$39,$39); // Color of a scale line
      V_IU_TB_unselectedScaleColor := rgbtocolor($44,$44,$44); // Color of a scale line for unselected track bar


      with V_IU_SB_DefaultBGColor do begin // default BG color of the shape
        red := $50;
        green := $50;
        blue := $50;
      end ;

      with V_IU_SB_CursorColor do begin // default scroll bar color
        red := $B0;
        green := $15;
        blue := $15;
      end;

      with V_IU_SB_CursorColorHigh do begin // HighLight scroll bar cursor color
        red := $D0 + K_IU_ScrollBar_HighLight;
        green := $0 ;
        blue := 0 ;
      end;

      with V_IU_Box_BGColor do begin      // default color for unchecked
        red := $2F;
        green :=$2F;
        blue := $2F;
      end;

      with V_IU_Box_DefaultColor do begin      // default color for unchecked
        red := $50;
        green := $50;
        blue := $50;
      end;

      with V_IU_Box_ColorHigh do begin          // highlight color for unchecked
        red := $80;
        green := $80;
        blue := $80;
      end;

      with V_IU_Box_Color_Checked do begin      // default color for checked
        red := $40;
        green := $B0;
        blue := $40;
      end;

      with V_IU_Box_Color_CheckedHigh do begin  // highlight color for checked
        red := $40;
        green := $E0;
        blue := $40;
      end;

      with V_IU_Box_TextColorDefault do begin   // Default text color
        red := $A0;
        green := $A0;
        blue := $A0;
      end;

      with V_IU_Box_TextColorHigh do begin      // higlight text color
        red := $F0;
        green := $F0;
        blue := $F0;
      end;

      // ***
      // * Add v0.13
      // *
      with V_IU_Button_unfocused do begin
        red := $2c;
        green := $2c;
        blue := $2c;
       end;

      with V_IU_Button_focused do begin
        red := $50;
        green := $50;
        blue := $50;
       end;
      // *
      // * End Add v0.13
      // ***


end.


