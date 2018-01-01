unit IU_Frames;

{$mode objfpc}{$H+}
// ***
// * Unit provides frames declarations and management for internal pictures and threads access (writing, reading, swap ...)
// * Creation Date : 2017 September
// *
// * Version : 0.2
// * Version Date : 2017 December
// * Version Contributors : Pascal Lemaître
// *
// * V0.2 : Adding memeory allocation adaptation according to installed RAM
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
  // IU use BGRABitmap without IU can't compil
  Classes, SysUtils, IU_Types, IU_Exceptions, IU_TemporaryPixFiles, IU_I18N_Messages,
  IU_CriticalSections,
  BGRABitmap, BGRABitmapTypes, IU_BGRA_Interface,   //needed for BGRABitmap <-> Images32 translations
  IU_ImagesUtils, math, IU_GeneralUtils;

// Principle :
// -----------
// IU_Frame manage memory for working.
// Works on pix are made by threads.
// Each thread are in charge of processing an area of the new pix
//      This area are named Thread_Frame
// To do that threads needs to access of the original pix.
// But some of data processing need an area a little more bigger than the
// Thread_Frame. For exemple wavelets need to access to some pixels around
//      the pixel processed. This area is named Access_frame
// And in some case, when pix are very big the memory can't get all of the
//      Access or Thread frame. Then this kind of area can be smaller than the
//      Thread_Frame or Access_Frame. This kind of frame is named Memory_Frame.
//
//    <************************ Pix_Frame *************************>
//    <--- Access_Frame  --->
//                        <--- Access_Frame  --->
//                       |  |                <--- Access_Frame  --->
//    <-- Thread_Frame ---><-- Thread_Frame --><-- Thread_Frame  -->
//    <-- Mem_Frame -->  |  |                |  |
//          <-- Mem_Frame -->                |  |
//                       |<-- Mem_Frame -->  |  |
//                       |  |   <-- Mem_Frame -->
//                       |  |                <-- Mem_Frame -->
//                       |  |                |  |  <-- Mem_Frame -->
//   Recovery area*      <><>                <><>
//   * see IU_Types unit for definition of Recovery area it depands on
//     algorithms.
//
// Each_Access frame are stored on disk in a temporary file
// This is in case of more than one Mem_Frame is needed per thread
// Then each thread can work independently from other threads in its Thread_frame.
// If Mem_Fram is smaller than Access_Frame, Mem_Frame shift with reading
// data from the temp file on disk.
//
// If Mem_Frame is smaller than de Thread_Frame, then the Mem_Thread shift with
// writing data in the temp file on disk.
//
// Each algorith follows columns first for processing.
// Then Frames (especialy Mem_Frame) adapt their width and take all the
// height of the pix.

// Temporary files must be managed by Acces frame not by Mem_Frame.
// Mem_frame only works in RAM.

// MemoryFrame are located into an array of pointers on ram area.
// It's for allocated memory on the heap when pragram is strating.
// If memory couldn't be allocated an exception is raised and the program
// don't start.
// This is done before the creation objects of memory manager.
// Then memory managers have the index of their memoryframe but not directly the
// pointer on the memory area dedicated to memoryframe data.

// ---------------------
// All methods for pixels access must be declared has inline methods. This is for speed optimization.
// When swap is active, stats manager is called each time an pixel is accessed in reading or writting.
// Then for speed optimization, methods must be declared inline.
// Exceptions : swap. In swap, memory access not need to be inline declared. Because disk access time
// is much more slower than memory access. Then creating a new heap context and jump doesn't need to be
// optimized for memory pixels access.
// ---------------------

type
  // ---------- Frames definition section
  // The management of this frame are under the responsability of objects T_IU_MemoryPix
  // (see T_IU_Memory_Pix class definition)
  T_IU_MemoryFrame = packed record
    // _memoryFrame : T_IU_PPictData ; // dynamic allocation
    // For portability, memoryFrame must be a pointer on byte in memory.
    // Then when we need to access pixel type must be cast in a T_IU_PictData for
    // accessing to its structure
    _memoryFrame : ^Byte ;
    isModified : boolean;           // Flag to detect if data of memoryFrame has been modified
    iu_alpha : boolean;             // is an alpha chanel in the pix ?
    iu_whsize : T_IU_WHPixSize ;    // Width and Height of the memoryframe (not of the pix)
    _startcol : T_IU_MaxPix;        // First Col from pix (0,0) coordonates
  end;

  T_IU_PixFrame = packed record
    // PixFrame is the pix itself.
    // This is properties of the pix in internal representation
    iu_alpha : boolean;
    iu_whsize : T_IU_WHPixSize;
  end;

  T_IU_AccessFrame = packed record
    // Access Frame is the area accessed by a thread to make the job
    // This area is greater than the area to calculate
    // The area is defined by start x from the begining of the pix (pixel 0,0) and width. Always all the height of the pix is used.
    // Because MemoryFrame could be smaller than AccesFrame, acces frame must manage an
    // temporary file for swaping.
    _iu_framestartx : T_IU_MaxPix;
    _iu_width : T_IU_MaxPix;
    _iu_temporaryFileForSwap : T_IU_TemporaryPictureFile;
    _swap : boolean; // If swap temporary file is needed.
                     // This is needed if memory frame is fewer than access frame
  end;

  T_IU_ThreadFrame = packed record
    // Thread frame is smaller than acces frame.
    // This is the area to calculate under the responsability of one thread
    // This area is defined by startx from the begining of the pix (pixel 0,0) and width.
    _iu_framestartx : T_IU_MaxPix;
    _iu_width : T_IU_MaxPix;
  end;
  // ---------- End of Frames definition section








  // ---------- Frame manager definitions section
  {
    For complexity management we decompose it by responsability set.
    Each concept (access frame, thread frame and memory frame) are implemented separately in
    dedicated blocks of code
    Just an exception the init. Init initialazing all. Because there are dependencies between concepts



    For frame management we need to manage access area, thread area and memory area
    We can have an object that manage all. Thsi object can include objects dedicated for
    specifics management of access frame, thread frame and memory frame.
    Or we can have a linear approach where all manager are on the same level (we don't
    need to pass thought pix manager, access manager to access at the frame manager).
    The second ways seem better for performance and don't need manu criticals sections to
    access at the right object in many thread environment.
    But it need to share memory area of access frame descriptor, thread frame descriptor between objects.
    We declare :  memoryPix object that init all characteristics of frame for one Pix
                  accessFrame object to manage swap if needed and coordonates transformation form pix reference to access frame reference
                  threadFrame object to manage thread area of pix

    AccessFrame manage de swap. If swap is needed, this frame is in charge of exchanges between memory and swap file.
    To do that AccessFrame use a statistical approach on frame access. It is to optimize mouvements between swap and memory
    But it is done only if swap is needed.

  }

  // Stats manager used by Access Frame manager for swap.
  // It will use only if a swap file is needed the for source frames not for target frames.
  //
  // This is to optimze swap politics.
  // Average access of row are in a area. This area must be in ram to limits swap
  // for example :
  //             X       <- this is the raw in calculation by thread
  //        xxxxx        <- this is the pixels read at the left of which one is calculated
  //              xxxxxx <- this is the pixels read at the right of which one is calculated
  //        <----------> <- this is the area must be in ram to optimize data computing
  // The StatsManager must indicate this area. To do that it calculates row accessed by threads
  // It define the area often accessed. And in some case it can indicate which extra row could be accessed


  // ***
  // * Calc stats on pixels access
  // * it is useful for know which rows must be swap to be efficient
  // *
  // * @author : Pascal Lemaître
  // *
  // * With all access this class define :
  // *                      the low (leffter) row accessed
  // *                      the high (righter) row accessed
  // *                      the average row accessed
  // *                      the lefter average row accessed
  // *                      the righter average row accessed
  // ***
  T_IU_StatsManager = class(TObject)
  private
    { private declarations }
    _mediumRow : longword;      // Adding all row accessed to define the average accessed row
    _averageMinRow : longword;  // Adding all row accessed on the left of the average row
    _averageMaxRow : longword;  // Adding all row accessed on the right of the average row
    _totalMesures : longword;   // How manu row are mesured
    _minRow, _maxRow : word;    // the min/max row accessed on the left/right of the average row
  public
    { public declaration }
    constructor Create() ; // Create and init StatsManager instance
    destructor Destroy ;   // free the instance
    procedure add_row(_row : T_IU_MaxPix) ; inline ; // Adding a new row accessed into the set
    function get_average : T_IU_MaxPix ; inline ;    // Return the average row accessed from the set of measures
    function get_averageMin : T_IU_MaxPix ; inline ; // Return the average min row accessed form the set
    function get_averageMax : T_IU_MaxPix ; inline ; // Return the average max row accessed from the set
    function get_Min : T_IU_MaxPix; inline ;         // Return the real min row accessed
    function get_Max : T_IU_MaxPix; inline ;         // Return the real max row accessed
    procedure reset ; inline ;
    procedure NewRow ; inline ; // Take new line in statistics set
    // When algos change their current row, Stat must be adapted.
    // Else at the end of the process, _minRow will be the first row of the frame
    // _max_Row will be the last row of the frame
    // and _mediumRow will be the average between first and last row of the frame.
    // Then stats will be unusefull. The objective is to know which rows around the current row are accessed
    // To know that for the current row the only way is to follow the current row.
    // It's mean that when algos change their current row all statistical value must be shift to incremente and follow
    // the current row.
    // This method must be call when each algo (when swap is needed) change their current row.
    // But stats is an object declared in a private zone of AccessFrame then access frame should offer a caller of this method

  end;







  // Frames manager global class
  // ***
  // * Manager of the internal picture representation
  // * Calc the characteristics of each frames which are necessary for internal representation
  // *     of a picture
  // *
  // * @author : Pascal Lemaître
  // *
  // * With picture this class define :
  // *                      pixframe (upper internal represatation visible by others methods)
  // *                      accessframe (area accessed by threads (one per thread))
  // *                      threadframe (calculation area for thread (one per thread)) when this area is use
  // *                          for save pixels after a calculation, the threadarea has the same properties than
  // *                          the accessarea.
  // *                      index on memory frame. Memory frame are areas in memory where pixels of a part of picture
  // *                                             are stored. Memory area is allocated and reserved at the program start up.
  // *                                             These area are not created by managers defined in this unit. There only managed
  // *                                             by them.
  // *
  // ***
  T_IU_MemoryPix = class(TObject)
  private
    { private declarations }

    procedure _init(width, height : T_IU_MaxPix ; alpha : boolean ; _type : T_IU_TypeOfFrame ; _origin_preview : T_IU_PixType );
                                                        // init : _iu_pixframe properties
                                                        //        _iu_accessframe properties
                                                        //        _iu_threadframe properties
                                                        //        _iu_memoryframe_idx properties
                                                        //        _type : type frame index (in frame ou out frame)
                                                        //        _origin_preview : is it an origin pix or a preview (no swap for preview)
    // width and height are the width (number of columns) and the height (number of lines) of the picture
    // alpha with or without an alpha chanel
  public
    { public declarations }
    // Type of frame identification
    _typeOfFrame : T_IU_TypeOfFrame;
    _source_temporary_target_frame : T_IU_PixType;
    // --------
    _iu_pixframe : T_IU_PixFrame; // Picture frame
    _iu_accessframe : array [1..K_IU_MaxThreads] of T_IU_AccessFrame; // Access frame (has a temporaty file for swapping)
    _iu_threadframe : array [1..K_IU_MaxThreads] of T_IU_ThreadFrame; // Area under frame calculation responsability
    _iu_memoryframe_idx : array [1..K_IU_MaxThreads] of T_IU_FrameIndex; // index to global array of memoryframe  (data in ram)
                                           // because memory area are allocated when program is strating
                                           // This is done before the creation of T_IU_MemoryPix objects


    constructor Create(width, height : T_IU_MaxPix ; alpha : boolean ;  _type : T_IU_TypeOfFrame);
    constructor PreviewCreate(width, height : T_IU_MaxPix ; alpha : boolean ;  _type : T_IU_TypeOfFrame);
    destructor Destroy;

    procedure getAccessFrameArea(var _accessFrame : T_IU_AccessFrame ; _threadIndex : T_IU_FrameThreadIndex);
    // return the accessframe descriptor (memory area of the descriptor)
    // _accessFrame : out parameter return the _iu_accessFrame area for thread _threadIndex
    //                _threadIndex : Thread for which one area will returned

    procedure getThreadFrameArea(var _threadFrame : T_IU_ThreadFrame ; _threadIndex : T_IU_FrameThreadIndex);
    // return the thread frame descriptor (memory area of the descriptor)
    // _threadFrame : out parameter return th _iu_threadframe area for thread _threadIndex
    //                _threadIndex : Thread for which one area will returned

  end;
  // ---------- End of Frame manager definitions section









  // ---------- Swap frame manager
  {
    swap manager make ling beween Accessframe manager and temporaryfile procedures and functions (see IU_TemporaryPixFiles)
    swap manager is used by Access frame manager.
    Access frame manager gives the area which has to stay in memory.
    Swap manager writes in file the rows before the zone which has to stay in memory
    Swap manager moves columns which has to stay in memory at the beginning of the frame memory area
    Swap manager reads from file the columns has to complete the memory frame area after the area at the beginning of the frame.
  }

  // ***
  // * Swap manager
  // * Make swap action between frames in memory and temporaries files on disk.
  // *
  // * @author : Pascal Lemaître
  // *
  // * This manager is useful only when there is no enought memory (RAM) for storing all pixels of pictures
  // * This manager use stats manager for knowing which rows must be writen on disk, which must be shift in memory
  // *      ans which must be read from disk
  // ***
  T_IU_SwapManager = class(TObject)
    private
      { private declarations }

    public
      { public declarations }
      _row : array [K_MinPix..K_MaxPix] of T_IU_PictData; // Row to be written or read
      // in plublic declaration because accessed by IU_TemporaryPixFile procedures

      constructor Create() ; // For standardization
      destructor Destroy ; // For standardisation

      procedure InitSwap (var _in_bitmap : TBGRABitmap ; _in_temporaryFile : T_IU_TemporaryPictureFile ; _firstrow, _nbRows : T_IU_MaxPix);
      // InitSwap file (already exists) with rows of _in_bitmap
      // _in_bitmap : BGRABitmap from which an area of row will write in swap file
      // _in_temporaryFile : swap temporary file descriptor (see IU_Types)
      //                     it is a record with file name, is an alpha channel and size of the area
      // _firstRow : firstRow of the accessframe to be swapped
      // _nbRows : nb of rows of the accessframe to be swapped

      procedure InitEmptySwap (_in_temporaryFile : T_IU_TemporaryPictureFile);
      // Init an empty swap file (all pixels are set to 0,0,0 and if an alpha channel exists is set to $FFFFFFFF
      // _in_temporayFile : swap temporary file descriptor (See IU_Types)

      procedure WriteRowsSwap (_in_temporaryFile : T_IU_TemporaryPictureFile ; var _in_memoryFrame : T_IU_MemoryFrame ; _in_swapRow, _in_memoryRow, _in_nbRows : T_IU_MaxPix);
      // Writes rows from memoryFrame to temporary swap file
      // _in_temporaryFile : Temporary swap File descriptor
      // _in_memoryFrame : MemoryFrame where rows should be written into the swap file
      // _in_swapRow : start row in swap file where rows should be written
      // _in_memoryRow : start row in memory frame where rows should be written in swap file
      // _in_nbRows : number of row should be written

      procedure ReadRowsSwap (_in_temporaryFile : T_IU_TemporaryPictureFile ; var _out_memoryFrame : T_IU_MemoryFrame ; _in_swapRow, _in_memoryRow, _in_nbRows : T_IU_MaxPix);
      // Read rows from temporary swap file and writes them into memoryFrame
      // _in_temporaryFile : Temporary swap File descriptor
      // _out_memoryFrame : MemoryFrame where rows should be written from the swap file
      // _in_swapRow : start row in swap file where rows should be written
      // _in_memoryRow : start row in memory frame where rows should be written in swap file
      // _in_nbRows : number of row should be written

  end;
  // ---------- End of Swap frame manager







  // ---------- Memory Frame Manager
  {
    Memory Frame manager gives direct access to pixels in memory.
    It makes the coordonates transformation between Bitmap reference and memory frame reference
    If there is enought room in memory frame then the coordonates in bitmap reference and memory frame reference
    are the same.
    Else a conversion must be done and swap is activated
    Then for swap manager Memory Frame Manager offer complete row access (setting row and reading row) to swap manager.
  }
  // ***
  // * Manager of pixels stored in memory (RAM) for a thread
  // *
  // * @author : Pascal Lemaître
  // *
  // * To minize the criticals sections each threads works with dedicated RAM area.
  // * These area are defined by frames (access frame, thread frame and memory frame)
  // * Then each frame are totaly independ from others
  // *
  // * This manager offers write and read acces on pixels managed by it
  // *              access are done in the memory references. Callers must translate
  // *              pixel coordonates into frame coordonate.
  // *
  // * This manager offers a shift service to help access frame manger in swap actions.
  // *              Columns numbers must be expessed in memory frame references
  // ***
  T_IU_MemoryFrameManager = class (TObject)
    private
      { private declarations }
      function _isOutOfBounds (var _memoryFrame : T_IU_MemoryFrame ; _row, _line : T_IU_MaxPix) : boolean ; inline ;
      // Testing if access is out of memory frame manager bounds
      // return true if it is out of bounds else return false

      function _isOutOfBounds (var _memoryFrame : T_IU_MemoryFrame ; _row : T_IU_MaxPix) : boolean ; inline ;
      // Testing if access is out of memory frame manager bounds
      // return true if it is out of bounds else return false


    public
      { public declarations }
      constructor Create;

      destructor Destroy ;

      // Pixels Direct access
      // 1- read pixels accessors
      procedure getPixel (var _memoryFrame : T_IU_MemoryFrame ; var _out_Color : T_IU_Color32 ; _row, _line : T_IU_MaxPix); inline ;
      // get accessor return a pixel RGB without alpha channel with 32 byte by channel
      // if _row or _line is out of memory frame limits then an exception is raised
      // _memoryFrame the frame where pixel is accessed
      // _out_Color : The returned color of the pixel. If the bitmap has an alpha channel, alpha channel is applyied on colors channels
      //              before returned the color
      // _row : the row of the pixel in the picture reference (not in the memory frame reference)
      // _line : the line of the pixel in the picture reference (not in the memory frame reference)

      procedure getPixel (var _memoryFrame : T_IU_MemoryFrame ; var _out_Color : T_IU_AlphaColor32 ; _row, _line : T_IU_MaxPix); inline ;
      // get accessor return a pixel RGB with alpha channel with 32 byte by channel
      // if _row or _line is out of memory frame limits then an exception is raised
      // _memoryFrame the frame where pixel is accessed
      // _out_Color : The returned color of the pixel. If the bitmap has no alpha channel, alpha channel is set to
      //              $FFFFFFFF before returned
      // _row : the row of the pixel in the picture reference (not in the memory frame reference)
      // _line : the line of the pixel in the picture reference (not in the memory frame reference)

      // 2- write pixels accessors
      procedure setPixel (var _memoryFrame : T_IU_MemoryFrame ; var _in_Color : T_IU_Color32 ; _row, _line : T_IU_MaxPix) ;  inline ;
      // set accessor return nothing. Memory frame to set is a parameter transmitted by addr. Then the proc write directly in the source memory frame
      // if _row or _line is out of memory frame limits then an exception is raised
      // _memoryFrame the frame where pixel is accessed
      // _in_Color : The returned color of the pixel. If the bitmap has an alpha channel, alpha channel is set to
      //              $FFFFFFFF
      // _row : the row of the pixel in the picture reference (not in the memory frame reference)
      // _line : the line of the pixel in the picture reference (not in the memory frame reference)

      procedure setPixel (var _memoryFrame : T_IU_MemoryFrame ; var _in_Color : T_IU_AlphaColor32 ; _row, _line : T_IU_MaxPix) ; inline ;
      // set accessor return nothing. Memory frame to set is a parameter transmitted by addr. Then the proc write directly in the source memory frame
      // if _row or _line is out of memory frame limits then an exception is raised
      // _memoryFrame the frame where pixel is accessed
      // _in_Color : The returned color of the pixel. If the bitmap has no alpha channel, alpha channel is applyied
      //             before to set the pixel in memory frame
      // _row : the row of the pixel in the picture reference (not in the memory frame reference)
      // _line : the line of the pixel in the picture reference (not in the memory frame reference)

      // 3- read row accessors
      procedure getRow (var _memoryFrame : T_IU_MemoryFrame ; var _outRow : T_IU_Color32_Addr ; _row : T_IU_MaxPix); inline ;
      // read accessor return the row from memory frame (all pixels of the row) without alpha channel
      // if _row is out of memory frame limits then an exception is raised
      // if memory frame has an alpha channel, alpha channel is applyied before returned the colors of all pixels of the row
      // _memoryFrame : the memory frame where the pixel is accessed
      // _outRow : a pointer on memory area (already allocated to the right size) where row will be copied
      // _row : The row to read

      procedure getRow (var _memoryFrame : T_IU_MemoryFrame ; var _outRow : T_IU_AlphaColor32_Addr ; _row : T_IU_MaxPix); inline ;
      // read accessor return the row from memory frame (all pixels of the row) with alpha channel
      // if _row is out of memory frame limits then an exception is raised
      // if memory frame has no alpha channel, alpha channel is set to $FFFFFFFF before returned the colors of all pixels of the row
      // _memoryFrame : the memory frame where the pixel is accessed
      // _outRow : a pointer on memory area (already allocated to the right size) where row will be copied
      // _row : The row to read

      // 4- write row accessors
      procedure setRow (var _memoryFrame : T_IU_MemoryFrame ; _inRow : T_IU_Color32_Addr ; _row : T_IU_MaxPix); inline ;
      // write accessor return nothing. _memoryFrame is passed by addr.
      // if _row is out of memory frame limits then an exception is raised
      // if memory frame has an alpha channel, alpha channel is set to $FFFFFFFF before setting all pixels of the row
      // _memoryFrame : the memory frame where the pixel is accessed
      // _inRow : a pointer on memory area (already allocated to the right size and setted) where pixels of the row will be read
      // _row : The row to write

      procedure setRow (var _memoryFrame : T_IU_MemoryFrame ; _inRow : T_IU_AlphaColor32_Addr ; _row : T_IU_MaxPix); inline ;
      // write accessor return nothing. _memoryFrame is passed by addr.
      // if _row is out of memory frame limits then an exception is raised
      // if memory frame has no alpha channel, alpha channel is applyied on the colors of all pixels of the row before set
      // _memoryFrame : the memory frame where the pixel is accessed
      // _inRow : a pointer on memory area (already allocated to the right size and setted) where pixels of the row will be read
      // _row : The row to copy

      // 5- shift rows utilities
      procedure shiftRow (var _memoryFrame : T_IU_MemoryFrame ; _startPosRow, _nbRows, _destinationPosRow : T_IU_MaxPix);
      // shiftRow moves _nbRows from row _startPosRow to _destinationPosRow
      // This procedure is needed when swap is activated. Old rows was written on swap file, the rows which stay in memory moves
      // and after new rows will be read from swap file.
      // write in swap file and read from swap file is under the responsability of the swap manager.
      // shift rows is under the responsability of the memory frame manager
      // In this case _startPosRow and _destinationPosRow are in the memory frame references
      // If _startPosRow or _destinationPosRow are out of memory frame limits then an exception is raised
      // If _startPosRow + _nbRows is out of memory frame limits then an exception is raised
      // If _destinationPosRow + _nbRows is out of memory frame limits an exception is raised

      procedure getFrameReferences(var _memoryFrame : T_IU_MemoryFrame ;
                                       var _width, _height, _firstRow : T_IU_MaxPix ;
                                       var _isalpha : boolean);
      // For swap, swap manager need to know memory frame bounds. It is needed for rows shifting.
  end;
  // ---------- End of Memory Frame Manager









  // ---------- Access frame manager
  {
    Access frame manager need swap manager. Frame manager knows if the frame memory needs to be swapped or not.
    To know if ans wich swap is needed, access frame manager use Statsmanager. Statsmanager gives the pattern of
    pixels access. Then from this pattern the next access could be anticipated. Access manager anticipe this futur access
    to define the rows put in file and the row put in memory
    To do that if swap is needed (see memory frame manager init) each pixel's access is recorder with the statsmanager
    The Access manager use swap manager to writes rows in file and to read rows from files
  }
  // ***
  // * Manager of pixels accessed by a thread in memory or on disk
  // *
  // * @author : Pascal Lemaître
  // *
  // * To minize the criticals sections each threads works with dedicated RAM area.
  // * These area are defined by frames (access frame, thread frame and memory frame)
  // * Then each frame are totaly independ from others
  // *
  // * This manager make swap action when pixels accessed are out of bounds of current memory frame window
  // *
  // * This manager offers read and write access on pixels. Acces are made in picture reference not in
  // *              access frame reference. The manager translates picture references into access frame references
  // ***
  T_IU_AccessFrameManager = class(TObject)
    private
      { private declarations }
      _swapStats : T_IU_StatsManager ;
      // Stats manager for defining the best memory frame area when swap is needed
      // It is used only if swap is needed for the pix. If room of memory area is enought then no swap is needed.
      // _swapStats must be created when AccessFrameManager is created
      // _swapStats must be released when AccessFrameManager is released
      // _swapStats is reseted after swap
      // _swapStats is updated at every time an pixel is accessed in the accessframe (except when swap is detected)

      _swapManager : T_IU_SwapManager ;
      // Instance of swap manager for swapin swapout

      _memoryFrameManager : T_IU_MemoryFrameManager ;
      // Instance of memoryframe manager (needed for accessing to rows or pixels and shifting methods)

      _frame : ^T_IU_AccessFrame ; // pointer on access frame descriptor to managed
      _memoryFrame : ^T_IU_MemoryFrame ; // pointer on memory frame descriptor

      procedure _swap (_in_accessedRow : T_IU_MaxPix);
      // Make swap when accessed pixel is out ouf memory frame current area
      // _in_accessedRow : row accessed in read or in write from the beginning of the pix not from the beginning of the access frame
      // !!!!!---- _swap procedure must be call before recording new access in stats.


    public
      { public declarations }
      constructor Create (var _in_accessFrame : T_IU_AccessFrame ; var _in_memoryFrame : T_IU_MemoryFrame);
      // Need to create _swapStats object
      // _accessFrame : descriptor of access frame to be managed (must exists).

      destructor Destroy;
      // Need to release _swapStats object

      procedure ChangeRow ;
      // See stats manager description
      // This procedure is needed to update and aquire correctly stats of accessed rows
      // It just call the stats manager method NewRow

    // ---- TODO
    // _getPixel --> Needed for direct access when threads process picture
    //           --> Threaded
    //           --> Need to convert Access frame coord into memory frame coord
    //               if access frame x start to 500 the memory frame x coord start to 0
    //               if access frame x start to 760 the memory frame x coord start to 0
    // _setPixel --> Needed for direct access when threads process picture
    //           --> Threaded
    //           --> Need to convert Access frame coord into memory frame coord
    //               if access frame x start to 500 the memory frame x coord start to 0
    //               if access frame x start to 760 the memory frame x coord start to 0
    // _getRow --> Is it needed ?
    // _setRow --> Is it needed ?
    // _load from a BGRABitmap : warning of the recovery area --> Needed when a new picture is opened
    //                                                        --> Not Threaded
    // _save into a BGRABitmap : no recovery area has to be copied into target BGRABitmap --> Needed to show or to save a processed picture
    //                                                                                    --> Not Threaded
    // _copy into an other accessframe of an other 32 bits per channel picture --> Needed to validate a processing from temporaty processed to source pix
    //                                                                          --> And to prepare strecht picture for previews 32 bits per channels pictures
    //                                                                          --> Threaded ?
  end;

  // ---------- End of Access frame manager








var
  // Each thread has its own memory frame.
  IU_MemoryFrames : array [1..K_IU_MaxThreads, K_IU_Source_Frames..K_IU_Target_Frames, K_IU_OriginPix..K_IU_PreviewPix] of T_IU_MemoryFrame;
  // One memoryFrame by thread. One Memory thread for reading and writting action and one memory frame for orgininal pix and another one for preview pix

  // Source and target memory frames for threads

  // ----------
  // TODO ----- check init code for memory manager
  //      ----- swap manager for memory manager (use statistics to define the best way for swap)
  //      -----      Define then central column and the area around it for minimizing swap :
  //      -----      Implements all methods as not still implemented
  //      ----- frame memory manager : implements all methods
  //      ----- access frame manager : make declarations and implementations
  // ----------

  procedure IU_AllocMemoryFrames ;
  // Init memoryFrame. Allocate physical memory once.
  // Must be call when program starts
  // This is to prevent from out of memory exception during program is working
  // And to prevent from memory fragmentation.
  // If memory needed by Tim couldn't be allocated then Tim doesn't start.
  // But user is warning why Tim doesn't start. <- Must be done by user interface units

  procedure IU_CleanMemoryFrames ;
  // Clean all memoryFrames. Release physical memory.
  // Must be call when program stops

  procedure IU_InitMemoryFrames (pixHeight : integer ; isAlpha : boolean);
  // Init properties of origin memory frames according to a new pix loaded into tim

  procedure IU_InitPreviewMemoryFrames (pixHeight : integer ; isAlpha : boolean);
  // Init properties of preview memory frames according to a new pix loaded into tim

  procedure IU_SetMemoryFrameToZero (var _memoryFrame : T_IU_MemoryFrame ; _type, _origin_preview : integer);
  // Set memoryFrame with zero bits
  // _type : in Frame ou out frame index type

  procedure IU_MemoryFrame_GetPixel(var _memoryFrame : T_IU_MemoryFrame ; x, y : integer ; var _out_pixel : T_IU_Color32);
  // Getting a pixel at x,y coord without alpha chanel
  //   Picture could have an alpha pixel then alpha pixel is omited

  procedure IU_MemoryFrame_GetPixel(var _memoryFrame : T_IU_MemoryFrame ; x, y : integer ; var _out_pixel : T_IU_AlphaColor32);
  // Getting a pixel at x,y coord without alpha chanel
  //   Picture could haven't an alpha pixel then alpha pixel is set to its max value

  procedure IU_MemoryFrame_SetPixel(var _memoryFrame : T_IU_MemoryFrame ; x, y : integer ; var _in_pixel : T_IU_Color32);
  // Setting a pixel at x,y coord without alpha chanel
  //   Picture could have an alpha pixel then alpha pixel is set to its max value

  procedure IU_MemoryFrame_SetPixel(var _memoryFrame : T_IU_MemoryFrame ; x, y : integer ; var _in_pixel : T_IU_AlphaColor32);
  // Setting a pixel at x,y coord without alpha chanel
  //   Picture could haven't an alpha pixel then alpha pixel is omited

implementation
// ------ Internal procedures ----------------

// ***
// * Internal procedure : Init all memory frames properties for frames type
// * Frame type define if the frame is the origin size of the frame
// *                                is a preview frame
// *
// *                                preview frame doesn't need swap. Their size is rather small to
// *                                store all pixels in memory frame.
// *
// * This procedure must be call when a new picture is loaded
// * It define numbers of cols could be saved in memory according to hight of the loaded picture
// *
// * @author : Pascal Lemaître
// *
// * in : number of rows of the loaded picture pixHeight
// * in : loaded picture has or hasn't an alpha chanel isAlpha
// * in : type of frame for the loaded picture  (source or preview size)
// ***
procedure _internal_IU_InitMemoryFrames (pixHeight : integer ; isAlpha : boolean ; _type : T_IU_PixType);
// Init properties of memory frames according to a new pix loaded into tim
var
  _csize, _nbcolsin, _nbcolsout, i : integer;
begin
  // We need to use critical section if multi threads wants to access
  // to memoryframe descriptor in same time
  EnterCriticalSection(IU_MemoryFrameDeclarationCriticalSection);
  // Cal the size of a column
  // size depends if an alpha chanel exists or not
  if isAlpha then
    _csize := pixHeight * sizeof(T_IU_AlphaColor32)
  else
    _csize := pixHeight * sizeof(T_IU_Color32);
  // Calc number of cols depends on max memoryframe and size of a column
  if _type = K_IU_OriginPix then begin
  _nbcolsin := V_IU_MaxInWindowSize div _csize;
  _nbcolsout := V_IU_MaxOutWindowSize div _csize;
  end else begin
    _nbcolsin := V_IU_MaxInPreviewSize div _csize;
    _nbcolsout := V_IU_MaxOutPreviewSize div _csize;
  end;
  // if nb_cols < 1 then there is a problem pix is too big....
  if (_nbcolsin < 1) or (_nbcolsout < 1) then // should never happend if memory manager works well (swap)
    raise IU_ETooBigPix.Create(IU_ExceptionsMessages[IU_CurrentLang, K_IU_ExceptMSG_TooBigPixError])
  else
    begin
      for i := 1 to K_IU_MaxThreads do
      begin
        // Init of source frames
        IU_MemoryFrames[i,K_IU_Source_Frames,_type].isModified := false ;          // no write action on memory frame
        IU_MemoryFrames[i,K_IU_Source_Frames,_type].iu_alpha := isAlpha;           // memory frame have or not an alpha chanel
        IU_MemoryFrames[i,K_IU_Source_Frames,_type].iu_whsize.width := _nbcolsin;  // Nb cols for this memory frame according to pix properties
        IU_MemoryFrames[i,K_IU_Source_Frames,_type].iu_whsize.height := pixHeight; // Nb lines for this memory frame according to pix properties
        IU_MemoryFrames[i,K_IU_Source_Frames,_type]._startcol:=max(0,((i-1)*_nbcolsout)-K_IU_RecoveryArea);
                                                                             // Starts col need to take the recovery area beetwen frames
                                                                             // But there is no recevery area before the first col of the first frame

        // Init of temporary frames
        IU_MemoryFrames[i,K_IU_Temporary_Frame,_type].isModified := false ;          // read and write actions on memory frame
        IU_MemoryFrames[i,K_IU_Temporary_Frame,_type].iu_alpha := isAlpha;           // memory frame have or not an alpha chanel
        IU_MemoryFrames[i,K_IU_Temporary_Frame,_type].iu_whsize.width := _nbcolsin;  // Nb cols for this memory frame according to pix properties
        IU_MemoryFrames[i,K_IU_Temporary_Frame,_type].iu_whsize.height := pixHeight; // Nb lines for this memory frame according to pix properties
        IU_MemoryFrames[i,K_IU_Temporary_Frame,_type]._startcol:=max(0,((i-1)*_nbcolsout)-K_IU_RecoveryArea);
                                                                             // Starts col need to take the recovery area beetwen frames
                                                                             // But there is no recevery area before the first col of the first frame
        // Init of target frames
        IU_MemoryFrames[i,K_IU_Target_Frames,_type].isModified := false ;          // no write action on memory frame
        IU_MemoryFrames[i,K_IU_Target_Frames,_type].iu_alpha := isAlpha;           // memory frame have or not an alpha chanel
        IU_MemoryFrames[i,K_IU_Target_Frames,_type].iu_whsize.width := _nbcolsout; // Nb cols for this memory frame according to pix properties
        IU_MemoryFrames[i,K_IU_Target_Frames,_type].iu_whsize.height := pixHeight; // Nb lines for this memory frame according to pix properties
        IU_MemoryFrames[i,K_IU_Target_Frames,_type]._startcol:=0;
        IU_MemoryFrames[i,K_IU_Target_Frames,_type]._startcol:=(i-1)*_nbcolSout;
      end;
    end;
  LeaveCriticalSection(IU_MemoryFrameDeclarationCriticalSection);
end;

// ------ End of Internal procedures ----------------















// ------ Implementation of externals procedures and functions --------------
// ***
// * MemoryFrame getPixel without alpha chanel
// *
// * @author : Pascal Lemaître
// *
// * in : memory frame (not the descriptor of the memory frame. The memory frame itself)
// * in : col of the pixel in the memory references (x)
// * in : row of the pixel in the memory references (y)
// * out : the color of the pixel in 32 bits per chanel _out_pixel
// ***
procedure IU_MemoryFrame_GetPixel(var _memoryFrame : T_IU_MemoryFrame ; x, y : integer ; var _out_pixel : T_IU_Color32);
// Getting a pixel at x,y coord without alpha chanel
//   Picture could have an alpha pixel then alpha pixel is omited
var _shift : T_IU_PosPix ;
  _pt : ^Byte;
  _access : T_IU_PPictData;
begin
  // Two ways with or without alpha chanel...
  // not needed to make code for exception forwarding. If x or y out of image bounds the
  //   exception raised by called function is right
  // Warning in IU_PSeek_Pix numcol = number of the col = x and numline = number of line = y
  if _memoryFrame._memoryFrame <> nil then begin
    _shift := IU_PSeek_Pix (_memoryFrame.iu_whsize, y, x, _memoryFrame.iu_alpha);
    // if pix has an alpha chanel getting alpha pixel we do not use it (it is omited)
    _pt := _memoryFrame._memoryFrame + _shift;
    _access := T_IU_PPictData(_pt);
    // T_IU_PictData is a variant record. We need to use correct structure if alpha is or is not present
    // T_IU_PictData = packed record
    //   case alpha : boolean of
    //     false : (IU_V_PixData : T_IU_Color32); Just 3 colors chanels
    //     true  : (IU_V_AlphaPixData : T_IU_AlphaColor32); 1 alpha chanel and a T_IU_Color32 record
    // end ;
    // in this case we only have to use the T_IU_Color32 record
    try
    _out_pixel := _access^.IU_V_PixData;
    except
      raise IU_EMemoryFrameAccessError.Create(IU_ExceptionsMessages[IU_CurrentLang, K_IU_ExceptMSG_MemoryFrameAccessError]);
    end;
  end else
    raise IU_EMemoryFrameAccessError.Create(IU_ExceptionsMessages[IU_CurrentLang, K_IU_ExceptMSG_MemoryFrameAccessError]);
end;


// ***
// * MemoryFrame getPixel with alpha chanel
// *
// * @author : Pascal Lemaître
// *
// * in : memory frame (not the descriptor of the memory frame. The memory frame itself)
// * in : col of the pixel in the memory references (x)
// * in : row of the pixel in the memory references (y)
// * out : the color of the pixel in 32 bits per chanel _out_pixel
// ***
procedure IU_MemoryFrame_GetPixel(var _memoryFrame : T_IU_MemoryFrame ; x, y : integer ; var _out_pixel : T_IU_AlphaColor32);
// Getting a pixel at x,y coord with alpha chanel
//   Picture could haven't an alpha pixel then alpha pixel is set to its max value
var _shift : T_IU_PosPix ;
  _alpha_pixel : T_IU_AlphaColor32;
  _pt : ^Byte;
  _access : T_IU_PPictData;
begin
  // Two ways with or without alpha chanel...
  // not needed to make code for exception forwarding. If x or y out of image bounds the
  //   exception raised by called function is right
  // Warning in IU_PSeek_Pix numcol = number of the col = x and numline = number of line = y
  if _memoryFrame._memoryFrame <> nil then begin
    _shift := IU_PSeek_Pix (_memoryFrame.iu_whsize, y, x, _memoryFrame.iu_alpha);
    _pt := _memoryFrame._memoryFrame + _shift;
    _access := T_IU_PPictData(_pt);
    // T_IU_PictData is a variant record. We need to use correct structure if alpha is or is not present
    // T_IU_PictData = packed record
    //   case alpha : boolean of
    //     false : (IU_V_PixData : T_IU_Color32); Just 3 colors chanels
    //     true  : (IU_V_AlphaPixData : T_IU_AlphaColor32); 1 alpha chanel and a T_IU_Color32 record
    // end ;
    // in this case we to use the right structure
    // if pix has an alpha chanel getting alpha
    try
    if _memoryFrame.iu_alpha then begin
      _alpha_pixel := _access^.IU_V_AlphaPixData;
    end else begin
      //we fixe alpha to max value
      _alpha_pixel.Color := _access^.IU_V_PixData;
      // Setting alpha chanel to maximum value
      _alpha_pixel.alpha:=K_Max32PixelColorValue;
    end;
    except
      raise IU_EMemoryFrameAccessError.Create(IU_ExceptionsMessages[IU_CurrentLang, K_IU_ExceptMSG_MemoryFrameAccessError]);
    end;
    _out_pixel := _alpha_pixel;
  end else
    raise IU_EMemoryFrameAccessError.Create(IU_ExceptionsMessages[IU_CurrentLang, K_IU_ExceptMSG_MemoryFrameAccessError]);
end;


// ***
// * MemoryFrame setPixel without alpha chanel
// *
// * @author : Pascal Lemaître
// *
// * in out : memory frame (not the descriptor of the memory frame. The memory frame itself)
// * in : col of the pixel in the memory reference (x)
// * in : row of the pixel in the memory reference (y)
// * in : the color of the pixel in 32 bits per chanel _in_pixel
// ***
procedure IU_MemoryFrame_SetPixel(var _memoryFrame : T_IU_MemoryFrame ; x, y : integer ; var _in_pixel : T_IU_Color32);
// Setting a pixel at x,y coord without alpha chanel
//   Picture could have an alpha pixel then alpha pixel is omited
var _shift : T_IU_PosPix ;
  _pt : ^Byte;
  _access : T_IU_PPictData;
begin
  // Two ways with or without alpha chanel...
  // not needed to make code for exception forwarding. If x or y out of image bounds the
  //   exception raised by called function is right
  // Warning in IU_PSeek_Pix numcol = number of the col = x and numline = number of line = y
  if _memoryFrame._memoryFrame <> nil then begin
    _shift := IU_PSeek_Pix (_memoryFrame.iu_whsize, y, x, _memoryFrame.iu_alpha);
    // if pix has an alpha chanel alpha pixel is set to its max value (it is omited)
    _pt := _memoryFrame._memoryFrame + _shift;
    _access := T_IU_PPictData(_pt);
    // T_IU_PictData is a variant record. We need to use correct structure if alpha is or is not present
    // T_IU_PictData = packed record
    //   case alpha : boolean of
    //     false : (IU_V_PixData : T_IU_Color32); Just 3 colors chanels
    //     true  : (IU_V_AlphaPixData : T_IU_AlphaColor32); 1 alpha chanel and a T_IU_Color32 record
    // end ;
    try
    if _memoryFrame.iu_alpha then begin
    // in this case we only have to use the T_IU_Color32 record
     _access^.IU_V_PixData := _in_pixel;
    end else begin
      _access^.IU_V_AlphaPixData.Color := _in_pixel;
      _access^.IU_V_AlphaPixData.alpha:= K_Max32PixelColorValue;
    end;
    except
      raise IU_EMemoryFrameAccessError.Create(IU_ExceptionsMessages[IU_CurrentLang, K_IU_ExceptMSG_MemoryFrameAccessError]);
    end;
  end else
    raise IU_EMemoryFrameAccessError.Create(IU_ExceptionsMessages[IU_CurrentLang, K_IU_ExceptMSG_MemoryFrameAccessError]);
end;


// ***
// * MemoryFrame setPixel with alpha chanel
// *
// * @author : Pascal Lemaître
// *
// * in out : memory frame (not the descriptor of the memory frame. The memory frame itself)
// * in : col of the pixel in the memory reference (x)
// * in : row of the pixel in the memory reference (y)
// * in : the color of the pixel in 32 bits per chanel _in_pixel
// ***
procedure IU_MemoryFrame_SetPixel(var _memoryFrame : T_IU_MemoryFrame ; x, y : integer ; var _in_pixel : T_IU_AlphaColor32);
// Setting a pixel at x,y coord with alpha chanel
//   Picture could haven't an alpha pixel then alpha pixel is omited
var _shift : T_IU_PosPix ;
  _pt : ^Byte;
  _access : T_IU_PPictData;
begin
  // Two ways with or without alpha chanel...
  // not needed to make code for exception forwarding. If x or y out of image bounds the
  //   exception raised by called function is right
  // Warning in IU_PSeek_Pix numcol = number of the col = x and numline = number of line = y
  if _memoryFrame._memoryFrame <> nil then begin
    _shift := IU_PSeek_Pix (_memoryFrame.iu_whsize, y, x, _memoryFrame.iu_alpha);
    _pt := _memoryFrame._memoryFrame + _shift;
    _access := T_IU_PPictData(_pt);
    // T_IU_PictData is a variant record. We need to use correct structure if alpha is or is not present
    // T_IU_PictData = packed record
    //   case alpha : boolean of
    //     false : (IU_V_PixData : T_IU_Color32); Just 3 colors chanels
    //     true  : (IU_V_AlphaPixData : T_IU_AlphaColor32); 1 alpha chanel and a T_IU_Color32 record
    // end ;
    // in this case we to use the right structure
    // if pix has an alpha chanel setting  alpha record
    try
    if _memoryFrame.iu_alpha then begin
      _access^.IU_V_AlphaPixData := _in_pixel;
    end else begin
      //we omit alpha value
      _access^.IU_V_PixData := _in_pixel.Color;
    end;
    except
      raise IU_EMemoryFrameAccessError.Create(IU_ExceptionsMessages[IU_CurrentLang, K_IU_ExceptMSG_MemoryFrameAccessError]);
    end;
  end else
    raise IU_EMemoryFrameAccessError.Create(IU_ExceptionsMessages[IU_CurrentLang, K_IU_ExceptMSG_MemoryFrameAccessError]);
end;


// ***
// * MemoryFrame memory allocation and reservation
// * This procedure must be call at the program start up.
// * The objective is to reserve memory for faster execution during processing
// * And to get out of memory error before processing a picture. Then work will not
// *     be lost due to not enought free memory for Tim
// *
// * @author : Pascal Lemaître
// *
// ***
procedure IU_AllocMemoryFrames ;
// Init memoryFrame. Allocate physical memory once.
// Must be call when program starts
var i, j, k : integer;
  _error : boolean;
begin
  // We need to use critical section if multi threads wants to access
  // to memoryframe descriptor in same time
  EnterCriticalSection(IU_MemoryFrameDeclarationCriticalSection);
  // init pointer for error management
  for i := 1 to K_IU_MaxThreads do
    for k := K_IU_OriginPix to K_IU_PreviewPix do begin
    IU_MemoryFrames[i,K_IU_Source_Frames,k]._memoryFrame := nil ;
    IU_MemoryFrames[i,K_IU_Temporary_Frame,k]._memoryFrame := nil ;
    IU_MemoryFrames[i,K_IU_Target_Frames,k]._memoryFrame := nil ;
  end;
  _error := false;
  for i := 1 to K_IU_MaxThreads do
  begin
    for j := K_IU_Source_Frames to K_IU_Target_Frames do begin
      // Trying to allocate all memory
      // In and temporary frame need the the recovery area (see begining of this unit and IU_Types unit explanation (end of the unit)
      if (j=K_IU_Source_Frames) or (j=K_IU_Temporary_Frame) then begin
        IU_MemoryFrames[i,j,K_IU_OriginPix]._memoryFrame := getmem(V_IU_MaxInWindowSize);
      end else begin
        IU_MemoryFrames[i,j,K_IU_OriginPix]._memoryFrame := getmem(V_IU_MaxOutWindowSize);
      end;
      if IU_MemoryFrames[i,j,K_IU_OriginPix]._memoryFrame = nil then
        begin
          _error := true;
          break;
        end;
      if (j=K_IU_Source_Frames) or (j=K_IU_Temporary_Frame) then begin
        IU_MemoryFrames[i,j,K_IU_PreviewPix]._memoryFrame := getmem(V_IU_MaxInPreviewSize);
      end else begin
        IU_MemoryFrames[i,j,K_IU_PreviewPix]._memoryFrame := getmem(V_IU_MaxInPreviewSize);
      end;
      if IU_MemoryFrames[i,j,K_IU_PreviewPix]._memoryFrame = nil then
        begin
          _error := true;
          break;
        end;
      // Something is pretty. Allocation do not allocate realy the Ram but
      // prepare to use dynamicaly ram
      // Then to reserve ram we need to fullfill it with zero data
      try
        IU_SetMemoryFrameToZero(IU_MemoryFrames[i,j,K_IU_OriginPix],j,K_IU_OriginPix);
      except
        _error := true;
        break;
      end;
      try
        IU_SetMemoryFrameToZero(IU_MemoryFrames[i,j,K_IU_PreviewPix],j,K_IU_PreviewPix);
      except
        _error := true;
        break;
      end;
    end;
  end;
  // in case of error release allocated memory
  if _error then begin
    for i := 1 to K_IU_MaxThreads do
    begin
      for j := K_IU_Source_Frames to K_IU_Target_Frames do begin
        if IU_MemoryFrames[i,j,K_IU_OriginPix]._memoryFrame <> nil then begin
          freemem(IU_MemoryFrames[i,j,K_IU_OriginPix]._memoryFrame);
        end;
      end;
    end;
    LeaveCriticalSection(IU_MemoryFrameDeclarationCriticalSection);
    // then raise an no enought memory exception
    raise IU_ENoEnoughtMemoryError.Create(IU_ExceptionsMessages[IU_CurrentLang, K_IU_ExceptMSG_NoEnoughtMemoryError]);
  end;
  LeaveCriticalSection(IU_MemoryFrameDeclarationCriticalSection);
end;


// ***
// * MemoryFrame release memory
// * This procedure must be call when program is stopping
// * It cleans and release all memory frame
// *
// * @author : Pascal Lemaître
// *
// ***
procedure IU_CleanMemoryFrames ;
// Clean all memoryFrames. Release physical memory.
// Must be call when program stops
var i, j, k : integer ;
begin
  // We need to use critical section if multi threads wants to access
  // to memoryframe descriptor in same time
  EnterCriticalSection(IU_MemoryFrameDeclarationCriticalSection);
  for i := 1 to K_IU_MaxThreads do
  begin
    for j := K_IU_Source_Frames to K_IU_Target_Frames do
      for k := K_IU_OriginPix  to K_IU_PreviewPix do
        if IU_MemoryFrames[i,j,k]._memoryFrame <> nil then begin // only allocated memory is released
          freemem(IU_MemoryFrames[i,j,k]._memoryFrame);
          IU_MemoryFrames[i,j,k]._memoryFrame := nil; // then we can free an no allocated ram
        end;
  end;
  LeaveCriticalSection(IU_MemoryFrameDeclarationCriticalSection);
end;


// ***
// * Initialisation of frames for source and temporary frames.
// * Just call the internal init procedure with good parameters
// *
// * @author : Pascal Lemaître
// *
// * in : number of rows of the loaded picture pixHeight
// * in : loaded picture has or hasn't an alpha chanel isAlpha
// ***
procedure IU_InitMemoryFrames (pixHeight : integer ; isAlpha : boolean);
// Init properties of origin memory frames according to a new pix loaded into tim
begin
  _internal_IU_InitMemoryFrames (pixHeight, isAlpha, K_IU_OriginPix);
end;


// ***
// * Initialisation of frames for previews frames.
// * Just call the internal init procedure with good parameters
// *
// * @author : Pascal Lemaître
// *
// * in : number of rows of the loaded picture pixHeight
// * in : loaded picture has or hasn't an alpha chanel isAlpha
// ***
procedure IU_InitPreviewMemoryFrames (pixHeight : integer ; isAlpha : boolean);
// Init properties of preview memory frames according to a new pix loaded into tim
begin
  _internal_IU_InitMemoryFrames (pixHeight, isAlpha, K_IU_PreviewPix);
end;


// ***
// * Fullfilling memory frames with 0 byte value
// * Allocation prepare the memory reservation. But realy do not lock it for application usage
// * To realy lock it we need to access at the memory.
// * This is to check if all memory could realy be reserved.
// * And to minimize memory segmentation
// *
// * @author : Pascal Lemaître
// *
// * in : memory frame descriptor
// * in : source, temporary or taget memory frame (size are different between them
// *                                               see IU_BackGroundExtractionArea in IU_Types unit)
// * in : origin or preview size for the frame
// ***
procedure IU_SetMemoryFrameToZero (var _memoryFrame : T_IU_MemoryFrame ; _type, _origin_preview : integer);
// Set memoryFrame with zero bits
var _pt : ^Byte;
begin
  _pt := _memoryFrame._memoryFrame;
  if _pt <> nil then begin
    if (_type = K_IU_Source_Frames) or (_type = K_IU_Temporary_Frame) then begin
      if _origin_preview = K_IU_OriginPix then
        fillchar((_pt^), V_IU_MaxInWindowSize, 0)
      else
        fillchar((_pt^), V_IU_MaxInPreviewSize, 0);
    end else begin
      if _origin_preview = K_IU_OriginPix then
        fillchar((_pt^), V_IU_MaxOutWindowSize, 0)
      else
        fillchar((_pt^), V_IU_MaxOutPreviewSize, 0);
    end;
  end;
end;

// ------ End of Implementation of externals procedures and functions --------------














// ------ memory frame manager implementation
// ***
// * Memory Frame Manager : testing if accessed pixel is out bounds
// *
// * @author : Pascal Lemaître
// *
// * in : memory frame descriptor _memoryFrame
// * in : accessed row (x) in picture reference _row
// * in : accessed line (y) in picture reference _line
// *
// * return true if out of bounds, else return false
// ***
function T_IU_MemoryFrameManager._isOutOfBounds (var _memoryFrame : T_IU_MemoryFrame ; _row, _line : T_IU_MaxPix) : boolean ; inline ;
// Testing if access is out of memory frame manager bounds
// return true if it is out of bounds else return false
begin
  if _row < _memoryFrame._startcol then
    _isOutOfBounds := true
  else if _row > _memoryFrame._startcol + _memoryFrame.iu_whsize.width then
    _isOutOfBounds := true
  else if _line < 0 then
    _isOutOfBounds := true
  else if _line > _memoryFrame.iu_whsize.height then
    _isOutOfBounds := true
  else _isOutOfBounds := false;
end;


// ***
// * Memory Frame Manager : testing if accessed row is out bounds
// *
// * @author : Pascal Lemaître
// *
// * in : memory frame descriptor _memoryFrame
// * in : accessed row (x) in picture reference _row
// *
// * return true if out of bounds, else return false
// ***
function T_IU_MemoryFrameManager._isOutOfBounds (var _memoryFrame : T_IU_MemoryFrame ; _row : T_IU_MaxPix) : boolean ; inline ;
// Testing if access is out of memory frame manager bounds
// return true if it is out of bounds else return false
begin
  if _row < _memoryFrame._startcol then
    _isOutOfBounds := true
  else if _row > _memoryFrame._startcol + _memoryFrame.iu_whsize.width then
    _isOutOfBounds := true
  else _isOutOfBounds := false;
end;


// ***
// * Memory Frame Manager : Constructor
// *
// * @author : Pascal Lemaître
// *
// ***
constructor T_IU_MemoryFrameManager.Create;
begin
  // nothing to do... Just for compatibility
end;


// ***
// * Memory Frame Manager : Destructor
// *
// * @author : Pascal Lemaître
// *
// ***
destructor T_IU_MemoryFrameManager.Destroy ;
begin
  // just call inherited method
  inherited;
end;

// Pixels Direct access
// 1- read pixels accessors

// ***
// * Memory Frame Manager : read pixel access without alpha chanel
// *
// * @author : Pascal Lemaître
// *
// * in : memory frame descriptor _memoryFrame
// * out : 32 bits per chanel pixel color without alpha chanel _out_Color
// * in : accessed row (x) in picture reference _row
// * in : accessed line (y) in picture reference _line
// *
// ***
procedure T_IU_MemoryFrameManager.getPixel (var _memoryFrame : T_IU_MemoryFrame ; var _out_Color : T_IU_Color32 ; _row, _line : T_IU_MaxPix); inline ;
// get accessor return a pixel RGB without alpha channel with 32 byte by channel
// if _row or _line is out of memory frame limits then an exception is raised
// _memoryFrame the frame where pixel is accessed
// _out_Color : The returned color of the pixel. If the bitmap has an alpha channel, alpha channel is applyied on colors channels
//              before returned the color
// _row : the row of the pixel in the picture reference (not in the memory frame reference)
// _line : the line of the pixel in the picture reference (not in the memory frame reference)
var _x, _y : T_IU_MaxPix;
  _pshift : T_IU_RealPosPix;
  _p : ^Byte;
  _coefalpha : float;
begin
  // Testing limits
  if _isOutOfBounds(_memoryFrame, _row, _line) then
    raise IU_EOutOfMemoryFrameBounds.Create(IU_ExceptionsMessages[IU_CurrentLang, K_IU_ExceptMSG_MemoryFrameManager_OutOfBounds])
  else begin
    // Calc the position of the pixel in the memory frame reference
    _x := _row - _memoryFrame._startcol;
    _y := _line;
    // transform coord into a pointer shifting
    _pshift := IU_PSeek_Pix(_memoryFrame.iu_whsize,_y,_x,_memoryFrame.iu_alpha);
    // shifting the pointer
    _p := _memoryFrame._memoryFrame + _pshift;
    // If is an alpha channel in memoryFrame gets its value
    if _memoryFrame.iu_alpha then begin
      _coefalpha := (T_IU_AlphaColor32_Addr(_p))^.alpha / $FFFFFFFF;
      _out_Color.R:=(T_IU_AlphaColor32_Addr(_p))^.Color.R;
      _out_Color.G:=(T_IU_AlphaColor32_Addr(_p))^.Color.G;
      _out_Color.B:=(T_IU_AlphaColor32_Addr(_p))^.Color.B;
    end else begin // (no conversion)
      // Getting colors channels
      _out_Color.R:=(T_IU_Color32_Addr(_p))^.R;
      _out_Color.G:=(T_IU_Color32_Addr(_p))^.G;
      _out_Color.B:=(T_IU_Color32_Addr(_p))^.B;
    end;
  end;
end;


// ***
// * Memory Frame Manager : read pixel access with alpha chanel
// *
// * @author : Pascal Lemaître
// *
// * in : memory frame descriptor _memoryFrame
// * out : 32 bits per chanel pixel color with alpha chanel _out_Color
// * in : accessed row (x) in picture reference _row
// * in : accessed line (y) in picture reference _line
// *
// ***
procedure T_IU_MemoryFrameManager.getPixel (var _memoryFrame : T_IU_MemoryFrame ; var _out_Color : T_IU_AlphaColor32 ; _row, _line : T_IU_MaxPix); inline ;
// get accessor return a pixel RGB with alpha channel with 32 byte by channel
// if _row or _line is out of memory frame limits then an exception is raised
// _memoryFrame the frame where pixel is accessed
// _out_Color : The returned color of the pixel. If the bitmap has no alpha channel, alpha channel is set to
//              $FFFFFFFF before returned
// _row : the row of the pixel in the picture reference (not in the memory frame reference)
// _line : the line of the pixel in the picture reference (not in the memory frame reference)
var _x, _y : T_IU_MaxPix;
  _pshift : T_IU_RealPosPix;
  _p : ^Byte;
begin
  // Testing limits
  if _isOutOfBounds(_memoryFrame, _row, _line) then
    raise IU_EOutOfMemoryFrameBounds.Create(IU_ExceptionsMessages[IU_CurrentLang, K_IU_ExceptMSG_MemoryFrameManager_OutOfBounds])
  else begin
    // Calc the position of the pixel in the memory frame reference
    _x := _row - _memoryFrame._startcol;
    _y := _line;
    // transform coord into a pointer shifting
    _pshift := IU_PSeek_Pix(_memoryFrame.iu_whsize,_y,_x,_memoryFrame.iu_alpha);
    // shifting the pointer
    _p := _memoryFrame._memoryFrame + _pshift;
    // If is not alpha channel in memoryFrame set its value to $FFFFFFFF
    if not(_memoryFrame.iu_alpha) then begin
      _out_Color.alpha:=$FFFFFFFF;
      _out_Color.Color.R:=(T_IU_Color32_Addr(_p))^.R;
      _out_Color.Color.G:=(T_IU_Color32_Addr(_p))^.R;
      _out_Color.Color.B:=(T_IU_Color32_Addr(_p))^.R;
    end else begin
      _out_Color.alpha:=(T_IU_AlphaColor32_Addr(_p))^.alpha ;
      // Getting colors channels
      _out_Color.Color.R:=(T_IU_AlphaColor32_Addr(_p))^.Color.R;
      _out_Color.Color.G:=(T_IU_AlphaColor32_Addr(_p))^.Color.G;
      _out_Color.Color.B:=(T_IU_AlphaColor32_Addr(_p))^.Color.B;
    end;
  end;
end;

// 2- write pixels accessors

// ***
// * Memory Frame Manager : write pixel access without alpha chanel
// *
// * @author : Pascal Lemaître
// *
// * in : memory frame descriptor _memoryFrame
// * out : 32 bits per chanel pixel color without alpha chanel _inColor
// * in : accessed row (x) in picture reference _row
// * in : accessed line (y) in picture reference _line
// *
// ***
procedure T_IU_MemoryFrameManager.setPixel (var _memoryFrame : T_IU_MemoryFrame ; var _in_Color : T_IU_Color32 ; _row, _line : T_IU_MaxPix) ; inline ;
// set accessor return nothing. Memory frame to set is a parameter transmitted by addr. Then the proc write directly in the source memory frame
// if _row or _line is out of memory frame limits then an exception is raised
// _memoryFrame the frame where pixel is accessed
// _in_Color : The returned color of the pixel. If the bitmap has an alpha channel, alpha channel is set to
//              $FFFFFFFF
// _row : the row of the pixel in the picture reference (not in the memory frame reference)
// _line : the line of the pixel in the picture reference (not in the memory frame reference)
var _x, _y : T_IU_MaxPix;
  _pshift : T_IU_RealPosPix;
  _p : ^Byte;
begin
  // Testing limits
  if _isOutOfBounds(_memoryFrame, _row, _line) then
    raise IU_EOutOfMemoryFrameBounds.Create(IU_ExceptionsMessages[IU_CurrentLang, K_IU_ExceptMSG_MemoryFrameManager_OutOfBounds])
  else begin
    // Calc the position of the pixel in the memory frame reference
    _x := _row - _memoryFrame._startcol;
    _y := _line;
    // transform coord into a pointer shifting
    _pshift := IU_PSeek_Pix(_memoryFrame.iu_whsize,_y,_x,_memoryFrame.iu_alpha);
    // shifting the pointer
    _p := _memoryFrame._memoryFrame + _pshift;
    // If is an alpha channel in memoryFrame sets its value to $FF
    if _memoryFrame.iu_alpha then begin
      (T_IU_AlphaColor32_Addr(_p))^.alpha := $FF;
      (T_IU_AlphaColor32_Addr(_p))^.Color.R := _in_Color.R;
      (T_IU_AlphaColor32_Addr(_p))^.Color.G := _in_Color.G;
      (T_IU_AlphaColor32_Addr(_p))^.Color.B := _in_Color.B;
    end else begin // (no conversion)
      // Getting colors channels
      (T_IU_Color32_Addr(_p))^.R := _in_Color.R;
      (T_IU_Color32_Addr(_p))^.G := _in_Color.G;
      (T_IU_Color32_Addr(_p))^.B := _in_Color.B;
    end;
  end;
end;


// ***
// * Memory Frame Manager : write pixel access with alpha chanel
// *
// * @author : Pascal Lemaître
// *
// * in : memory frame descriptor _memoryFrame
// * out : 32 bits per chanel pixel color with alpha chanel _inColor
// * in : accessed row (x) in picture reference _row
// * in : accessed line (y) in picture reference _line
// *
// ***
procedure T_IU_MemoryFrameManager.setPixel (var _memoryFrame : T_IU_MemoryFrame ; var _in_Color : T_IU_AlphaColor32 ; _row, _line : T_IU_MaxPix) ; inline ;
// set accessor return nothing. Memory frame to set is a parameter transmitted by addr. Then the proc write directly in the source memory frame
// if _row or _line is out of memory frame limits then an exception is raised
// _memoryFrame the frame where pixel is accessed
// _in_Color : The returned color of the pixel. If the bitmap has no alpha channel, alpha channel is applyied
//             before to set the pixel in memory frame
// _row : the row of the pixel in the picture reference (not in the memory frame reference)
// _line : the line of the pixel in the picture reference (not in the memory frame reference)
var _x, _y : T_IU_MaxPix;
  _pshift : T_IU_RealPosPix;
  _p : ^Byte;
  _coefalpha : float;
begin
  // Testing limits
  if _isOutOfBounds(_memoryFrame, _row, _line) then
    raise IU_EOutOfMemoryFrameBounds.Create(IU_ExceptionsMessages[IU_CurrentLang, K_IU_ExceptMSG_MemoryFrameManager_OutOfBounds])
  else begin
    // Calc the position of the pixel in the memory frame reference
    _x := _row - _memoryFrame._startcol;
    _y := _line;
    // transform coord into a pointer shifting
    _pshift := IU_PSeek_Pix(_memoryFrame.iu_whsize,_y,_x,_memoryFrame.iu_alpha);
    // shifting the pointer
    _p := _memoryFrame._memoryFrame + _pshift;
    // If isn't an alpha channel in memoryFrame apply it on colors channels
    if _memoryFrame.iu_alpha then begin
      _coefalpha := _in_Color.alpha / $FFFFFFFF;
      (T_IU_Color32_Addr(_p))^.R:=trunc(_in_Color.Color.R * _coefalpha);
      (T_IU_Color32_Addr(_p))^.G:=trunc(_in_Color.Color.G * _coefalpha);
      (T_IU_Color32_Addr(_p))^.B:=trunc(_in_Color.Color.B * _coefalpha);
    end else begin // (no conversion)
      // Getting colors channels
      (T_IU_AlphaColor32_Addr(_p))^.alpha:=_in_Color.alpha;
      (T_IU_AlphaColor32_Addr(_p))^.Color.R:=_in_Color.Color.R;
      (T_IU_AlphaColor32_Addr(_p))^.Color.G:=_in_Color.Color.G;
      (T_IU_AlphaColor32_Addr(_p))^.Color.B:=_in_Color.Color.B;
    end;
  end;
end;

// 3- read row accessors

// ***
// * Memory Frame Manager : accessing a row with pixels without alpha chanel
// *
// * @author : Pascal Lemaître
// *
// * in : memory frame descriptor _memoryFrame
// * out : first pixel of row with 32 bits per chanel pixel color without alpha chanel _outRow
// * in : accessed row (x) in picture reference _row
// *
// ***
procedure T_IU_MemoryFrameManager.getRow (var _memoryFrame : T_IU_MemoryFrame ; var _outRow : T_IU_Color32_Addr ; _row : T_IU_MaxPix); inline ;
// read accessor return the row from memory frame (all pixels of the row) without alpha channel
// if _row is out of memory frame limits then an exception is raised
// if memory frame has an alpha channel, alpha channel is applyied before returned the colors of all pixels of the row
// _memoryFrame : the memory frame where the pixel is accessed
// _outRow : a pointer on memory area (already allocated to the right size) where row will be copied
// _row : The row to read
var _i : T_IU_MaxPix;
  _p : T_IU_Color32_Addr;
begin
  // Testing if _outRow is not nil
  if _outRow = nil then // raise exception
    raise IU_ERowMemoryNotAllocated.Create(IU_ExceptionsMessages[IU_CurrentLang, K_IU_ExceptMSG_MemoryFrameManager_MemoryNotAllocated])
  else begin
    // Checkinf if _row in memory frame bounds
    if _isOutOfBounds(_memoryFrame, _row) then
      raise IU_EOutOfMemoryFrameBounds.Create(IU_ExceptionsMessages[IU_CurrentLang, K_IU_ExceptMSG_MemoryFrameManager_OutOfBounds])
    else begin
      // Init pixel pointer on the first line of the row
      _p := @_outrow ;
      for _i := 0 to _memoryFrame.iu_whsize.height-1 do begin
        self.getPixel(_memoryFrame, (_p)^, _row, _i);
        inc (_p);
      end;
    end;
  end;
end;

// ***
// * Memory Frame Manager : accessing a row with pixels with alpha chanel
// *
// * @author : Pascal Lemaître
// *
// * in : memory frame descriptor _memoryFrame
// * out : first pixel of row with 32 bits per chanel pixel color with alpha chanel _outRow
// * in : accessed row (x) in picture reference _row
// *
// ***
procedure T_IU_MemoryFrameManager.getRow (var _memoryFrame : T_IU_MemoryFrame ; var _outRow : T_IU_AlphaColor32_Addr ; _row : T_IU_MaxPix); inline ;
// read accessor return the row from memory frame (all pixels of the row) with alpha channel
// if _row is out of memory frame limits then an exception is raised
// if memory frame has no alpha channel, alpha channel is set to $FFFFFFFF before returned the colors of all pixels of the row
// _memoryFrame : the memory frame where the pixel is accessed
// _outRow : a pointer on memory area (already allocated to the right size) where row will be copied
// _row : The row to read
var _i : T_IU_MaxPix;
  _p : T_IU_AlphaColor32_Addr;
begin
  // Testing if _outRow is not nil
  if _outRow = nil then // raise exception
    raise IU_ERowMemoryNotAllocated.Create(IU_ExceptionsMessages[IU_CurrentLang, K_IU_ExceptMSG_MemoryFrameManager_MemoryNotAllocated])
  else begin
    // Checkinf if _row in memory frame bounds
    if _isOutOfBounds(_memoryFrame, _row) then
      raise IU_EOutOfMemoryFrameBounds.Create(IU_ExceptionsMessages[IU_CurrentLang, K_IU_ExceptMSG_MemoryFrameManager_OutOfBounds])
    else begin
      // Init pixel pointer on the first line of the row
      _p := @_outrow ;
      for _i := 0 to _memoryFrame.iu_whsize.height-1 do begin
        self.getPixel(_memoryFrame, (_p)^, _row, _i);
        inc (_p);
      end;
    end;
  end;
end;

// 4- write row accessors

// ***
// * Memory Frame Manager : write row with pixels without alpha chanel
// *
// * @author : Pascal Lemaître
// *
// * in : memory frame descriptor _memoryFrame
// * in : address of the first pixel in a buffer to write in the frame. Pixels of row have 32 bits per chanel pixel color without alpha chanel _inRow
// * in : accessed row (x) in picture reference _row
// *
// ***
procedure T_IU_MemoryFrameManager.setRow (var _memoryFrame : T_IU_MemoryFrame ; _inRow : T_IU_Color32_Addr ; _row : T_IU_MaxPix); inline ;
// write accessor return nothing. _memoryFrame is passed by addr.
// if _row is out of memory frame limits then an exception is raised
// if memory frame has an alpha channel, alpha channel is set to $FFFFFFFF before setting all pixels of the row
// _memoryFrame : the memory frame where the pixel is accessed
// _inRow : a pointer on memory area (already allocated to the right size and setted) where pixels of the row will be read
// _row : The row to write
var _i : T_IU_MaxPix;
  _p : T_IU_Color32_Addr;
begin
  // Testing if _outRow is not nil
  if _inRow = nil then // raise exception
    raise IU_ERowMemoryNotAllocated.Create(IU_ExceptionsMessages[IU_CurrentLang, K_IU_ExceptMSG_MemoryFrameManager_MemoryNotAllocated])
  else begin
    // Checkinf if _row in memory frame bounds
    if _isOutOfBounds(_memoryFrame, _row) then
      raise IU_EOutOfMemoryFrameBounds.Create(IU_ExceptionsMessages[IU_CurrentLang, K_IU_ExceptMSG_MemoryFrameManager_OutOfBounds])
    else begin
      // Init pixel pointer on the first line of the row
      _p := @_inrow ;
      for _i := 0 to _memoryFrame.iu_whsize.height-1 do begin
        self.setPixel(_memoryFrame, (_p)^, _row, _i);
        inc (_p);
      end;
    end;
  end;
end;


// ***
// * Memory Frame Manager : write row with pixels with alpha chanel
// *
// * @author : Pascal Lemaître
// *
// * in : memory frame descriptor _memoryFrame
// * in : address of the first pixel in a buffer to write in the frame. Pixels of row have 32 bits per chanel pixel color with alpha chanel _inRow
// * in : accessed row (x) in picture reference _row
// *
// ***
procedure T_IU_MemoryFrameManager.setRow (var _memoryFrame : T_IU_MemoryFrame ; _inRow : T_IU_AlphaColor32_Addr ; _row : T_IU_MaxPix); inline ;
// write accessor return nothing. _memoryFrame is passed by addr.
// if _row is out of memory frame limits then an exception is raised
// if memory frame has no alpha channel, alpha channel is applyied on the colors of all pixels of the row before set
// _memoryFrame : the memory frame where the pixel is accessed
// _inRow : a pointer on memory area (already allocated to the right size and setted) where pixels of the row will be read
// _row : The row to copy
var _i : T_IU_MaxPix;
  _p : T_IU_AlphaColor32_Addr;
begin
  // Testing if _outRow is not nil
  if _inRow = nil then // raise exception
    raise IU_ERowMemoryNotAllocated.Create(IU_ExceptionsMessages[IU_CurrentLang, K_IU_ExceptMSG_MemoryFrameManager_MemoryNotAllocated])
  else begin
    // Checkinf if _row in memory frame bounds
    if _isOutOfBounds(_memoryFrame, _row) then
      raise IU_EOutOfMemoryFrameBounds.Create(IU_ExceptionsMessages[IU_CurrentLang, K_IU_ExceptMSG_MemoryFrameManager_OutOfBounds])
    else begin
      // Init pixel pointer on the first line of the row
      _p := @_inrow ;
      for _i := 0 to _memoryFrame.iu_whsize.height-1 do begin
        self.getPixel(_memoryFrame, (_p)^, _row, _i);
        inc (_p);
      end;
    end;
  end;
end;

// 5- shift rows utilities

// ***
// * Memory Frame Manager : shift rows in a memory frame
// *
// * @author : Pascal Lemaître
// *
// * in : memory frame descriptor _memoryFrame
// * in : source of starting row to shift _startPosRow
// * in : number of rows to shift _nbRows
// * in : destination of shift _destinationPosRow
// *
// * Method make shift mode choce itself
// * Callers just give start pos destination pos and how many rows must be shifted
// * Method is autoadaptative with type of color (with or without alpha chanel)
// ***
procedure T_IU_MemoryFrameManager.shiftRow (var _memoryFrame : T_IU_MemoryFrame ; _startPosRow, _nbRows, _destinationPosRow : T_IU_MaxPix);
// shiftRow moves _nbRows from row _startPosRow to _destinationPosRow
// This procedure is needed when swap is activated. Old rows was written on swap file, the rows which stay in memory moves
// and after new rows will be read from swap file.
// write in swap file and read from swap file is under the responsability of the swap manager.
// shift rows is under the responsability of the memory frame manager
// In this case _startPosRow and _destinationPosRow are in the memory frame references
// If _startPosRow or _destinationPosRow are out of memory frame limits then an exception is raised
// If _startPosRow + _nbRows is out of memory frame limits then an exception is raised
// If _destinationPosRow + _nbRows is out of memory frame limits an exception is raised
var
  _outOfBounds, _loop : boolean;
  _x, _y, _start, _end : T_IU_MaxPix;
  _inc, _delta : integer;
  _alphaPixel : T_IU_AlphaColor32;
  _Pixel : T_IU_Color32;
begin
  // Testin bounds limits
  _outOfBounds := not(_isOutOfBounds(_memoryFrame, _startPosRow)); // testin start row (source left limit)
  _outOfBounds := _outOfBounds and not(_isOutOfBounds(_memoryFrame, _startPosRow + _nbRows)); // testing sources right limits
  _outOfBounds := _outOfBounds and not(_isOutOfBounds(_memoryFrame, _destinationPosRow)); // Testing destination left limit
  _outOfBounds := _outOfBounds and not(_isOutOfBounds(_memoryFrame, _destinationPosRow + _nbRows)) ; // Testing destination right limit
  if not(_outOfBounds) then
    raise IU_EOutOfMemoryFrameBounds.Create(IU_ExceptionsMessages[IU_CurrentLang, K_IU_ExceptMSG_MemoryFrameManager_OutOfBounds])
  else begin
    // preparing copy
    // if shift is on the left of source area then copy must start to _startPosRow to _startPosRow + _nbRows
    // If copy is done in the other way then a risk exists that the begining of the source area will overwritten before it was copied in the destination area
    // And for equivalents reasons in other way, the copy must be done from the _startPosRow + _nbRows downto _startPosRow
    if _startPosRow > _destinationPosRow then begin
      _start := _startPosRow;
      _end := _startPosRow + _nbRows;
      _inc := 1;
      _loop := true;
    end else if _startPosRow < _destinationPosRow then begin
      _start := _startPosRow + _nbRows;
      _end := _StartPosRow;
      _inc := -1;
      _loop := true;
    end else // If source area = destination area then copy is not needed
      _loop := false;
    // shifting
    _x := _start;
    _delta := _destinationPosRow - _startPosRow ;
    while _loop do begin
      for _y := 0 to _memoryFrame.iu_whsize.height-1 do begin
        if _memoryFrame.iu_alpha then begin // frameMemory has an alpha channel
          self.getPixel(_memoryFrame,_alphaPixel,_x,_y);
          self.setPixel(_memoryFrame,_alphaPixel,_x+_delta,_y);
        end else begin // frameMemory hasn't an alpha channel
          self.getPixel(_memoryFrame,_Pixel,_x,_y);
          self.setPixel(_memoryFrame,_Pixel,_x+_delta,_y);
        end;
      end;
      // testing if end of loop (before inc _x)
      if _x = _end then _loop := false; // last row was copied
      // inc _x for a new loop instance (if _loop is always setted to true)
      _x := _x + _inc;
    end;
  end;
end;


// ***
// * Memory Frame Manager : returns memory frame properties
// *
// * @author : Pascal Lemaître
// *
// * in : memory frame descriptor _memoryFrame
// * out : width of the memory frame (number of rows) _width
// * out : height of the memory frame (number of lines) _hieght
// * out : first row of the memory frame in the picture references
// * out : is there an alpha chanel or not (true there is, false there is not) _isalpha
// *
// ***
procedure T_IU_MemoryFrameManager.getFrameReferences(var _memoryFrame : T_IU_MemoryFrame ;
                                 var _width, _height, _firstRow : T_IU_MaxPix ;
                                 var _isalpha : boolean);
// For swap, swap manager need to know memory frame bounds. It is needed for rows shifting.
begin
  _width := _memoryFrame.iu_whsize.width;
  _height := _memoryFrame.iu_whsize.height;
  _firstRow := _memoryFrame._startcol;
  _isalpha := _memoryFrame.iu_alpha;
end;

// ------ End of frame manager implementation















// ------ Stats manager implementation
// ***
// * Stats Manager : Creator
// * Create object instance and init all statistical values
// *
// * @author : Pascal Lemaître
// *
// ***
constructor T_IU_StatsManager.Create() ; // Create and init StatsManager instance
begin
  self.reset;
end;


// ***
// * Stats Manager : Destructor
// *
// * @author : Pascal Lemaître
// *
// ***
destructor T_IU_StatsManager.Destroy ;   // free the instance
begin
  inherited;
end;

// ***
// * Stats Manager : Adding a new accessed row in statistical set of mesures
// *
// * @author : Pascal Lemaître
// *
// * in : row nummber accessed (referential is not important. But the same referential must be keep for all adding)
// *
// ***
procedure T_IU_StatsManager.add_row(_row : T_IU_MaxPix) ; inline ; // Adding a new row accessed into the set
var _average : T_IU_MaxPix;
begin
  if self._totalMesures = 0 then begin
    self._maxRow:=_row;
    self._minRow:=_row;
    self._averageMaxRow:=_row;
    self._averageMinRow:=_row;
    self._mediumRow:=_row;
    self._totalMesures:=1;
  end else
  begin
    _average := trunc (self._mediumRow / self._totalMesures);
    inc(self._totalMesures);
    self._mediumRow:=self._mediumRow+_row;
    if _row < _average then begin
      self._averageMinRow:=self._averageMinRow+_row;
      if _row < self._minRow then self._minRow:=_row;
    end else if _row > _average then begin
      self._averageMaxRow:=self._averageMaxRow+_row;
      if _row > self._maxRow then self._maxRow:=_row;
    end;
  end;
end;


// ***
// * Stats Manager : average accessed row
// *
// * @author : Pascal Lemaître
// *
// * Return the average row accessed from all the row registered with add method
// *
// ***
function T_IU_StatsManager.get_average : T_IU_MaxPix ; inline ;    // Return the average row accessed from the set of measures
begin
  if self._totalMesures = 0 then begin
    get_average := 0 ;
    raise IU_ENoStatsForFramesAccess.Create(IU_ExceptionsMessages[IU_CurrentLang, K_IU_ExceptMSG_StatsManager_NoStat]);
  end else get_average := trunc((self._mediumRow + self._averageMinRow + self._averageMaxRow) / self._totalMesures);
end;


// ***
// * Stats Manager : average left accessed row
// *
// * @author : Pascal Lemaître
// *
// * Return the average left accessed row from all the row registered with add method
// *
// ***
function T_IU_StatsManager.get_averageMin : T_IU_MaxPix ; inline ; // Return the average min row accessed form the set
begin
  if self._totalMesures = 0 then begin
    get_averageMin := 0;
    raise IU_ENoStatsForFramesAccess.Create(IU_ExceptionsMessages[IU_CurrentLang, K_IU_ExceptMSG_StatsManager_NoStat]);
  end else get_averageMin := trunc(self._averageMinRow / self._totalMesures);
end;


// ***
// * Stats Manager : average right accessed row
// *
// * @author : Pascal Lemaître
// *
// * Return the average right accessed row from all the row registered with add method
// *
// ***
function T_IU_StatsManager.get_averageMax : T_IU_MaxPix ; inline ; // Return the average max row accessed from the set
begin
  if self._totalMesures = 0 then begin
    get_averageMax := 0 ;
    raise IU_ENoStatsForFramesAccess.Create(IU_ExceptionsMessages[IU_CurrentLang, K_IU_ExceptMSG_StatsManager_NoStat]);
  end else get_averageMax := trunc(self._averageMaxRow / self._totalMesures);
end;


// ***
// * Stats Manager : lefter accessed row
// *
// * @author : Pascal Lemaître
// *
// * Return the lefter accessed row from all the row registered with add method
// *
// ***
function T_IU_StatsManager.get_Min : T_IU_MaxPix; inline ;         // Return the real min row accessed
begin
  get_Min := self._minRow;
  if self._totalMesures = 0 then
    raise IU_ENoStatsForFramesAccess.Create(IU_ExceptionsMessages[IU_CurrentLang, K_IU_ExceptMSG_StatsManager_NoStat]);
end;


// ***
// * Stats Manager : righter accessed row
// *
// * @author : Pascal Lemaître
// *
// * Return the righter accessed row from all the row registered with add method
// *
// ***
function T_IU_StatsManager.get_Max : T_IU_MaxPix; inline ;         // Return the real max row accessed
begin
  get_Max := self._maxRow;
  if self._totalMesures = 0 then
    raise IU_ENoStatsForFramesAccess.Create(IU_ExceptionsMessages[IU_CurrentLang, K_IU_ExceptMSG_StatsManager_NoStat]);
end;

// ***
// * Stats Manager : reset all registered value with _add method
// *
// * @author : Pascal Lemaître
// *
// ***
procedure T_IU_StatsManager.reset ; inline ;
begin
  self._mediumRow := 0;         // Adding all row accessed to define the average accessed row
  self._averageMinRow := 32767; // Adding all row accessed on the left of the average row
  self._averageMaxRow := 0;     // Adding all row accessed on the right of the average row
  self._totalMesures := 0;      // How manu row are mesured
  self._minRow := 0;            // the min/max row accessed on the left/right of the average row
  self._maxRow := 0;            // the min/max row accessed on the left/right of the average row
end;


// ***
// * Stats Manager : newrow directive to take in account in statisticals measures
// *
// * @author : Pascal Lemaître
// *
// * Picture processing follow donw the y before to follow right the x
// * When processing transit from x to x+1 row, stats manager must be informed of it to
// * adapt its measures to the new central row accessed by processing.
// *
// ***
procedure T_IU_StatsManager.NewRow ; inline ; // Take new line in statistics set
// When algos change their current row, Stat must be adapted.
// Else at the end of the process, _minRow will be the first row of the frame
// _max_Row will be the last row of the frame
// and _mediumRow will be the average between first and last row of the frame.
// Then stats will be unusefull. The objective is to know which rows around the current row are accessed
// To know that for the current row the only way is to follow the current row.
// It's mean that when algos change their current row all statistical value must be shift to incremente and follow
// the current row.
// This method must be call when each algo (when swap is needed) change their current row.
// But stats is an object declared in a private zone of AccessFrame then access frame should offer a caller of this method
begin
  self._mediumRow := self._mediumRow + 1;
  self._averageMinRow := self._averageMinRow +1;
  self._averageMaxRow := _averageMaxRow + 1;
  self._minRow := self._minRow + 1;
  self._maxRow := self._maxRow + 1;
end;

// ------ End of Stats manager implementation















// ------ Swap manager implementation

// ***
// * Swap Manager : Creator
// *
// * @author : Pascal Lemaître
// *
// ***
constructor T_IU_SwapManager.Create() ; // For standardization
begin
  // nothing to do. Just for creation standardization
end;


// ***
// * Swap Manager : Destructor
// *
// * @author : Pascal Lemaître
// *
// ***
destructor T_IU_SwapManager.Destroy ; // For standardisation
begin
  // nothing to do except inherited call. Just for standardization
  inherited;
end;


// ***
// * Swap Manager : Initialization of swap when a new loaded picture from BGRABitmap object
// *
// * @author : Pascal Lemaître
// *
// * in : BGRABitmap where picture was loaded _in_bitmap
// * in : temporary swap file descriptor (swap file must be created) _in_temporaryFile
// * in : firstrow to write in swap file _firstrow
// * in : number of rows to write in swap file _nbRows
// *
// ***
procedure T_IU_SwapManager.InitSwap (var _in_bitmap : TBGRABitmap ; _in_temporaryFile : T_IU_TemporaryPictureFile ; _firstrow, _nbRows : T_IU_MaxPix);
// InitSwap file (already exists) with rows of _in_bitmap
// _in_bitmap : BGRABitmap from which an area of row will write in swap file
// _in_temporaryFile : swap temporary file descriptor (see IU_Types)
//                     it is a record with file name, is an alpha channel and size of the area
var _lastrow : T_IU_MaxPix;
  _curRow : T_IU_MaxPix;
  _swapNumCol : T_IU_MaxPix;
begin
  // Copy of all row from _in_bitmap into the temporary swap file
  // Check if size between _in_bitmap and swap file are coherent.
  // 1- Checking lines number
  if _in_bitmap.Height <> _in_temporaryFile.iu_whsize.height then
    raise IU_EIncoherentSizeBetweenSwapFileAndPicture.Create(IU_ExceptionsMessages[IU_CurrentLang, K_IU_ExceptMSG_IncoherentSizeBetweenSwapAndPix])
  else begin
    // 2- Define _lastrow
    _lastrow := _firstrow + _nbRows ;
    // 3- Checking compatibility between area and picture
    if _firstrow < K_MinPix then
      raise IU_EIncoherentSizeBetweenSwapFileAndPicture.Create(IU_ExceptionsMessages[IU_CurrentLang, K_IU_ExceptMSG_IncoherentSizeBetweenSwapAndPix])
    else if _lastrow > _in_bitmap.Width - 1 then
      raise IU_EIncoherentSizeBetweenSwapFileAndPicture.Create(IU_ExceptionsMessages[IU_CurrentLang, K_IU_ExceptMSG_IncoherentSizeBetweenSwapAndPix]);
    // 4- here area are compatible. Fullfilment of swap file with _in_bitmap rows
    _swapNumCol := 0;
    // 4.1- loop on row to write
    for _curRow := _firstrow to _lastrow do begin
      // 4.1.1 Read row from BGRABitmap
      try
        if _in_temporaryFile.iu_alpha then
          IU_BGRA_I_getRow(_in_bitmap, _curRow, @(_row[0].IU_V_AlphaPixData))
        else
          IU_BGRA_I_getRow(_in_bitmap, _curRow, false, @(_row[0].IU_V_PixData));
      Except // Catch BGRA direct pixel access exceptions
        raise IU_ESwapSourceReadingBGRABitmapError.Create(IU_ExceptionsMessages[IU_CurrentLang, K_IU_ExceptMSG_DirectAccessBGRABitmapReadERRORWhenSwapInit]);
      end;
      // 4.1.2 Write row into temporary swap file
      try
        IU_WritingColsToFile (_in_temporaryFile, _swapNumCol, 1, @(self._row), sizeof(self._row));
      Except // Catch swap writting erreur
        raise IU_ESwapInitWrittingError.Create(IU_ExceptionsMessages[IU_CurrentLang, K_IU_ExceptMSG_SwapInitWrittingError]);
      end;
      // next row in swap file
      inc(_swapNumCol);
    end;
  end;
end;


// ***
// * Swap Manager : Initialization of an epmty swap file (could be usefull for target frames)
// *
// * @author : Pascal Lemaître
// *
// * in : Temporary swap file descriptor (swap file must be exists) _in_temporaryFile
// *
// ***
procedure T_IU_SwapManager.InitEmptySwap (_in_temporaryFile : T_IU_TemporaryPictureFile);
// Init an empty swap file (all pixels are set to 0,0,0 and if an alpha channel exist is set to 0
// _in_temporayFile : swap temporary file descriptor (See IU_Types)
var _curRow : T_IU_MaxPix;
begin
  // 1- Init _row array for swap writting operations. This array contains all pixels of a row
  fillchar(self._row[0], sizeof(self._row), 0);
  // 2- write all row of swap file
  for _curRow := 0 to _in_temporaryFile.iu_whsize.width do begin
    try
      IU_WritingColsToFile (_in_temporaryFile, _curRow, 1, @(self._row), sizeof(self._row));
    except // catch swap writting error
      raise IU_ESwapInitWrittingError.Create(IU_ExceptionsMessages[IU_CurrentLang, K_IU_ExceptMSG_SwapInitWrittingError]);
    end;
  end;
end;


// ***
// * Swap Manager : Rows writer in swap file
// *
// * @author : Pascal Lemaître
// *
// * in : temporary swap file descriptor
// * in : memory frame descriptor from where rows will be write in a swap file
// * in : starting write row in the swap file (in swap file reference => 0 is the first row in the swap file)
// * in : starting row of the memory frame to write in the swap file (in memory frame reference => 0 is the first row in the memory frame)
// * in : number of rows to write _in_nbRows
// *
// * method is autoadaptative on pixel with or without alpha chanel
// *
// ***
procedure T_IU_SwapManager.WriteRowsSwap (_in_temporaryFile : T_IU_TemporaryPictureFile ; var _in_memoryFrame : T_IU_MemoryFrame ; _in_swapRow, _in_memoryRow, _in_nbRows : T_IU_MaxPix);
// Writes rows from memoryFrame to temporary swap file
// _in_temporaryFile : Temporary swap File descriptor
// _in_memoryFrame : MemoryFrame where rows should be written into the swap file
// _in_swapRow : start row in swap file where rows should be written
// _in_memoryRow : start row in memory frame where rows should be written in swap file
// _in_nbRows : number of row should be written
var
  _pm : T_IU_PPictData ; // pointer on first row in memoryFrame
  _buffsize : T_IU_BufSize ; // size of memoryframe buffer from the _in_memoryRow to the _in_memoryRow + _in_nbRows + 1
begin
  // 1- Defining ending bounds
  if (_in_swapRow + _in_nbRows) > (_in_temporaryFile.iu_whsize.width - 1) then
    raise IU_EOutOfSwapFileBoundsError.Create(IU_ExceptionsMessages[IU_CurrentLang, K_IU_ExceptMSG_SwapWrittingSwapOutOfBoundsError])
  else if _in_memoryRow + _in_nbRows > _in_memoryFrame.iu_whsize.width - 1 then
    raise IU_EOutOfMemoryFrameBounds.Create(IU_ExceptionsMessages[IU_CurrentLang, K_IU_ExceptMSG_SwapWrittingMemoryOutOfBoundsError]);
  // 2- calc of buff size
  if _in_memoryFrame.iu_alpha then
    _buffsize := (_in_nbRows+1)*sizeof(T_IU_AlphaColor32)
  else
    _buffsize := (_in_nbRows+1)*sizeof(T_IU_Color32);
  // 3- swapping
  // Define memory address of the first row to copy
  _pm := T_IU_PPictData(_in_memoryFrame._memoryFrame + IU_PSeek_Col (_in_memoryFrame.iu_whsize, _in_memoryRow, _in_memoryFrame.iu_alpha));
  // Writting rows in swap file
  try
    IU_WritingColsToFile (_in_temporaryFile, _in_swapRow, _in_nbRows, _pm, _buffsize);
  Except
    raise IU_ESwappingWrittingError.Create(IU_ExceptionsMessages[IU_CurrentLang, K_IU_ExceptMSG_SwapWrittingError]);
  end;
end;


// ***
// * Swap Manager : Rows reader from swap file
// *
// * @author : Pascal Lemaître
// *
// * in : temporary swap file descriptor
// * in : memory frame descriptor to where rows read in from swap file will be write (data are writen into the memory frame area indicated by the frame descriptor
// *        not in the frame descriptor itself)
// * in : starting read row from the swap file (in swap file reference => 0 is the first row in the swap file)
// * in : starting row of the memory frame to write (in memory frame reference => 0 is the first row in the memory frame)
// * in : number of rows to write _in_nbRows
// *
// * method is autoadaptative on pixel with or without alpha chanel
// *
// ***
procedure T_IU_SwapManager.ReadRowsSwap (_in_temporaryFile : T_IU_TemporaryPictureFile ; var _out_memoryFrame : T_IU_MemoryFrame ; _in_swapRow, _in_memoryRow, _in_nbRows : T_IU_MaxPix);
// Read rows from temporary swap file and writes them into memoryFrame
// _in_temporaryFile : Temporary swap File descriptor
// _out_memoryFrame : MemoryFrame where rows should be written from the swap file
// _in_swapRow : start row in swap file where rows should be written
// _in_memoryRow : start row in memory frame where rows should be written in swap file
// _in_nbRows : number of row should be written
var
  _pm : T_IU_PPictData ; // pointer on first row in memoryFrame
  _buffsize : T_IU_BufSize ; // size of memoryframe buffer from the _in_memoryRow to the _in_memoryRow + _in_nbRows + 1
begin
  // 1- Defining ending bounds
  if (_in_swapRow + _in_nbRows) > (_in_temporaryFile.iu_whsize.width - 1) then
    raise IU_EOutOfSwapFileBoundsError.Create(IU_ExceptionsMessages[IU_CurrentLang, K_IU_ExceptMSG_SwapReadingSwapOutOfBoundsError])
  else if _in_memoryRow + _in_nbRows > _out_memoryFrame.iu_whsize.width - 1 then
    raise IU_EOutOfMemoryFrameBounds.Create(IU_ExceptionsMessages[IU_CurrentLang, K_IU_ExceptMSG_SwapReadingMemoryOutOfBoundsError]);
  // 2- calc of buff size
  if _out_memoryFrame.iu_alpha then
    _buffsize := (_in_nbRows+1)*sizeof(T_IU_AlphaColor32)
  else
    _buffsize := (_in_nbRows+1)*sizeof(T_IU_Color32);
  // 3- swapping
  // Define memory address of the first row to copy
  _pm := T_IU_PPictData(_out_memoryFrame._memoryFrame + IU_PSeek_Col (_out_memoryFrame.iu_whsize, _in_memoryRow, _out_memoryFrame.iu_alpha));
  // Writting rows in swap file
  try
    IU_WritingColsToFile (_in_temporaryFile, _in_swapRow, _in_nbRows, _pm, _buffsize);
  Except
    raise IU_ESwappingWrittingError.Create(IU_ExceptionsMessages[IU_CurrentLang, K_IU_ExceptMSG_SwapReadingError]);
  end;
end;

// ------ End of swap manager implementation










// ------ Access frame manager implementation
// ***
// * Access Frame Manager : ChangeRow directive for Stats manager
// * This method is needed for send new row directove to the stats manager (see newrom method of the stats manager)
// *
// * This method must be call by image processing when there change their current row in their alog
// *
// * @author : Pascal Lemaître
// *
// ***
procedure T_IU_AccessFrameManager.ChangeRow ;
// See stats manager description
// This procedure is needed to update and aquire correctly stats of accessed rows
// It just call the stats manager method NewRow
begin
  _swapStats.NewRow;
end;

// ***
// * Access Frame Manager : swap method for loading rows from swap file, shifting rows in memory frame and writing rows into swap file
// * This method make all the job. Just the last accessed row outside the bounds of memory frame is needed.
// *
// * @author : Pascal Lemaître
// *
// * in : last row of the pixel accessed in the picture references (Start row of the picture = 0)
// *
// ***
procedure T_IU_AccessFrameManager._swap (_in_accessedRow : T_IU_MaxPix);
// Make swap when accessed pixel is out ouf memory frame current area
// _in_accessedRow : row accessed in read or in write from the beginning of the pix not from the beginning of the access frame
// !!!!!---- _swap procedure must be call before recording new access in stats.

var
  _start_target_row, _StartWriting, _StartReading : T_IU_MaxPix;
  _sizeOfSwap : integer;
  _StartShifting, _EndShifting, _TargetShifting : T_IU_MaxPix;
  _sizeOfShifting : integer;
  _ShiftInc : integer;

  // internals procedures and functions
  // These following procedures or functions are created to manage complexity of swapping procedure
  // But for performance there are declared as inline.
  // Each procedure or function is more readable than a big procedure which include all inline declared procedures
  //                 _average
  // Lefter | | | | | | | | | | Righter
  //           |           _Average Right
  //           _Average Left
  // Determine the best area in memory

  // ***
  // * Access Frame Manager internal procedure of _swap method: procedure to calculate the best area will be seted in memory frame manager
  // *   to optimize (minimize) the number of swap
  // *
  // * @author : Pascal Lemaître
  // *
  // * in : last row of the pixel accessed in the picture references (Start row of the picture = 0)
  // *
  // * This procedure use the stats manager to determine the best area for memory manager
  // *
  // ***
  procedure _CalcBestArea (var _in_accessedRow : T_IU_MaxPix); inline;
  var
    _av_row, _av_leftrow, _av_rightrow, _lefter, _righter : T_IU_MaxPix;
    _av_left_target, _av_right_target, _lefter_target, _righter_target : T_IU_MaxPix;
    _areaNewSize : word;
  begin
    // 1.1- Getting the average row accessed
    _av_row := _swapStats.get_average;
    // 1.2- Getting the average of left row accessed
    _av_leftrow := _swapStats.get_averageMin;
    // 1.3- Getting the average of right row accessed
    _av_rightrow := _swapStats.get_averageMax;
    // 1.4- getting the lefter row accessed
    _lefter := _swapStats.get_Min;
    // 1.5- getting the righter row accessed
    _righter := _swapStats.get_Max;
    // 1.6- define bounds in target references
    //    - if accessed row not in stats area then it is mean than the area
    //    - is extended.
    //    - If _in_accessedRow < _lefter then area is extended on the left
    //    - If _in_accessedRow > _righter then area is extended on the right
    if _in_accessedRow < _av_leftrow then begin
      _lefter_target := _in_accessedRow;
      _righter_target := _righter;
    end else begin
      _lefter_target := _lefter;
      _righter_target := _in_accessedRow;
    end;
    // Copying average values
    _av_left_target := _av_leftrow;
    _av_right_target := _av_rightrow;
    // 1.8- define new width of the accessed area
    _areaNewSize := _righter_target - _lefter_target + 1;
    // 1.9- verifying of new size is compatible with memory frame size
    if _areaNewSize > self._memoryFrame^.iu_whsize.width then begin
      // New size is to big for memoryframe size. Then we must adjust area bounds.
      // Two cases :
      //   -1 If area is extended on the left, then area should start at the new accessed row
      //   -2 if area is extended on the right, then area should end at the new accessed row
      if _lefter_target < _lefter then begin
        _righter_target := _lefter_target + self._memoryFrame^.iu_whsize.width - 1;
      end else begin
        _lefter_target := _righter_target - self._memoryFrame^.iu_whsize.width + 1;
      end;
      // new size is set to memory frame size
      _areaNewSize := self._memoryFrame^.iu_whsize.width;
    end;
    // Setting the new start target row of the memory frame area
    _start_target_row := _lefter_target ;
  end;



  // Determine which rows swap out and swap in and nbrows to swap
  // ***
  // * Access Frame Manager internal procedure of _swap method: procedure to calculate which rows must be write in temporary swap file
  // *
  // * @author : Pascal Lemaître
  // *
  // ***
  procedure _IdentWritingReadingRows ; inline;
  //
  //  Original situation for right shifting
  //  -------------------------------------
  //  |3|4|5|6|7|
  //
  //  Shifting
  //  | | |3|4|5|
  //
  //  Writing rows
  //         _StartWriting
  //  | | | |6|7|
  //
  //  Reading rows
  //   _StartReading
  //  |1|2| | | |
  //
  //  Original situation for left shifting
  //  ------------------------------------
  //  |1|2|3|4|5|
  //
  //  Shifting
  //  |3|4|5| | |
  //
  //  Writing rows
  //   _StartWriting
  //  |1|2| | | |
  //
  //  Reading rows
  //         _StartReading
  //  | | | |6|7|
  begin
    if _start_target_row < self._memoryFrame^._startcol then begin
      // In this case switch is on the right. Then righter rows must be written in swap and lefter rows must be read from the swap
      _StartWriting := self._memoryFrame^.iu_whsize.width - 1 + self._memoryFrame^._startcol-_start_target_row;
      _StartReading := _start_target_row;
      _SizeOfSwap := self._memoryFrame^._startcol - _start_target_row ;
    end else begin
      // In this case switch is on the left. Then lefter rows must be written in swap and righter rows must be read from the swap
      _StartWriting :=  self._memoryFrame^._startcol ;
      _StartReading := self._memoryFrame^._startcol+self._memoryFrame^.iu_whsize.width;
      _SizeOfSwap := _start_target_row - self._memoryFrame^._startcol;
    end;
  end;



  // Determine rows switching in memory frame
  // ***
  // * Access Frame Manager internal procedure of _swap method: procedure to calculate the which rows must be shifted into memory frame
  // *
  // * @author : Pascal Lemaître
  // *
  // ***
  procedure _IdentRowsSwitching ; inline ;
  //
  //  Original situation for right shifting
  //  -------------------------------------
  //  |3|4|5|6|7|
  //
  //  Shifting
  //  | | |3|4|5|
  //
  //  Source Shifting
  //       _StartShifting
  //  |3|4|5| | |
  //   _EndShifting
  //       <- Copy direction
  //
  //  Target Shifting
  //           _TargetShifting
  //  | | |3|4|5|
  //           <- Copy direction
  //
  //  Original situation for left shifting
  //  ------------------------------------
  //  |1|2|3|4|5|
  //
  //  Shifting
  //  |3|4|5| | |
  //
  //  Source shifting
  //       _StartShifting
  //  | | |3|4|5|
  //           _EndShifting
  //       -> Copy Direction
  //
  //  Target Shifting
  //   _TargetShifting
  //  |3|4|5| | |
  //   -> Copy Direction
  //
  begin
    if _StartWriting < _StartReading then begin // Shift to left
      // Reference is the memory frame window
      _StartShifting := _sizeOfSwap ;
      _EndShifting := self._memoryFrame^.iu_whsize.width - 1;
      _TargetShifting := 0;
      _ShiftInc := 1;
    end else begin // Shift to right
      // Reference is the memory frame window
      _StartShifting := self._memoryFrame^.iu_whsize.width - 1 - _sizeOfSwap;
      _EndShifting := _sizeOfSwap ;
      _TargetShifting := self._memoryFrame^.iu_whsize.width - 1;
      _ShiftInc := -1;
    end;
    // The shift procedure of the memory frame works in the memory frame reference
    // In memory frame reference, first row is _Startcol then we need to add this _startcol value to
    // _StartShifting, _EndShifting and _TargetShifting
    _StartShifting := _StartShifting + self._memoryFrame^._startcol;
    _EndShifting := _EndShifting + self._memoryFrame^._startcol;
    _TargetShifting := _TargetShifting + self._memoryFrame^._startcol;
    // Calc size of shifting (nb cols to shift) needed for shifting method of memoryframemanager
    _sizeOfShifting := abs(_EndShifting - _StartShifting) + 1;
  end;



  // Write rows in swap
  // ***
  // * Access Frame Manager internal procedure of _swap method: procedure for writing rows from memory frame into temporary swap file
  // *
  // * @author : Pascal Lemaître
  // *
  // ***
  procedure _writeswap ; inline ;
  // Before calling swap manager of access frame rows references must be converted into
  // swap reference and memory frame reference (each started at 0)
  var
    _swaprow, _memoryrow : T_IU_MaxPix;
  begin
    // rows convertion
    _swaprow := _StartWriting - self._frame^._iu_framestartx;
    _memoryrow := _StartWriting - self._memoryFrame^._startcol;
    // Call of swap manager writer procedure
    // Exception are not catched as this level. It is to obtain directly the exception raised by the swap manager
    // At this level if there is a swap error nothing can be done
    self._swapManager.WriteRowsSwap(self._frame^._iu_temporaryFileForSwap, self._memoryFrame^, _swaprow, _memoryrow, _sizeOfSwap);
  end;



  // Shifting rows in memory frame
  // ***
  // * Access Frame Manager internal procedure of _swap method: procedure for shifting rows into memory frame
  // *
  // * @author : Pascal Lemaître
  // *
  // ***
  procedure _shift ; inline ;
  // Before calling memory frame procedure, this procedure should convert row into memory frame reference
  // First row in memory frame reference is 0
  var _memoryRowSource, _memoryRowTarget : T_IU_MaxPix;
  begin
    // converting pix rows into memory frame rows
    _memoryRowSource := _StartShifting - self._memoryFrame^._startcol;
    _memoryRowTarget := _TargetShifting - self._memoryFrame^._startcol;
    // Calling memory frame shifting method
    // Exception are not catched at this level. Is to obtain directly the exception raised by the memory frame manager
    self._memoryFrameManager.shiftRow(self._memoryFrame^, _memoryRowSource, _sizeOfShifting, _memoryRowTarget);
  end;



  // Reading rows from swap
  // ***
  // * Access Frame Manager internal procedure of _swap method: procedure to read rows from temporary swap file and to write them into memory frame
  // *
  // * @author : Pascal Lemaître
  // *
  // ***
  procedure _readswap ; inline ;
  // Before calling swap manager of access frame rows references must be converted into
  // swap reference and memory frame reference (each started at 0)
  var
    _swaprow, _memoryrow : T_IU_MaxPix;
  begin
    // rows convertion
    _swaprow := _StartReading - self._frame^._iu_framestartx;
    _memoryrow := _StartReading - self._memoryFrame^._startcol;
    // Call of swap manager writer procedure
    // Exception are not catched as this level. It is to obtain directly the exception raised by the swap manager
    // At this level if there is a swap error nothing can be done
    self._swapManager.ReadRowsSwap(self._frame^._iu_temporaryFileForSwap, self._memoryFrame^, _swaprow, _memoryrow, _sizeOfSwap);
  end;



  // Updating memory frame boundaries
  // ***
  // * Access Frame Manager internal procedure of _swap method: procedure to update memory frame bounds
  // *
  // * Start col in the picture references is not the same after the swap than before the swap.
  // * For exemple start col in memory frame is the 5th row of the picture. Two rows must be swaped on the left.
  // * Then after swap first col in memory frame if 7th row of the picture.
  // *
  // * @author : Pascal Lemaître
  // *
  // ***
  procedure _updateBounds ; inline ;
  // When rows are swapped then the first row of memory frame in pix reference change
  // If it is a left shift, pos of the first row increase
  // If it is a right shift, pos of the firs row decrease
  // Boundaries are definined by _StartReading from swap or by _TargetShifting
  // If right shifting, new first row is _StartReading from the swap file (pix reference)
  // If left shifting, new first row is _StartShifting
  //
  // Left shifting
  //       _StartShifting
  //  |5|6|7|8|9|
  //           _EndShifting
  //   _TargetShifting
  //
  //  |7|8|9| | |
  //         _StartReading
  //
  //
  // Right shifting
  //       _StartShifting
  //  |5|6|7|8|9|
  //   _EndShifting
  //           _TargetShifting
  //
  //  | | |5|6|7|
  //   _StartReading
  //
  // Then the new row value is the MIN between _StartReading and _StartShifting
  var
    _newBoundary : T_IU_MaxPix;
  begin
    _newBoundary := min(_StartReading, _StartShifting);
    self._memoryFrame^._startcol:=_newBoundary;
  end;

begin
  // 1- Calculating the best area to be in memory
  // 2- Identifying which rows to be writed in swap and which ones must be read
  // 3- Size of swap
  // 4- Identifying shiftings rows
  // 5- Writing rows in swap
  // 6- Shifting rows
  // 7- Reading rows from swap
  // 8- Updating memoryframe boundaries.

  // 1- Calculating the best area to be in memory
  _CalcBestArea(_in_accessedRow) ;
  // 2- Identifying which rows to be writed in swap and which ones must be read
  // 3- Size of swap
  _IdentWritingReadingRows ;
  // 4- Identifying shiftings rows
  _IdentRowsSwitching ;
  // 5- Writing rows in swap
  _writeswap ;
  // 6- Shifting rows
  _shift ;
  // 7- Reading rows from swap
  _readswap ;
  // 8- Updating memoryframe boundaries.
  _updateBounds ;
end;

// ***
// * Access Frame Manager : Creator
// *
// * @author : Pascal Lemaître
// *
// * in : Access frame that the manager has to manage _in_accessFrame
// * in : Memory frame corresponding to the Access frame that the manager has to manage _in_memoryFrame
// *
// ***
constructor T_IU_AccessFrameManager.Create (var _in_accessFrame : T_IU_AccessFrame ; var _in_memoryFrame : T_IU_MemoryFrame);
// Need to create :
//                 _swapStats object
//                 _swapManager object
//                 _memoryFrameManager object
// _accessFrame : descriptor of access frame to be managed (must exists).
// _memoryFrame : descriptor of memory frame to be used (must exists).
//
// Initialize private var and create instances of privates objects
begin
  // 1- Creating _swapStats manager
  try
    self._swapStats := T_IU_StatsManager.Create();
  Except
    raise IU_EAccessFrameManager_StatsManagerCreationError.Create(IU_ExceptionsMessages[IU_CurrentLang, K_IU_ExceptMSG_AccessFrameManager_StatsManagerCreationError]);
  end;
  // 2- Creating _swapManager
  try
    self._swapManager := T_IU_SwapManager.Create();
  Except
    // Release stats manager
    self._swapStats.Destroy;
    raise IU_EAccessFrameManager_SwapManagerCreationError.Create(IU_ExceptionsMessages[IU_CurrentLang, K_IU_ExceptMSG_AccessFrameManager_swapManagerCreationError]);
  end;
  // 3- Creating _memoryFrameManger
  try
    self._memoryFrameManager := T_IU_MemoryFrameManager.Create;
  Except
    // Release stats and swap managers
    self._swapStats.Destroy;
    self._swapManager.Destroy;
    raise IU_EAccessFrameManager_MemoryFrameManagerCreationError.Create(IU_ExceptionsMessages[IU_CurrentLang, K_IU_ExceptMSG_AccessFrameManager_MemoryFrameManagerCreationError]);
  end;
  // 4- Initializing accessFrame descriptor
  self._frame := @(_in_accessFrame);
  // 5- Initializing mamoryFrame descriptor
  self._memoryFrame:= @(_in_memoryFrame);
end;

// ***
// * Access Frame Manager : Destructor
// *
// * @author : Pascal Lemaître
// *
// ***
destructor T_IU_AccessFrameManager.Destroy;
// Need to release _swapStats object
begin
  // 1- Releasing _swapStats manager
  self._swapStats.Destroy;
  // 2- Releasing _swapManager
  self._swapManager.Destroy;
  // 3- Releasing _memoryFrameManager
  self._memoryFrameManager.Destroy;
  // 4- Set Descriptors to nil (not release them)
  self._frame:=nil;
  self._memoryFrame:=nil;
  // 5- call inherited destrucor
  inherited;
end;

// ------ End of access frame manager implementation









// ------ Memory Pix manager implementation
// ***
// * Pix Manager : internal init method
// *
// * This is a generic method that set access frame, memory frame and thread frame property according to size of the picture
// * This method init source, temporary, target frames for original size or preview size for the picture
// * This method is call by Create and previewCreate constructors
// *
// * @author : Pascal Lemaître
// *
// * in : number of rows of the picture (x size) width
// * in : nulber of lines of the picture (y size) height
// * in : if picture has or has not an alpha chanel alpha
// * in : type of the picture (source, temporary or preview) _type
// * in : size of the picture (original = original size of the picture, preview = preview size for the picture)
// *
// ***
procedure T_IU_MemoryPix._init(width, height : T_IU_MaxPix ; alpha : boolean ; _type : T_IU_TypeOfFrame ; _origin_preview : T_IU_PixType );
                                                    // init : _iu_pixframe properties
                                                    //        _iu_accessframe properties
                                                    //        _iu_threadframe properties
                                                    //        _iu_memoryframe_idx properties
                                                    //        _type : type frame index (in frame ou out frame)
                                                    //        _origin_preview : is it an origin pix or a preview (no swap for preview)
// width and height are the width (number of columns) and the height (number of lines) of the picture
// alpha with or without an alpha chanel
var
  _tempVal : T_IU_MaxPix ; // temporary var for calc
  _size : T_IU_WHPixSize ; // temporary var for temporary file creation procedure call
  _frameSize : T_IU_Size ; // Size needed by the frame depands if it's an in or an out frame
  i : T_IU_FrameThreadIndex ; // MemoryFrame index
begin
  // Define de size needed (in or out frame) -- see the recovery area explanation
  if _origin_preview = K_IU_OriginPix then begin
    if _type = K_IU_Source_Frames then _frameSize := V_IU_MaxInWindowSize
    else _frameSize := V_IU_MaxOutWindowSize;
  end else begin
    if _type = K_IU_Source_Frames then _frameSize := V_IU_MaxInPreviewSize
    else _frameSize := V_IU_MaxOutPreviewSize;
  end;
  // 1- Pix frame properties and type identification
  self._typeOfFrame := _type;                             // Needed for release
  self._source_temporary_target_frame := _origin_preview; // Needed for release
  self._iu_pixframe.iu_alpha:=alpha;
  self._iu_pixframe.iu_whsize.width:=width;
  self._iu_pixframe.iu_whsize.height:=height;
  // There are 1 to K_IU_MaxThreads memoryframes and accessed frames for one pix
  for i := 1 to K_IU_MaxThreads do
  begin
    // Init index descriptor
    self._iu_memoryframe_idx[i]._iu_index:=i;
    self._iu_memoryframe_idx[i]._iu_type:=_type;
    // 2- MemoryFrame Properties
    //    Must be set before access frame.
    //    Then access frame can know if swap file is needed or not
    // 2.1- pointer on memory area already allocated when program was started
    // 2.2- height of the frame
    IU_MemoryFrames[i, _type,_origin_preview].iu_whsize.height:=height;
    // 2.3- width of the frame
    //      The width depends if there is an alpha chanel or not and if it's a source or target frame
    if alpha then _tempVal := _frameSize div sizeof(T_IU_AlphaColor32)
    else _tempVal := _frameSize div sizeof(T_IU_Color32);
    if (_type = K_IU_Source_Frames) or (_type = K_IU_Temporary_Frame) then begin
      if (i = 1) or (i = K_IU_MaxThreads) then // first frame doesn't need a recovery area on its left side no frame before
                                               // last frame doesn't need a recovery area on its right side no frame after
        _tempVal := _tempVal + IU_BackGroudExtractionArea (max(width,height)) div 2
      else _tempVal := _tempVal + IU_BackGroudExtractionArea (max(width,height));
    end;
    IU_MemoryFrames[i, _type,_origin_preview].iu_whsize.width:=_tempVal;
    // 2.4- is an alpha chanel
    IU_MemoryFrames[i, _type,_origin_preview].iu_alpha:=alpha;
    // 2.5- memory is just initialized the it is not modified until now
    IU_MemoryFrames[i, _type,_origin_preview].isModified:=false;
    // 2.6- Start col from picture of frame memory
    //      This is made in accessframe init because at the beginning, the first colomn of the memoryframe
    //      is the same as accessframe.
    // 3- access frame properties
    // 3.1 Identify start colomn
    //     If it is the first frame then the beginning is always 0
    //     Else this is the end of the previous frame minus recovery area
    if i=1 then _tempVal := 0
    else begin
      // depends if its a sourceframe or a targetframe SourceFrame needs a recovery area
      if (_type = K_IU_Source_Frames) or (_type = K_IU_Temporary_Frame) then begin
        _tempVal := (width div K_IU_MaxThreads) - IU_BackGroudExtractionArea (max(width,height)) div 2 ;
        _tempVal := max(0, _tempVal) ; // To be sure that the beginning of the area is not outside pix area
      end else _tempVal := (width div K_IU_MaxThreads);
    end ;
    self._iu_accessframe[i]._iu_framestartx:=_tempVal;
    // 3.2 Set start col of memory frame according to start col of access frame
    IU_MemoryFrames[i, _type,_origin_preview]._startcol:=_tempVal;
    // 3.3 Identify the real width of the frame (with recovery area)
    //     If it is a target frame then there is no recovery area
    //     If it is the first or the last frame the the width can't be outside the pix area
    //     Else the width is the width of the pix divided by the max number of threads + 2 * the recovery area
    if i=K_IU_MaxThreads then _tempVal := width - self._iu_accessframe[i]._iu_framestartx // Made substraction to take note of rouded error (see IU_Types for K_IU_RecoveryArea declaration)
    else
      if i=1 then _tempVal := (width div K_IU_MaxThreads) + IU_BackGroudExtractionArea (max(width,height)) div 2 // If it is first frame there is no recovery area on the left (no frame before)
      else begin if (_type <> K_IU_Target_Frames) then begin // not a target frame then there are recovery area
        _tempVal := (width div K_IU_MaxThreads) + IU_BackGroudExtractionArea (max(width,height)) ; // K_IU_BKGRDSigExtract_MaxWindow = 2 * K_IU_RecoveryArea
                                                                                   // One recovery area on the left and other one on the right
        _tempVal := min(self._iu_accessframe[i]._iu_framestartx + _tempVal, width - self._iu_accessframe[i]._iu_framestartx) ; // To be sure that area of the frame is in picture area
      end else // target frame then there is not recovery area
        _tempVal := (width div K_IU_MaxThreads);
      end;
    self._iu_accessframe[i]._iu_width:=_tempVal;
    // 3.4 create the temporary file only if needed
    if _origin_preview = K_IU_OriginPix then begin
      _size.width:=self._iu_accessframe[i]._iu_width;
      _size.height:=self._iu_pixframe.iu_whsize.height;
      if self._iu_accessframe[i]._iu_width > IU_MemoryFrames[i, _type,K_IU_OriginPix].iu_whsize.width then
        begin
          self._iu_accessframe[i]._swap:=true;
          // if a swap file already exists it is reused
          if AnsiCompareText(self._iu_accessframe[i]._iu_temporaryFileForSwap.iu_temporaryFileName, '')=0 then begin
            try
              IU_CreateTemporaryFile (self._iu_accessframe[i]._iu_temporaryFileForSwap, alpha, _size);
            except
              // on error raise an exception but clean the frames properties to show wich one is in error (if needed)
              self._iu_pixframe.iu_alpha:=false;
              self._iu_pixframe.iu_whsize.width:=0;
              self._iu_pixframe.iu_whsize.height:=0;
              IU_MemoryFrames[i, _type,K_IU_OriginPix].iu_whsize.width:=0;
              IU_MemoryFrames[i, _type,K_IU_OriginPix].iu_whsize.height:=0;
              IU_MemoryFrames[i, _type,K_IU_OriginPix].isModified:=false;
              IU_MemoryFrames[i, _type,K_IU_OriginPix].iu_alpha:=false;
              IU_MemoryFrames[i, _type,K_IU_OriginPix]._startcol:=0;
              self._iu_accessframe[i]._iu_framestartx := 0;
              self._iu_accessframe[i]._iu_width := 0;
              raise IU_EMemoryManagerTempFileCreationError.Create(IU_ExceptionsMessages[IU_CurrentLang, K_IU_ExceptMSG_MemoryManagerError_CantCreateTempraryFile]);
            end;
          end;
        end
      else self._iu_accessframe[i]._swap:=false;
    end else self._iu_accessframe[i]._swap:=false;
    // 4- Thread frame properties
    // 4.1 Identify start colomn
    //     If it is the first frame then the beginning is always 0
    //     Else this is the end of the previous frame minus recovery area
    if i=1 then _tempVal := 1
    else _tempVal := (width div K_IU_MaxThreads);
    self._iu_threadframe[i]._iu_framestartx:=_tempVal;
    // 4.2 Identify the real width of the frame
    //     If it is the last frame the the width can't be outside the pix area
    //     Else the width is the width of the pix divided by the max number of threads + 2 * the recovery area
    if i=K_IU_MaxThreads then _tempVal := width - self._iu_threadframe[i]._iu_framestartx
    else _tempVal := (width div K_IU_MaxThreads) ;
    self._iu_threadframe[i]._iu_width :=_tempVal;
  end; // loop on number of frames
end;


// ***
// * Pix Frame Manager : Creator for picture according to the orignal picture size (loaded from disk)
// *
// * @author : Pascal Lemaître
// *
// * in : number of rows of the picture (x size) width
// * in : nulber of lines of the picture (y size) height
// * in : if picture has or has not an alpha chanel alpha
// * in : type of the picture (source, temporary or preview) _type
// *
// ***
constructor T_IU_MemoryPix.Create(width, height : T_IU_MaxPix ; alpha : boolean ; _type : T_IU_TypeOfFrame);
var
  i : integer;
begin
  // Creator should only init descriptors... Just call private _init procedure
  // Object creation some init must be done.
  //  e.j. swap file name must be set to empty string
  for i := 1 to K_IU_MaxThreads do
    self._iu_accessframe[i]._iu_temporaryFileForSwap.iu_temporaryFileName:='';
  try
    self._init(width, height, alpha, _type, K_IU_OriginPix);
  Except
    raise IU_EPixCreationSwapFileError.Create(IU_ExceptionsMessages[IU_CurrentLang, K_IU_ExceptMSG_MemoryManager_SwapFileCreationError]) ;
  end;
end;

// ***
// * Pix Frame Manager : Creator for preview picture
// *
// * Previews pictures are stretched picture from the original picture.
// * There are two dimensinal max size (max rows and max lines - maximum are identical
// *   see IU_Types K_IU_PreviewMaxSise constant)
// *
// * @author : Pascal Lemaître
// *
// * in : number of rows of the original picture (x size) width
// * in : nulber of lines of the original picture (y size) height
// * in : if picture has or has not an alpha chanel alpha
// * in : type of the picture (source, temporary or preview) _type
// *
// ***
constructor T_IU_MemoryPix.PreviewCreate(width, height : T_IU_MaxPix ; alpha : boolean ; _type : T_IU_TypeOfFrame);
begin
  // Creator should only init descriptors... Just verify max size for preview and call private _init procedure
  if (width > K_IU_PreviewMaxSize) or (height > K_IU_PreviewMaxSize) then
    raise IU_EPreviewPixMemoryCreationError.Create(IU_ExceptionsMessages[IU_CurrentLang, K_IU_ExceptMSG_PreviewPixCreationOverSized])
  else self._init(width, height, alpha, _type, K_IU_PreviewPix);
end;

// ***
// * Pix Frame Manager : Destructor
// *
// * @author : Pascal Lemaître
// *
// ***
destructor T_IU_MemoryPix.Destroy;
var i : T_IU_FrameThreadIndex;
  j : T_IU_PixType;
begin
  for i := 1 to K_IU_MaxThreads do
    begin
      // 1- Release indexes anf memory frame
      IU_MemoryFrames[self._iu_memoryframe_idx[i]._iu_index,self._iu_memoryframe_idx[i]._iu_type, j].isModified:=false;
      IU_MemoryFrames[self._iu_memoryframe_idx[i]._iu_index,self._iu_memoryframe_idx[i]._iu_type, j].iu_alpha:=false;
      IU_MemoryFrames[self._iu_memoryframe_idx[i]._iu_index,self._iu_memoryframe_idx[i]._iu_type, j].iu_whsize.width:=0;
      IU_MemoryFrames[self._iu_memoryframe_idx[i]._iu_index,self._iu_memoryframe_idx[i]._iu_type, j].iu_whsize.height:=0;
      // 2- Release threadframe
      self._iu_threadframe[i]._iu_framestartx:=0;
      self._iu_threadframe[i]._iu_width:=0;
      // 3- Release accessframe (and deleting swap file is needed)
      self._iu_accessframe[i]._iu_width:=0;
      self._iu_accessframe[i]._iu_framestartx:=0;
      if self._iu_accessframe[i]._swap then begin
         IU_DeleteATempFile(self._iu_accessframe[i]._iu_temporaryFileForSwap.iu_temporaryFileName);
         self._iu_accessframe[i]._iu_temporaryFileForSwap.iu_temporaryFileName:='';
         self._iu_accessframe[i]._iu_temporaryFileForSwap.iu_alpha:=false;
         self._iu_accessframe[i]._iu_temporaryFileForSwap.iu_whsize.height:=0;
         self._iu_accessframe[i]._iu_temporaryFileForSwap.iu_whsize.width:=0;
      end;
      self._iu_accessframe[i]._swap:=false;
      // 4- Release pixframe
      self._iu_pixframe.iu_whsize.width:=0;
      self._iu_pixframe.iu_whsize.height:=0;
      self._iu_pixframe.iu_alpha:=false;
    end;
  inherited;
end;

// ***
// * Pix Frame Manager : gives the Access Frame descriptor of the access frame managed by the manager
// * It is needed for given this information to the threads
// *
// * @author : Pascal Lemaître
// *
// * out : access frame descriptor _accessFrame
// * in : thread number (1 = first thread, 2 = second thread, 3 = third thread, 4 = fourth thread)
// *      no more values allowed. Tim works with 4 threads but it will change with K_IU_MaxThreads constant of IU_Types unit
// *
// ***
procedure T_IU_MemoryPix.getAccessFrameArea(var _accessFrame : T_IU_AccessFrame ; _threadIndex : T_IU_FrameThreadIndex);
// _accessFrame : out parameter return the _iu_accessFrame area for thread _threadIndex
//                _threadIndex : Thread for which one accessframe will returned
begin
  _accessFrame := _iu_accessframe[_threadIndex];
end;

// ***
// * Pix Frame Manager : gives the Thread Frame descriptor of the thread frame managed by the manager
// * It is needed for given this information to the threads
// *
// * @author : Pascal Lemaître
// *
// * out : access frame descriptor _accessFrame
// * in : thread number (1 = first thread, 2 = second thread, 3 = third thread, 4 = fourth thread)
// *      no more values allowed. Tim works with 4 threads but it will change with K_IU_MaxThreads constant of IU_Types unit
// *
// ***
procedure T_IU_MemoryPix.getThreadFrameArea(var _threadFrame : T_IU_ThreadFrame ; _threadIndex : T_IU_FrameThreadIndex);
// return the thread frame descriptor (memory area of the descriptor)
// _threadFrame : out parameter return th _iu_threadframe area for thread _threadIndex
//                _threadIndex : Thread for which one area will returned
begin
  _threadFrame := _iu_threadframe[_threadIndex];
end;

// ------ End of Memory Pix manager implementation

begin
  // init some var
  ReturnNilIfGrowHeapFails := true ;
  // if getmem fail then nil pointer is returned

  // ***
  // * Adding V0.2
  V_IU_MaxOutWindowSize := trunc (K_IU_MaxMemorySize * IU_getAdaptationRAMCoef / K_IU_MaxThreads);
  V_IU_MaxInWindowSize :=  V_IU_MaxOutWindowSize + (K_IU_BKGRDSigExtract_MaxWindow * 32 * 10000) ;
  // RecoveryArea is the half of the windows for background signal extraction.
  //
  // For background luminance alorithmes we need to have enought recovery area beetwen frames
  // It needs a temporary picture too for luminance mask (this pix is write and read pix)
  // By default in Tim V1.x the zone was 700 pixels max then in fact with this value we need
  // 700 * 16 * 32768 Bytes more for each frame (memory frame if we don't want a swap for on column)
  // It's around 350 MBytes more for each memory frame.
  // The best of camera is 52 Mpix. Each frame is sized to accept this size. It take aroud 800 Mb.
  // Tim manage 3 different types of pix (source, temporary and target) Then we need 3 * 800 Mb = 1.8 Gb.
  // For recovery area, the max for a 52 Mpix is around 14000 pixels for width and 10000 for height.
  // Then we can get 10000 * 4 * 4 * 700 as a max value (near 110 Mb) by frame.
  // 8 frames need recovery area (in and temporary). Only 4 need 110 Mb more and 4 need 55 Mb more
  //                  (left and right frame don't need 700 pixel of recovery area. 350 are enought)
  // The sum for three pix without recovery area is 800 Mb * 3 (pix) = 1.8 Gb
  // We must add recovery area :
  // 4 * 110 Mb + 4 * 55Mb = 4 * 165 Mb = 660 Mb.
  // Then we will use around 1.8 Gb + 0.65 Gb. It's around 2.5 Gb.

  // Memory frame allocation for previews pix
  V_IU_MaxInPreviewSize := trunc((K_IU_PreviewMaxSize * (K_IU_PreviewMaxSize + IU_BackGroudExtractionArea (K_IU_PreviewMaxSize)))  * 4 * 4 / K_IU_MaxThreads)+1;
  V_IU_MaxOutPreviewSize := trunc((K_IU_PreviewMaxSize * K_IU_PreviewMaxSize) * 4 * 4 / K_IU_MaxThreads)+1;
  // 4 * 4 Four channels, 4 bytes by channel

  // Then we need to add theses of the precedent allocation.
  // With FlexPoint at 3000 and coef at 1 (see constants declarations)
  // The area for a preview is 219 pixels (see the file RecoveryAreaModel at the root of the project)
  // Then we have 2 pix with 1200 * (1200 + 219) * 4 * 4 bytes and one with 1200 * 1200 * 4 * 4 bytes (four chanels of 4 Bytes)
  // We have 52 M bytes (0.05 Gb) more for source and temporary frames and 22 M bytes (0.02 Gb) for target frame.
  // The we need 2.5 Gb + 0.05 Gb + 0.02 Gb = 2.57 Gb.
  // Now, normaly, PC have 4 Gb. Some one (low powered) have only 3 Gb.
  // We can assume that Tim need 4 Gb to work correctly.
  // *
  // * End Adding V0.2
  // ***

end.

