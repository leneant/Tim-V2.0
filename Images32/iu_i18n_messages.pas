unit IU_I18N_Messages;

{$mode objfpc}{$H+}
// ***
// * Unit provides all declaration and messages initialization for internationalization
// * Creation Date : 2017 September
// *
// * Version : 0.7
// * Version Date : 2018 January
// * Version Contributors : Pascal Lemaître
// *
// * v0.7 : Adding text for contextual menu for importFile. Del button text for invert font color
// * v0.6 : For dedicated linux -> Adding text for list invert font color
// * v0.5 : Adding loading image file error message.
// * v0.4 : Adding text for import 1 file windows
// * v0.3 : Adding informationnal message
// * v0.2 : Adding exception messages for IU_T_StringDoubleChainedList
// *
// * @authors : Pascal Lemaître
// *
// * @see :
// *
// *
// * Team : TIm (Traitement d'Images)
// *
// * 2017-2018
// ***

interface

uses
  Classes, SysUtils;
const
  K_IU_I18N_FRENCH = 0;
  K_IU_I18N_ENGLISH = 1;

  // ExceptionsMessagesIndex
  K_IU_ExceptMSG_TmpCreationFail = 0;
  K_IU_ExceptMSG_OutOfPixBounds = 1;
  K_IU_ExceptMSG_ReadingBufferToSmall = 2;
  K_IU_ExceptMSG_WritingBufferToSmall = 3;
  K_IU_ExceptMSG_OpenFileError = 4;
  K_IU_ExceptMSG_DirecFileAccesError = 5;
  K_IU_ExceptMSG_BynaryFileWrintingError = 6;
  K_IU_ExceptMSG_BynaryFileReadingError = 7;
  K_IU_ExceptMSG_RegisterTemporaryFileError = 8;
  K_IU_ExceptMSG_UnRegisterTemporaryFileError = 9;
  K_IU_ExceptMSG_NoEnoughtMemoryError = 10;
  K_IU_ExceptMSG_TooBigPixError = 11;
  K_IU_ExceptMSG_MemoryFrameAccessError = 12;
  K_IU_ExceptMSG_MemoryManagerError_CantCreateTempraryFile = 13;
  K_IU_ExceptMSG_PreviewPixCreationOverSized = 14;
  K_IU_ExceptMSG_MemoryManager_SwapFileCreationError = 15;
  K_IU_ExceptMSG_StatsManager_NoStat = 16;
  K_IU_ExceptMSG_MemoryFrameManager_OutOfBounds = 17;
  K_IU_ExceptMSG_MemoryFrameManager_MemoryNotAllocated = 18;
  K_IU_ExceptMSG_IncoherentSizeBetweenSwapAndPix = 19;
  K_IU_ExceptMSG_DirectAccessBGRABitmapReadERRORWhenSwapInit = 20;
  K_IU_ExceptMSG_SwapInitWrittingError = 21;
  K_IU_ExceptMSG_SwapWrittingSwapOutOfBoundsError = 22;
  K_IU_ExceptMSG_SwapWrittingMemoryOutOfBoundsError = 23;
  K_IU_ExceptMSG_SwapWrittingError = 24;
  K_IU_ExceptMSG_SwapReadingSwapOutOfBoundsError = 25;
  K_IU_ExceptMSG_SwapReadingMemoryOutOfBoundsError = 26;
  K_IU_ExceptMSG_SwapReadingError = 27;
  K_IU_ExceptMSG_AccessFrameManager_swapManagerCreationError = 28;
  K_IU_ExceptMSG_AccessFrameManager_StatsManagerCreationError = 29;
  K_IU_ExceptMSG_AccessFrameManager_MemoryFrameManagerCreationError = 30;
  K_IU_ExceptMSG_ControlQueue_NoEnoughtMemoryError = 31;
  K_IU_ExceptMSG_ControlQueue_AddError = 32;
  K_IU_ExceptMSG_ControlQueue_AddFullError = 33;
  K_IU_ExceptMSG_ControlQueue_MoveError = 34;
  K_IU_ExceptMSG_ControlQueue_setFocusError = 35;
  K_IU_ExceptMSG_RBGroup_NoEnoughtMemoryError = 36;
  K_IU_ExceptMSG_RBGroup_AddError = 37;
  K_IU_ExceptMSG_RBGroup_AddFullError = 38;

  // ***
  // * Add v0.2
  K_IU_ExceptMSG_StringDoubleChainedList_NoItem = 39 ;
  K_IU_ExceptMSG_StringDoubleChainedList_NoMoreItem = 40 ;
  K_IU_ExceptMSG_StringDoubleChainedList_AccessError = 41 ;
  K_IU_ExceptMSG_StringDoubleChainedList_NoMoreAvailableMemory = 42 ;
  K_IU_ExceptMSG_StringDoubleChainedList_CopyAccessError = 43 ;
  K_IU_ExceptMSG_StringDoubleChainedList_CreateAccessError = 44 ;
  // *
  // * End Add v0.2
  // ***

  // ***
  // * Add v0.6
  K_IU_ExceptMSG_DirNotExists = 45;
  K_IU_ExceptMSG_FileNotExists = 46;
  K_IU_ExceptMSG_FileReadError = 47;
  K_IU_ExceptMSG_MkDirError = 48;
  K_IU_ExceptMSG_CreateFileError = 49;
  K_IU_ExceptMSG_FileWriteError = 50;
  // *
  // * End Add v0.6
  // ***

  // *************************************************************
  // IHM Messages

  // ***
  // * Add v0.3
  K_IU_HIMSG_ErrorBoxTitle = 0;
  K_IU_HIMSG_WarningBoxTitle = 1;
  K_IU_HIMSG_UnreachedDrive = 2;
  // *
  // * End Add v0.3
  // ***

  // ***
  // * Add v0.4
  K_IU_HIMSG_ImportFileWindowTitle = 3;
  K_IU_HIMSG_KB = 4; // Kilo Bytes
  K_IU_HIMSG_MB = 5; // Mega Bytes
  K_IU_HIMSG_GB = 6; // Giga Bytes
  K_IU_HIMSG_TB = 7; // Tera Bytes
  K_IU_HIMSG_Dir = 8; // Directories Caption
  K_IU_HIMSG_Files = 9; // Files Caption
  K_IU_HIMSG_DiskSize = 10;
  K_IU_HIMSG_DiskFree = 11;
  K_IU_HIMSG_FilesTypes = 12;
  K_IU_HIMSG_AllFiles = 13; // For selecting all files types
  K_IU_HIMSG_JpegFiles = 14; // For selecting jpeg files types
  K_IU_HIMSG_BmpFiles = 15; // For selecting bitmap (bmp and png) files types
  K_IU_HIMSG_TiffFiles = 16; // For selecting Tiff files types
  K_IU_HIMSG_ImportCommand = 17;
  K_IU_HIMSG_CancelCommand = 18;
  // *
  // * End Add v0.4
  // ***

  // ***
  // * Add v0.5
  K_IU_HIMSG_LoadingFileError = 19;
  // *
  // * End Add v0.5
  // ***

  // ***
  // * Del v0.6
  // ***
  // * Add v0.5
  // K_IU_HIMSG_ListInvFontColor = 20;
  // *
  // * End Add v0.5
  // ***
  // *
  // * End Del v0.6
  // ***

  // ***
  // * Add v0.6
  K_IU_HIMSG_setWindowsColor = 20;
  K_IU_HIMSG_setTextColor = 21;
  K_IU_HIMSG_setListColor = 22;
  K_IU_HIMSG_setListSelectedColor = 23;
  K_IU_HIMSG_setListTextColor = 24;
  K_IU_HIMSG_setDropDownColor = 25;
  K_IU_HIMSG_setDropDownSelectedColor = 26;
  K_IU_HIMSG_setDropDownTextColor = 27;
  K_IU_HIMSG_setButtonColor = 28;
  K_IU_HIMSG_setButtonSelectedColor = 29;
  K_IU_HIMSG_setStandardColors = 30;
  // *
  // * End Add v0.6
  // ***

  type
    T_IU_ExceptionMessages = array [K_IU_I18N_FRENCH..K_IU_I18N_ENGLISH, 0..500] of string;
    // Error messages

    T_IU_HI_Messages = array [K_IU_I18N_FRENCH..K_IU_I18N_ENGLISH, 0..500] of string;
    // Interface messages

    T_IU_STD_Messages = array [K_IU_I18N_FRENCH..K_IU_I18N_ENGLISH, 0..500] of string;
    // Standard messages


  var
    IU_CurrentLang : Byte;
    // ***
    // Add v0.3
    IU_HI_Messages : T_IU_HI_Messages;
    // ***
    IU_ExceptionsMessages : T_IU_ExceptionMessages;

implementation
begin
  //default lang is English
  IU_CurrentLang := K_IU_I18N_ENGLISH;

  // ***
  // * Add v0.3
  // init HI messages
  IU_HI_Messages[K_IU_I18N_FRENCH,K_IU_HIMSG_ErrorBoxTitle] := 'Erreur !';
  IU_HI_Messages[K_IU_I18N_ENGLISH,K_IU_HIMSG_ErrorBoxTitle] := 'Error !';

  IU_HI_Messages[K_IU_I18N_FRENCH,K_IU_HIMSG_WarningBoxTitle] := 'Avertissement !';
  IU_HI_Messages[K_IU_I18N_ENGLISH,K_IU_HIMSG_WarningBoxTitle] := 'Warning !';

  IU_HI_Messages[K_IU_I18N_FRENCH,K_IU_HIMSG_UnreachedDrive] := 'Drive non accessible !';
  IU_HI_Messages[K_IU_I18N_ENGLISH,K_IU_HIMSG_UnreachedDrive] := 'Drive unreachable !';
  // *
  // * End Add v0.3
  // ***

  // ***
  // * Add v0.4
  IU_HI_Messages[K_IU_I18N_FRENCH,K_IU_HIMSG_ImportFileWindowTitle] := 'Importation d''une image';
  IU_HI_Messages[K_IU_I18N_ENGLISH,K_IU_HIMSG_ImportFileWindowTitle] := 'Import one picture';

  // Kilo Bytes
  IU_HI_Messages[K_IU_I18N_FRENCH,K_IU_HIMSG_KB] := 'Ko';
  IU_HI_Messages[K_IU_I18N_ENGLISH,K_IU_HIMSG_KB] := 'KB';

  // Mega Bytes
  IU_HI_Messages[K_IU_I18N_FRENCH,K_IU_HIMSG_MB] := 'Mo';
  IU_HI_Messages[K_IU_I18N_ENGLISH,K_IU_HIMSG_MB] := 'MB';

  // Giga Bytes
  IU_HI_Messages[K_IU_I18N_FRENCH,K_IU_HIMSG_GB] := 'Go';
  IU_HI_Messages[K_IU_I18N_ENGLISH,K_IU_HIMSG_GB] := 'GB';

  // Tera Bytes
  IU_HI_Messages[K_IU_I18N_FRENCH,K_IU_HIMSG_TB] := 'To';
  IU_HI_Messages[K_IU_I18N_ENGLISH,K_IU_HIMSG_TB] := 'TB';

  // Directories Caption
  IU_HI_Messages[K_IU_I18N_FRENCH,K_IU_HIMSG_Dir] := 'Répertoires';
  IU_HI_Messages[K_IU_I18N_ENGLISH,K_IU_HIMSG_Dir] := 'Directories';

  // Files Caption
  IU_HI_Messages[K_IU_I18N_FRENCH,K_IU_HIMSG_Files] := 'Fichiers';
  IU_HI_Messages[K_IU_I18N_ENGLISH,K_IU_HIMSG_Files] := 'Files';

  IU_HI_Messages[K_IU_I18N_FRENCH,K_IU_HIMSG_DiskSize] := 'Taille disque :';
  IU_HI_Messages[K_IU_I18N_ENGLISH,K_IU_HIMSG_DiskSize] := 'Total disk size :';

  IU_HI_Messages[K_IU_I18N_FRENCH,K_IU_HIMSG_DiskFree] := 'Espace disponible :';
  IU_HI_Messages[K_IU_I18N_ENGLISH,K_IU_HIMSG_DiskFree] := 'Disk free :';

  IU_HI_Messages[K_IU_I18N_FRENCH,K_IU_HIMSG_FilesTypes] := 'Types d''image :';
  IU_HI_Messages[K_IU_I18N_ENGLISH,K_IU_HIMSG_FilesTypes] := 'Pictures types :';

  // For selecting all files types
  IU_HI_Messages[K_IU_I18N_FRENCH,K_IU_HIMSG_AllFiles] := 'Tous : *.*';
  IU_HI_Messages[K_IU_I18N_ENGLISH,K_IU_HIMSG_AllFiles] := 'All : *.*';

  // For selecting jpeg files types
  IU_HI_Messages[K_IU_I18N_FRENCH,K_IU_HIMSG_JpegFiles] := 'Jpeg : *.jpg, *.JPG, *.jpeg, *.JPEG';
  IU_HI_Messages[K_IU_I18N_ENGLISH,K_IU_HIMSG_JpegFiles] := 'Jpeg : *.jpg, *.JPG, *.jpeg, *.JPEG';

  // For selecting bitmap (bmp and png) files types
  IU_HI_Messages[K_IU_I18N_FRENCH,K_IU_HIMSG_BmpFiles] := 'Bitmap : *.bmp, *.BMP, *.png, *.PNG';
  IU_HI_Messages[K_IU_I18N_ENGLISH,K_IU_HIMSG_BmpFiles] := 'Bitmap : *.bmp, *.BMP, *.png, *.PNG';

  IU_HI_Messages[K_IU_I18N_FRENCH,K_IU_HIMSG_TiffFiles] := 'Tiff : *.tif, *.TIF';
  IU_HI_Messages[K_IU_I18N_ENGLISH,K_IU_HIMSG_TiffFiles] := 'Tiff : *.tif, *.TIF';

  IU_HI_Messages[K_IU_I18N_FRENCH,K_IU_HIMSG_ImportCommand] := 'Importer';
  IU_HI_Messages[K_IU_I18N_ENGLISH,K_IU_HIMSG_ImportCommand] := 'Import';

  IU_HI_Messages[K_IU_I18N_FRENCH,K_IU_HIMSG_CancelCommand] := 'Annuler';
  IU_HI_Messages[K_IU_I18N_ENGLISH,K_IU_HIMSG_CancelCommand] := 'Cancel';

  // *
  // * End Add v0.4
  // ***

  // ***
  // * Add v0.5
  IU_HI_Messages[K_IU_I18N_FRENCH,K_IU_HIMSG_LoadingFileError] := 'Erreur inattendue lors du chargement de l''image !';
  IU_HI_Messages[K_IU_I18N_ENGLISH,K_IU_HIMSG_LoadingFileError] := 'Unexcepted error when loading image file !';
  // *
  // * End Add v0.5
  // ***

  // ***
  // Del v0.6
  // ***
  // * Add v0.5
  // IU_HI_Messages[K_IU_I18N_FRENCH,K_IU_HIMSG_ListInvFontColor] := 'Liste : inverser la couleur du texte';
  // IU_HI_Messages[K_IU_I18N_ENGLISH,K_IU_HIMSG_ListInvFontColor] := 'List : invert font color';
  // *
  // * End Add v0.5
  // ***
  // *
  // * End Del v0.6
  // ***

  // ***
  // * Add v0.6
  IU_HI_Messages[K_IU_I18N_FRENCH,K_IU_HIMSG_setWindowsColor] := 'Couleur de la fenêtre';
  IU_HI_Messages[K_IU_I18N_ENGLISH,K_IU_HIMSG_setWindowsColor] := 'Windows Color';

  IU_HI_Messages[K_IU_I18N_FRENCH,K_IU_HIMSG_setTextColor] := 'Couleur du texte';
  IU_HI_Messages[K_IU_I18N_ENGLISH,K_IU_HIMSG_setTextColor] := 'Text Color';

  IU_HI_Messages[K_IU_I18N_FRENCH,K_IU_HIMSG_setListColor] := 'Couleur des listes';
  IU_HI_Messages[K_IU_I18N_ENGLISH,K_IU_HIMSG_setListColor] := 'Lists Color';

  IU_HI_Messages[K_IU_I18N_FRENCH,K_IU_HIMSG_setListSelectedColor] := 'Couleur des listes sélectionnées';
  IU_HI_Messages[K_IU_I18N_ENGLISH,K_IU_HIMSG_setListSelectedColor] := 'Selected lists Color';

  IU_HI_Messages[K_IU_I18N_FRENCH,K_IU_HIMSG_setListTextColor] := 'Couleur du texte des listes';
  IU_HI_Messages[K_IU_I18N_ENGLISH,K_IU_HIMSG_setListTextColor] := 'Text of lists Color';

  IU_HI_Messages[K_IU_I18N_FRENCH,K_IU_HIMSG_setDropDownColor] := 'Couleur des listes déroulantes';
  IU_HI_Messages[K_IU_I18N_ENGLISH,K_IU_HIMSG_setDropDownColor] := 'Dropdown lists Color';

  IU_HI_Messages[K_IU_I18N_FRENCH,K_IU_HIMSG_setDropDownSelectedColor] := 'Couleur des listes déroulantes sélectionnées';
  IU_HI_Messages[K_IU_I18N_ENGLISH,K_IU_HIMSG_setDropDownSelectedColor] := 'Selected Dropdown lists Color';

  IU_HI_Messages[K_IU_I18N_FRENCH,K_IU_HIMSG_setWindowsColor] := 'Couleur du texte des listes déroulantes';
  IU_HI_Messages[K_IU_I18N_ENGLISH,K_IU_HIMSG_setWindowsColor] := 'Text of Dropdown lists Color';

  IU_HI_Messages[K_IU_I18N_FRENCH,K_IU_HIMSG_setButtonColor] := 'Couleur des bouttons';
  IU_HI_Messages[K_IU_I18N_ENGLISH,K_IU_HIMSG_setButtonColor] := 'Buttons Color';

  IU_HI_Messages[K_IU_I18N_FRENCH,K_IU_HIMSG_setButtonSelectedColor] := 'Couleur des bouttons actifs';
  IU_HI_Messages[K_IU_I18N_ENGLISH,K_IU_HIMSG_setButtonSelectedColor] := 'Active buttons Color';

  IU_HI_Messages[K_IU_I18N_FRENCH,K_IU_HIMSG_setStandardColors] := 'Couleurs standards';
  IU_HI_Messages[K_IU_I18N_ENGLISH,K_IU_HIMSG_setStandardColors] := 'Standard Colors';
  // *
  // * End Add v0.6
  // ***

  // init exception messages
  IU_ExceptionsMessages[K_IU_I18N_FRENCH, K_IU_ExceptMSG_TmpCreationFail] := 'Création impossible du fichier temporaire de l''image !';
  IU_ExceptionsMessages[K_IU_I18N_ENGLISH, K_IU_ExceptMSG_TmpCreationFail] := 'Can''t create temporary picture file !';
  IU_ExceptionsMessages[K_IU_I18N_FRENCH, K_IU_ExceptMSG_OutOfPixBounds] := 'Index en dehors des limites de l''image !';
  IU_ExceptionsMessages[K_IU_I18N_ENGLISH, K_IU_ExceptMSG_OutOfPixBounds] := 'Index out of pix bounds !';
  IU_ExceptionsMessages[K_IU_I18N_FRENCH, K_IU_ExceptMSG_ReadingBufferToSmall] := 'Taille du buffer de lecture trop petite !';
  IU_ExceptionsMessages[K_IU_I18N_ENGLISH, K_IU_ExceptMSG_ReadingBufferToSmall] := 'Size of reading buffer too small !';
  IU_ExceptionsMessages[K_IU_I18N_FRENCH, K_IU_ExceptMSG_WritingBufferToSmall] := 'Taille du buffer d''écriture trop petite !';
  IU_ExceptionsMessages[K_IU_I18N_ENGLISH, K_IU_ExceptMSG_WritingBufferToSmall] := 'Size of writing buffer too small !';
  IU_ExceptionsMessages[K_IU_I18N_FRENCH, K_IU_ExceptMSG_OpenFileError] := 'Ouverture du fichier impossible !';
  IU_ExceptionsMessages[K_IU_I18N_ENGLISH, K_IU_ExceptMSG_OpenFileError] := 'Can''t open file !';
  IU_ExceptionsMessages[K_IU_I18N_FRENCH, K_IU_ExceptMSG_DirecFileAccesError] := 'Fichier : Erreur d''accès direct !';
  IU_ExceptionsMessages[K_IU_I18N_ENGLISH, K_IU_ExceptMSG_DirecFileAccesError] := 'File : Direct Access Error !';
  IU_ExceptionsMessages[K_IU_I18N_FRENCH, K_IU_ExceptMSG_BynaryFileWrintingError] := 'Erreur d''écriture binaire de fichier !';
  IU_ExceptionsMessages[K_IU_I18N_ENGLISH, K_IU_ExceptMSG_BynaryFileWrintingError] := 'Bynary file writing error !';
  IU_ExceptionsMessages[K_IU_I18N_FRENCH, K_IU_ExceptMSG_BynaryFileReadingError] := 'Erreur de lecture binaire de fichier !';
  IU_ExceptionsMessages[K_IU_I18N_ENGLISH, K_IU_ExceptMSG_BynaryFileReadingError] := 'Bynary file reading error !';
  IU_ExceptionsMessages[K_IU_I18N_FRENCH, K_IU_ExceptMSG_RegisterTemporaryFileError] := 'Référencement impossible d''un nouveau fichier temporaire !';
  IU_ExceptionsMessages[K_IU_I18N_ENGLISH, K_IU_ExceptMSG_RegisterTemporaryFileError] := 'Can''t registering a new temporary file !';
  IU_ExceptionsMessages[K_IU_I18N_FRENCH, K_IU_ExceptMSG_UnRegisterTemporaryFileError] := 'Suppression du référencement d''un fichier temporaire impossible !';
  IU_ExceptionsMessages[K_IU_I18N_ENGLISH, K_IU_ExceptMSG_UnRegisterTemporaryFileError] := 'Can''t unregistering a temporary file !';
  IU_ExceptionsMessages[K_IU_I18N_FRENCH, K_IU_ExceptMSG_NoEnoughtMemoryError] := 'Mémoire disponible insuffisante !';
  IU_ExceptionsMessages[K_IU_I18N_ENGLISH, K_IU_ExceptMSG_NoEnoughtMemoryError] := 'Not enought memory !';
  IU_ExceptionsMessages[K_IU_I18N_FRENCH, K_IU_ExceptMSG_TooBigPixError] := 'Image trop grande pour Tim... Désolé !';
  IU_ExceptionsMessages[K_IU_I18N_ENGLISH, K_IU_ExceptMSG_TooBigPixError] := 'Sorry. Too big pix for Tim !';
  IU_ExceptionsMessages[K_IU_I18N_FRENCH, K_IU_ExceptMSG_MemoryFrameAccessError] := 'Erreur d''accès mémoire. Peut être le bloc mémoire n''est pas alloué !';
  IU_ExceptionsMessages[K_IU_I18N_ENGLISH, K_IU_ExceptMSG_MemoryFrameAccessError] := 'Access to memory block error. May be memory area is not allocated !';
  IU_ExceptionsMessages[K_IU_I18N_FRENCH, K_IU_ExceptMSG_MemoryManagerError_CantCreateTempraryFile] := 'Erreur du gestionnaire de mémoire ! Création du fichier temporaire impossible (droit en écriture, volume plein ?)';
  IU_ExceptionsMessages[K_IU_I18N_ENGLISH, K_IU_ExceptMSG_MemoryManagerError_CantCreateTempraryFile] := 'Memory manager error ! Can''t create temporary file (write access granted, vol full ?)';
  IU_ExceptionsMessages[K_IU_I18N_FRENCH, K_IU_ExceptMSG_PreviewPixCreationOverSized] := 'Erreur de création image de prévisualisation ! Taille indiquée tro grande !';
  IU_ExceptionsMessages[K_IU_I18N_ENGLISH, K_IU_ExceptMSG_PreviewPixCreationOverSized] := 'Preview pix creation error ! Oversized !';
  IU_ExceptionsMessages[K_IU_I18N_FRENCH, K_IU_ExceptMSG_MemoryManager_SwapFileCreationError] := 'Impossible de créer le fichier de swap (vérifier les doits en écriture et la taille disponible du volume) !';
  IU_ExceptionsMessages[K_IU_I18N_ENGLISH, K_IU_ExceptMSG_MemoryManager_SwapFileCreationError] := 'Creation swap file error (check write access rights and empty room on the volume !';
  IU_ExceptionsMessages[K_IU_I18N_FRENCH, K_IU_ExceptMSG_StatsManager_NoStat] := 'Pas de statistiques d''accès à la frame !';
  IU_ExceptionsMessages[K_IU_I18N_ENGLISH, K_IU_ExceptMSG_StatsManager_NoStat] := 'No frame access statistics available !';
  IU_ExceptionsMessages[K_IU_I18N_FRENCH, K_IU_ExceptMSG_MemoryFrameManager_OutOfBounds] := 'Accès hors des limite de la memory frame !';
  IU_ExceptionsMessages[K_IU_I18N_ENGLISH, K_IU_ExceptMSG_MemoryFrameManager_OutOfBounds] := 'Out of bounds of memory frame access !';
  IU_ExceptionsMessages[K_IU_I18N_FRENCH, K_IU_ExceptMSG_MemoryFrameManager_MemoryNotAllocated] := 'Mémoire non allouée !';
  IU_ExceptionsMessages[K_IU_I18N_ENGLISH, K_IU_ExceptMSG_MemoryFrameManager_MemoryNotAllocated] := 'No memory allocated !';
  IU_ExceptionsMessages[K_IU_I18N_FRENCH, K_IU_ExceptMSG_IncoherentSizeBetweenSwapAndPix] := 'Tailles incompatibles entre fichier de swap et image !';
  IU_ExceptionsMessages[K_IU_I18N_ENGLISH, K_IU_ExceptMSG_IncoherentSizeBetweenSwapAndPix] := 'Not compatible sizes between swap file and picture !';
  IU_ExceptionsMessages[K_IU_I18N_FRENCH, K_IU_ExceptMSG_DirectAccessBGRABitmapReadERRORWhenSwapInit] := 'Erreur lors de l''initialisation du swap : Erreur de lecture de l''image source !';
  IU_ExceptionsMessages[K_IU_I18N_ENGLISH, K_IU_ExceptMSG_DirectAccessBGRABitmapReadERRORWhenSwapInit] := 'Swap init error : Error when source pix reading !';
  IU_ExceptionsMessages[K_IU_I18N_FRENCH, K_IU_ExceptMSG_SwapInitWrittingError] := 'Erreur lors de l''initialisation du swap : Erreur d''écriture du fichier (reste t-il assez de place sur le volume ?) !';
  IU_ExceptionsMessages[K_IU_I18N_ENGLISH, K_IU_ExceptMSG_SwapInitWrittingError] := 'Swap init error : Writting file error (is there enough room on the volume ?) !';
  IU_ExceptionsMessages[K_IU_I18N_FRENCH, K_IU_ExceptMSG_SwapWrittingSwapOutOfBoundsError] := 'Swap out Erreur : tentative d''écriture en dehors des limite du fichier de swap !';
  IU_ExceptionsMessages[K_IU_I18N_ENGLISH, K_IU_ExceptMSG_SwapWrittingSwapOutOfBoundsError] := 'Swap out : Out of bounds of swap file writting error !';
  IU_ExceptionsMessages[K_IU_I18N_FRENCH, K_IU_ExceptMSG_SwapWrittingMemoryOutOfBoundsError] := 'Swap out Erreur : tentative de lecture en dehors des limites de la zone mémoire réservée à l''image !';
  IU_ExceptionsMessages[K_IU_I18N_ENGLISH, K_IU_ExceptMSG_SwapWrittingMemoryOutOfBoundsError] := 'Swap out : Reading out of bounds of picture reserved memory area error !';
  IU_ExceptionsMessages[K_IU_I18N_FRENCH, K_IU_ExceptMSG_SwapWrittingError] := 'Swap out Erreur : Erreur au moment de l''écriture dans le fichier swap des colonnes d''une image !';
  IU_ExceptionsMessages[K_IU_I18N_ENGLISH, K_IU_ExceptMSG_SwapWrittingError] := 'Swap out : Error during picture''s row writting in a swap file !';
  IU_ExceptionsMessages[K_IU_I18N_FRENCH, K_IU_ExceptMSG_SwapReadingSwapOutOfBoundsError] := 'Swap in Erreur : tentative de lecture en dehors des limite du fichier de swap !';
  IU_ExceptionsMessages[K_IU_I18N_ENGLISH, K_IU_ExceptMSG_SwapReadingSwapOutOfBoundsError] := 'Swap in : Out of bounds of swap file reading error !';
  IU_ExceptionsMessages[K_IU_I18N_FRENCH, K_IU_ExceptMSG_SwapReadingMemoryOutOfBoundsError] := 'Swap in Erreur : tentative d''écriture en dehors des limites de la zone mémoire réservée à l''image !';
  IU_ExceptionsMessages[K_IU_I18N_ENGLISH, K_IU_ExceptMSG_SwapReadingMemoryOutOfBoundsError] := 'Swap in : Writting out of bounds of picture reserved memory area error !';
  IU_ExceptionsMessages[K_IU_I18N_FRENCH, K_IU_ExceptMSG_SwapReadingError] := 'Swap in Erreur : Erreur au moment de la lecture dans le fichier swap des colonnes d''une image !';
  IU_ExceptionsMessages[K_IU_I18N_ENGLISH, K_IU_ExceptMSG_SwapReadingError] := 'Swap in : Error during picture''s row reading from a swap file !';
  IU_ExceptionsMessages[K_IU_I18N_FRENCH, K_IU_ExceptMSG_AccessFrameManager_swapManagerCreationError] := 'Access Frame Manager : Erreur au moment de la création du swap manager !';
  IU_ExceptionsMessages[K_IU_I18N_ENGLISH, K_IU_ExceptMSG_AccessFrameManager_swapManagerCreationError] := 'Access Frame Manager : swap manager creation error !';
  IU_ExceptionsMessages[K_IU_I18N_FRENCH, K_IU_ExceptMSG_AccessFrameManager_StatsManagerCreationError] := 'Access Frame Manager : Erreur au moment de la création du stats manager !';
  IU_ExceptionsMessages[K_IU_I18N_ENGLISH, K_IU_ExceptMSG_AccessFrameManager_StatsManagerCreationError] := 'Access Frame Manager : stats manager creation error !';
  IU_ExceptionsMessages[K_IU_I18N_FRENCH, K_IU_ExceptMSG_AccessFrameManager_MemoryFrameManagerCreationError] := 'Access Frame Manager : Erreur au moment de la création du memory frame manager !';
  IU_ExceptionsMessages[K_IU_I18N_ENGLISH, K_IU_ExceptMSG_AccessFrameManager_MemoryFrameManagerCreationError] := 'Access Frame Manager : memory frame manager creation error !';
  IU_ExceptionsMessages[K_IU_I18N_FRENCH, K_IU_ExceptMSG_ControlQueue_NoEnoughtMemoryError] := 'Controls Graphics : Erreur de création mémoire insuffisante !';
  IU_ExceptionsMessages[K_IU_I18N_ENGLISH, K_IU_ExceptMSG_ControlQueue_NoEnoughtMemoryError] := 'Graphics Controls : No enought memory error !';
  IU_ExceptionsMessages[K_IU_I18N_FRENCH, K_IU_ExceptMSG_ControlQueue_AddError] := 'Controls Graphics : Erreur d''ajout dans la queue des controles !';
  IU_ExceptionsMessages[K_IU_I18N_ENGLISH, K_IU_ExceptMSG_ControlQueue_AddError] := 'Graphics Controls : queue adding error !';
  IU_ExceptionsMessages[K_IU_I18N_FRENCH, K_IU_ExceptMSG_ControlQueue_AddFullError] := 'Controls Graphics : Erreur d''ajout dans la queue des controles (queue pleine) !';
  IU_ExceptionsMessages[K_IU_I18N_ENGLISH, K_IU_ExceptMSG_ControlQueue_AddFullError] := 'Graphics Controls : queue adding error (queue is full) !';
  IU_ExceptionsMessages[K_IU_I18N_FRENCH, K_IU_ExceptMSG_ControlQueue_MoveError] := 'Controls Graphics : erreur de sélection suivant/précédent Erreur d''ajout dans la queue des controles !';
  IU_ExceptionsMessages[K_IU_I18N_ENGLISH, K_IU_ExceptMSG_ControlQueue_MoveError] := 'Graphics Controls : move to next or previous error !';
  IU_ExceptionsMessages[K_IU_I18N_FRENCH, K_IU_ExceptMSG_ControlQueue_setFocusError] := 'Controls Graphics : erreur positionnement du focus impossible !';
  IU_ExceptionsMessages[K_IU_I18N_ENGLISH, K_IU_ExceptMSG_ControlQueue_setFocusError] := 'Graphics Controls : set focus error !';

  IU_ExceptionsMessages[K_IU_I18N_FRENCH, K_IU_ExceptMSG_RBGroup_NoEnoughtMemoryError] := 'Groupe de RB : Erreur de création mémoire insuffisante !';
  IU_ExceptionsMessages[K_IU_I18N_ENGLISH, K_IU_ExceptMSG_RBGroup_NoEnoughtMemoryError] := 'RB''s group : set focus error !';
  IU_ExceptionsMessages[K_IU_I18N_FRENCH, K_IU_ExceptMSG_RBGroup_AddError] := 'Groupe de RB : Erreur d''ajout d''un nouveau Radio Bouton !';
  IU_ExceptionsMessages[K_IU_I18N_ENGLISH, K_IU_ExceptMSG_RBGroup_AddError] := 'RB''s group : Adding RB error !';
  IU_ExceptionsMessages[K_IU_I18N_FRENCH, K_IU_ExceptMSG_RBGroup_AddFullError] := 'Groupe de RB : Erreur d''ajout d''un nouveau Radio Bouton (Buffer plein) !';
  IU_ExceptionsMessages[K_IU_I18N_ENGLISH, K_IU_ExceptMSG_RBGroup_AddFullError] := 'RB''s group : Adding RB error (Buffer is full) !';

  // ***
  // * Add v0.2
  IU_ExceptionsMessages[K_IU_I18N_FRENCH, K_IU_ExceptMSG_StringDoubleChainedList_NoItem] := 'Erreur : Pas d''enregistrement dans la liste !';
  IU_ExceptionsMessages[K_IU_I18N_ENGLISH, K_IU_ExceptMSG_StringDoubleChainedList_NoItem] := 'Error : No item in the list !';

  IU_ExceptionsMessages[K_IU_I18N_FRENCH, K_IU_ExceptMSG_StringDoubleChainedList_NoMoreItem] := 'Erreur : Pas d''autre enregistrement dans la liste !';
  IU_ExceptionsMessages[K_IU_I18N_ENGLISH, K_IU_ExceptMSG_StringDoubleChainedList_NoMoreItem] := 'Error : No more item in the list !';

  IU_ExceptionsMessages[K_IU_I18N_FRENCH, K_IU_ExceptMSG_StringDoubleChainedList_AccessError] := 'Erreur d''accès mémoire à un enregistrement d''une liste !';
  IU_ExceptionsMessages[K_IU_I18N_ENGLISH, K_IU_ExceptMSG_StringDoubleChainedList_AccessError] := 'Error : Memory error when accessing to an item in a list !';

  IU_ExceptionsMessages[K_IU_I18N_FRENCH, K_IU_ExceptMSG_StringDoubleChainedList_NoMoreAvailableMemory] := 'Erreur : Impossible d''alouer la mémoire pour créer un nouvel enregistrement dans une liste !';
  IU_ExceptionsMessages[K_IU_I18N_ENGLISH, K_IU_ExceptMSG_StringDoubleChainedList_NoMoreAvailableMemory] := 'Error : Can''t allocate memory for creating a new record into a list !';

  IU_ExceptionsMessages[K_IU_I18N_FRENCH, K_IU_ExceptMSG_StringDoubleChainedList_CopyAccessError] := 'Erreur d''accès mémoire à un enregistrement d''une liste durant une copie !';
  IU_ExceptionsMessages[K_IU_I18N_ENGLISH, K_IU_ExceptMSG_StringDoubleChainedList_CopyAccessError] := 'Error : Memory error when accessing to an item in a list during list copy !';

  IU_ExceptionsMessages[K_IU_I18N_FRENCH, K_IU_ExceptMSG_StringDoubleChainedList_CreateAccessError] := 'Erreur d''accès mémoire à un enregistrement d''une liste durant l''ajout d''un item !';
  IU_ExceptionsMessages[K_IU_I18N_ENGLISH, K_IU_ExceptMSG_StringDoubleChainedList_CreateAccessError] := 'Error : Memory error when accessing to an item in a list during item add !';
  // *
  // * End Add v0.2
  // ***

  // ***
  // * Add v0.6
  IU_ExceptionsMessages[K_IU_I18N_FRENCH, K_IU_ExceptMSG_DirNotExists] := 'Répertoire inexistant !';
  IU_ExceptionsMessages[K_IU_I18N_ENGLISH, K_IU_ExceptMSG_DirNotExists] := 'Dir not exists !';

  IU_ExceptionsMessages[K_IU_I18N_FRENCH, K_IU_ExceptMSG_FileNotExists] := 'Fichier inexistant !';
  IU_ExceptionsMessages[K_IU_I18N_ENGLISH, K_IU_ExceptMSG_FileNotExists] := 'File not exists !';

  IU_ExceptionsMessages[K_IU_I18N_FRENCH, K_IU_ExceptMSG_FileReadError] := 'Erreur de lecture de fichier !';
  IU_ExceptionsMessages[K_IU_I18N_ENGLISH, K_IU_ExceptMSG_FileReadError] := 'File read error !';

  IU_ExceptionsMessages[K_IU_I18N_FRENCH, K_IU_ExceptMSG_MkDirError] := 'Impossible de créer un répertoire !';
  IU_ExceptionsMessages[K_IU_I18N_ENGLISH, K_IU_ExceptMSG_MkDirError] := 'Can''t create directory !';

  IU_ExceptionsMessages[K_IU_I18N_FRENCH, K_IU_ExceptMSG_CreateFileError] := 'Création de fichier impossible !';
  IU_ExceptionsMessages[K_IU_I18N_ENGLISH, K_IU_ExceptMSG_CreateFileError] := 'Can''t create file !';

  IU_ExceptionsMessages[K_IU_I18N_FRENCH, K_IU_ExceptMSG_FileWriteError] := 'Création de fichier impossible !';
  IU_ExceptionsMessages[K_IU_I18N_ENGLISH, K_IU_ExceptMSG_FileWriteError] := 'Can''t create file !';
  // *
  // * End Add v0.6
  // ***

end.

