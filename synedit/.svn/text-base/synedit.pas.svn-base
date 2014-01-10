{ -------------------------------------------------------------------------------
  The contents of this file are subject to the Mozilla Public License
  Version 1.1 (the "License"); you may not use this file except in compliance
  with the License. You may obtain a copy of the License at
  http://www.mozilla.org/MPL/

  Software distributed under the License is distributed on an "AS IS" basis,
  WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
  the specific language governing rights and limitations under the License.

  The Original Code is: SynEdit.pas, released 2000-04-07.
  The Original Code is based on mwCustomEdit.pas by Martin Waldenburg, part of
  the mwEdit component suite.
  Portions created by Martin Waldenburg are Copyright (C) 1998 Martin Waldenburg.
  All Rights Reserved.

  Contributors to the SynEdit and mwEdit projects are listed in the
  Contributors.txt file.

  Alternatively, the contents of this file may be used under the terms of the
  GNU General Public License Version 2 or later (the "GPL"), in which case
  the provisions of the GPL are applicable instead of those above.
  If you wish to allow use of your version of this file only under the terms
  of the GPL and not to allow others to use your version of this file
  under the MPL, indicate your decision by deleting the provisions above and
  replace them with the notice and other provisions required by the GPL.
  If you do not delete the provisions above, a recipient may use your version
  of this file under either the MPL or the GPL.

  $Id: synedit.pp 35464 2012-02-18 20:42:51Z martin $

  You may retrieve the latest version of this file at the SynEdit home page,
  located at http://SynEdit.SourceForge.net

  Known Issues:

  -DoubleBuffered
  -Font.CharSet
  -THintWindow
  -DragAcceptFiles
  -Font DBCS / MBCS  double, multi byte character set

  ------------------------------------------------------------------------------- }
unit SynEdit;
{$IFDEF Windows}  { off $DEFINE WinIME } {$ENDIF}
{$I synedit.inc}
{ $DEFINE SYNSCROLLDEBUG }

{$IFDEF LCLGTK1}
{$DEFINE EnableDoubleBuf} // gtk1 does not have double buffering
{$ENDIF}
{$IFDEF LCLGTK2}
{ $DEFINE EnableDoubleBuf } // gtk2.10 paints faster to memory
// gtk2.12 paints faster directly
{$ENDIF}

interface

{ $DEFINE VerboseKeys }
{ $DEFINE VerboseSynEditInvalidate }
{ $DEFINE SYNDEBUGPRINT }
{$IFDEF SynUndoDebug}
{$DEFINE SynUndoDebugItems}
{$DEFINE SynUndoDebugCalls}
{$ENDIF}

uses
  System.UITypes, System.Types,
  SysUtils, Classes, Messages, FMX.Controls, FMX.Forms, {FMX.StdCtrls,} FMX.ExtCtrls, FMX.Menus,
  SynEditTypes, SynEditSearch, SynEditKeyCmds, SynEditMouseCmds, SynEditMiscProcs,
  SynEditPointClasses, SynBeautifier, SynEditMarks,
  // Markup
  SynEditMarkup, SynEditMarkupHighAll, SynEditMarkupBracket, SynEditMarkupWordGroup,
  SynEditMarkupCtrlMouseLink, SynEditMarkupSpecialLine, SynEditMarkupSelection,
  SynEditMarkupSpecialChar,
  // Lines
  SynEditTextBase, LazSynEditText, SynEditTextBuffer, SynEditLines,
  SynEditTextTrimmer, SynEditTextTabExpander, SynEditTextDoubleWidthChars,
  SynEditFoldedView,
  // Gutter
  SynGutterBase, SynGutter, SynGutterCodeFolding, SynGutterChanges,
  SynGutterLineNumber, SynGutterMarks, SynGutterLineOverview,
  SynEditMiscClasses, SynEditHighlighter, LazSynTextArea, SynTextDrawer,
  FMX.Types, LazMethodList;

const
  ScrollBarWidth = 0;

  // SynDefaultFont is determined in InitSynDefaultFont()
  SynDefaultFontName: String = '';
  SynDefaultFontHeight: Integer = 13;
  SynDefaultFontSize: Integer = 10;
  SynDefaultFontPitch: TFontPitch = TFontPitch.fpFixed;
  SynDefaultFontQuality: TFontQuality = TFontQuality.fqNonAntialiased;

{$IFNDEF SYN_COMPILER_3_UP}
  // not defined in all Delphi versions
  WM_MOUSEWHEEL = $020A;
{$ENDIF}
  // maximum scroll range
  MAX_SCROLL = 32767;

{$IFDEF SYN_MBCSSUPPORT}
{$IFNDEF SYN_COMPILER_4_UP}

  { Windows.pas in D4 }
const
  C3_NONSPACING = 1; { nonspacing character }
  C3_DIACRITIC = 2; { diacritic mark }
  C3_VOWELMARK = 4; { vowel mark }
  C3_SYMBOL = 8; { symbols }
  C3_KATAKANA = $0010; { katakana character }
  C3_HIRAGANA = $0020; { hiragana character }
  C3_HALFWIDTH = $0040; { half width character }
  C3_FULLWIDTH = $0080; { full width character }
  C3_IDEOGRAPH = $0100; { ideographic character }
  C3_KASHIDA = $0200; { Arabic kashida character }
  C3_LEXICAL = $0400; { lexical character }
  C3_ALPHA = $8000; { any linguistic char (C1_ALPHA) }
  C3_NOTAPPLICABLE = 0; { ctype 3 is not applicable }
{$ENDIF}
{$ENDIF}

type
  TSynEditMarkupClass = SynEditMarkup.TSynEditMarkupClass;
  TSynReplaceAction = (raCancel, raSkip, raReplace, raReplaceAll);

  TSynDropFilesEvent = procedure(Sender: TObject; X, Y: Integer; AFiles: TStrings) of object;

  THookedCommandEvent = procedure(Sender: TObject; AfterProcessing: boolean; var Handled: boolean;
    var Command: TSynEditorCommand; var AChar: Char; Data: pointer; HandlerData: pointer) of object;
  THookedCommandFlag = (hcfInit, // run before On[User]CommandProcess (outside UndoBlock / should not do execution)
    hcfPreExec, // Run before CommandProcessor (unless handled by On[User]CommandProcess)
    hcfPostExec, // Run after CommandProcessor (unless handled by On[User]CommandProcess)
    hcfFinish // Run at the very end
    );
  THookedCommandFlags = set of THookedCommandFlag;

  THookedKeyTranslationEvent = procedure(Sender: TObject; Code: word; SState: TShiftState; var Data: pointer;
    var IsStartOfCombo: boolean; var Handled: boolean; var Command: TSynEditorCommand; FinishComboOnly: boolean;
    var ComboKeyStrokes: TSynEditKeyStrokes) of object;

  TPaintEvent = procedure(Sender: TObject; ACanvas: TCanvas) of object;

  TChangeUpdatingEvent = procedure(ASender: TObject; AnUpdating: boolean) of object;

  TProcessCommandEvent = procedure(Sender: TObject; var Command: TSynEditorCommand; var AChar: Char; Data: pointer)
    of object;

  TReplaceTextEvent = procedure(Sender: TObject; const ASearch, AReplace: string; Line, Column: Integer;
    var ReplaceAction: TSynReplaceAction) of object;

  TSynCopyPasteAction = (scaContinue, scaPlainText, scaAbort);
  TSynCopyPasteEvent = procedure(Sender: TObject; var AText: String; var AMode: TSynSelectionMode; ALogStartPos: TPoint;
    var AnAction: TSynCopyPasteAction) of object;

  TSynEditCaretType = SynEditPointClasses.TSynCaretType;

  TSynCaretAdjustMode = ( // used in TextBetweenPointsEx
    scamIgnore,
    // Caret stays at the same numeric values, if text is inserted before caret, the text moves, but the caret stays
    scamAdjust, // Caret moves with text, if text is inserted
    scamEnd, scamBegin);

  (* This is used, if text is *replaced*.
    What to do with marks in text that is deleted/replaced
  *)
  TSynMarksAdjustMode = ( // used in SetTextBetweenPoints
    smaMoveUp, //
    smaKeep
    // smaDrop
    );

  TSynEditTextFlag = (setSelect // select the new text
    );
  TSynEditTextFlags = set of TSynEditTextFlag;

  TSynStateFlag = (sfCaretChanged, sfHideCursor, sfEnsureCursorPos, sfEnsureCursorPosAtResize, sfIgnoreNextChar,
    sfPainting, sfHasScrolled, sfScrollbarChanged, sfHorizScrollbarVisible, sfVertScrollbarVisible,
    sfAfterLoadFromFileNeeded,
    // Mouse-states
    sfLeftGutterClick, sfRightGutterClick, sfDblClicked, sfTripleClicked, sfQuadClicked, sfWaitForDragging,
    sfIsDragging, sfWaitForMouseSelecting, sfMouseSelecting, sfMouseDoneSelecting, sfIgnoreUpClick, sfSelChanged);
  // mh 2000-10-30
  TSynStateFlags = set of TSynStateFlag;

  TSynEditorOption = (eoAutoIndent,
    // Will indent the caret on new lines with the same amount of leading white space as the preceding line
    eoBracketHighlight, // Highlight matching bracket
    eoEnhanceHomeKey, // home key jumps to line start if nearer, similar to visual studio
    eoGroupUndo,
    // When undoing/redoing actions, handle all continous changes of the same kind in one call instead undoing/redoing each command separately
    eoHalfPageScroll, // When scrolling with page-up and page-down commands, only scroll a half page at a time
    eoHideRightMargin, // Hides the right margin line
    eoKeepCaretX, // When moving through lines w/o Cursor Past EOL, keeps the X position of the cursor
    eoNoCaret, // Makes it so the caret is never visible
    eoNoSelection, // Disables selecting text
    eoPersistentCaret,
    // Do not hide caret when focus lost // TODO: Windows may hide it, if another component sets up a caret
    eoScrollByOneLess, // Forces scrolling to be one less
    eoScrollPastEof, // Allows the cursor to go past the end of file marker
    eoScrollPastEol, // Allows the cursor to go past the last character into the white space at the end of a line
    eoScrollHintFollows, // The scroll hint follows the mouse when scrolling vertically
    eoShowScrollHint, // Shows a hint of the visible line numbers when scrolling vertically
    eoShowSpecialChars, // Shows the special Characters
    eoSmartTabs, // When tabbing, the cursor will go to the next non-white space character of the previous line
    eoTabIndent, // When active <Tab> and <Shift><Tab> act as block indent, unindent when text is selected
    eoTabsToSpaces, // Converts a tab character to a specified number of space characters
    eoTrimTrailingSpaces, // Spaces at the end of lines will be trimmed and not saved

    // Not implemented
    eoAutoSizeMaxScrollWidth, // TODO Automatically resizes the MaxScrollWidth property when inserting text
    eoDisableScrollArrows,
    // TODO Disables the scroll bar arrow buttons when you can't scroll in that direction any more
    eoHideShowScrollbars,
    // TODO if enabled, then the scrollbars will only show when necessary.  If you have ScrollPastEOL, then it the horizontal bar will always be there (it uses MaxLength instead)
    eoDropFiles, // TODO Allows the editor accept file drops
    eoSmartTabDelete, // TODO similar to Smart Tabs, but when you delete characters
    eoSpacesToTabs, // Converts space characters to tabs and spaces
    eoAutoIndentOnPaste, // Indent text inserted from clipboard
    // eoSpecialLineDefaultFg,    //TODO disables the foreground text color override when using the OnSpecialLineColor event

    // Only for compatibility, moved to TSynEditorMouseOptions
    // keep in one block
    eoAltSetsColumnMode, //
    eoDragDropEditing, // Allows you to select a block of text and drag it within the document to another location
    eoRightMouseMovesCursor, // When clicking with the right mouse for a popup menu, move the cursor to that location
    eoDoubleClickSelectsLine, // Select line on double click
    eoShowCtrlMouseLinks // Pressing Ctrl (SYNEDIT_LINK_MODIFIER) will highlight the word under the mouse cursor
    );
  TSynEditorOptions = set of TSynEditorOption;

  TSynEditorOption2 = (eoCaretSkipsSelection, // Caret skips selection on VK_LEFT/VK_RIGHT
    eoCaretSkipTab, // Caret can not enter tabs
    eoAlwaysVisibleCaret, // Move caret to be always visible when scrolling
    eoEnhanceEndKey, // end key jumps to visual/hard line end whichever is nearer
    eoFoldedCopyPaste, // Remember folds in copy/paste operations
    eoPersistentBlock, // Keep block if caret moves away or text is edited
    eoOverwriteBlock, // Non persitent block, gets overwritten on insert/del
    eoAutoHideCursor // Hide the mouse cursor, on keyboard action
    );
  TSynEditorOptions2 = set of TSynEditorOption2;

  TSynEditorMouseOption = SynEditMouseCmds.TSynEditorMouseOption;
  // emUseMouseActions,
  // emAltSetsColumnMode,       // Alt modifier, triggers column mode selection
  // emDragDropEditing,         // Allows you to select a block of text and drag it within the document to another location
  // emRightMouseMovesCursor,   // When clicking with the right mouse for a popup menu, move the cursor to that location
  // emDoubleClickSelectsLine,  // Select line on double click
  // emShowCtrlMouseLinks       // Pressing Ctrl (SYNEDIT_LINK_MODIFIER) will highlight the word under the mouse cursor
  TSynEditorMouseOptions = SynEditMouseCmds.TSynEditorMouseOptions;

  // options for textbuffersharing
  TSynEditorShareOption = (eosShareMarks // Shared Editors use the same list of marks
    );
  TSynEditorShareOptions = set of TSynEditorShareOption;

  TSynVisibleSpecialChars = SynEditTypes.TSynVisibleSpecialChars;

const
  // MouseAction related options MUST NOT be included here
  SYNEDIT_DEFAULT_OPTIONS = [eoAutoIndent, eoScrollPastEol, eoSmartTabs, eoTabsToSpaces, eoTrimTrailingSpaces,
    eoGroupUndo, eoBracketHighlight];

  // Those will be prevented from being set => so evtl they may be removed
  SYNEDIT_UNIMPLEMENTED_OPTIONS = [eoAutoSizeMaxScrollWidth,
  // TODO Automatically resizes the MaxScrollWidth property when inserting text
  eoDisableScrollArrows, // TODO Disables the scroll bar arrow buttons when you can't scroll in that direction any more
  eoDropFiles, // TODO Allows the editor accept file drops
  eoHideShowScrollbars,
  // TODO if enabled, then the scrollbars will only show when necessary.  If you have ScrollPastEOL, then it the horizontal bar will always be there (it uses MaxLength instead)
  eoSmartTabDelete, // TODO similar to Smart Tabs, but when you delete characters
  /// /eoSpecialLineDefaultFg,    //TODO disables the foreground text color override when using the OnSpecialLineColor event
  eoAutoIndentOnPaste, // Indent text inserted from clipboard
  eoSpacesToTabs // Converts space characters to tabs and spaces
    ];

  SYNEDIT_OLD_MOUSE_OPTIONS = [eoAltSetsColumnMode, //
    eoDragDropEditing, // Allows you to select a block of text and drag it within the document to another location
  eoRightMouseMovesCursor, // When clicking with the right mouse for a popup menu, move the cursor to that location
  eoDoubleClickSelectsLine, // Select line on double click
  eoShowCtrlMouseLinks // Pressing Ctrl (SYNEDIT_LINK_MODIFIER) will highlight the word under the mouse cursor
    ];

  SYNEDIT_OLD_MOUSE_OPTIONS_MAP: array [eoAltSetsColumnMode .. eoShowCtrlMouseLinks] of TSynEditorMouseOption =
    (emAltSetsColumnMode, // eoAltSetsColumnMode
    emDragDropEditing, // eoDragDropEditing
    emRightMouseMovesCursor, // eoRightMouseMovesCursor
    emDoubleClickSelectsLine, // eoDoubleClickSelectsLine
    emShowCtrlMouseLinks // eoShowCtrlMouseLinks
    );

  SYNEDIT_DEFAULT_SHARE_OPTIONS = [eosShareMarks];

  SYNEDIT_DEFAULT_OPTIONS2 = [eoFoldedCopyPaste, eoOverwriteBlock];

  SYNEDIT_DEFAULT_MOUSE_OPTIONS = [];

  SYNEDIT_DEFAULT_VISIBLESPECIALCHARS = [vscSpace, vscTabAtLast];

type
  // use scAll to update a statusbar when another TCustomSynEdit got the focus
  TSynStatusChange = SynEditTypes.TSynStatusChange;
  TSynStatusChanges = SynEditTypes.TSynStatusChanges;

  TStatusChangeEvent = procedure(Sender: TObject; Changes: TSynStatusChanges) of object;

  TCustomSynEdit = class;

  TSynLineState = (slsNone, slsSaved, slsUnsaved);

  { TLazSynEditPlugin }

  TLazSynEditPlugin = class(TSynEditFriend)
  protected
    procedure RegisterToEditor(AValue: TCustomSynEdit);
    procedure UnRegisterFromEditor(AValue: TCustomSynEdit);
    procedure SetEditor(const AValue: TCustomSynEdit); virtual;
    function GetEditor: TCustomSynEdit;
    function OwnedByEditor: boolean; virtual; // if true, this will be destroyed by synedit
    procedure DoEditorDestroyed(const AValue: TCustomSynEdit); virtual;

    procedure DoAddEditor(AValue: TCustomSynEdit); virtual;
    procedure DoRemoveEditor(AValue: TCustomSynEdit); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Editor: TCustomSynEdit read GetEditor write SetEditor;
  end;

  { TSynHookedKeyTranslationList }

  TSynHookedKeyTranslationList = Class(TMethodList)
  public
    procedure CallHookedKeyTranslationHandlers(Sender: TObject; Code: word; SState: TShiftState; var Data: pointer;
      var IsStartOfCombo: boolean; var Handled: boolean; var Command: TSynEditorCommand;
      // ComboKeyStrokes decides, either FinishComboOnly, or new stroke
      var ComboKeyStrokes: TSynEditKeyStrokes);
  end;

  TSynMouseLinkEvent = procedure(Sender: TObject; X, Y: Integer; var AllowMouseLink: boolean) of object;

  TSynHomeMode = (synhmDefault, synhmFirstWord);

  TSynCoordinateMappingFlag = SynEditTypes.TSynCoordinateMappingFlag;
  TSynCoordinateMappingFlags = SynEditTypes.TSynCoordinateMappingFlags;

  TLazSynWordBoundary = (swbWordBegin, swbWordEnd, swbTokenBegin, swbTokenEnd, swbCaseChange);

  { TCustomSynEdit }

  TCustomSynEdit = class(TSynEditBase)
    procedure SelAvailChange(Sender: TObject);
  private
    procedure WMDropFiles(var Msg: TMessage); message WM_DROPFILES;
    procedure WMEraseBkgnd(var Msg: TMessage); message WM_ERASEBKGND;
//    procedure WMGetDlgCode(var Msg: TWMGetDlgCode); message WM_GETDLGCODE;
    procedure WMHScroll(var Msg: {$IFDEF SYN_LAZARUS}TLMScroll{$ELSE}TWMScroll{$ENDIF}); message WM_HSCROLL;
{$IFDEF SYN_MBCSSUPPORT}
    procedure WMImeComposition(var Msg: TMessage); message WM_IME_COMPOSITION;
    procedure WMImeNotify(var Msg: TMessage); message WM_IME_NOTIFY;
{$ENDIF}
    procedure WMKillFocus(var Msg: TWMKillFocus); message WM_KILLFOCUS;
    procedure WMExit(var Message: TWMClose); message WM_Close;
    procedure WMMouseWheel(var Message: TWMMouseWheel); message WM_MOUSEWHEEL;
    // procedure WMMouseWheel(var Msg: TMessage); message WM_MOUSEWHEEL;
    procedure WMSetFocus(var Msg: TWMSetFocus); message WM_SETFOCUS;
    procedure WMVScroll(var Msg: {$IFDEF SYN_LAZARUS}TLMScroll{$ELSE}TWMScroll{$ENDIF}); message WM_VSCROLL;
  private
    FBlockIndent: Integer;
    FBlockTabIndent: Integer;
    FCaret: TSynEditCaret;
    FInternalCaret: TSynEditCaret;
    FScreenCaret: TSynEditScreenCaret;
    FInternalBlockSelection: TSynEditSelection;
    FOnChangeUpdating: TChangeUpdatingEvent;
    FMouseSelectionMode: TSynSelectionMode;
    fMarkupManager: TSynEditMarkupManager;
    fMarkupHighAll: TSynEditMarkupHighlightAll;
    fMarkupHighCaret: TSynEditMarkupHighlightAllCaret;
    fMarkupBracket: TSynEditMarkupBracket;
    fMarkupWordGroup: TSynEditMarkupWordGroup;
    fMarkupCtrlMouse: TSynEditMarkupCtrlMouseLink;
    fMarkupSpecialLine: TSynEditMarkupSpecialLine;
    fMarkupSelection: TSynEditMarkupSelection;
    fMarkupSpecialChar: TSynEditMarkupSpecialChar;
    fFontDummy: TFont;
    FLastSetFontSize: Single;
{$IFDEF SYN_MBCSSUPPORT}
    fImeCount: Integer;
    fMBCSStepAside: boolean;
{$ENDIF}
    fInserting: boolean;
    fLastMouseCaret: TPoint; // Char; physical (screen)
    FLastMousePoint: TPoint; // Pixel
    FChangedLinesStart: Integer; // 1 based, 0 means invalid
    FChangedLinesEnd: Integer; // 1 based, 0 means invalid, -1 means rest of screen
    FBeautifier, FDefaultBeautifier: TSynCustomBeautifier;
    FBeautifyStartLineIdx, FBeautifyEndLineIdx: Integer;

    FFoldedLinesView: TSynEditFoldedView;
    FShareOptions: TSynEditorShareOptions;
    FVisibleSpecialChars: TSynVisibleSpecialChars;
    FTrimmedLinesView: TSynEditStringTrimmingList;
    FDoubleWidthChrLinesView: SynEditStringDoubleWidthChars;
    FTabbedLinesView: TSynEditStringTabExpander;
    FTheLinesView: TSynEditStrings;
    FLines: TSynEditStrings; // The real (un-mapped) line-buffer
    FStrings: TStrings; // External TStrings based interface to the Textbuffer
    FTopLinesView: TSynEditStrings; // The linesview that holds the real line-buffer/FLines
    FDisplayView: TLazSynDisplayView;

    fExtraCharSpacing: Integer;
    fMaxLeftChar: Integer; // 1024
    FOldWidth, FOldHeight: Integer;

    FPaintLock: Integer;
    FPaintLockOwnerCnt: Integer;
    FUndoBlockAtPaintLock: Integer;
    FScrollBarUpdateLock: Integer;
    FInvalidateRect: TRect;
    FIsInDecPaintLock: boolean;
    fReadOnly: boolean;
    // FScrollBars: TScrollStyle;
    FOldTopView: Integer;
    FLastTextChangeStamp: Int64;
    fHighlighter: TSynCustomHighlighter;
    fUndoList: TSynEditUndoList;
    fRedoList: TSynEditUndoList;
    FBookMarks: array [0 .. 9] of TSynEditMark;
    fMouseDownX: Integer;
    fMouseDownY: Integer;
    fBookMarkOpt: TSynBookMarkOpt;
    FMouseWheelAccumulator, FMouseWheelLinesAccumulator: Single;
    fHideSelection: boolean;
    fOverwriteCaret: TSynEditCaretType;
    fInsertCaret: TSynEditCaretType;
    FKeyStrokes: TSynEditKeyStrokes;
    FCurrentComboKeyStrokes: TSynEditKeyStrokes;
    // Holding info about the keystroke(s) already received for a mult-stroke-combo
    FMouseActions, FMouseSelActions, FMouseTextActions: TSynEditMouseInternalActions;
    FMouseActionSearchHandlerList: TSynEditMouseActionSearchList;
    FMouseActionExecHandlerList: TSynEditMouseActionExecList;
    FMarkList: TSynEditMarkList;
    fExtraLineSpacing: Integer;
    FUseUTF8: boolean;
    fWantTabs: boolean;
    FLeftGutter, FRightGutter: TSynGutter;
    fTabWidth: Integer;
    fTextDrawer: TheTextDrawer;
    FPaintLineColor, FPaintLineColor2: TSynSelectedColor;
    fStateFlags: TSynStateFlags;
    FOptions: TSynEditorOptions;
    FOptions2: TSynEditorOptions2;
    FMouseOptions: TSynEditorMouseOptions;
    fStatusChanges: TSynStatusChanges;
    fTSearch: TSynEditSearch;
    fHookedCommandHandlers: TList;
    FHookedKeyTranslationList: TSynHookedKeyTranslationList;
    FStatusChangedList: TObject;
    FPlugins: TList;
    fScrollTimer: TTimer;
    FScrollDeltaX, FScrollDeltaY: Integer;
    FInMouseClickEvent: boolean;
    FMouseClickDoPopUp: boolean;
    // event handlers
    FOnCutCopy: TSynCopyPasteEvent;
    FOnPaste: TSynCopyPasteEvent;
    fOnChange: TNotifyEvent;
    FOnClearMark: TPlaceMarkEvent; // djlp 2000-08-29
    fOnCommandProcessed: TProcessCommandEvent;
    fOnDropFiles: TSynDropFilesEvent;
    fOnPaint: TPaintEvent;
    FOnPlaceMark: TPlaceMarkEvent;
    fOnProcessCommand: TProcessCommandEvent;
    fOnProcessUserCommand: TProcessCommandEvent;
    fOnReplaceText: TReplaceTextEvent;
    fOnSpecialLineColors: TSpecialLineColorsEvent; // needed, because bug fpc 11926
    fOnStatusChange: TStatusChangeEvent;
    FOnSpecialLineMarkup: TSpecialLineMarkupEvent; // needed, because bug fpc 11926
    FOnClickLink: TMouseEvent;
    FOnMouseLink: TSynMouseLinkEvent;
    FPendingFoldState: String;

    procedure AquirePrimarySelection;
    function GetChangeStamp: Int64;
    function GetCharsInWindow: Integer;
    function GetCharWidth: Integer;
    function GetDefSelectionMode: TSynSelectionMode;
    function GetFoldState: String;
    function GetLeftChar: Integer;
    function GetLineHeight: Integer;
    function GetLinesInWindow: Integer;
    function GetModified: boolean;
    function GetMouseActions: TSynEditMouseActions;
    function GetMouseSelActions: TSynEditMouseActions;
    function GetMouseTextActions: TSynEditMouseActions;
    function GetPaintLockOwner: TSynEditBase;
    function GetPlugin(Index: Integer): TLazSynEditPlugin;
    function GetRightEdge: Integer;
    function GetRightEdgeColor: TColor;
    function GetTextBetweenPoints(aStartPoint, aEndPoint: TPoint): String;
    function GetTopLine: Integer;
    procedure SetBlockTabIndent(AValue: Integer);
    procedure SetBracketMatchColor(AValue: TSynSelectedColor);
    procedure SetDefSelectionMode(const AValue: TSynSelectionMode);
    procedure SetFoldedCodeColor(AValue: TSynSelectedColor);
    procedure SetFoldState(const AValue: String);
    procedure SetHighlightAllColor(AValue: TSynSelectedColor);
    procedure SetIncrementColor(AValue: TSynSelectedColor);
    procedure SetLineHighlightColor(AValue: TSynSelectedColor);
    procedure SetMouseActions(const AValue: TSynEditMouseActions);
    procedure SetMouseLinkColor(AValue: TSynSelectedColor);
    procedure SetMouseSelActions(const AValue: TSynEditMouseActions);
    procedure SetMouseTextActions(AValue: TSynEditMouseActions);
    procedure SetPaintLockOwner(const AValue: TSynEditBase);
    procedure SetShareOptions(const AValue: TSynEditorShareOptions);
    procedure SetTextBetweenPointsSimple(aStartPoint, aEndPoint: TPoint; const AValue: String);
    procedure SetTextBetweenPointsEx(aStartPoint, aEndPoint: TPoint; aCaretMode: TSynCaretAdjustMode;
      const AValue: String);
    procedure SetVisibleSpecialChars(AValue: TSynVisibleSpecialChars);
    procedure SurrenderPrimarySelection;
    procedure BookMarkOptionsChanged(Sender: TObject);
    procedure ComputeCaret(X, Y: Single);
    procedure DoBlockIndent;
    procedure DoBlockUnindent;
    procedure DoHomeKey(AMode: TSynHomeMode = synhmDefault);
    procedure DoEndKey;
    procedure DoTabKey;
    function FindHookedCmdEvent(AHandlerProc: THookedCommandEvent): Integer;
    function GetBlockBegin: TPoint;
    function GetBlockEnd: TPoint;
    function GetBracketHighlightStyle: TSynEditBracketHighlightStyle;
    function GetCanPaste: boolean;
    function GetCanRedo: boolean;
    function GetCanUndo: boolean;
    function GetCaretXY: TPoint;
    function GetFoldedCodeColor: TSynSelectedColor;
    function GetMarkup(Index: Integer): TSynEditMarkup;
    function GetMarkupByClass(Index: TSynEditMarkupClass): TSynEditMarkup;
    function GetCaretX: Integer;
    function GetCaretY: Integer;
    function GetCaretUndo: TSynEditUndoItem;
    function GetHighlightAllColor: TSynSelectedColor;
    function GetIncrementColor: TSynSelectedColor;
    function GetLineHighlightColor: TSynSelectedColor;
    function GetOnGutterClick: TGutterClickEvent;
    function GetSelectedColor: TSynSelectedColor;
    function GetBracketMatchColor: TSynSelectedColor;
    function GetMouseLinkColor: TSynSelectedColor;
    function GetTrimSpaceType: TSynEditStringTrimmingType;
    procedure SetBracketHighlightStyle(const AValue: TSynEditBracketHighlightStyle);
    procedure SetOnGutterClick(const AValue: TGutterClickEvent);
    procedure SetSelectedColor(const AValue: TSynSelectedColor);
    procedure SetSpecialLineColors(const AValue: TSpecialLineColorsEvent);
    procedure SetSpecialLineMarkup(const AValue: TSpecialLineMarkupEvent);
    function GetHookedCommandHandlersCount: Integer;
    function GetLineText: string;
    function GetCharLen(const Line: string; CharStartPos: Integer): Integer;
    function GetLogicalCaretXY: TPoint;
    procedure SetLogicalCaretXY(const NewLogCaretXY: TPoint);
    procedure SetBeautifier(NewBeautifier: TSynCustomBeautifier);
    function GetMaxUndo: Integer;
    function GetSelAvail: boolean;
    function GetSelText: string;
    procedure SetTrimSpaceType(const AValue: TSynEditStringTrimmingType);
    function SynGetText: string;
    procedure GutterChanged(Sender: TObject);
    procedure GutterResized(Sender: TObject);
    // x-pixel pos of first char on canvas
    function TextLeftPixelOffset(IncludeGutterTextDist: boolean = True): Integer;
    function TextRightPixelOffset: Integer;
    function IsPointInSelection(Value: TPoint): boolean;
    procedure LockUndo;
    procedure MoveCaretHorz(DX: Integer);
    procedure MoveCaretVert(DY: Integer);
    // procedure PrimarySelectionRequest(const RequestedFormatID: TClipboardFormat; Data: TStream);
    procedure ScanRanges(ATextChanged: boolean = True);
    procedure IdleScanRanges(Sender: TObject; var Done: boolean);
    procedure DoBlockSelectionChanged(Sender: TObject);
    procedure SetBlockBegin(Value: TPoint);
    procedure SetBlockEnd(Value: TPoint);
    procedure SetBlockIndent(const AValue: Integer);
    procedure SetCaretAndSelection(const ptCaret, ptBefore, ptAfter: TPoint; Mode: TSynSelectionMode = smCurrent;
      MakeSelectionVisible: boolean = False);
    procedure SetCaretX(const Value: Integer);
    procedure SetCaretY(const Value: Integer);
    procedure SetExtraLineSpacing(const Value: Integer);
    procedure SetGutter(const Value: TSynGutter);
    procedure SetRightGutter(const AValue: TSynGutter);
    procedure SetHideSelection(const Value: boolean);
    procedure SetHighlighter(const Value: TSynCustomHighlighter);
    procedure RemoveHooksFromHighlighter;
    procedure SetInsertCaret(const Value: TSynEditCaretType);
    procedure SetInsertMode(const Value: boolean);
    procedure SetKeystrokes(const Value: TSynEditKeyStrokes);
    procedure SetExtraCharSpacing(const Value: Integer);
    procedure SetLastMouseCaret(const AValue: TPoint);
    function CurrentMaxLeftChar: Integer;
    function CurrentMaxLineLen: Integer;
    procedure SetLeftChar(Value: Integer);
    procedure SetLineText(Value: string);
    procedure SetMaxLeftChar(Value: Integer);
    procedure SetMaxUndo(const Value: Integer);
    procedure SetModified(Value: boolean);
    procedure SetOptions(Value: TSynEditorOptions);
    procedure UpdateOptions;
    procedure SetOptions2(const Value: TSynEditorOptions2);
    procedure UpdateOptions2;
    procedure SetMouseOptions(AValue: TSynEditorMouseOptions);
    procedure UpdateMouseOptions;
    procedure SetOverwriteCaret(const Value: TSynEditCaretType);
    procedure SetRightEdge(Value: Integer);
    procedure SetRightEdgeColor(Value: TColor);
    // procedure SetScrollBars(const Value: TScrollStyle);
    function GetSelectionMode: TSynSelectionMode;
    procedure SetSelectionMode(const Value: TSynSelectionMode);
    procedure SetSelTextExternal(const Value: string);
    procedure SetTabWidth(Value: Integer);
    procedure SynSetText(const Value: string);
    function CurrentMaxTopView: Integer;
    procedure SetTopLine(Value: Integer);
    procedure ScrollAfterTopLineChanged;
    procedure SetWantTabs(const Value: boolean);
    procedure SetWordBlock(Value: TPoint);
    procedure SetLineBlock(Value: TPoint; WithLeadSpaces: boolean = True);
    procedure SetParagraphBlock(Value: TPoint);
    procedure RecalcCharsAndLinesInWin(CheckCaret: boolean);
    procedure StatusChanged(AChanges: TSynStatusChanges);
    procedure UndoRedoAdded(Sender: TObject);
    procedure ModifiedChanged(Sender: TObject);
    procedure UnlockUndo;
    procedure UpdateCaret(IgnorePaintLock: boolean = False);
    procedure UpdateScrollBars;
    procedure ChangeTextBuffer(NewBuffer: TSynEditStringList);
    function IsMarkListShared: boolean;
    procedure RecreateMarkList;
    procedure DestroyMarkList;
    procedure RemoveHandlers(ALines: TSynEditStrings = nil);
    procedure ExtraLineCharsChanged(Sender: TObject);
    procedure InternalBeginUndoBlock(aList: TSynEditUndoList = nil); // includes paintlock
    procedure InternalEndUndoBlock(aList: TSynEditUndoList = nil);
  protected
{$IFDEF EnableDoubleBuf}
    BufferBitmap: TBitmap; // the double buffer
    SavedCanvas: TCanvas; // the normal TCustomControl canvas during paint
{$ENDIF}
    FTextArea: TLazSynTextArea;
    FLeftGutterArea, FRightGutterArea: TLazSynGutterArea;
    FPaintArea: TLazSynSurfaceManager;

    procedure Paint; override;
    procedure StartPaintBuffer(const ClipRect: TRect);
    procedure EndPaintBuffer(const ClipRect: TRect);
    procedure DoOnPaint; virtual;

    procedure IncPaintLock;
    procedure DecPaintLock;
    procedure DoIncPaintLock(Sender: TObject);
    procedure DoDecPaintLock(Sender: TObject);
    procedure DoIncForeignPaintLock(Sender: TObject);
    procedure DoDecForeignPaintLock(Sender: TObject);
    procedure SetUpdateState(NewUpdating: boolean; Sender: TObject); virtual;
    // Called *before* paintlock, and *after* paintlock

    property PaintLockOwner: TSynEditBase read GetPaintLockOwner write SetPaintLockOwner;
  protected
    // procedure CreateHandle; override;
    // procedure CreateParams(var Params: TCreateParams); override;
    // procedure CreateWnd; override;
    // procedure DestroyWnd; override;
    procedure Loaded; override;
    function GetChildOwner: TComponent; override;
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;

    procedure KeyDown(var Key: word; var KeyChar: WideChar; Shift: TShiftState); override;
    // procedure UTF8KeyPress(var Key: TUTF8Char); override;
//    procedure KeyPress(var Key: Char); virtual;
    procedure KeyUp(var Key: word; var KeyChar: WideChar; Shift: TShiftState); override;

    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Single); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure ScrollTimerHandler(Sender: TObject);
    // procedure DoContextPopup(MousePos: TPoint; var Handled: Boolean); override;
    procedure FindAndHandleMouseAction(AButton: TSynMouseButton; AShift: TShiftState; X, Y: Single;
      ACCount: TSynMAClickCount; ADir: TSynMAClickDir; AWheelDelta: Integer = 0);
    function DoHandleMouseAction(AnActionList: TSynEditMouseActions; AnInfo: TSynEditMouseActionInfo): boolean;

  protected
    // procedure SetColor(Value: TColor); override;
    procedure DragOver(const Data: TDragObject; const Point: TPointF; var Accept: boolean); override;
//    procedure Resize; override;
    // function  RealGetText: TCaption; override;
    // procedure RealSetText(const Value: TCaption); override;
    function GetLines: TStrings; override;
    function GetViewedTextBuffer: TSynEditStrings; override;
    function GetFoldedTextBuffer: TObject; override;
    function GetTextBuffer: TSynEditStrings; override;
    procedure SetLines(Value: TStrings); override;
    function GetMarkupMgr: TObject; override;
    function GetCaretObj: TSynEditCaret; override;
    // procedure FontChanged(Sender: TObject); override;
    function GetReadOnly: boolean; virtual;
    procedure HighlighterAttrChanged(Sender: TObject);
    // note: FirstLine and LastLine don't need to be in correct order
    procedure InvalidateGutterLines(FirstLine, LastLine: Integer);
    procedure InvalidateLines(FirstLine, LastLine: Integer);
    Procedure LineCountChanged(Sender: TSynEditStrings; AIndex, ACount: Integer);
    Procedure LineTextChanged(Sender: TSynEditStrings; AIndex, ACount: Integer);
    procedure SizeOrFontChanged(bFont: boolean);
    procedure DoHighlightChanged(Sender: TSynEditStrings; AIndex, ACount: Integer);
    procedure ListCleared(Sender: TObject);
    procedure FoldChanged(Index: Integer);
    function GetTopView: Integer;
    procedure SetTopView(AValue: Integer);
    procedure MarkListChange(Sender: TSynEditMark; Changes: TSynEditMarkChangeReasons);
{$IFDEF SYN_MBCSSUPPORT}
    procedure MBCSGetSelRangeInLineWhenColumnSelectionMode(const s: string; var ColFrom, ColTo: Integer);
{$ENDIF}
    procedure NotifyHookedCommandHandlers(var Command: TSynEditorCommand; var AChar: Char; Data: pointer;
      ATime: THookedCommandFlag); virtual;
    function NextWordLogicalPos(ABoundary: TLazSynWordBoundary = swbWordBegin;
      WordEndForDelete: boolean = False): TPoint;
    function PrevWordLogicalPos(ABoundary: TLazSynWordBoundary = swbWordBegin): TPoint;
    procedure RecalcCharExtent;
    procedure RedoItem(Item: TSynEditUndoItem);
    procedure SetCaretXY(Value: TPoint);
    procedure CaretChanged(Sender: TObject);
//    procedure SetName(const Value: TComponentName); override;
    procedure SetReadOnly(Value: boolean); virtual;
    procedure SetSelTextPrimitive(PasteMode: TSynSelectionMode; Value: PChar; AddToUndoList: boolean = False);
    procedure UndoItem(Item: TSynEditUndoItem);
    procedure UpdateCursor;
    procedure DoOnCommandProcessed(Command: TSynEditorCommand; AChar: Char; Data: pointer); virtual;
    // no method DoOnDropFiles, intercept the WM_DROPFILES instead
    procedure DoOnProcessCommand(var Command: TSynEditorCommand; var AChar: Char; Data: pointer); virtual;
    function DoOnReplaceText(const ASearch, AReplace: string; Line, Column: Integer): TSynReplaceAction; virtual;
    procedure DoOnStatusChange(Changes: TSynStatusChanges); virtual;
    property LastMouseCaret: TPoint read fLastMouseCaret write SetLastMouseCaret;
    function GetSelEnd: Integer; // L505
    function GetSelStart: Integer;
    procedure SetSelEnd(const Value: Integer);
    procedure SetSelStart(const Value: Integer);
    property TextView: TSynEditFoldedView read FFoldedLinesView;
    property TopView: Integer read GetTopView write SetTopView; // TopLine converted into Visible(View) lines
    // function PasteFromClipboardEx(ClipHelper: TSynClipboardStream): Boolean;
    function FindNextUnfoldedLine(iLine: Integer; Down: boolean): Integer;
    // Todo: Reduce the argument list of Creategutter
    function CreateGutter(AOwner: TSynEditBase; ASide: TSynGutterSide; ATextDrawer: TheTextDrawer): TSynGutter; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AfterLoadFromFile;

    procedure BeginUndoBlock{$IFDEF SynUndoDebugBeginEnd}(ACaller: String = ''){$ENDIF};
    procedure BeginUpdate(WithUndoBlock: boolean = True);
    procedure EndUndoBlock{$IFDEF SynUndoDebugBeginEnd}(ACaller: String = ''){$ENDIF};
    procedure EndUpdate;

  public
    // Caret
    function CaretXPix: Integer;
    function CaretYPix: Integer;
    procedure EnsureCursorPosVisible;
    procedure MoveCaretToVisibleArea;
    procedure MoveCaretIgnoreEOL(const NewCaret: TPoint);
    procedure MoveLogicalCaretIgnoreEOL(const NewLogCaret: TPoint);

    property CaretX: Integer read GetCaretX write SetCaretX;
    property CaretY: Integer read GetCaretY write SetCaretY;
    property CaretXY: TPoint read GetCaretXY write SetCaretXY; // screen position
    property LogicalCaretXY: TPoint read GetLogicalCaretXY write SetLogicalCaretXY;

    // Selection
    procedure ClearSelection;
    procedure SelectAll;
    procedure SelectToBrace;
    procedure SelectWord;
    procedure SelectLine(WithLeadSpaces: boolean = True);
    procedure SelectParagraph;

    property BlockBegin: TPoint read GetBlockBegin write SetBlockBegin;
    // Set Blockbegin. For none persistent also sets Blockend. Setting Caret may undo this and should be done before setting block
    property BlockEnd: TPoint read GetBlockEnd write SetBlockEnd;
    property SelStart: Integer read GetSelStart write SetSelStart;
    property SelEnd: Integer read GetSelEnd write SetSelEnd;
    property SelAvail: boolean read GetSelAvail;
    property SelText: string read GetSelText write SetSelTextExternal;

    // Text
    procedure ClearAll;
    procedure InsertTextAtCaret(AText: String; aCaretMode: TSynCaretAdjustMode = scamEnd);

    property TextBetweenPoints[aStartPoint, aEndPoint: TPoint]: String // Logical Points
      read GetTextBetweenPoints write SetTextBetweenPointsSimple;
    property TextBetweenPointsEx[aStartPoint, aEndPoint: TPoint; CaretMode: TSynCaretAdjustMode]: String
      write SetTextBetweenPointsEx;
    procedure SetTextBetweenPoints(aStartPoint, aEndPoint: TPoint; const AValue: String; aFlags: TSynEditTextFlags = [];
      aCaretMode: TSynCaretAdjustMode = scamIgnore; aMarksMode: TSynMarksAdjustMode = smaMoveUp;
      aSelectionMode: TSynSelectionMode = smNormal);

    property LineText: string read GetLineText write SetLineText;
    property Text: string read SynGetText write SynSetText; // No uncommited (trailing/trimmable) spaces

    function GetLineState(ALine: Integer): TSynLineState;
    procedure MarkTextAsSaved;

    // BoorMark
    procedure ClearBookMark(BookMark: Integer);
    function GetBookMark(BookMark: Integer; var X, Y: Integer): boolean;
    procedure GotoBookMark(BookMark: Integer);
    function IsBookmark(BookMark: Integer): boolean;
    procedure SetBookMark(BookMark: Integer; X: Integer; Y: Integer);
    property Marks: TSynEditMarkList read FMarkList;

    // Undo/Redo
    procedure ClearUndo;
    procedure Redo;
    procedure Undo;
    property CanRedo: boolean read GetCanRedo;
    property CanUndo: boolean read GetCanUndo;

    // Clipboard
    procedure CopyToClipboard;
    procedure CutToClipboard;
    procedure PasteFromClipboard;
    procedure DoCopyToClipboard(SText: string; FoldInfo: String = '');
    property CanPaste: boolean read GetCanPaste;

    procedure DragDrop(const Data: TDragObject; const Point: TPointF); override;
{$IFDEF SYN_COMPILER_4_UP}
    function ExecuteAction(ExeAction: TBasicAction): boolean; override;
{$ENDIF}
    procedure CommandProcessor(Command: TSynEditorCommand; AChar: Char; Data: pointer); virtual;
    procedure ExecuteCommand(Command: TSynEditorCommand; const AChar: Char; Data: pointer); virtual;

    function GetHighlighterAttriAtRowCol(XY: TPoint; out Token: string; out Attri: TSynHighlighterAttributes): boolean;
    function GetHighlighterAttriAtRowColEx(XY: TPoint; out Token: string; out TokenType, Start: Integer;
      out Attri: TSynHighlighterAttributes): boolean; // L505
    procedure GetWordBoundsAtRowCol(const XY: TPoint; out StartX, EndX: Integer);
    function GetWordAtRowCol(XY: TPoint): string;
    function NextTokenPos: TPoint; virtual; deprecated; // use next word pos instead
    function NextWordPos: TPoint; virtual;
    function PrevWordPos: TPoint; virtual;
    function IdentChars: TSynIdentChars;
    function IsIdentChar(const c: Char): boolean;

    function IsLinkable(Y, X1, X2: Integer): boolean;
    procedure InvalidateGutter;
    procedure InvalidateLine(Line: Integer);

    // Byte to Char
    function LogicalToPhysicalPos(const p: TPoint): TPoint;
    function LogicalToPhysicalCol(const Line: String; Index, LogicalPos: Integer): Integer;
    // Char to Byte
    function PhysicalToLogicalPos(const p: TPoint): TPoint;
    function PhysicalToLogicalCol(const Line: string; Index, PhysicalPos: Integer): Integer;
    function PhysicalLineLength(Line: String; Index: Integer): Integer;

    // Pixel
    function ScreenColumnToXValue(Col: Integer): Integer; // map screen column to screen pixel
    // RowColumnToPixels: Physical coords
    function RowColumnToPixels(RowCol: TPoint): TPoint;
    function PixelsToRowColumn(Pixels: TPoint; aFlags: TSynCoordinateMappingFlags = [scmLimitToLines]): TPoint;
    function PixelsToLogicalPos(const Pixels: TPoint): TPoint;
    //
    function ScreenRowToRow(ScreenRow: Integer; LimitToLines: boolean = True): Integer;
    function RowToScreenRow(PhysicalRow: Integer): Integer;

    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure RegisterCommandHandler(AHandlerProc: THookedCommandEvent; AHandlerData: pointer;
      aFlags: THookedCommandFlags = [hcfPreExec, hcfPostExec]);
    procedure UnregisterCommandHandler(AHandlerProc: THookedCommandEvent);

    procedure RegisterMouseActionSearchHandler(AHandlerProc: TSynEditMouseActionSearchProc);
    procedure UnregisterMouseActionSearchHandler(AHandlerProc: TSynEditMouseActionSearchProc);
    procedure RegisterMouseActionExecHandler(AHandlerProc: TSynEditMouseActionExecProc);
    procedure UnregisterMouseActionExecHandler(AHandlerProc: TSynEditMouseActionExecProc);

    procedure RegisterKeyTranslationHandler(AHandlerProc: THookedKeyTranslationEvent);
    procedure UnRegisterKeyTranslationHandler(AHandlerProc: THookedKeyTranslationEvent);

    procedure RegisterStatusChangedHandler(AStatusChangeProc: TStatusChangeEvent; AChanges: TSynStatusChanges);
    procedure UnRegisterStatusChangedHandler(AStatusChangeProc: TStatusChangeEvent);

    function SearchReplace(const ASearch, AReplace: string; AOptions: TSynSearchOptions): Integer;
    function SearchReplaceEx(const ASearch, AReplace: string; AOptions: TSynSearchOptions; AStart: TPoint): Integer;

    procedure SetUseIncrementalColor(const AValue: boolean);
    procedure SetDefaultKeystrokes; virtual;
    procedure ResetMouseActions; // set mouse-actions according to current Options / may clear them
    procedure SetOptionFlag(Flag: TSynEditorOption; Value: boolean);
    Procedure SetHighlightSearch(const ASearch: String; AOptions: TSynSearchOptions);
{$IFDEF SYN_COMPILER_4_UP}
    function UpdateAction(TheAction: TBasicAction): boolean; override;
{$ENDIF}
//    procedure WndProc(var Msg: TMessage); override;
//    procedure EraseBackground(DC: HDC); override;
  public
    procedure FindMatchingBracket; overload; virtual;
    function FindMatchingBracket(PhysStartBracket: TPoint; StartIncludeNeighborChars, MoveCaret, SelectBrackets,
      OnlyVisible: boolean): TPoint; overload; virtual;
    // code fold
    procedure CodeFoldAction(iLine: Integer); deprecated;
    procedure UnfoldAll;
    procedure FoldAll(StartLevel: Integer = 0; IgnoreNested: boolean = False);
    property FoldState: String read GetFoldState write SetFoldState;

    procedure AddKey(Command: TSynEditorCommand; Key1: word; SS1: TShiftState; Key2: word; SS2: TShiftState);
  public
    property CharsInWindow: Integer read GetCharsInWindow;
    property CharWidth: Integer read GetCharWidth;
    property LeftChar: Integer read GetLeftChar write SetLeftChar;
    property LineHeight: Integer read GetLineHeight;
    property LinesInWindow: Integer read GetLinesInWindow;
    property MaxLeftChar: Integer read fMaxLeftChar write SetMaxLeftChar default 1024;
    property TopLine: Integer read GetTopLine write SetTopLine;

    property UseIncrementalColor: boolean write SetUseIncrementalColor;
    property Modified: boolean read GetModified write SetModified;
    property PaintLock: Integer read FPaintLock;

    property UseUTF8: boolean read FUseUTF8;
//    procedure Update; override;
//    procedure Invalidate; override;
    property ChangeStamp: Int64 read GetChangeStamp;
    procedure ShareTextBufferFrom(AShareEditor: TCustomSynEdit);
    procedure UnShareTextBuffer;

    function PluginCount: Integer;
    property Plugin[Index: Integer]: TLazSynEditPlugin read GetPlugin;
    function MarkupCount: Integer;
    property Markup[Index: Integer]: TSynEditMarkup read GetMarkup;
    property MarkupByClass[Index: TSynEditMarkupClass]: TSynEditMarkup read GetMarkupByClass;
    property TrimSpaceType: TSynEditStringTrimmingType read GetTrimSpaceType write SetTrimSpaceType;
  public
    // Caret
    property InsertCaret: TSynEditCaretType read fInsertCaret write SetInsertCaret default ctVerticalLine;
    property OverwriteCaret: TSynEditCaretType read fOverwriteCaret write SetOverwriteCaret default ctBlock;

    // Selection
    property HideSelection: boolean read fHideSelection write SetHideSelection default False;
    property DefaultSelectionMode: TSynSelectionMode read GetDefSelectionMode write SetDefSelectionMode
      default smNormal;
    property SelectionMode: TSynSelectionMode read GetSelectionMode write SetSelectionMode default smNormal;
    property SelectedColor: TSynSelectedColor read GetSelectedColor write SetSelectedColor;

    // Colors
//    property Color default clWhite;
    property IncrementColor: TSynSelectedColor read GetIncrementColor write SetIncrementColor;
    property HighlightAllColor: TSynSelectedColor read GetHighlightAllColor write SetHighlightAllColor;
    property BracketMatchColor: TSynSelectedColor read GetBracketMatchColor write SetBracketMatchColor;
    property MouseLinkColor: TSynSelectedColor read GetMouseLinkColor write SetMouseLinkColor;
    property LineHighlightColor: TSynSelectedColor read GetLineHighlightColor write SetLineHighlightColor;
    property FoldedCodeColor: TSynSelectedColor read GetFoldedCodeColor write SetFoldedCodeColor;

    property Beautifier: TSynCustomBeautifier read FBeautifier write SetBeautifier;
    property BookMarkOptions: TSynBookMarkOpt read fBookMarkOpt write fBookMarkOpt;
    property BlockIndent: Integer read FBlockIndent write SetBlockIndent default 2;
    property BlockTabIndent: Integer read FBlockTabIndent write SetBlockTabIndent default 0;
    property ExtraCharSpacing: Integer read fExtraCharSpacing write SetExtraCharSpacing default 0;
    property ExtraLineSpacing: Integer read fExtraLineSpacing write SetExtraLineSpacing default 0;
    property Highlighter: TSynCustomHighlighter read fHighlighter write SetHighlighter;
    property Gutter: TSynGutter read FLeftGutter write SetGutter;
    property RightGutter: TSynGutter read FRightGutter write SetRightGutter;
    property InsertMode: boolean read fInserting write SetInsertMode default True;
    property Keystrokes: TSynEditKeyStrokes read FKeyStrokes write SetKeystrokes;
    property MouseActions: TSynEditMouseActions read GetMouseActions write SetMouseActions;
    property MouseTextActions: TSynEditMouseActions read GetMouseTextActions write SetMouseTextActions;
    property MouseSelActions: TSynEditMouseActions // Mouseactions, if mouse is over selection => fallback to normal
      read GetMouseSelActions write SetMouseSelActions;
    property MaxUndo: Integer read GetMaxUndo write SetMaxUndo default 1024;
    property Options: TSynEditorOptions read FOptions write SetOptions
    // See SYNEDIT_UNIMPLEMENTED_OPTIONS for deprecated Values
      default SYNEDIT_DEFAULT_OPTIONS;
    property Options2: TSynEditorOptions2 read FOptions2 write SetOptions2 default SYNEDIT_DEFAULT_OPTIONS2;
    property MouseOptions: TSynEditorMouseOptions read FMouseOptions write SetMouseOptions
      default SYNEDIT_DEFAULT_MOUSE_OPTIONS;
//    property ShareOptions: TSynEditorShareOptions read FShareOptions write SetShareOptions
//      default SYNEDIT_DEFAULT_SHARE_OPTIONS;
//    experimental;
    property VisibleSpecialChars: TSynVisibleSpecialChars read FVisibleSpecialChars write SetVisibleSpecialChars;
    property ReadOnly: boolean read GetReadOnly write SetReadOnly default False;
    property RightEdge: Integer read GetRightEdge write SetRightEdge default 80;
    property RightEdgeColor: TColor read GetRightEdgeColor write SetRightEdgeColor default TColorRec.Silver;
    // property ScrollBars: TScrollStyle
    // read FScrollBars write SetScrollBars default ssBoth;
    property BracketHighlightStyle: TSynEditBracketHighlightStyle read GetBracketHighlightStyle
      write SetBracketHighlightStyle;
    property TabWidth: Integer read fTabWidth write SetTabWidth default 8;
    property WantTabs: boolean read fWantTabs write SetWantTabs default False;

    // Events
    property OnChange: TNotifyEvent read fOnChange write fOnChange;
    property OnChangeUpdating: TChangeUpdatingEvent read FOnChangeUpdating write FOnChangeUpdating;
    property OnCutCopy: TSynCopyPasteEvent read FOnCutCopy write FOnCutCopy;
    property OnPaste: TSynCopyPasteEvent read FOnPaste write FOnPaste;
    property OnDropFiles: TSynDropFilesEvent read fOnDropFiles write fOnDropFiles;
    property OnGutterClick: TGutterClickEvent read GetOnGutterClick write SetOnGutterClick;
    property OnPaint: TPaintEvent read fOnPaint write fOnPaint;
    // OnPlaceBookmark only triggers for Bookmarks
    property OnPlaceBookmark: TPlaceMarkEvent read FOnPlaceMark write FOnPlaceMark;
    // OnClearBookmark only triggers for Bookmarks
    property OnClearBookmark: TPlaceMarkEvent read FOnClearMark write FOnClearMark;
    property OnKeyDown;
//    property OnKeyPress;
    property OnProcessCommand: TProcessCommandEvent read fOnProcessCommand write fOnProcessCommand;
    property OnProcessUserCommand: TProcessCommandEvent read fOnProcessUserCommand write fOnProcessUserCommand;
    property OnCommandProcessed: TProcessCommandEvent read fOnCommandProcessed write fOnCommandProcessed;
    property OnReplaceText: TReplaceTextEvent read fOnReplaceText write fOnReplaceText;
//    property OnSpecialLineColors: TSpecialLineColorsEvent read fOnSpecialLineColors write SetSpecialLineColors;
//    deprecated;
    property OnSpecialLineMarkup: TSpecialLineMarkupEvent read FOnSpecialLineMarkup write SetSpecialLineMarkup;
    property OnStatusChange: TStatusChangeEvent read fOnStatusChange write fOnStatusChange;
  end;

  TSynEdit = class(TCustomSynEdit)
  published
    // inherited properties
    property Align;
    property Beautifier;
    property BlockIndent;
    property BlockTabIndent;
//    property BorderSpacing;
{$IFDEF SYN_COMPILER_4_UP}
    property Anchors;
    property Constraints;
{$ENDIF}
//    property Color;
    property Cursor default crIBeam;
    property Enabled;
//    property Font;
    property Height;
    property Name;
//    property ParentColor;
//    property ParentFont;
//    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
//    property TabStop default True;
    property Tag;
    property Visible;
    property Width;
    // inherited events
    property OnClick;
    property OnDblClick;
//    property OnTripleClick;
//    property OnQuadClick;
    property OnDragDrop;
    property OnDragOver;
{$IFDEF SYN_COMPILER_4_UP}
    // ToDo Docking
    property OnEndDock;
{$ENDIF}
//    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
//    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnClickLink: TMouseEvent read FOnClickLink write FOnClickLink;
    property OnMouseLink: TSynMouseLinkEvent read FOnMouseLink write FOnMouseLink;
    property OnMouseEnter;
    property OnMouseLeave;
{$IFDEF SYN_COMPILER_4_UP}
    // ToDo Docking
    property OnStartDock;
{$ENDIF}
//    property OnStartDrag;
    // TCustomSynEdit properties
    property BookMarkOptions;
//    property BorderStyle default bsSingle;
    property ExtraCharSpacing;
    property ExtraLineSpacing;
    property Gutter;
    property RightGutter;
    property HideSelection;
    property Highlighter;
    property InsertCaret;
    property InsertMode;
    property Keystrokes;
    property MouseActions;
    property MouseSelActions;
    property Lines;
    property MaxLeftChar;
    property MaxUndo;
    property Options;
    property Options2;
    property MouseOptions;
    property VisibleSpecialChars;
    property OverwriteCaret;
    property ReadOnly;
    property RightEdge;
    property RightEdgeColor;
//    property ScrollBars;
    property SelectedColor;
    property IncrementColor;
    property HighlightAllColor;
    property BracketHighlightStyle;
    property BracketMatchColor;
    property FoldedCodeColor;
    property MouseLinkColor;
    property LineHighlightColor;
    property DefaultSelectionMode;
    property SelectionMode;
    property TabWidth;
    property WantTabs;
    // TCustomSynEdit events
    property OnChange;
    property OnChangeUpdating;
    property OnCutCopy;
    property OnPaste;
    property OnClearBookmark; // djlp 2000-08-29
    property OnCommandProcessed;
    property OnDropFiles;
    property OnGutterClick;
    property OnPaint;
    property OnPlaceBookmark;
    property OnProcessCommand;
    property OnProcessUserCommand;
    property OnReplaceText;
//    property OnShowHint;
//    property OnSpecialLineColors; deprecated;
    property OnSpecialLineMarkup;
    property OnStatusChange;
  end;

procedure Register;

implementation

const
  GutterTextDist = 2; // Pixel

type

  { TSynEditMarkListInternal }

  TSynEditMarkListInternal = class(TSynEditMarkList)
  private
    function GetLinesView: TSynEditStrings;
    procedure SetLinesView(const AValue: TSynEditStrings);
  protected
    procedure AddOwnerEdit(AEdit: TSynEditBase);
    procedure RemoveOwnerEdit(AEdit: TSynEditBase);
    property LinesView: TSynEditStrings read GetLinesView write SetLinesView;
  end;

  TSynStatusChangedHandlerList = Class(TSynFilteredMethodList)
  public
    procedure Add(AHandler: TStatusChangeEvent; Changes: TSynStatusChanges);
    procedure Remove(AHandler: TStatusChangeEvent);
    procedure CallStatusChangedHandlers(Sender: TObject; Changes: TSynStatusChanges);
  end;

  { TSynEditUndoCaret }

  TSynEditUndoCaret = class(TSynEditUndoItem)
  private
    FCaretPos: TPoint;
  protected
    function IsEqualContent(AnItem: TSynEditUndoItem): boolean; override;
    function DebugString: String; override;
  public
    constructor Create(CaretPos: TPoint);
    function IsCaretInfo: boolean; override;
    function PerformUndo(Caller: TObject): boolean; override;
  end;

  { TSynEditUndoSelCaret }

  TSynEditUndoSelCaret = class(TSynEditUndoItem)
  private
    FCaretPos, FBeginPos, FEndPos: TPoint;
    FBlockMode: TSynSelectionMode;
  protected
    function IsEqualContent(AnItem: TSynEditUndoItem): boolean; override;
    function DebugString: String; override;
  public
    function IsCaretInfo: boolean; override;
    constructor Create(CaretPos, BeginPos, EndPos: TPoint; BlockMode: TSynSelectionMode);
    function PerformUndo(Caller: TObject): boolean; override;
  end;

  { TSynEditUndoIndent }

  TSynEditUndoIndent = class(TSynEditUndoItem)
  public
    FPosY1, FPosY2, FCnt, FTabCnt: Integer;
  public
    constructor Create(APosY, EPosY, ACnt, ATabCnt: Integer);
    function PerformUndo(Caller: TObject): boolean; override;
  end;

  { TSynEditUndoUnIndent }

  TSynEditUndoUnIndent = class(TSynEditUndoItem)
  public
    FPosY1, FPosY2: Integer;
    FText: String;
  public
    constructor Create(APosY, EPosY: Integer; AText: String);
    function PerformUndo(Caller: TObject): boolean; override;
  end;

  { TSynEditMouseGlobalActions }

  TSynEditMouseGlobalActions = class(TSynEditMouseInternalActions)
  protected
    procedure InitForOptions(AnOptions: TSynEditorMouseOptions); override;
  end;

  { TSynEditMouseTextActions }

  TSynEditMouseTextActions = class(TSynEditMouseInternalActions)
  protected
    procedure InitForOptions(AnOptions: TSynEditorMouseOptions); override;
  end;

  { TSynEditMouseSelActions }

  TSynEditMouseSelActions = class(TSynEditMouseInternalActions)
  protected
    procedure InitForOptions(AnOptions: TSynEditorMouseOptions); override;
  end;

  { THookedCommandHandlerEntry }

  THookedCommandHandlerEntry = class(TObject)
  private
    FEvent: THookedCommandEvent;
    FData: pointer;
    FFlags: THookedCommandFlags;
    function Equals(AEvent: THookedCommandEvent): boolean; reintroduce;
  public
    constructor Create(AEvent: THookedCommandEvent; AData: pointer; aFlags: THookedCommandFlags);
  end;

  { TSynEditUndoCaret }

function TSynEditUndoCaret.IsEqualContent(AnItem: TSynEditUndoItem): boolean;
begin
  Result := (FCaretPos.X = TSynEditUndoCaret(AnItem).FCaretPos.X) and
    (FCaretPos.Y = TSynEditUndoCaret(AnItem).FCaretPos.Y);
end;

function TSynEditUndoCaret.DebugString: String;
begin
  Result := 'CaretPos=' ;//+ IntToStr(FCaretPos);
end;

constructor TSynEditUndoCaret.Create(CaretPos: TPoint);
begin
  FCaretPos := CaretPos;
{$IFDEF SynUndoDebugItems}debugln(['---  Undo Insert ', DbgSName(self), ' ', dbgs(self), ' - ', DebugString]);
  {$ENDIF}
end;

function TSynEditUndoCaret.IsCaretInfo: boolean;
begin
  Result := True;
end;

function TSynEditUndoCaret.PerformUndo(Caller: TObject): boolean;
begin
  Result := Caller is TCustomSynEdit;
  if Result then
{$IFDEF SynUndoDebugItems}debugln(['---  Undo Perform ', DbgSName(self), ' ', dbgs(self), ' - ', DebugString]);
  {$ENDIF}
  with TCustomSynEdit(Caller) do
  begin
    FCaret.LineCharPos := FCaretPos;
    FTheLinesView.CurUndoList.AddChange(TSynEditUndoCaret.Create(FCaretPos));
  end;
end;

{ TSynEditUndoSelCaret }

constructor TSynEditUndoSelCaret.Create(CaretPos, BeginPos, EndPos: TPoint; BlockMode: TSynSelectionMode);
begin
  FCaretPos := CaretPos;
  FBeginPos := BeginPos;
  FEndPos := EndPos;
  FBlockMode := BlockMode;
{$IFDEF SynUndoDebugItems}debugln(['---  Undo Insert ', DbgSName(self), ' ', dbgs(self), ' - ', DebugString]);
  {$ENDIF}
end;

function TSynEditUndoSelCaret.IsEqualContent(AnItem: TSynEditUndoItem): boolean;
begin
  Result := (FCaretPos.X = TSynEditUndoSelCaret(AnItem).FCaretPos.X) and
    (FCaretPos.Y = TSynEditUndoSelCaret(AnItem).FCaretPos.Y) and
    (FBeginPos.X = TSynEditUndoSelCaret(AnItem).FBeginPos.X) and
    (FBeginPos.Y = TSynEditUndoSelCaret(AnItem).FBeginPos.Y) and (FEndPos.X = TSynEditUndoSelCaret(AnItem).FEndPos.X)
    and (FEndPos.Y = TSynEditUndoSelCaret(AnItem).FEndPos.Y) and (FBlockMode = TSynEditUndoSelCaret(AnItem).FBlockMode);
end;

function TSynEditUndoSelCaret.DebugString: String;
begin
  Result := 'CaretPos=';// + dbgs(FCaretPos) + ' Begin=' + dbgs(FBeginPos) + ' End=' + dbgs(FEndPos) + ' Mode=' + dbgs(ord(FBlockMode));
end;

function TSynEditUndoSelCaret.IsCaretInfo: boolean;
begin
  Result := True;
end;

function TSynEditUndoSelCaret.PerformUndo(Caller: TObject): boolean;
begin
  Result := Caller is TCustomSynEdit;
  if Result then
{$IFDEF SynUndoDebugItems}debugln(['---  Undo Perform ', DbgSName(self), ' ', dbgs(self), ' - ', DebugString]);
  {$ENDIF}
  with TCustomSynEdit(Caller) do
  begin
    SetCaretAndSelection(FCaretPos, FBeginPos, FEndPos, FBlockMode, True);
    FTheLinesView.CurUndoList.AddChange(TSynEditUndoSelCaret.Create(FCaretPos, FBeginPos, FEndPos, FBlockMode));
  end;
end;

{ TSynEditUndoIndent }

constructor TSynEditUndoIndent.Create(APosY, EPosY, ACnt, ATabCnt: Integer);
begin
  FPosY1 := APosY;
  FPosY2 := EPosY;
  FCnt := ACnt;
  FTabCnt := ATabCnt;
end;

function TSynEditUndoIndent.PerformUndo(Caller: TObject): boolean;
begin
  Result := False;
end;

{ TSynEditUndoUnIndent }

constructor TSynEditUndoUnIndent.Create(APosY, EPosY: Integer; AText: String);
begin
  FPosY1 := APosY;
  FPosY2 := EPosY;
  FText := AText;
end;

function TSynEditUndoUnIndent.PerformUndo(Caller: TObject): boolean;
begin
  Result := False;
end;

function Roundoff(X: Extended): Longint;
begin
  if (X >= 0) then
  begin
    Result := Trunc(X + 0.5)
  end
  else
  begin
    Result := Trunc(X - 0.5);
  end;
end;

{ TSynEditMouseGlobalActions }

procedure TSynEditMouseGlobalActions.InitForOptions(AnOptions: TSynEditorMouseOptions);
begin
  AddCommand(emcWheelScrollDown, False, mbXWheelDown, ccAny, cdDown, [], []);
  AddCommand(emcWheelScrollUp, False, mbXWheelUp, ccAny, cdDown, [], []);

  if emCtrlWheelZoom in AnOptions then
  begin
    AddCommand(emcWheelZoomOut, False, mbXWheelDown, ccAny, cdDown, [ssCtrl], [ssCtrl]);
    AddCommand(emcWheelZoomIn, False, mbXWheelUp, ccAny, cdDown, [ssCtrl], [ssCtrl]);
  end;
end;

{ TSynEditMouseTextActions }

procedure TSynEditMouseTextActions.InitForOptions(AnOptions: TSynEditorMouseOptions);
var
  rmc: boolean;
begin
  Clear;
  rmc := (emRightMouseMovesCursor in AnOptions);
  /// / eoRightMouseMovesCursor
  // if (eoRightMouseMovesCursor in ChangedOptions) then begin
  // for i := FMouseActions.Count-1 downto 0 do
  // if FMouseActions[i].Button = mbXRight then
  // FMouseActions[i].MoveCaret := (eoRightMouseMovesCursor in fOptions);
  // end;

  AddCommand(emcStartSelections, True, mbXLeft, ccSingle, cdDown, [], [ssShift, ssAlt], emcoSelectionStart);
  AddCommand(emcStartSelections, True, mbXLeft, ccSingle, cdDown, [ssShift], [ssShift, ssAlt], emcoSelectionContinue);
  if (emAltSetsColumnMode in AnOptions) then
  begin
    AddCommand(emcStartColumnSelections, True, mbXLeft, ccSingle, cdDown, [ssAlt], [ssShift, ssAlt],
      emcoSelectionStart);
    AddCommand(emcStartColumnSelections, True, mbXLeft, ccSingle, cdDown, [ssShift, ssAlt], [ssShift, ssAlt],
      emcoSelectionContinue);
  end;
  if (emShowCtrlMouseLinks in AnOptions) then
    AddCommand(emcMouseLink, False, mbXLeft, ccSingle, cdUp, [SYNEDIT_LINK_MODIFIER], [ssShift, ssAlt, ssCtrl]);

  if (emDoubleClickSelectsLine in AnOptions) then
  begin
    AddCommand(emcSelectLine, True, mbXLeft, ccDouble, cdDown, [], []);
    AddCommand(emcSelectPara, True, mbXLeft, ccTriple, cdDown, [], []);
  end
  else
  begin
    AddCommand(emcSelectWord, True, mbXLeft, ccDouble, cdDown, [], []);
    AddCommand(emcSelectLine, True, mbXLeft, ccTriple, cdDown, [], []);
  end;
  AddCommand(emcSelectPara, True, mbXLeft, ccQuad, cdDown, [], []);

  AddCommand(emcContextMenu, rmc, mbXRight, ccSingle, cdUp, [], [], emcoSelectionCaretMoveNever);

  AddCommand(emcPasteSelection, True, mbXMiddle, ccSingle, cdDown, [], []);
end;

{ TSynEditMouseSelActions }

procedure TSynEditMouseSelActions.InitForOptions(AnOptions: TSynEditorMouseOptions);
begin
  Clear;
  // rmc := (eoRightMouseMovesCursor in AnOptions);

  if (emDragDropEditing in AnOptions) then
    AddCommand(emcStartDragMove, False, mbXLeft, ccSingle, cdDown, [], []);
end;

{ THookedCommandHandlerEntry }

constructor THookedCommandHandlerEntry.Create(AEvent: THookedCommandEvent; AData: pointer; aFlags: THookedCommandFlags);
begin
  inherited Create;
  FEvent := AEvent;
  FData := AData;
  FFlags := aFlags;
end;

function THookedCommandHandlerEntry.Equals(AEvent: THookedCommandEvent): boolean;
begin
  with TMethod(FEvent) do
    Result := (Code = TMethod(AEvent).Code) and (Data = TMethod(AEvent).Data);
end;

procedure InitSynDefaultFont;
begin
  if SynDefaultFontName <> '' then
    exit;
//  Screen.Fonts;
{$UNDEF SynDefaultFont}
{$IFDEF LCLgtk}
  SynDefaultFontName := '-adobe-courier-medium-r-normal-*-*-140-*-*-*-*-iso10646-1';
  SynDefaultFontHeight := 14;
{$DEFINE SynDefaultFont}
{$ENDIF}
{$IFDEF LCLcarbon}
  SynDefaultFontName := 'Monaco'; // Note: carbon is case sensitive
  SynDefaultFontHeight := 12;
{$DEFINE SynDefaultFont}
{$ENDIF}
  // LCLgtk2 and LCLQt use default settings
{$IFNDEF SynDefaultFont}
  SynDefaultFontName := 'Courier New';
  SynDefaultFontHeight := -13;
{$ENDIF}
//  if Screen.Fonts.IndexOf(SynDefaultFontName) >= 0 then
//    exit;
//  if Screen.Fonts.IndexOf('DejaVu Sans Mono') >= 0 then
//  begin
//    SynDefaultFontName := 'DejaVu Sans Mono';
//    SynDefaultFontHeight := 13;
//  end;
end;

{ TCustomSynEdit }

procedure TCustomSynEdit.AquirePrimarySelection;
//var
//  FormatList: Array [0 .. 1] of TClipboardFormat;
begin
//  if (not SelAvail) or (PrimarySelection.OnRequest = @PrimarySelectionRequest) then
//    exit;
//  FormatList[0] := CF_TEXT;
//  FormatList[1] := TSynClipboardStream.ClipboardFormatId;
//  try
//    PrimarySelection.SetSupportedFormats(2, @FormatList[0]);
//    PrimarySelection.OnRequest := @PrimarySelectionRequest;
//  except
//  end;
end;

function TCustomSynEdit.GetChangeStamp: Int64;
begin
  Result := TSynEditStringList(FLines).TextChangeStamp;
end;

function TCustomSynEdit.GetCharsInWindow: Integer;
begin
  Result := FTextArea.CharsInWindow;
end;

function TCustomSynEdit.GetCharWidth: Integer;
begin
  Result := FTextArea.CharWidth;
end;

function TCustomSynEdit.GetDefSelectionMode: TSynSelectionMode;
begin
  Result := FBlockSelection.SelectionMode;
end;

function TCustomSynEdit.GetFoldState: String;
begin
  Result := FFoldedLinesView.GetFoldDescription(0, 0, -1, -1, True);
end;

function TCustomSynEdit.GetLeftChar: Integer;
begin
  Result := FTextArea.LeftChar;
end;

function TCustomSynEdit.GetLineHeight: Integer;
begin
  Result := FTextArea.LineHeight;
end;

function TCustomSynEdit.GetLinesInWindow: Integer;
begin
  Result := FTextArea.LinesInWindow;
end;

function TCustomSynEdit.GetModified: boolean;
begin
  Result := TSynEditStringList(FLines).Modified;
end;

function TCustomSynEdit.GetMouseActions: TSynEditMouseActions;
begin
  Result := FMouseActions.UserActions;
end;

function TCustomSynEdit.GetMouseSelActions: TSynEditMouseActions;
begin
  Result := FMouseSelActions.UserActions;
end;

function TCustomSynEdit.GetMouseTextActions: TSynEditMouseActions;
begin
  Result := FMouseTextActions.UserActions;
end;

function TCustomSynEdit.GetPaintLockOwner: TSynEditBase;
begin
  Result := TSynEditStringList(FLines).PaintLockOwner;
end;

function TCustomSynEdit.GetPlugin(Index: Integer): TLazSynEditPlugin;
begin
  Result := TLazSynEditPlugin(FPlugins[Index]);
end;

function TCustomSynEdit.GetRightEdge: Integer;
begin
  Result := FTextArea.RightEdgeColumn;
end;

function TCustomSynEdit.GetRightEdgeColor: TColor;
begin
  Result := FTextArea.RightEdgeColor;
end;

function TCustomSynEdit.GetTextBetweenPoints(aStartPoint, aEndPoint: TPoint): String;
begin
  FInternalBlockSelection.SelectionMode := smNormal;
  FInternalBlockSelection.StartLineBytePos := aStartPoint;
  FInternalBlockSelection.EndLineBytePos := aEndPoint;
  Result := FInternalBlockSelection.SelText;
end;

function TCustomSynEdit.GetTopLine: Integer;
begin
  Result := FFoldedLinesView.ViewPosToTextIndex(FTextArea.TopLine) + 1;
end;

procedure TCustomSynEdit.SetBlockTabIndent(AValue: Integer);
begin
  if FBlockTabIndent = AValue then
    exit;
  FBlockTabIndent := AValue;
end;

procedure TCustomSynEdit.SetBracketMatchColor(AValue: TSynSelectedColor);
begin
  fMarkupBracket.MarkupInfo.Assign(AValue);
end;

procedure TCustomSynEdit.SetDefSelectionMode(const AValue: TSynSelectionMode);
begin
  FBlockSelection.SelectionMode := AValue; // Includes active
end;

procedure TCustomSynEdit.SetFoldedCodeColor(AValue: TSynSelectedColor);
begin
  FFoldedLinesView.MarkupInfoFoldedCode.Assign(AValue);
end;

procedure TCustomSynEdit.SurrenderPrimarySelection;
begin
//  if PrimarySelection.OnRequest = @PrimarySelectionRequest then
//    PrimarySelection.OnRequest := nil;
end;

function TCustomSynEdit.PixelsToRowColumn(Pixels: TPoint;
  aFlags: TSynCoordinateMappingFlags = [scmLimitToLines]): TPoint;
// converts the client area coordinate
// to Caret position (physical position, (1,1) based)
// To get the text/logical position use PixelsToLogicalPos
begin
  Result := FTextArea.PixelsToRowColumn(Pixels, aFlags);
  Result := Point(Result.X, ScreenRowToRow(Result.Y, scmLimitToLines in aFlags));
end;

function TCustomSynEdit.PixelsToLogicalPos(const Pixels: TPoint): TPoint;
begin
  Result := PhysicalToLogicalPos(PixelsToRowColumn(Pixels));
end;

function TCustomSynEdit.ScreenRowToRow(ScreenRow: Integer; LimitToLines: boolean = True): Integer;
// ScreenRow is 0-base
// result is 1-based
begin
  Result := FFoldedLinesView.ScreenLineToTextIndex(ScreenRow) + 1;
  if LimitToLines and (Result >= Lines.Count) then
    Result := Lines.Count;
  // DebugLn(['=== SrceenRow TO Row   In:',ScreenRow,'  out:',Result, ' topline=',TopLine, '  view topline=',FFoldedLinesView.TopLine]);
end;

function TCustomSynEdit.RowToScreenRow(PhysicalRow: Integer): Integer;
// returns -1 for lines above visible screen (<TopLine)
// 0 for the first line
// 0 to LinesInWindow for visible lines (incl last partial visble line)
// and returns LinesInWindow+1 for lines below visible screen
begin
  Result := FFoldedLinesView.TextIndexToScreenLine(PhysicalRow - 1);
  if Result < -1 then
    Result := -1;
  if Result > LinesInWindow + 1 then
    Result := LinesInWindow + 1;
  // DebugLn(['=== Row TO ScreenRow   In:',PhysicalRow,'  out:',Result]);
end;

function TCustomSynEdit.RowColumnToPixels(RowCol: TPoint): TPoint;
// converts screen position (1,1) based
// to client area coordinate (0,0 based on canvas)
begin
  RowCol.Y := RowToScreenRow(RowCol.Y);
  Result := FTextArea.RowColumnToPixels(RowCol);
end;

procedure TCustomSynEdit.ComputeCaret(X, Y: Single);
// set caret to pixel position
begin
  FCaret.LineCharPos := PixelsToRowColumn(Point(Trunc(X), Trunc(Y)));
end;

procedure TCustomSynEdit.DoCopyToClipboard(SText: string; FoldInfo: String = '');
//var
//  ClipHelper: TSynClipboardStream;
//  PasteAction: TSynCopyPasteAction;
//  PMode: TSynSelectionMode;
begin
//  PasteAction := scaContinue;
//  if length(FoldInfo) = 0 then
//    PasteAction := scaPlainText;
//  PMode := SelectionMode;
//  if assigned(FOnCutCopy) then
//  begin
//    FOnCutCopy(self, SText, PMode, FBlockSelection.FirstLineBytePos, PasteAction);
//    if PasteAction = scaAbort then
//      exit;;
//  end;
//
//  if SText = '' then
//    exit;
//  Clipboard.Clear;
//  ClipHelper := TSynClipboardStream.Create;
//  try
//    ClipHelper.Text := SText;
//    ClipHelper.SelectionMode := PMode; // TODO if scaPlainText and smNormal, then avoid synedits own clipboard format
//
//    if PasteAction = scaContinue then
//    begin
//      // Fold
//      if length(FoldInfo) > 0 then
//        ClipHelper.AddTag(synClipTagFold, @FoldInfo[1], length(FoldInfo));
//    end;
//
//    if not ClipHelper.WriteToClipboard(Clipboard) then
//    begin
//{$IFDEF SynClipboardExceptions}raise ESynEditError.Create('Clipboard copy operation failed'); {$ENDIF}
//    end;
//  finally
//    ClipHelper.Free;
//  end;
end;

procedure TCustomSynEdit.CopyToClipboard;
//var
//  FInfo: String;
begin
//  if SelAvail then
//  begin
//    if eoFoldedCopyPaste in FOptions2 then
//      FInfo := FFoldedLinesView.GetFoldDescription(FBlockSelection.FirstLineBytePos.Y - 1,
//        FBlockSelection.FirstLineBytePos.X, FBlockSelection.LastLineBytePos.Y - 1, FBlockSelection.LastLineBytePos.X);
//    DoCopyToClipboard(SelText, FInfo);
//  end;
end;

procedure TCustomSynEdit.CutToClipboard;
//var
//  FInfo: String;
begin
//  if SelAvail then
//  begin
//    if eoFoldedCopyPaste in FOptions2 then
//      FInfo := FFoldedLinesView.GetFoldDescription(FBlockSelection.FirstLineBytePos.Y - 1,
//        FBlockSelection.FirstLineBytePos.X, FBlockSelection.LastLineBytePos.Y - 1, FBlockSelection.LastLineBytePos.X);
//    DoCopyToClipboard(SelText, FInfo);
//    SetSelTextExternal('');
//  end;
end;

constructor TCustomSynEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  SetInline(True);
//  ControlStyle := ControlStyle + [csOwnedChildrenNotSelectable];
  FScrollBarUpdateLock := 0;
  FPaintLock := 0;
  FUndoBlockAtPaintLock := 0;

  FStatusChangedList := TSynStatusChangedHandlerList.Create;

  FDefaultBeautifier := TSynBeautifier.Create(self);
  FBeautifier := FDefaultBeautifier;

  FLines := TSynEditStringList.Create;
  TSynEditStringList(FLines).AttachSynEdit(self);

  FCaret := TSynEditCaret.Create;
  FCaret.MaxLeftChar := CurrentMaxLineLen;
  FCaret.AddChangeHandler({$IFDEF FPC}@{$ENDIF}CaretChanged);
  FInternalCaret := TSynEditCaret.Create;
  FInternalCaret.MaxLeftChar := CurrentMaxLineLen;

  // Create the lines/views
  FTrimmedLinesView := TSynEditStringTrimmingList.Create(FLines, FCaret);

  FDoubleWidthChrLinesView := SynEditStringDoubleWidthChars.Create(FTrimmedLinesView);

  // ftab, currently has LengthOfLongestLine, therefore must be after DoubleWidthChar
  FTabbedLinesView := TSynEditStringTabExpander.Create(FDoubleWidthChrLinesView);

  // Pointer to the First/Lowest View
  // TODO: this should be Folded...
  FTheLinesView := FTabbedLinesView;
  FTopLinesView := FTrimmedLinesView;

  FFoldedLinesView := TSynEditFoldedView.Create(FTheLinesView, FCaret);
  FFoldedLinesView.OnFoldChanged := {$IFDEF FPC}@{$ENDIF}FoldChanged;
  FFoldedLinesView.OnLineInvalidate := {$IFDEF FPC}@{$ENDIF}InvalidateGutterLines;
  FFoldedLinesView.DisplayView.NextView := FTheLinesView.DisplayView;

  FDisplayView := FFoldedLinesView.DisplayView;

  // External Accessor
  FStrings := TSynEditLines.Create(TSynEditStringList(FLines), {$IFDEF FPC}@{$ENDIF}MarkTextAsSaved);

  FCaret.Lines := FTheLinesView;
  FInternalCaret.Lines := FTheLinesView;
  fFontDummy := TFont.Create;
  FOldWidth := -1;
  FOldHeight := -1;

  with TSynEditStringList(FLines) do
  begin
    AddChangeHandler(senrLineCount, {$IFDEF FPC}@{$ENDIF}LineCountChanged);
    AddChangeHandler(senrLineChange, {$IFDEF FPC}@{$ENDIF}LineTextChanged);
    AddChangeHandler(senrHighlightChanged, {$IFDEF FPC}@{$ENDIF}DoHighlightChanged);
    AddNotifyHandler(senrCleared, {$IFDEF FPC}@{$ENDIF}ListCleared);
    AddNotifyHandler(senrUndoRedoAdded, {$IFDEF FPC}@{$ENDIF}self.UndoRedoAdded);
    AddNotifyHandler(senrModifiedChanged, {$IFDEF FPC}@{$ENDIF}ModifiedChanged);
    AddNotifyHandler(senrIncPaintLock, {$IFDEF FPC}@{$ENDIF}DoIncPaintLock);
    AddNotifyHandler(senrDecPaintLock, {$IFDEF FPC}@{$ENDIF}DoDecPaintLock);
    AddNotifyHandler(senrIncOwnedPaintLock, {$IFDEF FPC}@{$ENDIF}DoIncForeignPaintLock);
    AddNotifyHandler(senrDecOwnedPaintLock, {$IFDEF FPC}@{$ENDIF}DoDecForeignPaintLock);
  end;

  FScreenCaret := TSynEditScreenCaret.Create(self);
  FScreenCaret.OnExtraLineCharsChanged := {$IFDEF FPC}@{$ENDIF}ExtraLineCharsChanged;

  fUndoList := TSynEditStringList(FLines).UndoList;
  fRedoList := TSynEditStringList(FLines).RedoList;
  fUndoList.OnNeedCaretUndo := {$IFDEF FPC}@{$ENDIF}GetCaretUndo;
{$IFDEF SynUndoDebugCalls}
  fUndoList.DebugName := 'UNDO';
  fRedoList.DebugName := 'REDO';
{$ENDIF}
  FBlockSelection := TSynEditSelection.Create(FTheLinesView, True);
  FBlockSelection.Caret := FCaret;
  FBlockSelection.InvalidateLinesMethod := {$IFDEF FPC}@{$ENDIF}InvalidateLines;
  FBlockSelection.AddChangeHandler({$IFDEF FPC}@{$ENDIF}DoBlockSelectionChanged);

  FInternalBlockSelection := TSynEditSelection.Create(FTheLinesView, False);
  FInternalBlockSelection.InvalidateLinesMethod := {$IFDEF FPC}@{$ENDIF}InvalidateLines;
  // No need for caret, on interanl block

  FFoldedLinesView.BlockSelection := FBlockSelection;

  FWordBreaker := TSynWordBreaker.Create;

  RecreateMarkList;

{$IFNDEF EnableDoubleBuf}
//  DoubleBuffered := True;
{$ENDIF}
  fTextDrawer := TheTextDrawer.Create([TFontStyle.fsBold], fFontDummy);
  FPaintLineColor := TSynSelectedColor.Create;
  FPaintLineColor2 := TSynSelectedColor.Create;
  fBookMarkOpt := TSynBookMarkOpt.Create(self);
  fBookMarkOpt.OnChange := {$IFDEF FPC}@{$ENDIF}BookMarkOptionsChanged;

  FLeftGutter := CreateGutter(self, gsLeft, fTextDrawer);
  FLeftGutter.RegisterChangeHandler({$IFDEF FPC}@{$ENDIF}GutterChanged);
  FLeftGutter.RegisterResizeHandler({$IFDEF FPC}@{$ENDIF}GutterResized);
  FRightGutter := CreateGutter(self, gsRight, fTextDrawer);
  FRightGutter.RegisterChangeHandler({$IFDEF FPC}@{$ENDIF}GutterChanged);
  FRightGutter.RegisterResizeHandler({$IFDEF FPC}@{$ENDIF}GutterResized);

//  ControlStyle := ControlStyle + [csOpaque, csSetCaption, csTripleClicks, csQuadClicks];
  Height := 150;
  Width := 200;
  Cursor := crIBeam;
  FPlugins := TList.Create;
  FHookedKeyTranslationList := TSynHookedKeyTranslationList.Create;
  // needed before setting color
  fMarkupHighCaret := TSynEditMarkupHighlightAllCaret.Create(self);
  fMarkupHighCaret.Selection := FBlockSelection;
  fMarkupHighAll := TSynEditMarkupHighlightAll.Create(self);
  fMarkupBracket := TSynEditMarkupBracket.Create(self);
  fMarkupWordGroup := TSynEditMarkupWordGroup.Create(self);
  fMarkupCtrlMouse := TSynEditMarkupCtrlMouseLink.Create(self);
  fMarkupSpecialLine := TSynEditMarkupSpecialLine.Create(self);
  fMarkupSelection := TSynEditMarkupSelection.Create(self, FBlockSelection);
  fMarkupSpecialChar := TSynEditMarkupSpecialChar.Create(self);

  fMarkupManager := TSynEditMarkupManager.Create(self);
  fMarkupManager.AddMarkUp(fMarkupSpecialChar);
  fMarkupManager.AddMarkUp(fMarkupSpecialLine);
  fMarkupManager.AddMarkUp(fMarkupHighCaret);
  fMarkupManager.AddMarkUp(fMarkupHighAll);
  fMarkupManager.AddMarkUp(fMarkupCtrlMouse);
  fMarkupManager.AddMarkUp(fMarkupBracket);
  fMarkupManager.AddMarkUp(fMarkupWordGroup);
  fMarkupManager.AddMarkUp(fMarkupSelection);
  fMarkupManager.Lines := FTheLinesView;
  fMarkupManager.Caret := FCaret;
  fMarkupManager.InvalidateLinesMethod := InvalidateLines;

  fFontDummy.Family := SynDefaultFontName;

  fFontDummy.Size := SynDefaultFontHeight;
//  fFontDummy.Pitch := SynDefaultFontPitch;
//  fFontDummy.Quality := SynDefaultFontQuality;

  FLastSetFontSize := fFontDummy.Size;
  fLastMouseCaret := Point(-1, -1);
  FLastMousePoint := Point(-1, -1);
  FBlockIndent := 2;

  FTextArea := TLazSynTextArea.Create(self, fTextDrawer);
  FTextArea.RightEdgeVisible := not(eoHideRightMargin in SYNEDIT_DEFAULT_OPTIONS);
  FTextArea.ExtraCharSpacing := 0;
  FTextArea.ExtraLineSpacing := 0;
  FTextArea.MarkupManager := fMarkupManager;
  FTextArea.TheLinesView := FTheLinesView;
  FTextArea.Highlighter := nil;

  FLeftGutterArea := TLazSynGutterArea.Create(self);
  FLeftGutterArea.TextArea := FTextArea;
  FLeftGutterArea.Gutter := FLeftGutter;

  FRightGutterArea := TLazSynGutterArea.Create(self);
  FRightGutterArea.TextArea := FTextArea;
  FRightGutterArea.Gutter := FRightGutter;

  FPaintArea := TLazSynSurfaceManager.Create(self);
  FPaintArea.TextArea := FTextArea;
  FPaintArea.LeftGutterArea := FLeftGutterArea;
  FPaintArea.RightGutterArea := FRightGutterArea;
  FPaintArea.DisplayView := FDisplayView;

//  Color := clWhite;
//  Font.Assign(fFontDummy);
//  Font.OnChange := {$IFDEF FPC}@{$ENDIF}FontChanged;
//  FontChanged(nil);
//  ParentFont := False;
//  ParentColor := False;
//  TabStop := True;
  fInserting := True;
  fMaxLeftChar := 1024;
//  ScrollBars := ssBoth;
//  BorderStyle := bsSingle;
  fInsertCaret := ctVerticalLine;
  fOverwriteCaret := ctBlock;
  FKeyStrokes := TSynEditKeyStrokes.Create(self);
  FCurrentComboKeyStrokes := nil;
  if assigned(Owner) and not(csLoading in Owner.ComponentState) then
  begin
    SetDefaultKeystrokes;
  end;

  FMouseActions := TSynEditMouseGlobalActions.Create(self);
  FMouseSelActions := TSynEditMouseSelActions.Create(self);
  FMouseTextActions := TSynEditMouseTextActions.Create(self);
  FMouseActionSearchHandlerList := TSynEditMouseActionSearchList.Create;
  FMouseActionExecHandlerList := TSynEditMouseActionExecList.Create;

{$IFDEF SYN_MBCSSUPPORT}
  fImeCount := 0;
  fMBCSStepAside := False;
{$ENDIF}
  fWantTabs := False;
  fTabWidth := 8;
  FOldTopView := 1;
  FFoldedLinesView.TopLine := 1;
  // find / replace
  fTSearch := TSynEditSearch.Create;
  FOptions := SYNEDIT_DEFAULT_OPTIONS;
  FOptions2 := SYNEDIT_DEFAULT_OPTIONS2;
  FMouseOptions := SYNEDIT_DEFAULT_MOUSE_OPTIONS;
  FShareOptions := SYNEDIT_DEFAULT_SHARE_OPTIONS;
  FVisibleSpecialChars := SYNEDIT_DEFAULT_VISIBLESPECIALCHARS;
  fMarkupSpecialChar.VisibleSpecialChars := SYNEDIT_DEFAULT_VISIBLESPECIALCHARS;
  UpdateOptions;
  UpdateOptions2;
  UpdateMouseOptions;
  fScrollTimer := TTimer.Create(self);
  fScrollTimer.Enabled := False;
  fScrollTimer.Interval := 100;
  fScrollTimer.OnTimer := {$IFDEF FPC}@{$ENDIF}ScrollTimerHandler;

  // Accessibility
//  AccessibleRole := larTextEditorMultiline;
//  AccessibleValue := self.Text;
//  AccessibleDescription := 'source code editor';
end;

function TCustomSynEdit.GetChildOwner: TComponent;
begin
  Result := self;
end;

procedure TCustomSynEdit.GetChildren(Proc: TGetChildProc; Root: TComponent);
begin
  if Root = self then
  begin
    Proc(FLeftGutter.Parts);
    // only save right gutter, if it has gutter-parts
    // move to parts-class
    if FRightGutter.Parts.Count > 0 then
      Proc(FRightGutter.Parts);
  end;
end;

(*procedure TCustomSynEdit.CreateParams(var Params: TCreateParams);
  const
  ScrollBar: array[TScrollStyle] of DWORD = (0, WS_HSCROLL, WS_VSCROLL,
  WS_HSCROLL or WS_VSCROLL, WS_HSCROLL, WS_VSCROLL, WS_HSCROLL or WS_VSCROLL);
  BorderStyles: array[TBorderStyle] of DWORD = (0, WS_BORDER);
  ClassStylesOff = CS_VREDRAW or CS_HREDRAW;
begin
  inherited CreateParams(Params);

    with Params do begin
    {$IFOPT R+}{$DEFINE RangeCheckOn}{$R-}{$ENDIF}
    WindowClass.Style := WindowClass.Style and not Cardinal(ClassStylesOff);
    Style := Style or ScrollBar[FScrollBars] or BorderStyles[BorderStyle]
    or WS_CLIPCHILDREN;
    {$IFDEF RangeCheckOn}{$R+}{$ENDIF}
    if NewStyleControls {$IFNDEF SYN_LAZARUS}and Ctl3D{$ENDIF} and (BorderStyle = bsSingle) then begin
    Style := Style and not Cardinal(WS_BORDER);
    ExStyle := ExStyle or WS_EX_CLIENTEDGE;
    end;
    end;
end;
        *)
procedure TCustomSynEdit.IncPaintLock;
begin
  if FIsInDecPaintLock then
    exit;
  if (PaintLockOwner = nil) then
  begin
    PaintLockOwner := self;
    FLines.SendNotification(senrIncOwnedPaintLock, self); // DoIncForeignPaintLock
  end;
  inc(FPaintLockOwnerCnt);
  if FPaintLockOwnerCnt = 1 then
    FLines.BeginUpdate(self);
end;

procedure TCustomSynEdit.DecPaintLock;
begin
  if FIsInDecPaintLock then
    exit;
  if FPaintLockOwnerCnt = 1 then
    FLines.EndUpdate(self);
  dec(FPaintLockOwnerCnt);
  if (PaintLockOwner = self) and (FPaintLockOwnerCnt = 0) then
  begin
    FLines.SendNotification(senrDecOwnedPaintLock, self); // DoDecForeignPaintLock
    PaintLockOwner := nil;
  end;
end;

procedure TCustomSynEdit.DoIncForeignPaintLock(Sender: TObject);
begin
  if Sender = self then
    exit;
  FCaret.IncAutoMoveOnEdit;
  FBlockSelection.IncPersistentLock;
end;

procedure TCustomSynEdit.DoDecForeignPaintLock(Sender: TObject);
begin
  if Sender = self then
    exit;
  FBlockSelection.DecPersistentLock;
  FCaret.DecAutoMoveOnEdit;
end;

procedure TCustomSynEdit.SetUpdateState(NewUpdating: boolean; Sender: TObject);
begin
  if assigned(FOnChangeUpdating) then
    FOnChangeUpdating(self, NewUpdating);
end;

procedure TCustomSynEdit.DoIncPaintLock(Sender: TObject);
begin
  if FIsInDecPaintLock then
    exit;
  if FPaintLock = 0 then
  begin
    SetUpdateState(True, self);
    FInvalidateRect := Rect(-1, -1, -2, -2);
    FOldTopView := TopView;
    FLastTextChangeStamp := TSynEditStringList(FLines).TextChangeStamp;
  end;
  inc(FPaintLock);
  fMarkupManager.IncPaintLock;
  FFoldedLinesView.Lock; // DecPaintLock triggers ScanFrom, and folds must wait
  FTrimmedLinesView.Lock; // Lock before caret
  FBlockSelection.Lock;
  FCaret.Lock;
  FScreenCaret.Lock;
end;

procedure TCustomSynEdit.DoDecPaintLock(Sender: TObject);
begin
  if FIsInDecPaintLock then
    exit;
  FIsInDecPaintLock := True;
  try
    if (FUndoBlockAtPaintLock >= FPaintLock) then
    begin
      if (FUndoBlockAtPaintLock > FPaintLock) then
//        debugln(['***** SYNEDIT: Fixing auto-undo-block FUndoBlockAtPaintLock=', FUndoBlockAtPaintLock, ' FPaintLock=', FPaintLock]);
      FUndoBlockAtPaintLock := 0;
      EndUndoBlock{$IFDEF SynUndoDebugBeginEnd}('TCustomSynEdit.DoDecPaintLock'){$ENDIF};
    end;
    if (FPaintLock = 1) {and HandleAllocated }then
    begin
      ScanRanges(FLastTextChangeStamp <> TSynEditStringList(FLines).TextChangeStamp);
      if sfAfterLoadFromFileNeeded in fStateFlags then
        AfterLoadFromFile;
      if FChangedLinesStart > 0 then
      begin
        InvalidateLines(FChangedLinesStart, FChangedLinesEnd);
        InvalidateGutterLines(FChangedLinesStart, FChangedLinesEnd);
      end;
      FChangedLinesStart := 0;
      FChangedLinesEnd := 0;
    end;
    FCaret.Unlock; // Maybe after FFoldedLinesView
    FBlockSelection.Unlock;
    FTrimmedLinesView.Unlock; // Must be unlocked after caret // May Change lines
    FFoldedLinesView.Unlock; // after ScanFrom, but before UpdateCaret
    fMarkupManager.DecPaintLock;
    dec(FPaintLock);
    if (FPaintLock = 0) {and HandleAllocated }then
    begin
      ScrollAfterTopLineChanged;
      if sfScrollbarChanged in fStateFlags then
        UpdateScrollBars;
      // must be past UpdateScrollbars; but before UpdateCaret (for ScrollBar-Auto-show)
      if sfEnsureCursorPos in fStateFlags then
        EnsureCursorPosVisible; // TODO: This may call SetTopLine, change order
      // This does Paintlock, should be before final decrease
      // Must be after EnsureCursorPosVisible (as it does MoveCaretToVisibleArea)
      if FCaret.LinePos > FLines.Count then
        FCaret.LinePos := FLines.Count;
      if sfCaretChanged in fStateFlags then
        UpdateCaret;
      // if sfScrollbarChanged in fStateFlags then
      // UpdateScrollbars;
      fMarkupHighCaret.CheckState; // Todo: need a global lock, including the markup
      // Todo: Markup can do invalidation, should be before ScrollAfterTopLineChanged;
    end;
    if (FPaintLock = 0) then
    begin
      FBlockSelection.AutoExtend := False;
      if fStatusChanges <> [] then
        DoOnStatusChange(fStatusChanges);
    end;
  finally
    FScreenCaret.Unlock;
    FIsInDecPaintLock := False;
    if FPaintLock = 0 then
    begin
      SetUpdateState(False, self);
      if FInvalidateRect.Bottom > FInvalidateRect.Top then
      begin
//        InvalidateRect(Handle, @FInvalidateRect, False);
{$IFDEF SynCheckPaintLock}
        debugln('Returning from Paintlock, wich had Paint called while active');
        DumpStack;
{$ENDIF}
      end;
      if sfSelChanged in fStateFlags then
        SelAvailChange(nil);
    end;
  end;
end;

destructor TCustomSynEdit.Destroy;
var
  i: Integer;
  p: TList;
begin
//  Application.RemoveOnIdleHandler(@IdleScanRanges);
  SurrenderPrimarySelection;
  Highlighter := nil;
  Beautifier := nil;
  FFoldedLinesView.BlockSelection := nil;

  if FPlugins <> nil then
  begin
    p := FPlugins;
    FPlugins := nil;
    for i := p.Count - 1 downto 0 do
      TLazSynEditPlugin(p[i]).DoEditorDestroyed(self);
    p.Free;
  end;

  // free listeners while other fields are still valid
  if assigned(fHookedCommandHandlers) then
  begin
    for i := 0 to fHookedCommandHandlers.Count - 1 do
      THookedCommandHandlerEntry(fHookedCommandHandlers[i]).Free;
    FreeAndNil(fHookedCommandHandlers);
  end;

  RemoveHandlers;
  FLeftGutter.UnRegisterChangeHandler({$IFDEF FPC}@{$ENDIF}GutterChanged);
  FLeftGutter.UnRegisterResizeHandler({$IFDEF FPC}@{$ENDIF}GutterResized);
  FRightGutter.UnRegisterChangeHandler({$IFDEF FPC}@{$ENDIF}GutterChanged);
  FRightGutter.UnRegisterResizeHandler({$IFDEF FPC}@{$ENDIF}GutterResized);

  FreeAndNil(FHookedKeyTranslationList);
  fHookedCommandHandlers := nil;
  FPlugins := nil;
  FCaret.Lines := nil;
  FInternalCaret.Lines := nil;
  FMarkList.UnRegisterChangeHandler({$IFDEF FPC}@{$ENDIF}MarkListChange);
  FreeAndNil(FPaintArea);
  FreeAndNil(FLeftGutterArea);
  FreeAndNil(FRightGutterArea);
  FreeAndNil(FTextArea);
  FreeAndNil(fTSearch);
  FreeAndNil(fMarkupManager);
  FreeAndNil(fBookMarkOpt);
  FreeAndNil(FKeyStrokes);
  FreeAndNil(FMouseActionSearchHandlerList);
  FreeAndNil(FMouseActionExecHandlerList);
  FreeAndNil(FMouseActions);
  FreeAndNil(FMouseSelActions);
  FreeAndNil(FMouseTextActions);
  FreeAndNil(FLeftGutter);
  FreeAndNil(FRightGutter);
  FreeAndNil(FPaintLineColor);
  FreeAndNil(FPaintLineColor2);
  FreeAndNil(fTextDrawer);
  FreeAndNil(fFontDummy);
  DestroyMarkList; // before detach from FLines
  FreeAndNil(FWordBreaker);
  FreeAndNil(FFoldedLinesView); // has reference to caret
  FreeAndNil(FInternalBlockSelection);
  FreeAndNil(FBlockSelection);
  FreeAndNil(FStrings);
  FreeAndNil(FTabbedLinesView);
  FreeAndNil(FTrimmedLinesView); // has reference to caret
  FreeAndNil(FDoubleWidthChrLinesView);
  TSynEditStringList(FLines).DetachSynEdit(self);
  if TSynEditStringList(FLines).AttachedSynEditCount = 0 then
    FreeAndNil(FLines);
  FreeAndNil(FCaret);
  FreeAndNil(FInternalCaret);
  FreeAndNil(FScreenCaret);
  FreeAndNil(FStatusChangedList);
  FBeautifier := nil;
  FreeAndNil(FDefaultBeautifier);
  inherited Destroy;
end;

function TCustomSynEdit.GetBlockBegin: TPoint;
begin
  Result := FBlockSelection.FirstLineBytePos;
end;

function TCustomSynEdit.GetBlockEnd: TPoint;
begin
  Result := FBlockSelection.LastLineBytePos;
end;

function TCustomSynEdit.GetBracketHighlightStyle: TSynEditBracketHighlightStyle;
begin
  Result := fMarkupBracket.HighlightStyle;
end;

function TCustomSynEdit.CaretXPix: Integer;
var
  p: TPoint;
begin
  p := Point(CaretX, CaretY);
  Result := RowColumnToPixels(p).X;
end;

function TCustomSynEdit.CaretYPix: Integer;
begin
  Result := RowColumnToPixels(Point(1, CaretY)).Y;
end;

{procedure TCustomSynEdit.FontChanged(Sender: TObject);
begin // TODO: inherited ?
  FTextArea.ForegroundColor := Font.Color;
  FLastSetFontSize := Font.Height;
  RecalcCharExtent;
end;
 }
function TCustomSynEdit.GetTextBuffer: TSynEditStrings;
begin
  Result := FLines;
end;

function TCustomSynEdit.GetLineText: string;
begin
  Result := FCaret.LineText;
end;

function TCustomSynEdit.GetMarkupByClass(Index: TSynEditMarkupClass): TSynEditMarkup;
begin
  Result := fMarkupManager.MarkupByClass[Index];
end;

function TCustomSynEdit.GetHighlightAllColor: TSynSelectedColor;
begin
  Result := fMarkupHighAll.MarkupInfo;
end;

function TCustomSynEdit.GetIncrementColor: TSynSelectedColor;
begin
  Result := fMarkupSelection.MarkupInfoIncr;
end;

function TCustomSynEdit.GetLineHighlightColor: TSynSelectedColor;
begin
  Result := fMarkupSpecialLine.MarkupLineHighlightInfo;
end;

function TCustomSynEdit.GetOnGutterClick: TGutterClickEvent;
begin
  Result := FLeftGutter.OnGutterClick;
end;

function TCustomSynEdit.GetSelectedColor: TSynSelectedColor;
begin
  Result := fMarkupSelection.MarkupInfoSeletion;
end;

procedure TCustomSynEdit.SetSelectedColor(const AValue: TSynSelectedColor);
begin
  fMarkupSelection.MarkupInfoSeletion.Assign(AValue);
end;

procedure TCustomSynEdit.SetSpecialLineColors(const AValue: TSpecialLineColorsEvent);
begin
  fOnSpecialLineColors := AValue;
  fMarkupSpecialLine.OnSpecialLineColors := AValue;
end;

procedure TCustomSynEdit.SetSpecialLineMarkup(const AValue: TSpecialLineMarkupEvent);
begin
  FOnSpecialLineMarkup := AValue;
  fMarkupSpecialLine.OnSpecialLineMarkup := AValue;
end;

function TCustomSynEdit.GetBracketMatchColor: TSynSelectedColor;
begin
  Result := fMarkupBracket.MarkupInfo;
end;

function TCustomSynEdit.GetMouseLinkColor: TSynSelectedColor;
begin
  Result := fMarkupCtrlMouse.MarkupInfo;
end;

function TCustomSynEdit.GetTrimSpaceType: TSynEditStringTrimmingType;
begin
  Result := FTrimmedLinesView.TrimType;
end;

function TCustomSynEdit.GetViewedTextBuffer: TSynEditStrings;
begin
  Result := FTheLinesView;
end;

function TCustomSynEdit.GetFoldedTextBuffer: TObject;
begin
  Result := FFoldedLinesView;
end;

procedure TCustomSynEdit.SetBracketHighlightStyle(const AValue: TSynEditBracketHighlightStyle);
begin
  fMarkupBracket.HighlightStyle := AValue;
end;

procedure TCustomSynEdit.SetOnGutterClick(const AValue: TGutterClickEvent);
begin
  FLeftGutter.OnGutterClick := AValue; // Todo: the IDE uses this for the left gutter only
end;

procedure TCustomSynEdit.SetUseIncrementalColor(const AValue: boolean);
begin
  fMarkupSelection.UseIncrementalColor := AValue;
end;

function TCustomSynEdit.GetCharLen(const Line: string; CharStartPos: Integer): Integer;
begin
  if UseUTF8 and (length(Line) >= CharStartPos) then
    Result := {UTF8CharacterLength(@Line[CharStartPos])}1
  else
    Result := 1;
end;

function TCustomSynEdit.GetLogicalCaretXY: TPoint;
begin
  Result := FCaret.LineBytePos;
end;

procedure TCustomSynEdit.SetLogicalCaretXY(const NewLogCaretXY: TPoint);
begin
  FCaret.ChangeOnTouch;
  FCaret.LineBytePos := NewLogCaretXY;
end;

procedure TCustomSynEdit.SetBeautifier(NewBeautifier: TSynCustomBeautifier);
begin
  if FBeautifier = NewBeautifier then
    exit;
  if NewBeautifier = nil then
    FBeautifier := FDefaultBeautifier
  else
    FBeautifier := NewBeautifier;
end;

function TCustomSynEdit.GetSelAvail: boolean;
begin
  Result := FBlockSelection.SelAvail;
end;

function TCustomSynEdit.GetSelText: string;
begin
  Result := FBlockSelection.SelText;
end;

procedure TCustomSynEdit.SetTrimSpaceType(const AValue: TSynEditStringTrimmingType);
begin
  FTrimmedLinesView.TrimType := AValue;
end;

function TCustomSynEdit.SynGetText: string;
begin
  Result := FLines.Text;
end;

{function TCustomSynEdit.RealGetText: TCaption;
begin
  if FLines <> nil then
    Result := FLines.Text
  else
    Result := '';
end;
}
{$IFDEF SYN_MBCSSUPPORT}

procedure TCustomSynEdit.WMImeComposition(var Msg: TMessage);
var
  imc: HIMC;
  p: PChar;
begin
  if ((Msg.LParam and GCS_RESULTSTR) <> 0) then
  begin
    imc := ImmGetContext(Handle);
    try
      fImeCount := ImmGetCompositionString(imc, GCS_RESULTSTR, nil, 0);
      GetMem(p, fImeCount + 1);
      try
        ImmGetCompositionString(imc, GCS_RESULTSTR, p, fImeCount + 1);
        p[fImeCount] := #0;
        CommandProcessor(ecImeStr, #0, p);
      finally
        FreeMem(p, fImeCount + 1);
      end;
    finally
      ImmReleaseContext(Handle, imc);
    end;
  end;
  inherited;
end;

procedure TCustomSynEdit.WMImeNotify(var Msg: TMessage);
var
  imc: HIMC;
  logFont: TLogFont;
begin
  with Msg do
  begin
    case WParam of
      IMN_SETOPENSTATUS:
        begin
          imc := ImmGetContext(Handle);
          if (imc <> 0) then
          begin
            GetObject(Font.Handle, SizeOf(TLogFont), @logFont);
            ImmSetCompositionFont(imc, @logFont);

            ImmReleaseContext(Handle, imc);
          end;
        end;
    end;
  end;
  inherited;
end;
{$ENDIF}

procedure TCustomSynEdit.InvalidateGutter;
begin
  InvalidateGutterLines(-1, -1);
end;

procedure TCustomSynEdit.InvalidateGutterLines(FirstLine, LastLine: Integer); // Todo: move to gutter
var
  TopFoldLine: Longint;
begin
  if sfPainting in fStateFlags then
    exit;
  if Visible {and HandleAllocated }then
  begin
{$IFDEF VerboseSynEditInvalidate}
    DebugLnEnter(['TCustomSynEdit.InvalidateGutterLines ', DbgSName(self), ' FirstLine=', FirstLine, ' LastLine=',
      LastLine]);
{$ENDIF}
    if (FirstLine = -1) and (LastLine = -1) then
    begin
      FPaintArea.InvalidateGutterLines(-1, -1);
    end
    else
    begin
      // pretend we haven't scrolled
      TopFoldLine := FFoldedLinesView.TopLine;
      if FOldTopView <> TopView then
        FFoldedLinesView.TopLine := FOldTopView;

      if (LastLine <> -1) and (LastLine < FirstLine) then
        SwapInt(FirstLine, LastLine);

      FPaintArea.InvalidateGutterLines(FirstLine - 1, LastLine - 1);

      FFoldedLinesView.TopLine := TopFoldLine;
    end;
{$IFDEF VerboseSynEditInvalidate}
    DebugLnExit(['TCustomSynEdit.InvalidateGutterLines ', DbgSName(self)]);
{$ENDIF}
  end;
end;

procedure TCustomSynEdit.InvalidateLines(FirstLine, LastLine: Integer);
var
  TopFoldLine: Longint;
begin
  if sfPainting in fStateFlags then
    exit;
  if Visible {and HandleAllocated }then
  begin
{$IFDEF VerboseSynEditInvalidate}
    DebugLnEnter(['TCustomSynEdit.InvalidateTextLines ', DbgSName(self), ' FirstLine=', FirstLine, ' LastLine=',
      LastLine]);
{$ENDIF}
    if (FirstLine = -1) and (LastLine = -1) then
    begin
      FPaintArea.InvalidateTextLines(-1, -1);
    end
    else
    begin
      // pretend we haven't scrolled
      TopFoldLine := FFoldedLinesView.TopLine;
      if FOldTopView <> TopView then
        FFoldedLinesView.TopLine := FOldTopView;

      if (LastLine <> -1) and (LastLine < FirstLine) then
        SwapInt(FirstLine, LastLine);

      FPaintArea.InvalidateTextLines(FirstLine - 1, LastLine - 1);

      FFoldedLinesView.TopLine := TopFoldLine;
    end;
{$IFDEF VerboseSynEditInvalidate}
    DebugLnExit(['TCustomSynEdit.InvalidateTextLines ', DbgSName(self)]);
{$ENDIF}
  end;
end;

procedure TCustomSynEdit.KeyDown(var Key: word; var KeyChar: WideChar; Shift: TShiftState);
var
  Data: pointer;
  c: Char;
  Cmd: TSynEditorCommand;
  IsStartOfCombo, Handled: boolean;
begin
  FInMouseClickEvent := False;
{$IFDEF VerboseKeys}
  debugln('[TCustomSynEdit.KeyDown] ', dbgs(Key), ' ', dbgs(Shift));
{$ENDIF}
  inherited;
  if assigned(fMarkupCtrlMouse) then
    fMarkupCtrlMouse.UpdateCtrlState(Shift);

  if Key {in [VK_SHIFT, VK_CONTROL, VK_MENU, VK_LSHIFT, VK_LCONTROL, VK_LMENU, VK_RSHIFT, VK_RCONTROL, VK_RMENU, VK_LWIN,
    VK_RWIN] } = 0then
    exit;

  Data := nil;
  c := #0;
  try
    // If the translations requires Data, memory will be allocated for it via a
    // GetMem call.  The client must call FreeMem on Data if it is not NIL.
    IsStartOfCombo := False;
    Handled := False;

    // Check 2nd stroke in SynEdit.KeyStrokes
    if FCurrentComboKeyStrokes <> nil then
    begin
      // Run hooked first, it might want to "steal" the key(s)
      FHookedKeyTranslationList.CallHookedKeyTranslationHandlers(self, Key, Shift, Data, IsStartOfCombo, Handled, Cmd,
        FCurrentComboKeyStrokes);

      if not Handled then
      begin
        Cmd := Keystrokes.FindKeycodeEx(Key, Shift, Data, IsStartOfCombo, True, FCurrentComboKeyStrokes);
        if IsStartOfCombo then
          FCurrentComboKeyStrokes := FKeyStrokes;
        Handled := (Cmd <> ecNone) or IsStartOfCombo;
      end;

      if not IsStartOfCombo then
      begin
        FCurrentComboKeyStrokes.ResetKeyCombo;
        FCurrentComboKeyStrokes := nil;
      end;
    end;
    assert(Handled or (FCurrentComboKeyStrokes = nil), 'FCurrentComboKeyStrokes<>nil, should be handled');

    // Check 1st/single stroke in Hooked KeyStrokes
    if not Handled then
    begin
      FCurrentComboKeyStrokes := nil;
      FHookedKeyTranslationList.CallHookedKeyTranslationHandlers(self, Key, Shift, Data, IsStartOfCombo, Handled, Cmd,
        FCurrentComboKeyStrokes);
      if (not IsStartOfCombo) and (FCurrentComboKeyStrokes <> nil) then
        FCurrentComboKeyStrokes.ResetKeyCombo; // should not happen
    end;
    // Check 1st/single stroke in SynEdit.KeyStrokes
    if not Handled then
    begin
      FKeyStrokes.ResetKeyCombo;
      Cmd := Keystrokes.FindKeycodeEx(Key, Shift, Data, IsStartOfCombo);
      if IsStartOfCombo then
        FCurrentComboKeyStrokes := FKeyStrokes;
    end;

    if Cmd <> ecNone then
    begin
      // Reset FCurrentComboKeyStrokes => no open combo
      assert(FCurrentComboKeyStrokes = nil, 'FCurrentComboKeyStrokes<>nil, should be ecNone');
      if FCurrentComboKeyStrokes <> nil then
        FCurrentComboKeyStrokes.ResetKeyCombo;
      FCurrentComboKeyStrokes := nil;

      Include(fStateFlags, sfHideCursor);
      LastMouseCaret := Point(-1, -1); // includes update cursor
      // DebugLn(['[TCustomSynEdit.KeyDown] key translated ',cmd]);
      Key := 0; // eat it.
      Include(fStateFlags, sfIgnoreNextChar);
      CommandProcessor(Cmd, c, Data);
    end
    else if IsStartOfCombo then
    begin
      // this key could be the start of a two-key-combo shortcut
      Key := 0; // eat it.
      Include(fStateFlags, sfIgnoreNextChar);
    end
    else
      Exclude(fStateFlags, sfIgnoreNextChar);
  finally
    if Data <> nil then
      FreeMem(Data);
  end;
  UpdateCursor;
  SelAvailChange(nil);
  // DebugLn('[TCustomSynEdit.KeyDown] END ',dbgs(Key),' ',dbgs(Shift));
end;

procedure TCustomSynEdit.KeyUp(var Key: word; var KeyChar: WideChar; Shift: TShiftState);
begin
{$IFDEF VerboseKeys}
  debugln(['[TCustomSynEdit.KeyUp] ', Key, ' Shift=', ssShift in Shift, ' Ctrl=', ssCtrl in Shift, ' Alt=',
    ssAlt in Shift]);
{$ENDIF}
  inherited KeyUp(Key, KeyChar, Shift);

  if sfIgnoreNextChar in fStateFlags then
    Exclude(fStateFlags, sfIgnoreNextChar);

  if assigned(fMarkupCtrlMouse) then
    fMarkupCtrlMouse.UpdateCtrlState(Shift);
  UpdateCursor;
end;

procedure TCustomSynEdit.Loaded;
begin
  inherited Loaded;
end;

(*procedure TCustomSynEdit.UTF8KeyPress(var Key: TUTF8Char);
begin
  if Key = '' then
    exit;
  // don't fire the event if key is to be ignored
  if not(sfIgnoreNextChar in fStateFlags) then
  begin
    Include(fStateFlags, sfHideCursor);
    if assigned(OnUTF8KeyPress) then
      OnUTF8KeyPress(self, Key);
    // The key will be handled in UTFKeyPress always and KeyPress won't be called
    // so we we fire the OnKeyPress here
    if (ord(Key[1]) < % 11000000) and (Key[1] <> #0) and assigned(OnKeyPress) then
      OnKeyPress(self, Key[1]);
{$IFDEF VerboseKeys}
    debugln('TCustomSynEdit.UTF8KeyPress ', DbgSName(self), ' Key="', DbgStr(Key), '" UseUTF8=', dbgs(UseUTF8));
{$ENDIF}
    CommandProcessor(ecChar, Key, nil);
    // Check if ecChar has handled the Key; Todo: move the condition, in one common place
    if not ReadOnly and ((Key = #13) or (Key >= #32)) and (Key <> #127) then
      Key := '';
  end
  else
  begin
    // don't ignore further keys
    Exclude(fStateFlags, sfIgnoreNextChar);
    // Key was handled anyway, so eat it!
    Key := '';
  end;
end;

procedure TCustomSynEdit.KeyPress(var Key: Char);
begin
  if Key = #0 then
    exit;
  // don't fire the event if key is to be ignored
  if not(sfIgnoreNextChar in fStateFlags) then
  begin
    Include(fStateFlags, sfHideCursor);
{$IFDEF VerboseKeys}
    debugln('TCustomSynEdit.KeyPress ', DbgSName(self), ' Key="', DbgStr(Key), '" UseUTF8=', dbgs(UseUTF8));
{$ENDIF}
    if assigned(OnKeyPress) then
      OnKeyPress(self, Key);
    CommandProcessor(ecChar, Key, nil);
    // Check if ecChar has handled the Key; Todo: move the condition, in one common place
    if not ReadOnly and ((Key = #13) or (Key >= #32)) and (Key <> #127) then
      Key := #0;
  end
  else
  begin
    // don't ignore further keys
    Exclude(fStateFlags, sfIgnoreNextChar);
    // Key was handled anyway, so eat it!
    Key := #0;
  end;
end;*)

function TCustomSynEdit.DoHandleMouseAction(AnActionList: TSynEditMouseActions;
  AnInfo: TSynEditMouseActionInfo): boolean;
var
  CaretDone: boolean;
  AnAction: TSynEditMouseAction;

function MulDiv(Factor1,Factor2,Divisor:integer):integer;
begin
  Result:=(int64(Factor1)*int64(Factor2)) div Divisor;
end;


  procedure MoveCaret;
  begin
    FCaret.LineCharPos := AnInfo.NewCaret.LineCharPos;
    CaretDone := True;
  end;

  function GetWheelScrollAmount(APageSize: Integer): Integer;
  const
    WHEEL_DELTA = 120;
    // WHEEL_PAGESCROLL = MAXDWORD;
  var
    WClicks, WLines: Single;
  begin
    FMouseWheelAccumulator := FMouseWheelAccumulator + AnInfo.WheelDelta;
    FMouseWheelLinesAccumulator :=  FMouseWheelLinesAccumulator + (APageSize * AnInfo.WheelDelta);
    WClicks := FMouseWheelAccumulator / WHEEL_DELTA;
    WLines := FMouseWheelLinesAccumulator / WHEEL_DELTA;
    FMouseWheelAccumulator := FMouseWheelAccumulator - (WClicks * WHEEL_DELTA);
    FMouseWheelLinesAccumulator := FMouseWheelLinesAccumulator - (WLines * WHEEL_DELTA);

    case AnAction.Option of
      emcoWheelScrollSystem:
        begin
          Result := Trunc(Abs(WLines));
        end;
      emcoWheelScrollLines:
        begin
          Result := Trunc(Abs(WClicks));
          If Result = 0 then
            exit;
          if AnAction.Option2 > 0 then
            Result := Result * AnAction.Option2;
          if (Result > APageSize) then
            Result := APageSize;
          exit;
        end;
      emcoWheelScrollPages:
        Result := Trunc(Abs(WClicks)) * APageSize;
      emcoWheelScrollPagesLessOne:
        Result := Trunc(Abs(WClicks)) * (APageSize - 1);
    else
      begin
        Result := Trunc(Abs(WLines));
        exit;
      end;
    end;

    If Result = 0 then
      exit;

    if AnAction.Option2 > 0 then
      Result := MulDiv(Result, AnAction.Option2, 100);
    if (Result > APageSize) then
      Result := APageSize;
    if (Result < 1) then
      Result := 1;
  end;

var
  ACommand: TSynEditorMouseCommand;
  Handled: boolean;
//  ClipHelper: TSynClipboardStream;
  i, j: Integer;
begin
  AnAction := nil;
  Result := False;
  while not Result do
  begin
    AnAction := AnActionList.FindCommand(AnInfo, AnAction);

    if AnAction = nil then
      exit(False);
    ACommand := AnAction.Command;
    AnInfo.CaretDone := False;

    // Opening the context menu must not unset the block selection
    // Therefore if a non persistent block is given, it shall ignore the caret move.
    if (ACommand = emcContextMenu) and FBlockSelection.SelAvail and not FBlockSelection.Persistent then
    begin
      case AnAction.Option of
        emcoSelectionCaretMoveOutside:
          AnInfo.CaretDone := (CompareCarets(AnInfo.NewCaret.LineBytePos, FBlockSelection.FirstLineBytePos) <= 0) and
            (CompareCarets(AnInfo.NewCaret.LineBytePos, FBlockSelection.LastLineBytePos) >= 0);
        emcoSelectionCaretMoveAlways:
          AnInfo.CaretDone := False;
      else
        AnInfo.CaretDone := True;
      end;
    end;

    // Plugins/External
    Result := FMouseActionExecHandlerList.CallExecHandlers(AnAction, AnInfo);
    // Gutter
    if not Result then
      Result := FLeftGutter.DoHandleMouseAction(AnAction, AnInfo);
    if not Result then
      Result := FRightGutter.DoHandleMouseAction(AnAction, AnInfo);

    if Result then
    begin
      if (not AnInfo.CaretDone) and AnAction.MoveCaret then
        MoveCaret;
      if (AnAction.IgnoreUpClick) then
        AnInfo.IgnoreUpClick := True;
      exit;
    end;

    Result := True;
    CaretDone := AnInfo.CaretDone;
//    MouseCapture := False;

    if (ACommand = emcWheelScrollDown) then
    begin
      // sroll dependant on visible scrollbar / or not at all
      if (sfVertScrollbarVisible in fStateFlags) then
        ACommand := emcWheelVertScrollDown
      else if (sfHorizScrollbarVisible in fStateFlags) then
        ACommand := emcWheelHorizScrollDown;
    end;

    if (ACommand = emcWheelScrollUp) then
    begin
      // sroll dependant on visible scrollbar / or not at all
      if (sfVertScrollbarVisible in fStateFlags) then
        ACommand := emcWheelVertScrollUp
      else if (sfHorizScrollbarVisible in fStateFlags) then
        ACommand := emcWheelHorizScrollUp;
    end;

    case ACommand of
      emcNone:
        ; // do nothing, but result := true
      emcStartSelections, emcStartColumnSelections, emcStartLineSelections:
        begin
          FBlockSelection.AutoExtend := AnAction.Option = emcoSelectionContinue;
          FCaret.ChangeOnTouch;
          MoveCaret;
          case ACommand of
            emcStartColumnSelections:
              FMouseSelectionMode := smColumn;
            emcStartLineSelections:
              FMouseSelectionMode := smLine;
          else
            FMouseSelectionMode := FBlockSelection.SelectionMode;
          end;
          if (AnAction.Option = emcoSelectionContinue) then
          begin
            // only set ActiveSelectionMode if we continue an existing selection
            // Otherwise we are just setting the caret, selection will start on mouse move
            FBlockSelection.ActiveSelectionMode := FMouseSelectionMode;
            Include(fStateFlags, sfMouseDoneSelecting);
          end;
//          MouseCapture := True;
          Include(fStateFlags, sfWaitForMouseSelecting);
        end;
      emcSelectWord:
        begin
          if AnAction.MoveCaret then
            MoveCaret;
          SetWordBlock(AnInfo.NewCaret.LineBytePos);
//          MouseCapture := False;
        end;
      emcSelectLine:
        begin
          if AnAction.MoveCaret then
            MoveCaret;
          SetLineBlock(AnInfo.NewCaret.LineBytePos, AnAction.Option = emcoSelectLineFull);
//          MouseCapture := False;
        end;
      emcSelectPara:
        begin
          if AnAction.MoveCaret then
            MoveCaret;
          SetParagraphBlock(AnInfo.NewCaret.LineBytePos);
//          MouseCapture := False;
        end;
      emcStartDragMove:
        begin
          if SelAvail and (SelectionMode = smNormal) then
          begin
            Include(fStateFlags, sfWaitForDragging);
//            MouseCapture := True;
          end
          else
            Result := False; // Currently only drags smNormal
        end;
      emcPasteSelection:
        begin
//          ClipHelper := TSynClipboardStream.Create;
//          try
//            ClipHelper.ReadFromClipboard(PrimarySelection);
//            if ClipHelper.TextP <> nil then
//            begin
//              MoveCaret;
//              if (not FBlockSelection.Persistent) then
//                FBlockSelection.Clear;
//              Result := PasteFromClipboardEx(ClipHelper);
//            end
//            else
//              Result := False;
//          finally
//            ClipHelper.Free;
//          end;
        end;
      emcMouseLink:
        begin
          if assigned(fMarkupCtrlMouse) and fMarkupCtrlMouse.IsMouseOverLink and assigned(FOnClickLink) then
            FOnClickLink(self, SynMouseButtonBackMap[AnInfo.Button], AnInfo.Shift, AnInfo.MouseX, AnInfo.MouseY)
          else
            Result := False;
        end;
      emcContextMenu:
        begin
          Handled := False;
          if AnAction.MoveCaret and (not CaretDone) then
          begin
            MoveCaret;
          end;
//          inherited DoContextPopup(Point(AnInfo.MouseX, AnInfo.MouseY), Handled);
          // Open PopUpMenu after DecPaintlock
          if not Handled then
            FMouseClickDoPopUp := True;
        end;
      emcSynEditCommand:
        begin
          if AnAction.MoveCaret then
            MoveCaret;
          CommandProcessor(AnAction.Option, #0, nil);
        end;
      emcWheelHorizScrollDown, emcWheelHorizScrollUp:
        begin
          i := GetWheelScrollAmount(CharsInWindow);
          if ACommand = emcWheelHorizScrollUp then
            i := -i;
          if i <> 0 then
            LeftChar := LeftChar + i;
        end;
      emcWheelVertScrollDown, emcWheelVertScrollUp:
        begin
          i := GetWheelScrollAmount(LinesInWindow);
          if ACommand = emcWheelVertScrollUp then
            i := -i;
          if i <> 0 then
            TopView := TopView + i;
        end;
      emcWheelZoomOut, emcWheelZoomIn:
        begin
          if ((ACommand = emcWheelZoomOut) {and (Abs(Font.Size) < 3)}) or
            ((ACommand = emcWheelZoomIn) {and (Abs(Font.Size) > 50)}) then
          begin
            Result := False;
          end
          else
          begin
            j := 1;
            if ACommand = emcWheelZoomIn then
              j := -1;
//            i := FLastSetFontSize;
//            if Font.Height < 0 then
//              Font.Height := Font.Height + j
//            else
//              Font.Height := Font.Height - j;
//            FLastSetFontSize := i;
          end;
        end;
      emcWheelZoomNorm:
        begin
//          Font.Height := FLastSetFontSize;
        end;
    else
      Result := False; // ACommand was not handled => Fallback to parent Context
    end;

    if Result and (not CaretDone) and AnAction.MoveCaret then
      MoveCaret;
    if Result and (AnAction.IgnoreUpClick) then
      AnInfo.IgnoreUpClick := True;
  end;
end;

{procedure TCustomSynEdit.SetColor(Value: TColor);
begin
  inherited SetColor(Value);
  FTextArea.BackgroundColor := Color;
end;
 }
procedure TCustomSynEdit.FindAndHandleMouseAction(AButton: TSynMouseButton; AShift: TShiftState; X, Y: Single;
  ACCount: TSynMAClickCount; ADir: TSynMAClickDir; AWheelDelta: Integer = 0);
var
  Info: TSynEditMouseActionInfo;
begin
  FInternalCaret.AssignFrom(FCaret);
  FInternalCaret.LineCharPos := PixelsToRowColumn(Point(Trunc(X), Trunc(Y)));
  with Info do
  begin
    NewCaret := FInternalCaret;
    Button := AButton;
    Shift := AShift;
    MouseX := X;
    MouseY := Y;
    WheelDelta := AWheelDelta;
    CCount := ACCount;
    Dir := ADir;
    IgnoreUpClick := False;
  end;
  try
    // Check plugins/external handlers
    if FMouseActionSearchHandlerList.CallSearchHandlers(Info,
{$IFDEF FPC}@{$ENDIF}DoHandleMouseAction) then
      exit;

    if FLeftGutter.Visible and (X < FLeftGutter.Width) then
    begin
      // mouse event occured in Gutter ?
      if FLeftGutter.MaybeHandleMouseAction(Info, {$IFDEF FPC}@{$ENDIF}DoHandleMouseAction) then
        exit;
    end
    else if FRightGutter.Visible {and (X > ClientWidth - FRightGutter.Width)} then
    begin
      // mouse event occured in Gutter ?
      if FRightGutter.MaybeHandleMouseAction(Info, {$IFDEF FPC}@{$ENDIF}DoHandleMouseAction) then
        exit;
    end
    else
    begin
      // mouse event occured in selected block ?
      if SelAvail and (X >= FTextArea.Bounds.Left) and (X < FTextArea.Bounds.Right) and (Y >= FTextArea.Bounds.Top) and
        (Y < FTextArea.Bounds.Bottom) and IsPointInSelection(FInternalCaret.LineBytePos) then
        if DoHandleMouseAction(FMouseSelActions.GetActionsForOptions(FMouseOptions), Info) then
          exit;
      // mouse event occured in text?
      if DoHandleMouseAction(FMouseTextActions.GetActionsForOptions(FMouseOptions), Info) then
        exit;
    end;

    DoHandleMouseAction(FMouseActions.GetActionsForOptions(FMouseOptions), Info);
  finally
    if Info.IgnoreUpClick then
      Include(fStateFlags, sfIgnoreUpClick);
  end;
end;

procedure TCustomSynEdit.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single);
var
  CType: TSynMAClickCount;
begin
  // DebugLn(['TCustomSynEdit.MouseDown START Mouse=',X,',',Y,' Caret=',CaretX,',',CaretY,', BlockBegin=',BlockBegin.X,',',BlockBegin.Y,' BlockEnd=',BlockEnd.X,',',BlockEnd.Y]);
  Exclude(fStateFlags, sfHideCursor);
  FInMouseClickEvent := True;
  if (X >= Width - ScrollBarWidth) or (Y >= Height - ScrollBarWidth) then
  begin
    inherited MouseDown(Button, Shift, X, Y);
    exit;
  end;

  LastMouseCaret := PixelsToRowColumn(Point(Trunc(X), Trunc(Y)));
  fMouseDownX := Trunc(X);
  fMouseDownY := Trunc(Y);

  fStateFlags := fStateFlags - [sfDblClicked, sfTripleClicked, sfQuadClicked, sfLeftGutterClick, sfRightGutterClick,
    sfWaitForMouseSelecting, sfMouseSelecting, sfMouseDoneSelecting, sfWaitForDragging, sfIgnoreUpClick];

//  if ssQuad in Shift then
//  begin
//    CType := ccQuad;
//    Include(fStateFlags, sfQuadClicked);
//  end
//  else if ssTriple in Shift then
//  begin
//    CType := ccTriple;
//    Include(fStateFlags, sfTripleClicked);
//  end
//  else
if ssDouble in Shift then
  begin
    CType := ccDouble;
    Include(fStateFlags, sfDblClicked);
  end
  else
    CType := ccSingle;

  FMouseClickDoPopUp := False;
  IncPaintLock;
  try
    if (X < TextLeftPixelOffset(False)) then
    begin
      Include(fStateFlags, sfLeftGutterClick);
      FLeftGutter.MouseDown(Button, Shift, X, Y);
    end;
    if (X > Width - TextRightPixelOffset - ScrollBarWidth) then
    begin
      Include(fStateFlags, sfRightGutterClick);
      FRightGutter.MouseDown(Button, Shift, X, Y);
    end;
    FindAndHandleMouseAction(SynMouseButtonMap[Button], Shift, X, Y, CType, cdDown);
  finally
    DecPaintLock;
  end;
  if FMouseClickDoPopUp and (PopupMenu <> nil) then
  begin
    PopupMenu.PopupComponent := self;
//    PopupMenu.PopUp;
  end;

  inherited MouseDown(Button, Shift, X, Y);
  self.SetFocus;
  UpdateCaret;
  SelAvailChange(nil);
  // debugln('TCustomSynEdit.MouseDown END sfWaitForDragging=',dbgs(sfWaitForDragging in fStateFlags),' ');
end;

procedure TCustomSynEdit.MouseMove(Shift: TShiftState; X, Y: Single);
begin
  Exclude(fStateFlags, sfHideCursor);
  inherited MouseMove(Shift, X, Y);
  if (sfLeftGutterClick in fStateFlags) then
    FLeftGutter.MouseMove(Shift, X, Y);
  if (sfRightGutterClick in fStateFlags) then
    FRightGutter.MouseMove(Shift, X, Y);

  FLastMousePoint := Point(Trunc(X), Trunc(Y));
  LastMouseCaret := PixelsToRowColumn(Point(Trunc(X), Trunc(Y)));
  UpdateCursor;

  if (sfWaitForMouseSelecting in fStateFlags) {and MouseCapture }and
    ((Abs(fMouseDownX - X) >= MinMax(CharWidth div 2, 2, 4)) or (Abs(fMouseDownY - Y) >= MinMax(LineHeight div 2, 2, 4)))
  then
    fStateFlags := fStateFlags - [sfWaitForMouseSelecting] + [sfMouseSelecting];

  // debugln('TCustomSynEdit.MouseMove sfWaitForDragging=',dbgs(sfWaitForDragging in fStateFlags),' MouseCapture=',dbgs(MouseCapture),' GetCaptureControl=',DbgSName(GetCaptureControl));
  if {MouseCapture and} (sfWaitForDragging in fStateFlags) then
  begin
    if (Abs(fMouseDownX - X) >= 0 )or (Abs(fMouseDownY - Y) >= 0){GetSystemMetrics(SM_CYDRAG))}
    then
    begin
      fStateFlags := fStateFlags - [sfWaitForDragging, sfWaitForMouseSelecting, sfMouseSelecting] + [sfIsDragging];
      // debugln('TCustomSynEdit.MouseMove BeginDrag');
//      BeginDrag(True);
    end;
  end
  else if (fStateFlags * [sfMouseSelecting, sfIsDragging] <> []) {and MouseCapture }then
  begin
    // DebugLn(' TCustomSynEdit.MouseMove CAPTURE Mouse=',dbgs(X),',',dbgs(Y),' Caret=',dbgs(CaretXY),', BlockBegin=',dbgs(BlockBegin),' BlockEnd=',dbgs(BlockEnd));
    if sfIsDragging in fStateFlags then
      FBlockSelection.IncPersistentLock;
    FInternalCaret.AssignFrom(FCaret);
    FInternalCaret.LineCharPos := PixelsToRowColumn(Point(Trunc(X), Trunc(Y)));

    // compare to Bounds => Padding area does not scroll
    if ((X >= FTextArea.Bounds.Left) or (LeftChar <= 1)) and
      ((X < FTextArea.Bounds.Right) or (LeftChar >= CurrentMaxLeftChar)) and
      ((Y >= FTextArea.Bounds.Top) or (TopView <= 1)) and
      ((Y < FTextArea.Bounds.Bottom) or (TopView >= CurrentMaxTopView)) then
    begin
      if (sfMouseSelecting in fStateFlags) and not FInternalCaret.IsAtPos(FCaret) then
        Include(fStateFlags, sfMouseDoneSelecting);
      FBlockSelection.AutoExtend := sfMouseSelecting in fStateFlags;
      FCaret.LineBytePos := FInternalCaret.LineBytePos;
      FBlockSelection.AutoExtend := False;
    end
    else
    begin
      // begin scrolling?
      if X < FTextArea.Bounds.Left then
        FScrollDeltaX := Min(trunc(X - FTextArea.Bounds.Left - CharWidth) div CharWidth, -1)
      else if X >= FTextArea.Bounds.Right then
        FScrollDeltaX := Max(trunc(X - FTextArea.Bounds.Right + 1 + CharWidth) div CharWidth, 1)
      else
        FScrollDeltaX := 0;

      if Y < FTextArea.Bounds.Top then
        FScrollDeltaY := Min(trunc(Y - FTextArea.Bounds.Top - LineHeight) div LineHeight, -1)
      else if Y >= FTextArea.Bounds.Bottom then
        FScrollDeltaY := Max(trunc(Y - FTextArea.Bounds.Bottom + 1 + LineHeight) div LineHeight, 1)
      else
        FScrollDeltaY := 0;

      fScrollTimer.Enabled := (FScrollDeltaX <> 0) or (FScrollDeltaY <> 0);
      if (sfMouseSelecting in fStateFlags) and ((FScrollDeltaX <> 0) or (FScrollDeltaY <> 0)) then
        Include(fStateFlags, sfMouseDoneSelecting);
    end;
    if sfMouseDoneSelecting in fStateFlags then
      FBlockSelection.ActiveSelectionMode := FMouseSelectionMode;
    if sfIsDragging in fStateFlags then
      FBlockSelection.DecPersistentLock;
  end
  else if {MouseCapture and }(fStateFlags * [sfIsDragging, sfWaitForMouseSelecting] = []) then
  begin
//    MouseCapture := False;
    fScrollTimer.Enabled := False;
  end;
end;

procedure TCustomSynEdit.ScrollTimerHandler(Sender: TObject);
var
  c: TPoint;
  CurMousePos: TPoint;
  X, Y: Integer;
begin
  // changes to line / column in one go
  if sfIsDragging in fStateFlags then
    FBlockSelection.IncPersistentLock;
  DoIncPaintLock(self); // No editing is taking place
  try
//    GetCursorPos(CurMousePos);
//    CurMousePos := ScreenToClient(CurMousePos);
    c := PixelsToLogicalPos(CurMousePos);

    // recalculate scroll deltas
    if CurMousePos.X < FTextArea.Bounds.Left then
      FScrollDeltaX := Min(Trunc(CurMousePos.X - FTextArea.Bounds.Left - CharWidth) div CharWidth, -1)
    else if CurMousePos.X >= FTextArea.Bounds.Right then
      FScrollDeltaX := Max(Trunc(CurMousePos.X - FTextArea.Bounds.Right + 1 + CharWidth) div CharWidth, 1)
    else
      FScrollDeltaX := 0;

    if CurMousePos.Y < FTextArea.Bounds.Top then
      FScrollDeltaY := Min(Trunc(CurMousePos.Y - FTextArea.Bounds.Top - LineHeight) div LineHeight, -1)
    else if CurMousePos.Y >= FTextArea.Bounds.Bottom then
      FScrollDeltaY := Max(Trunc(CurMousePos.Y - FTextArea.Bounds.Bottom + 1 + LineHeight) div LineHeight, 1)
    else
      FScrollDeltaY := 0;

    fScrollTimer.Enabled := (FScrollDeltaX <> 0) or (FScrollDeltaY <> 0);
    // now scroll
    if FScrollDeltaX <> 0 then
    begin
      LeftChar := LeftChar + FScrollDeltaX;
      X := LeftChar;
      if FScrollDeltaX > 0 then // scrolling right?
        inc(X, CharsInWindow);
      FCaret.LineCharPos := Point(X, c.Y);
      if (not(sfIsDragging in fStateFlags)) then
        SetBlockEnd(LogicalCaretXY);
    end;
    if FScrollDeltaY <> 0 then
    begin
//      if GetKeyState(VK_SHIFT) < 0 then
//        TopView := TopView + FScrollDeltaY * LinesInWindow
//      else
//        TopView := TopView + FScrollDeltaY;
      if FScrollDeltaY > 0 then
        Y := FFoldedLinesView.TextIndex[LinesInWindow - 1] + 1 // scrolling down
      else
        Y := TopLine; // scrolling up
      if Y < 1 // past end of file
      then
        Y := FCaret.LinePos;
      FCaret.LineCharPos := Point(c.X, Y);
      if (not(sfIsDragging in fStateFlags)) then
        SetBlockEnd(LogicalCaretXY);
    end;
  finally
    DoDecPaintLock(self);
    if sfIsDragging in fStateFlags then
      FBlockSelection.DecPersistentLock;
  end;
end;

{procedure TCustomSynEdit.DoContextPopup(MousePos: TPoint; var Handled: boolean);
begin
  Handled := FInMouseClickEvent;
  if not Handled then
    Exclude(fStateFlags, sfHideCursor);
end;}

procedure TCustomSynEdit.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single);
var
  wasDragging, wasSelecting, ignoreUp: boolean;
  CType: TSynMAClickCount;
begin
  Exclude(fStateFlags, sfHideCursor);
  // DebugLn('TCustomSynEdit.MouseUp Mouse=',X,',',Y,' Caret=',CaretX,',',CaretY,', BlockBegin=',BlockBegin.X,',',BlockBegin.Y,' BlockEnd=',BlockEnd.X,',',BlockEnd.Y);
  FInMouseClickEvent := True;
  wasDragging := (sfIsDragging in fStateFlags);
  wasSelecting := (sfMouseDoneSelecting in fStateFlags);
  ignoreUp := (sfIgnoreUpClick in fStateFlags);
  Exclude(fStateFlags, sfIsDragging);
  Exclude(fStateFlags, sfWaitForMouseSelecting);
  Exclude(fStateFlags, sfMouseSelecting);
  Exclude(fStateFlags, sfMouseDoneSelecting);
  Exclude(fStateFlags, sfIgnoreUpClick);
  fScrollTimer.Enabled := False;
  inherited MouseUp(Button, Shift, X, Y);
//  MouseCapture := False;

  if sfQuadClicked in fStateFlags then
  begin
    CType := ccQuad;
    Include(fStateFlags, sfQuadClicked);
  end
  else if sfTripleClicked in fStateFlags then
  begin
    CType := ccTriple;
    Include(fStateFlags, sfTripleClicked);
  end
  else if sfDblClicked in fStateFlags then
  begin
    CType := ccDouble;
    Include(fStateFlags, sfDblClicked);
  end
  else
    CType := ccSingle;
  fStateFlags := fStateFlags - [sfDblClicked, sfTripleClicked, sfQuadClicked];

  if sfWaitForDragging in fStateFlags then
  begin
    ComputeCaret(Trunc(X), Trunc(Y));
    SetBlockBegin(LogicalCaretXY);
    SetBlockEnd(LogicalCaretXY);
    Exclude(fStateFlags, sfWaitForDragging);
  end;

  if (X >= Width - ScrollBarWidth) or (Y >= Height - ScrollBarWidth) then
    exit;
  LastMouseCaret := PixelsToRowColumn(Point(Trunc(X), Trunc(Y)));

  if wasDragging or wasSelecting or ignoreUp then
    exit;

  FMouseClickDoPopUp := False;
  IncPaintLock;
  try
    if (sfLeftGutterClick in fStateFlags) then
    begin
      FLeftGutter.MouseUp(Button, Shift, X, Y);
      Exclude(fStateFlags, sfLeftGutterClick);
    end;
    if (sfRightGutterClick in fStateFlags) then
    begin
      FRightGutter.MouseUp(Button, Shift, X, Y);
      Exclude(fStateFlags, sfRightGutterClick);
    end;
    FindAndHandleMouseAction(SynMouseButtonMap[Button], Shift, X, Y, CType, cdUp);
  finally
    DecPaintLock;
  end;
  if FMouseClickDoPopUp and (PopupMenu <> nil) then
  begin
    PopupMenu.PopupComponent := self;
//    PopupMenu.PopUp;
  end;
  SelAvailChange(nil);
  // DebugLn('TCustomSynEdit.MouseUp END Mouse=',X,',',Y,' Caret=',CaretX,',',CaretY,', BlockBegin=',BlockBegin.X,',',BlockBegin.Y,' BlockEnd=',BlockEnd.X,',',BlockEnd.Y);
end;

procedure TCustomSynEdit.Paint;
//var
//  rcClip: TRect;
begin
  // Get the invalidated rect. Compute the invalid area in lines / columns.
//  rcClip := Canvas.Width;

  If FPaintLock > 0 then
  begin
//    debugln(['Warning: SynEdit.Paint called during PaintLock']);
{$IFDEF SynCheckPaintLock}
    DumpStack;
{$ENDIF}
    // Ensure this will be repainted after PaintLock
//    if FInvalidateRect.Top < 0 then
//      FInvalidateRect := rcClip
//    else
//      Types.UnionRect(FInvalidateRect, FInvalidateRect, rcClip);
    // Just paint the background
//    SetBkColor(Canvas.Handle, ColorToRGB(Color));
//    InternalFillRect(Canvas.Handle, rcClip);
//    if rcClip.Left <= TextLeftPixelOffset(False) then
//    begin
//      rcClip.Right := TextLeftPixelOffset(False) + 1;
//      SetBkColor(Canvas.Handle, ColorToRGB(FLeftGutter.Color));
//      InternalFillRect(Canvas.Handle, rcClip);
//    end;
    exit;
  end;

{$IFDEF EnableDoubleBuf}
  // rcClip:=Rect(0,0,ClientWidth,ClientHeight);
  StartPaintBuffer(rcClip);
{$ENDIF}
{$IFDEF SYNSCROLLDEBUG}
  debugln(['PAINT ', DbgSName(self), ' sfHasScrolled=', dbgs(sfHasScrolled in fStateFlags), ' rect=', dbgs(rcClip)]);
{$ENDIF}
  Include(fStateFlags, sfPainting);
  Exclude(fStateFlags, sfHasScrolled);
  // Now paint everything while the caret is hidden.
  FScreenCaret.Hide;
  try
//    FPaintArea.Paint(Canvas, rcClip);
    DoOnPaint;
  finally
{$IFDEF EnableDoubleBuf}
    EndPaintBuffer(rcClip);
{$ENDIF}
    UpdateCaret;
    Exclude(fStateFlags, sfPainting);
  end;
end;

procedure TCustomSynEdit.CodeFoldAction(iLine: Integer);
// iLine is 1 based as parameter
begin
  if (iLine <= 0) or (iLine > FTheLinesView.Count) then
    exit;
  dec(iLine);
  // DebugLn(['****** FoldAction at ',iLine,' scrline=',FFoldedLinesView.TextIndexToScreenLine(iLine), ' type ', SynEditCodeFoldTypeNames[FFoldedLinesView.FoldType[FFoldedLinesView.TextIndexToScreenLine(iLine)]],  '  view topline=',FFoldedLinesView.TopLine  ]);
  if FFoldedLinesView.FoldType[FFoldedLinesView.TextIndexToScreenLine(iLine)] * [cfCollapsedFold, cfCollapsedHide] <> []
  then
    FFoldedLinesView.UnFoldAtTextIndex(iLine)
  else if FFoldedLinesView.FoldType[FFoldedLinesView.TextIndexToScreenLine(iLine)] * [cfFoldStart] <> [] then
    FFoldedLinesView.FoldAtTextIndex(iLine);
end;

function TCustomSynEdit.FindNextUnfoldedLine(iLine: Integer; Down: boolean): Integer;
// iLine is 1 based
begin
  Result := iLine;
  if Down then
    while (Result < FTheLinesView.Count) and (FFoldedLinesView.FoldedAtTextIndex[Result - 1]) do
      inc(Result)
  else
    while (Result > 1) and (FFoldedLinesView.FoldedAtTextIndex[Result - 1]) do
      dec(Result);
end;

function TCustomSynEdit.CreateGutter(AOwner: TSynEditBase; ASide: TSynGutterSide; ATextDrawer: TheTextDrawer)
  : TSynGutter;
begin
  Result := TSynGutter.Create(AOwner, ASide, ATextDrawer);
end;

procedure TCustomSynEdit.UnfoldAll;
begin
  FFoldedLinesView.UnfoldAll;
//  Invalidate;
end;

procedure TCustomSynEdit.FoldAll(StartLevel: Integer = 0; IgnoreNested: boolean = False);
begin
  FFoldedLinesView.FoldAll(StartLevel, IgnoreNested);
//  Invalidate;
end;

procedure TCustomSynEdit.StartPaintBuffer(const ClipRect: TRect);
{$IFDEF EnableDoubleBuf}
var
  NewBufferWidth: Integer;
  NewBufferHeight: Integer;
{$ENDIF}
begin
{$IFDEF EnableDoubleBuf}
  if (SavedCanvas <> nil) then
    RaiseGDBException('');
  if BufferBitmap = nil then
    BufferBitmap := TBitmap.Create;
  NewBufferWidth := BufferBitmap.Width;
  NewBufferHeight := BufferBitmap.Height;
  if NewBufferWidth < ClipRect.Right then
    NewBufferWidth := ClipRect.Right;
  if NewBufferHeight < ClipRect.Bottom then
    NewBufferHeight := ClipRect.Bottom;
  BufferBitmap.Width := NewBufferWidth;
  BufferBitmap.Height := NewBufferHeight;
  SavedCanvas := Canvas;
  Canvas := BufferBitmap.Canvas;
{$ENDIF}
end;

procedure TCustomSynEdit.EndPaintBuffer(const ClipRect: TRect);
begin
{$IFDEF EnableDoubleBuf}
  if (SavedCanvas = nil) then
    RaiseGDBException('');
  if not(SavedCanvas is TControlCanvas) then
    RaiseGDBException('');
  Canvas := SavedCanvas;
  SavedCanvas := nil;
  Canvas.CopyRect(ClipRect, BufferBitmap.Canvas, ClipRect);
{$ENDIF}
end;

function TCustomSynEdit.NextWordLogicalPos(ABoundary: TLazSynWordBoundary; WordEndForDelete: boolean): TPoint;
var
  i, j, CX, CY, NX, OX, LineLen: Integer;
  Line, ULine: string;
  CWidth: TPhysicalCharWidths;
  r, InclCurrent: boolean;
begin
  Result := LogicalCaretXY;
  CX := Result.X;
  CY := Result.Y;

  if (CY < 1) then
  begin
    Result.X := 1;
    Result.Y := 1;
    exit;
  end;
  i := FTheLinesView.Count;
  if (CY > i) or ((CY = i) and (CX > length(FTheLinesView[i - 1]))) then
  begin
    Result.Y := i;
    Result.X := length(FTheLinesView[Result.Y - 1]) + 1;
    exit;
  end;

  Line := FTheLinesView[CY - 1];
  InclCurrent := False;
  LineLen := length(Line);
  if CX > LineLen then
  begin
    Line := FTheLinesView[CY];
    LineLen := length(Line);
    inc(CY);
    CX := 1;
    InclCurrent := True;
  end;

  case ABoundary of
    swbWordBegin:
      begin
        CX := WordBreaker.NextWordStart(Line, CX, InclCurrent);
        if (CX <= 0) and not InclCurrent then
          CX := LineLen + 1;
        if (CX <= 0) and InclCurrent then
          CX := 1;
      end;
    swbWordEnd:
      begin
        CX := WordBreaker.NextWordEnd(Line, CX);
        if (CX <= 0) then
          CX := LineLen + 1;
      end;
    swbTokenBegin:
      begin
        if not(InclCurrent and ((CX <= 1) or (Line[CX - 1] in FWordBreaker.WhiteChars)) and
          ((CX > LineLen) or not(Line[CX] in FWordBreaker.WhiteChars))) then
          CX := WordBreaker.NextBoundary(Line, CX);
        if (CX > 0) and (CX <= LineLen) and (Line[CX] in FWordBreaker.WhiteChars) then
          CX := WordBreaker.NextBoundary(Line, CX);
        if (CX <= 0) then
          CX := LineLen + 1;
      end;
    swbTokenEnd:
      begin
        CX := WordBreaker.NextBoundary(Line, CX);
        if (CX > 1) and (Line[CX - 1] in FWordBreaker.WhiteChars) then
          CX := WordBreaker.NextBoundary(Line, CX);
        if (CX <= 0) then
          CX := LineLen + 1;
      end;
    swbCaseChange:
      begin
        NX := WordBreaker.NextWordStart(Line, CX, InclCurrent);
        if (NX <= 0) and not InclCurrent then
          NX := LineLen + 1;
        if (NX <= 0) and InclCurrent then
          NX := 1;

        ULine := UpperCase(Line);
        CWidth := FTheLinesView.GetPhysicalCharWidths(CY - 1); // for utf 8
        OX := CX;
        i := length(ULine);
        // skip upper
        While (CX < NX) and (CX <= i) do
        begin // check entire next utf-8 char to be equal
          r := (CX = OX) or (CX <= 1) or (Line[CX - 1] <> '_') or ((CX <= i) and (Line[CX] = '_'));
          j := CX;
          repeat
            r := r and (Line[j] = ULine[j]);
            inc(j);
          until (j > i) or (CWidth[j - 1] <> 0);
          if not r then
            break;
          CX := j;
        end;
        // skip lowercase
        ULine := LowerCase(Line);
        While (CX < NX) and (CX <= i) do
        begin // check entire next utf-8 char to be equal
          r := (CX = OX) or (CX <= 1) or (Line[CX - 1] <> '_') or ((CX <= i) and (Line[CX] = '_'));
          j := CX;
          repeat
            r := r and (Line[j] = ULine[j]);
            inc(j);
          until (j > i) or (CWidth[j - 1] <> 0);
          if not r then
            break;
          CX := j;
        end;
      end;
  end;

  Result := Point(CX, CY);
end;

function TCustomSynEdit.PrevWordLogicalPos(ABoundary: TLazSynWordBoundary): TPoint;

  procedure CheckLineStart(var CX, CY: Integer);
  var
    Line: String;
  begin
    if CX <= 0 then
      if CY > 1 then
      begin
        // just position at the end of the previous line
        // Todo skip spaces
        dec(CY);
        Line := FTheLinesView[CY - 1];
        CX := length(Line) + 1;
      end
      else
        CX := 1;
  end;

var
  i, j, CX, CY, NX, OX: Integer;
  Line, ULine: string;
  CWidth: TPhysicalCharWidths;
  r: boolean;
begin
  Result := LogicalCaretXY;
  CX := Result.X;
  CY := Result.Y;

  if (CY < 1) then
  begin
    Result.X := 1;
    Result.Y := 1;
    exit;
  end;
  if (CY > FTheLinesView.Count) then
  begin
    Result.Y := FTheLinesView.Count;
    Result.X := length(FTheLinesView[Result.Y - 1]) + 1;
    exit;
  end;

  Line := FTheLinesView[CY - 1];

  case ABoundary of
    swbWordBegin:
      begin
        CX := WordBreaker.PrevWordStart(Line, Min(CX, length(Line) + 1));
        CheckLineStart(CX, CY);
      end;
    swbWordEnd:
      begin
        CX := WordBreaker.PrevWordEnd(Line, Min(CX, length(Line) + 1));
        CheckLineStart(CX, CY);
      end;
    swbTokenBegin:
      begin
        CX := WordBreaker.PrevBoundary(Line, Min(CX, length(Line) + 1));
        if (CX > 0) and (Line[CX] in FWordBreaker.WhiteChars) then
          CX := WordBreaker.PrevBoundary(Line, Min(CX, length(Line) + 1));
        if CX = 1 then
          CX := -1;
        CheckLineStart(CX, CY);
      end;
    swbTokenEnd:
      begin
        CX := WordBreaker.PrevBoundary(Line, Min(CX, length(Line) + 1));
        if (CX > 1) and (Line[CX - 1] in FWordBreaker.WhiteChars) then
          CX := WordBreaker.PrevBoundary(Line, Min(CX, length(Line) + 1));
        if CX = 1 then
          CX := -1;
        CheckLineStart(CX, CY);
      end;
    swbCaseChange:
      begin
        NX := WordBreaker.PrevWordStart(Line, Min(CX, length(Line) + 1));

        ULine := LowerCase(Line);
        CWidth := FTheLinesView.GetPhysicalCharWidths(CY - 1); // for utf 8
        OX := CX;
        i := length(ULine);
        if CX > i + 1 then
          CX := i + 1;
        // skip lowercase
        While (CX > NX) and (CX - 1 > 0) do
        begin // check entire previous utf-8 char to be equal
          r := (CX = OX) or (Line[CX - 1] <> '_') or ((CX <= i) and (Line[CX] = '_'));
          j := CX;
          repeat
            dec(j);
            r := r and (Line[j] = ULine[j]);
          until (j < 1) or (CWidth[j - 1] <> 0);
          if not r then
            break;
          CX := j;
        end;
        // skip upper
        While (CX > NX) and (CX - 1 > 0) do
        begin // check entire previous utf-8 char to be not equal
          j := CX;
          r := True;
          repeat
            dec(j);
            r := r and (Line[j] = ULine[j]);
          until (j < 1) or (CWidth[j - 1] <> 0);
          r := r or not((CX = OX) or (Line[CX - 1] <> '_') or ((CX <= i) and (Line[CX] = '_')));
          if r then
            break;
          CX := j;
        end;
        if (CX - 1 < 1) then
          CX := NX;
        CheckLineStart(CX, CY);
      end;
  end;

  Result := Point(CX, CY);
end;

{procedure TCustomSynEdit.EraseBackground(DC: HDC);
begin
  // we are painting everything ourselves, so not need to erase background
end;

procedure TCustomSynEdit.Update;
begin
  Invalidate;
end;

procedure TCustomSynEdit.Invalidate;
begin
  inherited Invalidate;
end;
}
function TCustomSynEdit.PluginCount: Integer;
begin
  Result := FPlugins.Count;
end;

function TCustomSynEdit.MarkupCount: Integer;
begin
  Result := fMarkupManager.Count;
end;

procedure TCustomSynEdit.PasteFromClipboard;
//var
//  ClipHelper: TSynClipboardStream;
begin
//  ClipHelper := TSynClipboardStream.Create;
//  try
//    ClipHelper.ReadFromClipboard(Clipboard);
//    PasteFromClipboardEx(ClipHelper);
//  finally
//    ClipHelper.Free;
//  end;
end;
(*
function TCustomSynEdit.PasteFromClipboardEx(ClipHelper: TSynClipboardStream): boolean;
var
  PTxt: PChar;
  PStr: String;
  PMode: TSynSelectionMode;
  InsStart: TPoint;
  PasteAction: TSynCopyPasteAction;
begin
  Result := False;
  InternalBeginUndoBlock;
  try
    PTxt := ClipHelper.TextP;
    PMode := ClipHelper.SelectionMode;
    PasteAction := scaContinue;
    if assigned(FOnPaste) then
    begin
      if ClipHelper.IsPlainText then
        PasteAction := scaPlainText;
      InsStart := FCaret.LineBytePos;
      if SelAvail and (not FBlockSelection.Persistent) and (eoOverwriteBlock in FOptions2) then
        InsStart := FBlockSelection.FirstLineBytePos;
      PStr := PTxt;
      FOnPaste(self, PStr, PMode, InsStart, PasteAction);
      PTxt := PChar(PStr);
      if (PStr = '') or (PasteAction = scaAbort) then
        exit;
    end;

    if ClipHelper.TextP = nil then
      exit;

    Result := True;
    if SelAvail and (not FBlockSelection.Persistent) and (eoOverwriteBlock in FOptions2) then
      FBlockSelection.SelText := '';
    InsStart := FCaret.LineBytePos;
    FInternalBlockSelection.StartLineBytePos := InsStart;
    FInternalBlockSelection.SetSelTextPrimitive(PMode, PTxt);
    FCaret.LineBytePos := FInternalBlockSelection.StartLineBytePos;

    if PasteAction = scaPlainText then
      exit;

    if eoFoldedCopyPaste in FOptions2 then
    begin
      PTxt := ClipHelper.GetTagPointer(synClipTagFold);
      if PTxt <> nil then
      begin
        ScanRanges;
        FFoldedLinesView.ApplyFoldDescription(InsStart.Y - 1, InsStart.X, FInternalBlockSelection.StartLinePos - 1,
          FInternalBlockSelection.StartBytePos, PTxt, ClipHelper.GetTagLen(synClipTagFold));
      end;
    end;
  finally
    InternalEndUndoBlock;
  end;
end;
  *)
procedure TCustomSynEdit.SelectAll;
var
  LastPt: TPoint;
begin
  DoIncPaintLock(self); // No editing is taking place
  LastPt := Point(1, FTheLinesView.Count);
  if LastPt.Y > 0 then
    inc(LastPt.X, length(FTheLinesView[LastPt.Y - 1]))
  else
    LastPt.Y := 1;
  SetCaretAndSelection(LogicalToPhysicalPos(LastPt), Point(1, 1), LastPt);
  FBlockSelection.ActiveSelectionMode := smNormal;
  DoDecPaintLock(self);
end;

procedure TCustomSynEdit.SetHighlightSearch(const ASearch: String; AOptions: TSynSearchOptions);
begin
  fMarkupHighAll.SearchOptions := AOptions;
  fMarkupHighAll.SearchString := ASearch;
end;

procedure TCustomSynEdit.SelectToBrace;
begin
  FindMatchingBracket(CaretXY, True, True, True, False);
end;

procedure TCustomSynEdit.SelectWord;
begin
  SetWordBlock(LogicalCaretXY);
end;

procedure TCustomSynEdit.SelectLine(WithLeadSpaces: boolean = True);
begin
  SetLineBlock(CaretXY, WithLeadSpaces);
end;

procedure TCustomSynEdit.SelectParagraph;
begin
  SetParagraphBlock(CaretXY);
end;

procedure TCustomSynEdit.DoBlockSelectionChanged(Sender: TObject);
begin
  StatusChanged([scSelection]);
  if {HandleAllocated and }IsFocused then
    SelAvailChange(nil);
end;

procedure TCustomSynEdit.SetBlockBegin(Value: TPoint); // logical position (byte)
begin
  FBlockSelection.StartLineBytePos := Value;
end;

procedure TCustomSynEdit.SetBlockEnd(Value: TPoint); // logical position (byte)
begin
  FBlockSelection.EndLineBytePos := Value;
end;

procedure TCustomSynEdit.SetBlockIndent(const AValue: Integer);
begin
  if FBlockIndent = AValue then
    exit;
  FBlockIndent := AValue;
end;

function TCustomSynEdit.GetCaretX: Integer;
begin
  Result := FCaret.CharPos;
end;

function TCustomSynEdit.GetCaretY: Integer;
begin
  Result := FCaret.LinePos;
end;

function TCustomSynEdit.GetCaretUndo: TSynEditUndoItem;
begin
  if SelAvail then
    Result := TSynEditUndoSelCaret.Create(FCaret.LineCharPos, FBlockSelection.StartLineBytePos,
      FBlockSelection.EndLineBytePos, FBlockSelection.ActiveSelectionMode)
  else
    Result := TSynEditUndoCaret.Create(FCaret.LineCharPos);
end;

function TCustomSynEdit.GetMarkup(Index: Integer): TSynEditMarkup;
begin
  Result := fMarkupManager.Markup[Index];
end;

procedure TCustomSynEdit.SetCaretX(const Value: Integer);
begin
  FCaret.ChangeOnTouch; // setting the caret always clears selection (even setting to current pos / no change)
  FCaret.CharPos := Value;
end;

procedure TCustomSynEdit.SetCaretY(const Value: Integer);
begin
  FCaret.ChangeOnTouch; // setting the caret always clears selection (even setting to current pos / no change)
  FCaret.LinePos := Value;
end;

function TCustomSynEdit.GetCaretXY: TPoint;
begin
  Result := FCaret.LineCharPos;
end;

function TCustomSynEdit.GetFoldedCodeColor: TSynSelectedColor;
begin
  Result := FFoldedLinesView.MarkupInfoFoldedCode;
end;

function TCustomSynEdit.GetLines: TStrings;
begin
  Result := FStrings;
end;

procedure TCustomSynEdit.SetCaretXY(Value: TPoint);
// physical position (screen)
begin
  FCaret.ChangeOnTouch; // setting the caret always clears selection (even setting to current pos / no change)
  FCaret.LineCharPos := Value;
end;

procedure TCustomSynEdit.CaretChanged(Sender: TObject);
begin
  Include(fStateFlags, sfCaretChanged);
  if FCaret.OldCharPos <> FCaret.CharPos then
    Include(fStatusChanges, scCaretX);
  if FCaret.OldLinePos <> FCaret.LinePos then
  begin
    Include(fStatusChanges, scCaretY);
    InvalidateGutterLines(FCaret.OldLinePos, FCaret.OldLinePos);
    InvalidateGutterLines(FCaret.LinePos, FCaret.LinePos);
  end;
  EnsureCursorPosVisible;
  if FPaintLock = 0 then
    fMarkupHighCaret.CheckState; // Todo need a global lock, including the markup
end;

function TCustomSynEdit.CurrentMaxLeftChar: Integer;
begin
//  if not HandleAllocated then // don't know chars in window yet
//    exit(MaxInt);
  Result := FTheLinesView.LengthOfLongestLine;
  if (eoScrollPastEol in Options) and (Result < fMaxLeftChar) then
    Result := fMaxLeftChar;
  Result := Result - CharsInWindow + 1 + FScreenCaret.ExtraLineChars;
end;

function TCustomSynEdit.CurrentMaxLineLen: Integer;
begin
//  if not HandleAllocated then // don't know chars in window yet
//    exit(MaxInt);
  Result := FTheLinesView.LengthOfLongestLine + 1;
  if (eoScrollPastEol in Options) and (Result < fMaxLeftChar) then
    Result := fMaxLeftChar;
end;

procedure TCustomSynEdit.SetLeftChar(Value: Integer);
begin
  Value := Min(Value, CurrentMaxLeftChar);
  Value := Max(Value, 1);
  if Value <> FTextArea.LeftChar then
  begin
    FTextArea.LeftChar := Value;
    UpdateScrollBars;
    InvalidateLines(-1, -1);
    StatusChanged([scLeftChar]);
  end;
end;

procedure TCustomSynEdit.SetLines(Value: TStrings);
begin
//  if HandleAllocated then
    FStrings.Assign(Value);
end;

function TCustomSynEdit.GetMarkupMgr: TObject;
begin
  Result := fMarkupManager;
end;

function TCustomSynEdit.GetCaretObj: TSynEditCaret;
begin
  Result := FCaret;
end;

procedure TCustomSynEdit.SetLineText(Value: string);
begin
  FCaret.LineText := Value;
end;

{procedure TCustomSynEdit.SetName(const Value: TComponentName);
var
  TextToName: boolean;
begin
  TextToName := (ComponentState * [csDesigning, csLoading] = [csDesigning]) and (TrimRight(Text) = Name);
  inherited SetName(Value);
  if TextToName then
    Text := Value;
end;}

{procedure TCustomSynEdit.CreateHandle;
begin
  Application.RemoveOnIdleHandler(@IdleScanRanges);
  DoIncPaintLock(nil);
  try
    inherited CreateHandle; // SizeOrFontChanged will be called
    FLeftGutter.RecalcBounds;
    FRightGutter.RecalcBounds;
    UpdateScrollBars;
  finally
    DoDecPaintLock(nil);
  end;
end;}

{ procedure TCustomSynEdit.SetScrollBars(const Value: TScrollStyle);
  begin
  if (FScrollBars <> Value) then begin
  FScrollBars := Value;
  UpdateScrollBars;
  Invalidate;
  end;
  end; }

procedure TCustomSynEdit.SetSelTextPrimitive(PasteMode: TSynSelectionMode; Value: PChar;
  AddToUndoList: boolean = False);
Begin
  IncPaintLock;
  if not AddToUndoList then
  begin
    fUndoList.Lock;
    fRedoList.Lock;
  end;
  try
    FBlockSelection.SetSelTextPrimitive(PasteMode, Value);
  finally
    if not AddToUndoList then
    begin
      fUndoList.Unlock;
      fRedoList.Unlock;
    end;
    DecPaintLock;
  end;
end;

procedure TCustomSynEdit.SetSelTextExternal(const Value: string);
begin
  // undo entry added
  InternalBeginUndoBlock;
  try
    FBlockSelection.SelText := Value;
  finally
    InternalEndUndoBlock;
  end;
end;

procedure TCustomSynEdit.SynSetText(const Value: string);
begin
  FLines.Text := Value;
end;

{procedure TCustomSynEdit.RealSetText(const Value: TCaption);
begin
  FLines.Text := Value; // Do not trim
end;}

function TCustomSynEdit.CurrentMaxTopView: Integer;
begin
  Result := FFoldedLinesView.Count;
  if not(eoScrollPastEof in Options) then
    Result := Result + 1 - Max(0, LinesInWindow);
  Result := Max(Result, 1);
end;

procedure TCustomSynEdit.SetTopLine(Value: Integer);
var
  NewTopView: Integer;
begin
  // TODO : Above hidden line only if folded, if hidden then use below
  if FFoldedLinesView.FoldedAtTextIndex[Value - 1] then
    Value := FindNextUnfoldedLine(Value, False);
  if FFoldedLinesView.FoldedAtTextIndex[Value - 1] then
    Value := FindNextUnfoldedLine(Value, True);

  NewTopView := FFoldedLinesView.TextIndexToViewPos(Value - 1);
  if NewTopView <> TopView then
  begin
    TopView := NewTopView;
  end;
end;

procedure TCustomSynEdit.ScrollAfterTopLineChanged;
var
  Delta: Integer;
  srect: TRect;
begin
  if (sfPainting in fStateFlags) or (FPaintLock <> 0){ or (not HandleAllocated) }then
    exit;
  Delta := FOldTopView - TopView;
{$IFDEF SYNSCROLLDEBUG}
  if (sfHasScrolled in fStateFlags) then
    debugln(['ScrollAfterTopLineChanged with sfHasScrolled Delta=', Delta, ' topline=', TopLine, '  FOldTopView=',
      FOldTopView]);
{$ENDIF}
  if Delta <> 0 then
  begin
    // TODO: SW_SMOOTHSCROLL --> can't get it work
    if (Abs(Delta) >= LinesInWindow) or (sfHasScrolled in fStateFlags) then
    begin
{$IFDEF SYNSCROLLDEBUG}
      debugln(['ScrollAfterTopLineChanged does invalidet Delta=', Delta]);
{$ENDIF}
//      Invalidate;
    end
    else
    begin
      srect := FPaintArea.Rect;
      srect.Top := FTextArea.TextBounds.Top;
      srect.Bottom := FTextArea.TextBounds.Bottom;
//      if ScrollWindowEx(Handle, 0, LineHeight * Delta, @srect, @srect, 0, nil, SW_INVALIDATE) then
//      begin
//{$IFDEF SYNSCROLLDEBUG}
//        debugln(['ScrollAfterTopLineChanged did scroll Delta=', Delta]);
//{$ENDIF}
//        Include(fStateFlags, sfHasScrolled);
//      end
//      else
      begin
//        Invalidate; // scrollwindow failed, invalidate all
{$IFDEF SYNSCROLLDEBUG}
        debugln(['ScrollAfterTopLineChanged does invalidet (scroll failed) Delta=', Delta]);
{$ENDIF}
      end;
    end;
  end;
  FOldTopView := TopView;
  if (Delta <> 0) and (eoAlwaysVisibleCaret in FOptions2) then
    MoveCaretToVisibleArea;
end;

procedure TCustomSynEdit.MoveCaretToVisibleArea;
// scroll to make the caret visible
var
  NewCaretXY: TPoint;
  MaxY: Longint;
begin
{$IFDEF SYNDEBUG}
  if (sfEnsureCursorPos in fStateFlags) then
    debugln('SynEdit. skip MoveCaretToVisibleArea');
{$ENDIF}
  if {(not HandleAllocated) or }(sfEnsureCursorPos in fStateFlags) then
    exit;

  NewCaretXY := CaretXY;
  if NewCaretXY.X < LeftChar then
    NewCaretXY.X := LeftChar
  else if NewCaretXY.X > LeftChar + CharsInWindow - FScreenCaret.ExtraLineChars then
    NewCaretXY.X := LeftChar + CharsInWindow - FScreenCaret.ExtraLineChars;
  if NewCaretXY.Y < TopLine then
    NewCaretXY.Y := TopLine
  else
  begin
    MaxY := ScreenRowToRow(Max(0, LinesInWindow - 1));
    if NewCaretXY.Y > MaxY then
      NewCaretXY.Y := MaxY;
  end;
  if CompareCarets(CaretXY, NewCaretXY) <> 0 then
  begin
    // DebugLn(['TCustomSynEdit.MoveCaretToVisibleArea Old=',dbgs(CaretXY),' New=',dbgs(NewCaretXY)]);
    FCaret.LineCharPos := NewCaretXY;
  end;
end;

procedure TCustomSynEdit.MoveCaretIgnoreEOL(const NewCaret: TPoint);
begin
  FCaret.IncForcePastEOL;
  FCaret.LineCharPos := NewCaret;
  FCaret.DecForcePastEOL;
end;

procedure TCustomSynEdit.MoveLogicalCaretIgnoreEOL(const NewLogCaret: TPoint);
begin
  MoveCaretIgnoreEOL(LogicalToPhysicalPos(NewLogCaret));
end;

procedure TCustomSynEdit.UpdateCaret(IgnorePaintLock: boolean = False);
{$IFDEF WinIME}
var
  cf: CompositionForm;
  imc: HIMC;
{$ENDIF}
begin
  if ((PaintLock <> 0) and not IgnorePaintLock){ or (not HandleAllocated) }then
  begin
    Include(fStateFlags, sfCaretChanged);
  end
  else
  begin
    Exclude(fStateFlags, sfCaretChanged);
    if eoAlwaysVisibleCaret in FOptions2 then
      MoveCaretToVisibleArea;

    FScreenCaret.DisplayPos := Point(CaretXPix, CaretYPix);

{$IFDEF WinIME}
    if HandleAllocated then
    begin
      cf.dwStyle := CFS_POINT;
      cf.ptCurrentPos := Point(CaretXPix, CaretYPix);
      imc := ImmGetContext(TWinControl(Owner).Handle);
      ImmSetCompositionWindow(imc, @cf);
      ImmReleaseContext(TWinControl(Owner).Handle, imc);
    end;
{$ENDIF}
  end;
end;

procedure TCustomSynEdit.UpdateScrollBars;
//var
//  ScrollInfo: TScrollInfo;
begin
  if FScrollBarUpdateLock <> 0 then
    exit;
  if {not HandleAllocated or }(PaintLock <> 0) then
    Include(fStateFlags, sfScrollbarChanged)
  else
  begin
    Exclude(fStateFlags, sfScrollbarChanged);
//    ScrollInfo.cbSize := SizeOf(ScrollInfo);
//    ScrollInfo.fMask := SIF_ALL or SIF_DISABLENOSCROLL and not SIF_TRACKPOS;
//    ScrollInfo.nMin := 1;
//    ScrollInfo.nTrackPos := 0;

    // Horizontal
//    ScrollInfo.nMax := FTheLinesView.LengthOfLongestLine + 1;
//    if (eoScrollPastEol in Options) and (ScrollInfo.nMax < fMaxLeftChar + 1) then
//      ScrollInfo.nMax := fMaxLeftChar + 1;
//    inc(ScrollInfo.nMax, FScreenCaret.ExtraLineChars);
    { if ((fScrollBars in [ssBoth, ssHorizontal]) or
      ((fScrollBars in [ssAutoBoth, ssAutoHorizontal]) and (ScrollInfo.nMax - 1 > CharsInWindow))
      ) xor (sfHorizScrollbarVisible in fStateFlags)
      then begin
      if (sfHorizScrollbarVisible in fStateFlags)
      then exclude(fStateFlags, sfHorizScrollbarVisible)
      else include(fStateFlags, sfHorizScrollbarVisible);
      if fStateFlags * [sfEnsureCursorPos, sfEnsureCursorPosAtResize] <> [] then
      include(fStateFlags, sfEnsureCursorPosAtResize);
      ShowScrollBar(Handle, SB_Horz, sfHorizScrollbarVisible in fStateFlags);
      RecalcCharsAndLinesInWin(True);
      end; }
//    ScrollInfo.nPage := CharsInWindow;
//    ScrollInfo.nPos := LeftChar;
//    SetScrollInfo(Handle, SB_HORZ, ScrollInfo, True);
{$IFNDEF LCLWin32} {$IFNDEF SynScrollBarWorkaround}
//    if not(sfHorizScrollbarVisible in fStateFlags) then
//      ShowScrollBar(Handle, SB_HORZ, False);
{$ENDIF} {$ENDIF}
    // DebugLn('[TCustomSynEdit.UpdateScrollbars] nMin=',ScrollInfo.nMin,' nMax=',ScrollInfo.nMax,
    // ' nPage=',ScrollInfo.nPage,' nPos=',ScrollInfo.nPos,' ClientW=',ClientWidth);

    // Vertical
//    ScrollInfo.nMax := FFoldedLinesView.Count + 1;
//    if (eoScrollPastEof in Options) then
//      inc(ScrollInfo.nMax, LinesInWindow - 1);
    { if ((fScrollBars in [ssBoth, ssVertical]) or
      ((fScrollBars in [ssAutoBoth, ssAutoVertical]) and (ScrollInfo.nMax - 1 > LinesInWindow))
      ) xor (sfVertScrollbarVisible in fStateFlags)
      then begin
      if (sfVertScrollbarVisible in fStateFlags)
      then exclude(fStateFlags, sfVertScrollbarVisible)
      else include(fStateFlags, sfVertScrollbarVisible);
      if fStateFlags * [sfEnsureCursorPos, sfEnsureCursorPosAtResize] <> [] then
      include(fStateFlags, sfEnsureCursorPosAtResize);
      ShowScrollBar(Handle, SB_Vert, sfVertScrollbarVisible in fStateFlags);
      RecalcCharsAndLinesInWin(True);
      end; }
//    ScrollInfo.nPage := LinesInWindow;
//    ScrollInfo.nPos := TopView;
//    SetScrollInfo(Handle, SB_VERT, ScrollInfo, True);
{$IFNDEF LCLWin32} {$IFNDEF SynScrollBarWorkaround}
//    if not(sfVertScrollbarVisible in fStateFlags) then
//      ShowScrollBar(Handle, SB_VERT, False);
{$ENDIF} {$ENDIF}
  end;
end;

procedure TCustomSynEdit.SelAvailChange(Sender: TObject);
begin
  if PaintLock > 0 then
  begin
    Include(fStateFlags, sfSelChanged);
    exit;
  end;
  Exclude(fStateFlags, sfSelChanged);
  if SelAvail then
    AquirePrimarySelection
  else
    SurrenderPrimarySelection;
end;

procedure TCustomSynEdit.WMDropFiles(var Msg: TMessage);
{$IFNDEF SYN_LAZARUS}
// ToDo DropFiles
var
  i, iNumberDropped: Integer;
  szPathName: array [0 .. 260] of Char;
  Point: TPoint;
  FilesList: TStringList;
{$ENDIF}
begin
{$IFDEF SYN_LAZARUS}
  LastMouseCaret := Point(-1, -1);
{$ELSE}
  try
    if assigned(fOnDropFiles) then
    begin
      FilesList := TStringList.Create;
      try
//        iNumberDropped := DragQueryFile(THandle(Msg.WParam), Cardinal(-1), nil, 0);
//        DragQueryPoint(THandle(Msg.WParam), Point);

        for i := 0 to iNumberDropped - 1 do
        begin
//          DragQueryFile(THandle(Msg.WParam), i, szPathName, SizeOf(szPathName));
          FilesList.Add(szPathName);
        end;
        fOnDropFiles(self, Point.X, Point.Y, FilesList);
      finally
        FilesList.Free;
      end;
    end;
  finally
    Msg.Result := 0;
//    DragFinish(THandle(Msg.WParam));
  end;
{$ENDIF}
end;

procedure TCustomSynEdit.WMExit(var Message: TWMClose);
begin
  LastMouseCaret := Point(-1, -1);
end;

procedure TCustomSynEdit.WMEraseBkgnd(var Msg: TMessage);
begin
  Msg.Result := 1;
end;

{procedure TCustomSynEdit.WMGetDlgCode(var Msg: TWMGetDlgCode);
begin
  inherited;
  Msg.Result := DLGC_WANTARROWS or DLGC_WANTCHARS or DLGC_WANTALLKEYS;
  if fWantTabs and (GetKeyState(VK_CONTROL) >= 0) then
    Msg.Result := Msg.Result or DLGC_WANTTAB;
end;}

procedure TCustomSynEdit.WMHScroll(var Msg: {$IFDEF SYN_LAZARUS}TLMScroll{$ELSE}TWMScroll{$ENDIF});
begin
//  case Msg.ScrollCode of
    // Scrolls to start / end of the line
////    SB_TOP:
////      LeftChar := 1;
//    SB_BOTTOM:
//      LeftChar := CurrentMaxLeftChar;
    // Scrolls one char left / right
//    SB_LINEDOWN:
//      LeftChar := LeftChar + 1;
//    SB_LINEUP:
//      LeftChar := LeftChar - 1;
    // Scrolls one page of chars left / right
//    SB_PAGEDOWN:
//      LeftChar := LeftChar + Max(1, (CharsInWindow - ord(eoScrollByOneLess in FOptions)));
//    SB_PAGEUP:
//      LeftChar := LeftChar - Max(1, (CharsInWindow - ord(eoScrollByOneLess in FOptions)));
    // Scrolls to the current scroll bar position
//    SB_THUMBPOSITION, SB_THUMBTRACK:
//      LeftChar := Msg.Pos;
//  end;
end;

procedure TCustomSynEdit.WMKillFocus(var Msg: TWMKillFocus);
begin
  if FCaret = nil then
    exit; // This SynEdit is in Destroy
  Exclude(fStateFlags, sfHideCursor);
  inherited;
{$IFDEF VerboseFocus}
  debugln(['[TCustomSynEdit.WMKillFocus] A ', Name, ' time=', dbgs(Now * 86640)]);
{$ENDIF}
  LastMouseCaret := Point(-1, -1);
  // Todo: Under Windows, keeping the Caret only works, if no other component creates a caret
  if not(eoPersistentCaret in FOptions) then
  begin
    FScreenCaret.Visible := False;
    FScreenCaret.DestroyCaret;
  end;
//  if fHideSelection and SelAvail then
//    Invalidate;
  inherited;
end;

procedure TCustomSynEdit.WMSetFocus(var Msg: TWMSetFocus);
begin
  if FCaret = nil then
    exit; // This SynEdit is in Destroy
  Exclude(fStateFlags, sfHideCursor);
  LastMouseCaret := Point(-1, -1);
{$IFDEF VerboseFocus}
  debugln(['[TCustomSynEdit.WMSetFocus] A ', Name, ':', ClassName, ' time=', dbgs(Now * 86640)]);
{$ENDIF}
  FScreenCaret.DestroyCaret;
  // Ensure recreation. On Windows only one caret exists, and it must be moved to the focused editor
  FScreenCaret.Visible := True;
  // if FHideSelection and SelAvail then
  // Invalidate;
  inherited;
  // DebugLn('[TCustomSynEdit.WMSetFocus] END');
end;

{procedure TCustomSynEdit.DoOnResize;
begin
  inherited;
  if (not HandleAllocated) or ((ClientWidth = FOldWidth) and (ClientHeight = FOldHeight)) then
    exit;
  FOldWidth := ClientWidth;
  FOldHeight := ClientHeight;
  inc(FScrollBarUpdateLock);
  FScreenCaret.Lock;
  try
    FLeftGutter.RecalcBounds;
    FRightGutter.RecalcBounds;
    SizeOrFontChanged(False);
    if sfEnsureCursorPosAtResize in fStateFlags then
      EnsureCursorPosVisible;
    Exclude(fStateFlags, sfEnsureCursorPosAtResize);
  finally
    FScreenCaret.Unlock;
    dec(FScrollBarUpdateLock);
    UpdateScrollBars;
  end;
  // debugln('TCustomSynEdit.Resize ',dbgs(Width),',',dbgs(Height),',',dbgs(ClientWidth),',',dbgs(ClientHeight));
  // SetLeftChar(LeftChar);                                                     //mh 2000-10-19
end;}

{var
  ScrollHintWnd: THintWindow;}

{function GetScrollHint: THintWindow;
begin
  if ScrollHintWnd = nil then
  begin
    ScrollHintWnd := HintWindowClass.Create(Application);
    ScrollHintWnd.Name := 'SynEditScrollHintWnd';
    ScrollHintWnd.Visible := False;
  end;
  Result := ScrollHintWnd;
  Result.AutoHide := True; // Because SB_ENDSCROLL never happens under LCL-GTK2
  Result.HideInterval := 1500;
end;}

procedure TCustomSynEdit.WMVScroll(var Msg: {$IFDEF SYN_LAZARUS}TLMScroll{$ELSE}TWMScroll{$ENDIF});
{var
  s: ShortString;
  rc: TRect;
  pt: TPoint;
  ScrollHint: THintWindow;}
begin
  // debugln('TCustomSynEdit.WMVScroll A ',DbgSName(Self),' Msg.ScrollCode=',dbgs(Msg.ScrollCode),' SB_PAGEDOWN=',dbgs(SB_PAGEDOWN),' SB_PAGEUP=',dbgs(SB_PAGEUP));
//  case Msg.ScrollCode of
//    // Scrolls to start / end of the text
//    SB_TOP:
//      TopView := 1;
//    SB_BOTTOM:
//      TopView := FFoldedLinesView.Count;
//    // Scrolls one line up / down
//    SB_LINEDOWN:
//      TopView := TopView + 1;
//    SB_LINEUP:
//      TopView := TopView - 1;
//    // Scrolls one page of lines up / down
//    SB_PAGEDOWN:
//      TopView := TopView + Max(1, (LinesInWindow - ord(eoScrollByOneLess in FOptions))); // TODO: scroll half page ?
//    SB_PAGEUP:
//      TopView := TopView - Max(1, (LinesInWindow - ord(eoScrollByOneLess in FOptions)));
//    // Scrolls to the current scroll bar position
//    SB_THUMBPOSITION, SB_THUMBTRACK:
//      begin
//        TopView := Msg.Pos;
//
//        if eoShowScrollHint in FOptions then
//        begin
//          ScrollHint := GetScrollHint;
//          if not ScrollHint.Visible then
//          begin
//            ScrollHint.Color := Application.HintColor;
//            ScrollHint.Visible := True;
//          end;
//          s := Format('line %d', [TopLine]);
//          rc := ScrollHint.CalcHintRect(200, s, nil);
//          pt := ClientToScreen(Point(ClientWidth - ScrollBarWidth - rc.Right - 4, 10));
//          if eoScrollHintFollows in FOptions then
//            pt.Y := Mouse.CursorPos.Y - (rc.Bottom div 2);
//          OffsetRect(rc, pt.X, pt.Y);
//          ScrollHint.ActivateHint(rc, s);
//          ScrollHint.Invalidate;
//          ScrollHint.Update;
//        end;
//      end;
//    // Ends scrolling
//    SB_ENDSCROLL:
//      if eoShowScrollHint in FOptions then
//        with GetScrollHint do
//        begin
//          Visible := False;
//          ActivateHint(Rect(0, 0, 0, 0), '');
//        end;
//  end;
end;

procedure TCustomSynEdit.ScanRanges(ATextChanged: boolean = True);
begin
//  if not HandleAllocated then
//  begin
//    Application.RemoveOnIdleHandler(@IdleScanRanges); // avoid duplicate add
//    if assigned(fHighlighter) then
//      Application.AddOnIdleHandler(@IdleScanRanges, False);
//    exit;
//  end;
  if not assigned(fHighlighter) then
  begin
    if ATextChanged then
    begin
      fMarkupManager.TextChanged(FChangedLinesStart, FChangedLinesEnd);
      // TODO: see TSynEditFoldedView.LineCountChanged, this is only needed, because NeedFixFrom does not always work
      FFoldedLinesView.FixFoldingAtTextIndex(FChangedLinesStart, FChangedLinesEnd);
    end;
    TopView := TopView;
    exit;
  end;
  fHighlighter.CurrentLines := FLines; // Trailing spaces are not needed
  fHighlighter.ScanRanges;

  // Todo: text may not have changed
  if ATextChanged then
    fMarkupManager.TextChanged(FChangedLinesStart, FChangedLinesEnd);
  TopView := TopView;
end;

procedure TCustomSynEdit.IdleScanRanges(Sender: TObject; var Done: boolean);
begin
//  Application.RemoveOnIdleHandler(@IdleScanRanges);
  if not assigned(fHighlighter) then
    exit;

  fHighlighter.CurrentLines := FLines; // Trailing spaces are not needed
  if not fHighlighter.IdleScanRanges then
    exit;

  // Move to the end; give others a change too
//  Application.AddOnIdleHandler(@IdleScanRanges, False);
  Done := False;
end;

procedure TCustomSynEdit.LineCountChanged(Sender: TSynEditStrings; AIndex, ACount: Integer);
begin
{$IFDEF SynFoldDebug}debugln(['FOLD-- LineCountChanged Aindex', AIndex, '  ACount=', ACount]); {$ENDIF}
  if (AIndex < FBeautifyStartLineIdx) or (FBeautifyStartLineIdx < 0) then
    FBeautifyStartLineIdx := AIndex;
  if ACount > 0 then
  begin
    if (AIndex > FBeautifyEndLineIdx) then
      FBeautifyEndLineIdx := AIndex + ACount - 1
    else
      FBeautifyEndLineIdx := FBeautifyEndLineIdx + ACount;
  end
  else
  begin
    FBeautifyEndLineIdx := FBeautifyEndLineIdx + ACount;
    if (FBeautifyEndLineIdx < AIndex) then
      FBeautifyEndLineIdx := AIndex;
  end;

  if PaintLock > 0 then
  begin
    // FChangedLinesStart is also given to Markup.TextChanged; but it is not used there
    if (FChangedLinesStart < 1) or (FChangedLinesStart > AIndex + 1) then
      FChangedLinesStart := AIndex + 1;
    FChangedLinesEnd := -1; // Invalidate the rest of lines
  end
  else
  begin
    ScanRanges;
    InvalidateLines(AIndex + 1, -1);
    InvalidateGutterLines(AIndex + 1, -1);
    if FCaret.LinePos > FLines.Count then
      FCaret.LinePos := FLines.Count;
  end;
  if TopLine > AIndex + 1 then
    TopLine := TopLine + ACount // will call UpdateScrollBars
  else
    UpdateScrollBars;
end;

procedure TCustomSynEdit.LineTextChanged(Sender: TSynEditStrings; AIndex, ACount: Integer);
begin
{$IFDEF SynFoldDebug}debugln(['FOLD-- LineTextChanged Aindex', AIndex, '  ACount=', ACount]); {$ENDIF}
  if (AIndex < FBeautifyStartLineIdx) or (FBeautifyStartLineIdx < 0) then
    FBeautifyStartLineIdx := AIndex;
  if (AIndex + ACount - 1 > FBeautifyEndLineIdx) then
    FBeautifyEndLineIdx := AIndex + ACount - 1;

  if PaintLock > 0 then
  begin
    if (FChangedLinesStart < 1) or (FChangedLinesStart > AIndex + 1) then
      FChangedLinesStart := AIndex + 1;
    if (FChangedLinesEnd >= 0) and (FChangedLinesEnd < AIndex + 1) then
      FChangedLinesEnd := AIndex + 1 + Max(ACount, 0);
  end
  else
  begin
    ScanRanges;
    InvalidateLines(AIndex + 1, AIndex + ACount);
    InvalidateGutterLines(AIndex + 1, AIndex + ACount);
  end;
  UpdateScrollBars;
end;

procedure TCustomSynEdit.DoHighlightChanged(Sender: TSynEditStrings; AIndex, ACount: Integer);
begin
  InvalidateLines(AIndex + 1, AIndex + 1 + ACount);
  InvalidateGutterLines(AIndex + 1, AIndex + 1 + ACount);
  FFoldedLinesView.FixFoldingAtTextIndex(AIndex, AIndex + ACount);
  if FPendingFoldState <> '' then
    SetFoldState(FPendingFoldState);
end;

procedure TCustomSynEdit.ListCleared(Sender: TObject);
begin
  ClearUndo;
  // invalidate the *whole* client area
//  Invalidate;
  // set caret and selected block to start of text
  SetBlockBegin(Point(1, 1));
  SetCaretXY(Point(1, 1));
  // scroll to start of text
  TopView := 1;
  LeftChar := 1;
  StatusChanged(scTextCleared);
end;

procedure TCustomSynEdit.FoldChanged(Index: Integer);
var
  i: Integer;
begin
{$IFDEF SynFoldDebug}debugln(['FOLD-- FoldChanged; Index=', Index, ' TopView=', TopView, '  ScreenRowToRow(LinesInWindow + 1)=', ScreenRowToRow(LinesInWindow + 1)]); {$ENDIF}
  TopView := TopView;
  i := FFoldedLinesView.CollapsedLineForFoldAtLine(CaretY);
  if i > 0 then
  begin
    SetCaretXY(Point(1, i));
    UpdateCaret;
  end
  else if eoAlwaysVisibleCaret in FOptions2 then
    MoveCaretToVisibleArea;
  UpdateScrollBars;
  if Index + 1 > Max(1, ScreenRowToRow(LinesInWindow + 1)) then
    exit;
  if Index + 1 < TopLine then
    Index := TopLine - 1;
  InvalidateLines(Index + 1, -1);
  InvalidateGutterLines(Index + 1, -1);
end;

procedure TCustomSynEdit.SetTopView(AValue: Integer);
begin
  // don't use MinMax here, it will fail in design mode (Lines.Count is zero,
  // but the painting code relies on TopLine >= 1)
{$IFDEF SYNSCROLLDEBUG}
  if (FPaintLock = 0) and (not FIsInDecPaintLock) then
    debugln(['SetTopView outside Paintlock New=', AValue, ' Old=', FFoldedLinesView.TopLine]);
  if (sfHasScrolled in fStateFlags) then
    debugln(['SetTopView with sfHasScrolled Value=', AValue, '  FOldTopView=', FOldTopView]);
{$ENDIF}
  AValue := Min(AValue, CurrentMaxTopView);
  AValue := Max(AValue, 1);

  (* ToDo: FFoldedLinesView.TopLine := AValue;
    Required, if "TopView := TopView" or "TopLine := TopLine" is called,
    after ScanRanges (used to be: LineCountChanged / LineTextChanged)
  *)
  FFoldedLinesView.TopLine := AValue;

  if FTextArea.TopLine <> AValue then
  begin
    FTextArea.TopLine := AValue;
    UpdateScrollBars;
    // call MarkupMgr before ScrollAfterTopLineChanged, in case we aren't in a PaintLock
    fMarkupManager.TopLine := TopLine;
//    if (sfPainting in fStateFlags) then
//      debugln('SetTopline inside paint');
    ScrollAfterTopLineChanged;
    StatusChanged([scTopLine]);
  end
  else
    fMarkupManager.TopLine := TopLine;

{$IFDEF SYNSCROLLDEBUG}
  if (FPaintLock = 0) and (not FIsInDecPaintLock) then
    debugln('SetTopline outside Paintlock EXIT');
{$ENDIF}
end;

function TCustomSynEdit.GetTopView: Integer;
begin
  Result := FTextArea.TopLine;
end;

procedure TCustomSynEdit.SetWordBlock(Value: TPoint);
var
  TempString: string;
  X: Integer;
begin
  { Value is the position of the Caret in bytes }
  Value.Y := Trunc(MinMax(Value.Y, 1, FTheLinesView.Count));
  TempString := FTheLinesView[Value.Y - 1];
  if TempString = '' then
    exit;
  X := Trunc(MinMax(Value.X, 1, length(TempString) + 1));

  Value.X := WordBreaker.PrevWordStart(TempString, X, True);
  if Value.X < 0 then
    Value.X := WordBreaker.NextWordStart(TempString, X);
  if Value.X < 0 then
    exit;

  DoIncPaintLock(self); // No editing is taking place
  FBlockSelection.StartLineBytePos := Value;
  Value.X := WordBreaker.NextWordEnd(TempString, Value.X);
  FBlockSelection.EndLineBytePos := Value;
  FBlockSelection.ActiveSelectionMode := smNormal;
  FCaret.LineBytePos := Value;
  DoDecPaintLock(self);
end;

procedure TCustomSynEdit.SetLineBlock(Value: TPoint; WithLeadSpaces: boolean = True);
var
  ALine: string;
  X, X2: Integer;
begin
  DoIncPaintLock(self); // No editing is taking place
  FBlockSelection.StartLineBytePos := Point(1, Trunc(MinMax(Value.Y, 1, FTheLinesView.Count)));
  FBlockSelection.EndLineBytePos := Point(1, Trunc(MinMax(Value.Y + 1, 1, FTheLinesView.Count)));
  if (FBlockSelection.StartLinePos >= 1) and (FBlockSelection.StartLinePos <= FTheLinesView.Count) then
  begin
    ALine := FTheLinesView[FBlockSelection.StartLinePos - 1];
    X2 := length(ALine) + 1;
    if not WithLeadSpaces then
    begin
      X := FBlockSelection.StartBytePos;
      while (X < length(ALine)) and (ALine[X] in [' ', #9]) do
        inc(X);
      FBlockSelection.StartLineBytePos := Point(X, Trunc(MinMax(Value.Y, 1, FTheLinesView.Count)));
      while (X2 > X) and (ALine[X2 - 1] in [' ', #9]) do
        dec(X2);
    end;
    FBlockSelection.EndLineBytePos := Point(X2, Trunc(MinMax(Value.Y, 1, FTheLinesView.Count)));
  end;
  FBlockSelection.ActiveSelectionMode := smNormal;
  LogicalCaretXY := FBlockSelection.EndLineBytePos;
  // DebugLn(' FFF2 ',Value.X,',',Value.Y,' BlockBegin=',BlockBegin.X,',',BlockBegin.Y,' BlockEnd=',BlockEnd.X,',',BlockEnd.Y);
  DoDecPaintLock(self);
end;

procedure TCustomSynEdit.SetParagraphBlock(Value: TPoint);
var
  ParagraphStartLine, ParagraphEndLine, ParagraphEndX: Integer;

begin
  DoIncPaintLock(self); // No editing is taking place
  ParagraphStartLine := Trunc(MinMax(Value.Y, 1, FTheLinesView.Count));
  ParagraphEndLine := Trunc(MinMax(Value.Y + 1, 1, FTheLinesView.Count));
  ParagraphEndX := 1;
  while (ParagraphStartLine > 1) and (Trim(FTheLinesView[ParagraphStartLine - 1]) <> '') do
    dec(ParagraphStartLine);
  while (ParagraphEndLine <= FTheLinesView.Count) and (Trim(FTheLinesView[ParagraphEndLine - 1]) <> '') do
    inc(ParagraphEndLine);
  if (ParagraphEndLine > FTheLinesView.Count) then
  begin
    dec(ParagraphEndLine);
    ParagraphEndX := length(FTheLinesView[ParagraphEndLine - 1]) + 1;
  end;
  FBlockSelection.StartLineBytePos := Point(1, ParagraphStartLine);
  FBlockSelection.EndLineBytePos := Point(ParagraphEndX, ParagraphEndLine);
  FBlockSelection.ActiveSelectionMode := smNormal;
  CaretXY := FBlockSelection.EndLineBytePos;
  // DebugLn(' FFF3 ',Value.X,',',Value.Y,' BlockBegin=',BlockBegin.X,',',BlockBegin.Y,' BlockEnd=',BlockEnd.X,',',BlockEnd.Y);
  DoDecPaintLock(self);
end;

function TCustomSynEdit.GetCanUndo: boolean;
begin
  Result := fUndoList.CanUndo;
end;

function TCustomSynEdit.GetCanRedo: boolean;
begin
  Result := fRedoList.CanUndo;
end;

function TCustomSynEdit.GetCanPaste: boolean;
begin
  Result := false;//Clipboard.HasFormat(CF_TEXT) or Clipboard.HasFormat(TSynClipboardStream.ClipboardFormatId)
end;

procedure TCustomSynEdit.Redo;
var
  Item: TSynEditUndoItem;
  Group: TSynEditUndoGroup;
begin
  Group := fRedoList.PopItem;
  if Group <> nil then
  begin;
{$IFDEF SynUndoDebugCalls}
    DebugLnEnter(['>> TCustomSynEdit.Redo ', DbgSName(self), ' ', dbgs(self), ' Group', dbgs(Group), ' cnt=',
      Group.Count]);
{$ENDIF}
    IncPaintLock;
    FTheLinesView.IsRedoing := True;
    Item := Group.Pop;
    if Item <> nil then
    begin
      InternalBeginUndoBlock;
      fUndoList.CurrentGroup.Reason := Group.Reason;
      fUndoList.IsInsideRedo := True;
      try
        repeat
          RedoItem(Item);
          Item := Group.Pop;
        until (Item = nil);
      finally
        InternalEndUndoBlock;
      end;
    end;
    FTheLinesView.IsRedoing := False;
    Group.Free;
    if fRedoList.IsTopMarkedAsUnmodified then
      fUndoList.MarkTopAsUnmodified;
    DecPaintLock;
{$IFDEF SynUndoDebugCalls}
    DebugLnExit(['<< TCustomSynEdit.Redo ', DbgSName(self), ' ', dbgs(self)]);
  end
  else
  begin
    debugln(['<< TCustomSynEdit.Redo - NO GROUP ', DbgSName(self), ' ', dbgs(self)]);
{$ENDIF}
  end;
end;

procedure TCustomSynEdit.RedoItem(Item: TSynEditUndoItem);
var
  Line, StrToDelete: PChar;
  X, Y, Len, Len2: Integer;

  function GetLeadWSLen: Integer;
  var
    Run: PChar;
  begin
    Run := Line;
    while (Run[0] in [' ', #9]) do
      inc(Run);
    Result := Run - Line;
  end;

begin
  if assigned(Item) then
    try
      FCaret.IncForcePastEOL;
      if Item.ClassType = TSynEditUndoIndent then
      begin // re-insert the column
        SetCaretAndSelection(LogicalToPhysicalPos(Point(1, TSynEditUndoIndent(Item).FPosY1)),
          Point(1, TSynEditUndoIndent(Item).FPosY1), Point(2, TSynEditUndoIndent(Item).FPosY2), smNormal);
        X := FBlockIndent;
        Y := FBlockTabIndent;
        FBlockIndent := TSynEditUndoIndent(Item).FCnt;
        FBlockTabIndent := TSynEditUndoIndent(Item).FTabCnt;
        DoBlockIndent;
        FBlockIndent := X;
        FBlockTabIndent := Y;
      end
      else if Item.ClassType = TSynEditUndoUnIndent then
      begin // re-delete the (raggered) column
        // add to undo list
        fUndoList.AddChange(TSynEditUndoUnIndent.Create(TSynEditUndoUnIndent(Item).FPosY1,
          TSynEditUndoUnIndent(Item).FPosY2, TSynEditUndoUnIndent(Item).FText));
        // Delete string
        fUndoList.Lock;
        StrToDelete := PChar(TSynEditUndoUnIndent(Item).FText);
        X := -1;
        for Y := TSynEditUndoUnIndent(Item).FPosY1 to TSynEditUndoUnIndent(Item).FPosY2 do
        begin
          Line := PChar(FTheLinesView[Y - 1]);
          Len := GetLeadWSLen;
          Len2 := GetEOL(StrToDelete) - StrToDelete;
          if (Len2 > 0) and (Len >= Len2) then
            FTheLinesView.EditDelete(1 + Len - Len2, Y, Len2);
          inc(StrToDelete, Len2 + 1);
        end;
        fUndoList.Unlock;
      end
      else if not Item.PerformUndo(self) then
        FTheLinesView.EditRedo(Item);
    finally
      FCaret.DecForcePastEOL;
      Item.Free;
    end;
end;

procedure TCustomSynEdit.UpdateCursor;
begin
  if (sfHideCursor in fStateFlags) and (eoAutoHideCursor in FOptions2) then
  begin
//    SetCursor(crNone);
    exit;
  end;

  if (FLastMousePoint.X >= FTextArea.Bounds.Left) and (FLastMousePoint.X < FTextArea.Bounds.Right) and
    (FLastMousePoint.Y >= FTextArea.Bounds.Top) and (FLastMousePoint.Y < FTextArea.Bounds.Bottom) then
  begin
    if assigned(fMarkupCtrlMouse) and (fMarkupCtrlMouse.Cursor <> crDefault) then
      Cursor := fMarkupCtrlMouse.Cursor
    else
      Cursor := crIBeam;
  end
  else
    Cursor := crDefault;
end;

procedure TCustomSynEdit.Undo;
var
  Item: TSynEditUndoItem;
  Group: TSynEditUndoGroup;
begin
  Group := fUndoList.PopItem;
  if Group <> nil then
  begin;
{$IFDEF SynUndoDebugCalls}
    DebugLnEnter(['>> TCustomSynEdit.Undo ', DbgSName(self), ' ', dbgs(self), ' Group', dbgs(Group), ' cnt=',
      Group.Count]);
{$ENDIF}
    IncPaintLock;
    FTheLinesView.IsUndoing := True;
    Item := Group.Pop;
    if Item <> nil then
    begin
      InternalBeginUndoBlock(fRedoList);
      fRedoList.CurrentGroup.Reason := Group.Reason;
      fUndoList.Lock;
      try
        repeat
          UndoItem(Item);
          Item := Group.Pop;
        until (Item = nil);
      finally
        // Todo: Decide what do to, If there are any trimable spaces.
        FTrimmedLinesView.ForceTrim;
        fUndoList.Unlock;
        InternalEndUndoBlock(fRedoList);
      end;
    end;
    FTheLinesView.IsUndoing := False;
    Group.Free;
    if fUndoList.IsTopMarkedAsUnmodified then
      fRedoList.MarkTopAsUnmodified;
    DecPaintLock;
{$IFDEF SynUndoDebugCalls}
    DebugLnExit(['<< TCustomSynEdit.Undo ', DbgSName(self), ' ', dbgs(self)]);
  end
  else
  begin
    debugln(['<< TCustomSynEdit.Undo - NO GROUP ', DbgSName(self), ' ', dbgs(self)]);
{$ENDIF}
  end;
end;

procedure TCustomSynEdit.UndoItem(Item: TSynEditUndoItem);
var
  Line, OldText: PChar;
  Y, Len, Len2, LenT: Integer;

  function GetLeadWSLen: Integer;
  var
    Run: PChar;
  begin
    Run := Line;
    while (Run[0] in [' ', #9]) do
      inc(Run);
    Result := Run - Line;
  end;

begin
  if assigned(Item) then
    try
      FCaret.IncForcePastEOL;

      if Item.ClassType = TSynEditUndoIndent then
      begin
        // add to redo list
        fRedoList.AddChange(TSynEditUndoIndent.Create(TSynEditUndoIndent(Item).FPosY1, TSynEditUndoIndent(Item).FPosY2,
          TSynEditUndoIndent(Item).FCnt, TSynEditUndoIndent(Item).FTabCnt));
        // quick unintend (must all be spaces, as inserted...)
        fRedoList.Lock;
        Len2 := TSynEditUndoIndent(Item).FCnt;
        LenT := TSynEditUndoIndent(Item).FTabCnt;
        for Y := TSynEditUndoIndent(Item).FPosY1 to TSynEditUndoIndent(Item).FPosY2 do
        begin
          Line := PChar(FTheLinesView[Y - 1]);
          Len := GetLeadWSLen;
          FTheLinesView.EditDelete(Len + 1 - Len2, Y, Len2);
          FTheLinesView.EditDelete(1, Y, LenT);
        end;
        fRedoList.Unlock;
      end
      else

        if Item.ClassType = TSynEditUndoUnIndent then
      begin
        fRedoList.AddChange(TSynEditUndoUnIndent.Create(TSynEditUndoUnIndent(Item).FPosY1,
          TSynEditUndoUnIndent(Item).FPosY2, TSynEditUndoUnIndent(Item).FText));
        // reinsert the string
        fRedoList.Lock;
        OldText := PChar(TSynEditUndoUnIndent(Item).FText);
        for Y := TSynEditUndoUnIndent(Item).FPosY1 to TSynEditUndoUnIndent(Item).FPosY2 do
        begin
          Line := PChar(FTheLinesView[Y - 1]);
          Len := GetLeadWSLen;
          Len2 := GetEOL(OldText) - OldText;
          if Len2 > 0 then
            FTheLinesView.EditInsert(Len + 1, Y, copy(OldText, 1, Len2));
          inc(OldText, Len2 + 1);
        end;
        fRedoList.Unlock;
      end

      else if not Item.PerformUndo(self) then
        FTheLinesView.EditUndo(Item);
    finally
      FTrimmedLinesView.UndoTrimmedSpaces := False;
      FCaret.DecForcePastEOL;
      Item.Free;
    end;
end;

procedure TCustomSynEdit.SetFoldState(const AValue: String);
begin
  if assigned(fHighlighter) then
  begin
    fHighlighter.CurrentLines := FTheLinesView;
    if fHighlighter.NeedScan then
    begin
      FPendingFoldState := AValue;
      exit;
    end;
  end;
  if sfAfterLoadFromFileNeeded in fStateFlags then
  begin
    FPendingFoldState := AValue;
    exit;
  end;
  FFoldedLinesView.Lock;
  FFoldedLinesView.ApplyFoldDescription(0, 0, -1, -1, PChar(AValue), length(AValue), True);
  TopView := TopView; // Todo: reset TopView on foldedview
  FFoldedLinesView.Unlock;
  FPendingFoldState := '';
end;

procedure TCustomSynEdit.SetHighlightAllColor(AValue: TSynSelectedColor);
begin
  fMarkupHighAll.MarkupInfo.Assign(AValue);
end;

procedure TCustomSynEdit.SetIncrementColor(AValue: TSynSelectedColor);
begin
  fMarkupSelection.MarkupInfoIncr.Assign(AValue);
end;

procedure TCustomSynEdit.SetLineHighlightColor(AValue: TSynSelectedColor);
begin
  fMarkupSpecialLine.MarkupLineHighlightInfo.Assign(AValue);
end;

procedure TCustomSynEdit.SetMouseActions(const AValue: TSynEditMouseActions);
begin
  FMouseActions.UserActions := AValue;
end;

procedure TCustomSynEdit.SetMouseLinkColor(AValue: TSynSelectedColor);
begin
  fMarkupCtrlMouse.MarkupInfo.Assign(AValue);
end;

procedure TCustomSynEdit.SetMouseSelActions(const AValue: TSynEditMouseActions);
begin
  FMouseSelActions.UserActions := AValue;
end;

procedure TCustomSynEdit.SetMouseTextActions(AValue: TSynEditMouseActions);
begin
  FMouseTextActions.UserActions := AValue;
end;

procedure TCustomSynEdit.SetPaintLockOwner(const AValue: TSynEditBase);
begin
  TSynEditStringList(FLines).PaintLockOwner := AValue;
end;

procedure TCustomSynEdit.SetShareOptions(const AValue: TSynEditorShareOptions);
var
  ChangedOptions: TSynEditorShareOptions;
  OldMarkList: TSynEditMarkList;
  it: TSynEditMarkIterator;
  MListShared: boolean;
begin
  if FShareOptions = AValue then
    exit;

  ChangedOptions := (FShareOptions - AValue) + (AValue - FShareOptions);
  FShareOptions := AValue;

  if (eosShareMarks in ChangedOptions) then
  begin
    MListShared := IsMarkListShared;
    if ((FShareOptions * [eosShareMarks] = []) and MListShared) or
      ((eosShareMarks in FShareOptions) and (not MListShared) and (TSynEditStringList(FLines).AttachedSynEditCount > 1))
    then
    begin
      OldMarkList := FMarkList;
      FMarkList := nil;
      RecreateMarkList;
      it := TSynEditMarkIterator.Create(OldMarkList);
      it.GotoBOL;
      while it.Next do
      begin
        // Todo: prevent notifications
        if it.Mark.OwnerEdit = self then
          FMarkList.Add(it.Mark);
      end;
      it.Free;
      FreeAndNil(FMarkList);
    end;
  end;
end;

procedure TCustomSynEdit.ChangeTextBuffer(NewBuffer: TSynEditStringList);
var
  OldBuffer: TSynEditStringList;
  LView: TSynEditStrings;
  i: Integer;
  TempPlugins: TList;
begin
  FLines.SendNotification(senrTextBufferChanging, FLines); // Send the old buffer
  DestroyMarkList;

  // Remember all Plugins; Detach from Lines
  TempPlugins := TList.Create;
  for i := FPlugins.Count - 1 downto 0 do
  begin
    TempPlugins.Add(FPlugins[i]);
    TLazSynEditPlugin(FPlugins[i]).Editor := nil;
  end;
  // Detach Highlighter
  if fHighlighter <> nil then
    fHighlighter.DetachFromLines(FLines);

  // Set the New Lines
  OldBuffer := TSynEditStringList(FLines);

  FLines := NewBuffer;
  TSynEditStringList(FLines).AttachSynEdit(self);
  TSynEditStringsLinked(FTopLinesView).NextLines := FLines;

  // Todo: Todo Refactor all classes with events, so they an be told to re-attach
  NewBuffer.CopyHanlders(OldBuffer, self);
  LView := FTheLinesView;
  while (LView is TSynEditStringsLinked) and (LView <> FLines) do
  begin
    NewBuffer.CopyHanlders(OldBuffer, LView);
    LView := TSynEditStringsLinked(LView).NextLines;
  end;
  NewBuffer.CopyHanlders(OldBuffer, FFoldedLinesView);
  // NewBuffer.CopyHanlders(OldBuffer, FMarkList);
  NewBuffer.CopyHanlders(OldBuffer, FCaret);
  NewBuffer.CopyHanlders(OldBuffer, FInternalCaret);
  NewBuffer.CopyHanlders(OldBuffer, FBlockSelection);
  NewBuffer.CopyHanlders(OldBuffer, FInternalBlockSelection);
  NewBuffer.CopyHanlders(OldBuffer, fMarkupManager);
  for i := 0 to fMarkupManager.Count - 1 do
    NewBuffer.CopyHanlders(OldBuffer, fMarkupManager.Markup[i]);

  fUndoList := NewBuffer.UndoList;
  fRedoList := NewBuffer.RedoList;

  // Recreate te public access to FLines
  FreeAndNil(FStrings);
  FStrings := TSynEditLines.Create(TSynEditStringList(FLines), {$IFDEF FPC}@{$ENDIF}MarkTextAsSaved);

  // Flines has been set to the new buffer; and self is attached to the new FLines
  // FTheLinesView points to new FLines
  RecreateMarkList;

  // Attach Highlighter
  if fHighlighter <> nil then
    fHighlighter.AttachToLines(FLines);

  // Restore Plugins; Attach to Lines
  for i := 0 to TempPlugins.Count - 1 do
    TLazSynEditPlugin(TempPlugins[i]).Editor := self;
  TempPlugins.Free;

  RemoveHandlers(OldBuffer);
  OldBuffer.DetachSynEdit(self);
  FLines.SendNotification(senrTextBufferChanged, OldBuffer); // Send the old buffer
  OldBuffer.SendNotification(senrTextBufferChanged, OldBuffer); // Send the old buffer
  if OldBuffer.AttachedSynEditCount = 0 then
    OldBuffer.Free;
end;

function TCustomSynEdit.IsMarkListShared: boolean;
var
  i, j: Integer;
begin
  j := 0;
  i := TSynEditStringList(FLines).AttachedSynEditCount - 1;
  while (i >= 0) and (j <= 1) do
  begin
    if TCustomSynEdit(TSynEditStringList(FLines).AttachedSynEdits[i]).FMarkList = FMarkList then
      inc(j);
    dec(i);
  end;
  Result := j > 1;
end;

procedure TCustomSynEdit.RecreateMarkList;
var
  s: TSynEditBase;
  i: Integer;
begin
  DestroyMarkList;

  if (TSynEditStringList(FLines).AttachedSynEditCount > 1) and (eosShareMarks in FShareOptions) then
  begin
    s := TSynEditStringList(FLines).AttachedSynEdits[0];
    if s = self then
      s := TSynEditStringList(FLines).AttachedSynEdits[1];
    FMarkList := TCustomSynEdit(s).FMarkList;
    TSynEditMarkListInternal(FMarkList).AddOwnerEdit(self);
    for i := 0 to 9 do
      FBookMarks[i] := TCustomSynEdit(s).FBookMarks[i];
  end
  else
  begin
    FMarkList := TSynEditMarkListInternal.Create(self, FTheLinesView);
    for i := 0 to 9 do
      FBookMarks[i] := nil;
  end;

  FMarkList.RegisterChangeHandler({$IFDEF FPC}@{$ENDIF}MarkListChange,
    [low(TSynEditMarkChangeReason) .. high(TSynEditMarkChangeReason)]);
end;

procedure TCustomSynEdit.DestroyMarkList;
var
  it: TSynEditMarkIterator;
  s: TSynEditBase;
begin
  if FMarkList = nil then
    exit;

  TSynEditMarkListInternal(FMarkList).RemoveOwnerEdit(self);
  FMarkList.UnRegisterChangeHandler({$IFDEF FPC}@{$ENDIF}MarkListChange);

  if IsMarkListShared then
  begin
    s := TSynEditStringList(FLines).AttachedSynEdits[0];
    if s = self then
      s := TSynEditStringList(FLines).AttachedSynEdits[1];
    // TODO: find one that shares the MarkList (if someday partial sharing of Marks is avail)

    if TSynEditMarkListInternal(FMarkList).LinesView = FTheLinesView then
      TSynEditMarkListInternal(FMarkList).LinesView := TCustomSynEdit(s).FTheLinesView;

    it := TSynEditMarkIterator.Create(FMarkList);
    it.GotoBOL;
    while it.Next do
    begin
      // Todo: prevent notifications
      if it.Mark.OwnerEdit = self then
        it.Mark.OwnerEdit := s;
    end;
    it.Free;
    FMarkList := nil;
  end
  else
    FreeAndNil(FMarkList);
end;

procedure TCustomSynEdit.ShareTextBufferFrom(AShareEditor: TCustomSynEdit);
begin
//  if FPaintLock <> 0 then
//    RaiseGDBException('Cannot change TextBuffer while paintlocked');

  ChangeTextBuffer(TSynEditStringList(AShareEditor.FLines));
end;

procedure TCustomSynEdit.UnShareTextBuffer;
begin
//  if FPaintLock <> 0 then
//    RaiseGDBException('Cannot change TextBuffer while paintlocked');
  if TSynEditStringList(FLines).AttachedSynEditCount = 1 then
    exit;

  ChangeTextBuffer(TSynEditStringList.Create);
end;

procedure TCustomSynEdit.RemoveHandlers(ALines: TSynEditStrings = nil);
var
  LView: TSynEditStrings;
  i: Integer;
begin
  if not assigned(ALines) then
    ALines := FLines;

  // Todo: aggregated objects, should be responsible themself
  TSynEditStringList(ALines).RemoveHanlders(self);
  LView := FTheLinesView;
  while (LView is TSynEditStringsLinked) and (LView <> ALines) do
  begin
    TSynEditStringList(ALines).RemoveHanlders(LView);
    LView := TSynEditStringsLinked(LView).NextLines;
  end;
  TSynEditStringList(ALines).RemoveHanlders(FFoldedLinesView);
  TSynEditStringList(ALines).RemoveHanlders(FCaret);
  TSynEditStringList(ALines).RemoveHanlders(FInternalCaret);
  TSynEditStringList(ALines).RemoveHanlders(FBlockSelection);
  TSynEditStringList(ALines).RemoveHanlders(FInternalBlockSelection);
  TSynEditStringList(ALines).RemoveHanlders(fMarkupManager);
  for i := 0 to fMarkupManager.Count - 1 do
    TSynEditStringList(ALines).RemoveHanlders(fMarkupManager.Markup[i]);
end;

procedure TCustomSynEdit.ExtraLineCharsChanged(Sender: TObject);
begin
  UpdateScrollBars;
end;

procedure TCustomSynEdit.SetTextBetweenPointsSimple(aStartPoint, aEndPoint: TPoint; const AValue: String);
begin
  InternalBeginUndoBlock;
  try
    FInternalBlockSelection.SelectionMode := smNormal;
    FInternalBlockSelection.StartLineBytePos := aStartPoint;
    FInternalBlockSelection.EndLineBytePos := aEndPoint;
    FInternalBlockSelection.SelText := AValue;
  finally
    InternalEndUndoBlock;
  end;
end;

procedure TCustomSynEdit.SetTextBetweenPointsEx(aStartPoint, aEndPoint: TPoint; aCaretMode: TSynCaretAdjustMode;
  const AValue: String);
begin
  SetTextBetweenPoints(aStartPoint, aEndPoint, AValue, [], aCaretMode);
end;

procedure TCustomSynEdit.SetTextBetweenPoints(aStartPoint, aEndPoint: TPoint; const AValue: String;
  aFlags: TSynEditTextFlags; aCaretMode: TSynCaretAdjustMode; aMarksMode: TSynMarksAdjustMode;
  aSelectionMode: TSynSelectionMode);
begin
  InternalBeginUndoBlock;
  try
    if aCaretMode = scamAdjust then
      FCaret.IncAutoMoveOnEdit;

    if aSelectionMode = smCurrent then
      FInternalBlockSelection.SelectionMode := FBlockSelection.ActiveSelectionMode
    else
      FInternalBlockSelection.SelectionMode := aSelectionMode;
    FInternalBlockSelection.StartLineBytePos := aStartPoint;
    FInternalBlockSelection.EndLineBytePos := aEndPoint;
    aStartPoint := FInternalBlockSelection.FirstLineBytePos;

    if aCaretMode = scamBegin then
      FCaret.LineBytePos := aStartPoint;

    FInternalBlockSelection.SetSelTextPrimitive(FInternalBlockSelection.ActiveSelectionMode, PChar(AValue),
      aMarksMode = smaKeep);
    if aCaretMode = scamEnd then
      FCaret.LineBytePos := FInternalBlockSelection.StartLineBytePos;
    if setSelect in aFlags then
    begin
      FBlockSelection.StartLineBytePos := aStartPoint;
      FBlockSelection.ActiveSelectionMode := FInternalBlockSelection.SelectionMode;
      FBlockSelection.EndLineBytePos := FInternalBlockSelection.StartLineBytePos;
      if FBlockSelection.ActiveSelectionMode = smLine then
        FBlockSelection.EndLineBytePos := Point(FBlockSelection.StartBytePos + 1, FBlockSelection.EndLinePos - 1);
    end;
  finally
    if aCaretMode = scamAdjust then
      FCaret.DecAutoMoveOnEdit;
    InternalEndUndoBlock;
  end;
end;

procedure TCustomSynEdit.SetVisibleSpecialChars(AValue: TSynVisibleSpecialChars);
begin
  if FVisibleSpecialChars = AValue then
    exit;
  FVisibleSpecialChars := AValue;
  fMarkupSpecialChar.VisibleSpecialChars := AValue;
  if eoShowSpecialChars in Options then
    FTextArea.VisibleSpecialChars := AValue
  else
    FTextArea.VisibleSpecialChars := [];
//  if eoShowSpecialChars in Options then
//    Invalidate;
end;

function TCustomSynEdit.GetLineState(ALine: Integer): TSynLineState;
begin
  with TSynEditStringList(FLines) do
    if [sfModified, sfSaved] * Flags[ALine] = [sfModified] then
      Result := slsUnsaved
    else if [sfModified, sfSaved] * Flags[ALine] = [sfModified, sfSaved] then
      Result := slsSaved
    else
      Result := slsNone;
end;

procedure TCustomSynEdit.ClearBookMark(BookMark: Integer);
begin
  if (BookMark in [0 .. 9]) and assigned(FBookMarks[BookMark]) then
    FBookMarks[BookMark].Free;
end;

procedure TCustomSynEdit.GotoBookMark(BookMark: Integer);
var
  LogCaret: TPoint;
begin
  if (BookMark in [0 .. 9]) and assigned(FBookMarks[BookMark]) and (FBookMarks[BookMark].Line <= FLines.Count) then
  begin
    LogCaret := Point(FBookMarks[BookMark].Column, FBookMarks[BookMark].Line);
    DoIncPaintLock(self); // No editing is taking place
    FCaret.LineBytePos := LogCaret;
    SetBlockEnd(LogCaret);
    SetBlockBegin(LogCaret);
    DoDecPaintLock(self);
  end;
end;

function TCustomSynEdit.IsLinkable(Y, X1, X2: Integer): boolean;
begin
  Result := X1 <> X2;
  if Result and assigned(FOnMouseLink) then
    FOnMouseLink(self, X1, Y, Result);
end;

procedure TCustomSynEdit.SetBookMark(BookMark: Integer; X: Integer; Y: Integer);
var
  i: Integer;
  Mark: TSynEditMark;
begin
  if (BookMark in [0 .. 9]) and (Y >= 1) and (Y <= Max(1, FLines.Count)) then
  begin
    Mark := TSynEditMark.Create(self);
    X := PhysicalToLogicalPos(Point(X, Y)).X;
    with Mark do
    begin
      Line := Y;
      Column := X;
      ImageIndex := BookMark;
      BookmarkNumber := BookMark;
      Visible := True;
      InternalImage := (fBookMarkOpt.BookmarkImages = nil);
    end;
    for i := 0 to 9 do
      if assigned(FBookMarks[i]) and (FBookMarks[i].Line = Y) then
        ClearBookMark(i);
    if assigned(FBookMarks[BookMark]) then
      ClearBookMark(BookMark);
    FMarkList.Add(Mark);
  end;
end;

{procedure TCustomSynEdit.WndProc(var Msg: TMessage);
// Prevent Alt-Backspace from beeping
const
  ALT_KEY_DOWN = $20000000;
begin
  if (Msg.Msg = WM_SYSCHAR) and (Msg.WParam = VK_BACK) and (Msg.LParam and ALT_KEY_DOWN <> 0) then
    Msg.Msg := 0
  else
    inherited;
end;}

procedure TCustomSynEdit.InsertTextAtCaret(AText: String; aCaretMode: TSynCaretAdjustMode = scamEnd);
begin
  TextBetweenPointsEx[FCaret.LineBytePos, FCaret.LineBytePos, aCaretMode] := AText;
end;

procedure TCustomSynEdit.DragOver(const Data: TDragObject; const Point: TPointF; var Accept: boolean);
begin
  inherited;
  LastMouseCaret := TPoint.create(-1, -1);
  if (Data.Source is TCustomSynEdit) and not TCustomSynEdit(Data.Source).ReadOnly then
  begin
    Accept := True;
    // Ctrl is pressed => change cursor to indicate copy instead of move
{    if GetKeyState(VK_CONTROL) < 0 then
      DragCursor := crMultiDrag
    else}
//      DragCursor := crDrag;
    FBlockSelection.IncPersistentLock;
//    if State = dsDragLeave then // restore prev caret position
//      ComputeCaret(fMouseDownX, fMouseDownY)
//    else // position caret under the mouse cursor
      ComputeCaret(Point.X, Point.Y);
    FBlockSelection.DecPersistentLock;
  end;
end;

procedure TCustomSynEdit.DragDrop(const Data: TDragObject; const Point: TPointF);
var
  NewCaret: TPoint;
  DoDrop, DropAfter, DropMove: boolean;
  BB, BE: TPoint;
  DragDropText: string;
  Adjust: Integer;
  FoldInfo: String;
  BlockSel: TSynEditSelection;
begin
  if not ReadOnly and (Data.Source is TCustomSynEdit) and TCustomSynEdit(Data.Source).SelAvail then
  begin
    IncPaintLock;
    try
      inherited;
      ComputeCaret(Point.X, Point.Y);
      NewCaret := CaretXY;
      // if from other control then move when SHIFT, else copy
      // if from Self then copy when CTRL, else move
      if Data.Source <> self then
      begin
        DropMove := false;//GetKeyState(VK_SHIFT) < 0;
        DoDrop := True;
        DropAfter := False;
      end
      else
      begin
        DropMove := false;//GetKeyState(VK_CONTROL) >= 0;
        BB := BlockBegin;
        BE := BlockEnd;
        DropAfter := (NewCaret.Y > BE.Y) or ((NewCaret.Y = BE.Y) and (NewCaret.X > BE.X));
        DoDrop := DropAfter or (NewCaret.Y < BB.Y) or ((NewCaret.Y = BB.Y) and (NewCaret.X < BB.X));
      end;
      if DoDrop then
      begin
        InternalBeginUndoBlock; // mh 2000-11-20
        try
          DragDropText := TCustomSynEdit(Data.Source).SelText;
          BlockSel := TCustomSynEdit(Data.Source).FBlockSelection;
          if eoFoldedCopyPaste in FOptions2 then
            FoldInfo := TCustomSynEdit(Data.Source).FFoldedLinesView.GetFoldDescription(BlockSel.FirstLineBytePos.Y - 1,
              BlockSel.FirstLineBytePos.X, BlockSel.LastLineBytePos.Y - 1, BlockSel.LastLineBytePos.X);
          // delete the selected text if necessary
          if DropMove then
          begin
            if Data.Source <> self then
              TCustomSynEdit(Data.Source).SelText := ''
            else
            begin
              SetSelTextExternal('');
              // adjust horizontal drop position
              if DropAfter and (NewCaret.Y = BE.Y) then
              begin
                if BB.Y = BE.Y then
                  Adjust := BE.X - BB.X
                else
                  Adjust := BE.X - 1;
                dec(NewCaret.X, Adjust);
              end;
              // adjust vertical drop position
              if DropAfter and (BE.Y > BB.Y) then
                dec(NewCaret.Y, BE.Y - BB.Y);
            end;
          end;
          // insert the selected text
          FCaret.IncForcePastEOL;
          try
            CaretXY := NewCaret;
            BlockBegin := NewCaret;
            SetSelTextPrimitive(smNormal, PChar(DragDropText), True);
            if FoldInfo <> '' then
            begin
              ScanRanges;
              FFoldedLinesView.ApplyFoldDescription(NewCaret.Y - 1, NewCaret.X, FBlockSelection.StartLinePos - 1,
                FBlockSelection.StartBytePos, PChar(FoldInfo), length(FoldInfo));
            end;
          finally
            FCaret.DecForcePastEOL;
          end;
          FCaret.LineCharPos := NewCaret;
          BlockBegin := PhysicalToLogicalPos(NewCaret);
          BlockEnd := LogicalCaretXY;
        finally
          InternalEndUndoBlock;
        end;
      end;
    finally
      DecPaintLock;
    end;
  end
  else
    inherited;
end;

procedure TCustomSynEdit.SetRightEdge(Value: Integer);
begin
  if FTextArea.RightEdgeColumn <> Value then
  begin
    FTextArea.RightEdgeColumn := Value;
//    if FTextArea.RightEdgeVisible then
//      Invalidate;
  end;
end;

procedure TCustomSynEdit.SetRightEdgeColor(Value: TColor);
var
  NX: Integer;
  rcInval: TRect;
begin
  if RightEdgeColor <> Value then
  begin
    FTextArea.RightEdgeColor := Value;
(*    if HandleAllocated then
    begin
      NX := FTextArea.ScreenColumnToXValue(FTextArea.RightEdgeColumn + 1);
      rcInval := Rect(NX - 1, 0, NX + 1, ClientHeight - ScrollBarWidth);
{$IFDEF VerboseSynEditInvalidate}
      debugln(['TCustomSynEdit.SetRightEdgeColor ', dbgs(rcInval)]);
{$ENDIF}
      InvalidateRect(Handle, @rcInval, False);
    end;*)
  end;
end;

function TCustomSynEdit.GetMaxUndo: Integer;
begin
  Result := fUndoList.MaxUndoActions;
end;

procedure TCustomSynEdit.SetMaxUndo(const Value: Integer);
begin
  if Value > -1 then
  begin
    fUndoList.MaxUndoActions := Value;
    fRedoList.MaxUndoActions := Value;
  end;
end;

procedure TCustomSynEdit.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then
  begin
    if AComponent = fHighlighter then
    begin
      fHighlighter.DetachFromLines(FLines);
      fHighlighter := nil;
      fMarkupHighCaret.Highlighter := nil;
      fMarkupWordGroup.Highlighter := nil;
      FFoldedLinesView.Highlighter := nil;
      FTextArea.Highlighter := nil;
      if not(csDestroying in ComponentState) then
      begin
        RecalcCharExtent;
//        Invalidate;
      end;
    end;
//    if (fBookMarkOpt <> nil) then
//      if (AComponent = fBookMarkOpt.BookmarkImages) then
//      begin
//        fBookMarkOpt.BookmarkImages := nil;
//        InvalidateGutterLines(-1, -1);
//      end;
  end;
end;

procedure TCustomSynEdit.RemoveHooksFromHighlighter;
begin
  if not assigned(fHighlighter) then
    exit;
  fHighlighter.UnhookAttrChangeEvent({$IFDEF FPC}@{$ENDIF}HighlighterAttrChanged);
  fHighlighter.DetachFromLines(FLines);
  fHighlighter.RemoveFreeNotification(self);
end;

procedure TCustomSynEdit.SetHighlighter(const Value: TSynCustomHighlighter);
begin
  if Value <> fHighlighter then
  begin
    FPendingFoldState := '';
    RemoveHooksFromHighlighter;
    if assigned(Value) then
    begin
      Value.HookAttrChangeEvent(
{$IFDEF FPC}@{$ENDIF}HighlighterAttrChanged);
      Value.FreeNotification(self);
      Value.AttachToLines(FLines);
    end;
    fHighlighter := Value;
    IncPaintLock;
    try
      // Ensure to free all copies in SynEit.Notification too
      fMarkupHighCaret.Highlighter := Value;
      fMarkupWordGroup.Highlighter := Value;
      FFoldedLinesView.Highlighter := Value;
      FTextArea.Highlighter := Value;
      FWordBreaker.Reset;
      if fHighlighter <> nil then
      begin
//        fTSearch.IdentChars := fHighlighter.IdentChars;
        FWordBreaker.IdentChars := fHighlighter.IdentChars;
        FWordBreaker.WordBreakChars := fHighlighter.WordBreakChars;
      end
      else
      begin
//        fTSearch.ResetIdentChars;
      end;
      RecalcCharExtent;
      ScanRanges; // Todo: Skip if paintlocked
    finally
      DecPaintLock;
    end;
  end;
end;

procedure TCustomSynEdit.SetHideSelection(const Value: boolean);
begin
  if fHideSelection <> Value then
  begin
    fHideSelection := Value;
//    Invalidate;
  end;
end;

procedure TCustomSynEdit.SetInsertMode(const Value: boolean);
begin
  if fInserting <> Value then
  begin
    fInserting := Value;
    if InsertMode then
      FScreenCaret.DisplayType := fInsertCaret
    else
      FScreenCaret.DisplayType := fOverwriteCaret;
    StatusChanged([scInsertMode]);
  end;
end;

procedure TCustomSynEdit.SetInsertCaret(const Value: TSynEditCaretType);
begin
  if fInsertCaret <> Value then
  begin
    fInsertCaret := Value;
    if InsertMode then
      FScreenCaret.DisplayType := fInsertCaret;
  end;
end;

procedure TCustomSynEdit.SetOverwriteCaret(const Value: TSynEditCaretType);
begin
  if fOverwriteCaret <> Value then
  begin
    fOverwriteCaret := Value;
    if not InsertMode then
      FScreenCaret.DisplayType := fOverwriteCaret;
  end;
end;

procedure TCustomSynEdit.SetMaxLeftChar(Value: Integer);
begin
  Value := Trunc(MinMax(Value, 1, MAX_SCROLL)); // horz scrolling is only 16 bit
  if fMaxLeftChar <> Value then
  begin
    fMaxLeftChar := Value;
//    Invalidate;
  end;
end;

procedure TCustomSynEdit.EnsureCursorPosVisible;
var
  PhysCaretXY: TPoint;
  MinX: Integer;
  MaxX: Integer;
  PhysBlockBeginXY: TPoint;
  PhysBlockEndXY: TPoint;
begin
  if (PaintLockOwner <> nil) and (PaintLockOwner <> self) and (not(eoAlwaysVisibleCaret in FOptions2)) then
    exit;

  if {(not HandleAllocated) or }(FPaintLock > 0) {or (FWinControlFlags * [wcfInitializing, wcfCreatingHandle] <> [])} then
  begin
    Include(fStateFlags, sfEnsureCursorPos);
    exit;
  end;

  Exclude(fStateFlags, sfEnsureCursorPos);
  DoIncPaintLock(self); // No editing is taking place
  try
    // Make sure X is visible
    // DebugLn('[TCustomSynEdit.EnsureCursorPosVisible] A CaretX=',CaretX,' LeftChar=',LeftChar,' CharsInWindow=',CharsInWindow,' ClientWidth=',ClientWidth);
    PhysCaretXY := CaretXY;
    // try to make the current selection visible as well
    MinX := PhysCaretXY.X;
    MaxX := PhysCaretXY.X;
    if SelAvail then
    begin
      PhysBlockBeginXY := LogicalToPhysicalPos(BlockBegin);
      PhysBlockEndXY := LogicalToPhysicalPos(BlockEnd);
      if (PhysBlockBeginXY.X <> PhysBlockEndXY.X) or (PhysBlockBeginXY.Y <> PhysBlockEndXY.Y) then
      begin
        if (FBlockSelection.ActiveSelectionMode <> smColumn) and (PhysBlockBeginXY.Y <> PhysBlockEndXY.Y) then
          PhysBlockBeginXY.X := 1;
        if MinX > PhysBlockBeginXY.X then
          MinX := Max(PhysBlockBeginXY.X, PhysCaretXY.X - CharsInWindow + 1);
        if MinX > PhysBlockEndXY.X then
          MinX := Max(PhysBlockEndXY.X, PhysCaretXY.X - CharsInWindow + 1);
        if MaxX < PhysBlockBeginXY.X then
          MaxX := Min(PhysBlockBeginXY.X, MinX + CharsInWindow - 1);
        if MaxX < PhysBlockEndXY.X then
          MaxX := Min(PhysBlockEndXY.X, MinX + CharsInWindow - 1);
      end;
    end;
    { DebugLn('TCustomSynEdit.EnsureCursorPosVisible A CaretX=',dbgs(PhysCaretXY.X),
      ' BlockX=',dbgs(PhysBlockBeginXY.X)+'-'+dbgs(PhysBlockEndXY.X),
      ' CharsInWindow='+dbgs(CharsInWindow), MinX='+dbgs(MinX),' MaxX='+dbgs(MaxX),
      ' LeftChar='+dbgs(LeftChar), ''); }
    if MinX < LeftChar then
      LeftChar := MinX
    else if LeftChar < MaxX - (Max(1, CharsInWindow) - 1 - FScreenCaret.ExtraLineChars) then
      LeftChar := MaxX - (Max(1, CharsInWindow) - 1 - FScreenCaret.ExtraLineChars)
    else
      LeftChar := LeftChar; // mh 2000-10-19
    // DebugLn(['TCustomSynEdit.EnsureCursorPosVisible B LeftChar=',LeftChar,' MinX=',MinX,' MaxX=',MaxX,' CharsInWindow=',CharsInWindow]);
    // Make sure Y is visible
    if CaretY < TopLine then
      TopLine := CaretY
    else if CaretY > ScreenRowToRow(Max(1, LinesInWindow) - 1) then // mh 2000-10-19
      TopLine := FFoldedLinesView.TextPosAddLines(CaretY, -Max(0, LinesInWindow - 1))
    else
      TopView := TopView; // mh 2000-10-19
  finally
    DoDecPaintLock(self);
  end;
end;

procedure TCustomSynEdit.SetKeystrokes(const Value: TSynEditKeyStrokes);
begin
  if Value = nil then
    FKeyStrokes.Clear
  else
    FKeyStrokes.Assign(Value);
end;

procedure TCustomSynEdit.SetExtraCharSpacing(const Value: Integer);
begin
  if fExtraCharSpacing = Value then
    exit;
  fExtraCharSpacing := Value;
  FTextArea.ExtraCharSpacing := Value;
//  FontChanged(self);
end;

procedure TCustomSynEdit.SetLastMouseCaret(const AValue: TPoint);
begin
  if (fLastMouseCaret.X = AValue.X) and (fLastMouseCaret.Y = AValue.Y) then
    exit;
  fLastMouseCaret := AValue;
  if assigned(fMarkupCtrlMouse) then
    fMarkupCtrlMouse.LastMouseCaret := AValue;
  UpdateCursor;
end;

procedure TCustomSynEdit.SetDefaultKeystrokes;
begin
  FKeyStrokes.ResetDefaults;
end;

procedure TCustomSynEdit.ResetMouseActions;
begin
  FMouseActions.Options := FMouseOptions;
  FMouseActions.ResetUserActions;
  FMouseSelActions.Options := FMouseOptions;
  FMouseSelActions.ResetUserActions;
  FMouseTextActions.Options := FMouseOptions;
  FMouseTextActions.ResetUserActions;

  FLeftGutter.ResetMouseActions;
  FRightGutter.ResetMouseActions;
end;

procedure TCustomSynEdit.CommandProcessor(Command: TSynEditorCommand; AChar: Char; Data: pointer);
var
  InitialCmd: TSynEditorCommand;
begin
{$IFDEF VerboseKeys}
  debugln(['[TCustomSynEdit.CommandProcessor] ', Command, ' AChar=', AChar, ' Data=', dbgs(Data)]);
{$ENDIF}
  // first the program event handler gets a chance to process the command
  InitialCmd := Command;
  NotifyHookedCommandHandlers(Command, AChar, Data, hcfInit);
  DoOnProcessCommand(Command, AChar, Data);
  if Command <> ecNone then
  begin
    try
      InternalBeginUndoBlock;
      FBeautifyStartLineIdx := -1;
      FBeautifyEndLineIdx := -1;
      if assigned(FBeautifier) then
      begin
        FBeautifier.AutoIndent := (eoAutoIndent in FOptions);
        FBeautifier.BeforeCommand(self, FTheLinesView, FCaret, Command, InitialCmd);
      end;
      // notify hooked command handlers before the command is executed inside of
      // the class
      if Command <> ecNone then
        NotifyHookedCommandHandlers(Command, AChar, Data, hcfPreExec);
      // internal command handler
      if (Command <> ecNone) and (Command < ecUserFirst) then
        ExecuteCommand(Command, AChar, Data);
      // notify hooked command handlers after the command was executed inside of
      // the class
      if Command <> ecNone then
        NotifyHookedCommandHandlers(Command, AChar, Data, hcfPostExec);
      if Command <> ecNone then
        DoOnCommandProcessed(Command, AChar, Data);

      if assigned(FBeautifier) then
      begin
        TSynEditStringList(FLines).FlushNotificationCache;
        FBeautifier.AutoIndent := (eoAutoIndent in FOptions);
        FBeautifier.AfterCommand(self, FTheLinesView, FCaret, Command, InitialCmd, FBeautifyStartLineIdx + 1,
          FBeautifyEndLineIdx + 1);
      end;
    finally
      InternalEndUndoBlock;
{$IFDEF SynCheckPaintLock}
      if (FPaintLock > 0) and (FInvalidateRect.Bottom > FInvalidateRect.Top) then
      begin
        debugln(['TCustomSynEdit.CommandProcessor: Paint called while locked  InitialCmd=', InitialCmd, ' Command=',
          Command]);
        DumpStack;
      end;
{$ENDIF}
    end;
  end;
  NotifyHookedCommandHandlers(Command, AChar, Data, hcfFinish);
end;

procedure TCustomSynEdit.ExecuteCommand(Command: TSynEditorCommand; const AChar: Char; Data: pointer);
const
  SEL_MODE: array [ecNormalSelect .. ecLineSelect] of TSynSelectionMode = (smNormal, smColumn, smLine);
var
  CX: Integer;
  Len: Integer;
  Temp: string;
  Helper: string;
  moveBkm: boolean;
  WP: TPoint;
  Caret: TPoint;
  CaretNew: TPoint;
{$IFDEF SYN_MBCSSUPPORT}
  StartOfBlock: TPoint;
  i: Integer;
  s: string;
{$ENDIF}
  counter: Integer;
  LogCounter: Integer;
  LogCaretXY: TPoint;
  CY: Integer;

begin
  IncPaintLock;
  try
    fUndoList.CurrentReason := Command;

    if Command in [ecSelColCmdRangeStart .. ecSelColCmdRangeEnd] then
      FBlockSelection.ActiveSelectionMode := smColumn;
    if Command in [ecSelCmdRangeStart .. ecSelCmdRangeEnd] then
      FBlockSelection.ActiveSelectionMode := FBlockSelection.SelectionMode;

    FBlockSelection.AutoExtend := Command in [ecSelectionStart .. ecSelectionEnd];
    FCaret.ChangeOnTouch;

    case Command of
      // horizontal caret movement or selection
      ecLeft, ecSelLeft, ecColSelLeft:
        begin
          if (eoCaretSkipsSelection in Options2) and (Command = ecLeft) and SelAvail and
            FCaret.IsAtLineByte(FBlockSelection.LastLineBytePos) then
          begin
            FBlockSelection.IgnoreNextCaretMove;
            FCaret.LineBytePos := FBlockSelection.FirstLineBytePos;
          end
          else
            MoveCaretHorz(-1);
        end;
      ecRight, ecSelRight, ecColSelRight:
        begin
          if (eoCaretSkipsSelection in Options2) and (Command = ecRight) and SelAvail and
            FCaret.IsAtLineByte(FBlockSelection.FirstLineBytePos) then
          begin
            FBlockSelection.IgnoreNextCaretMove;
            FCaret.LineBytePos := FBlockSelection.LastLineBytePos;
          end
          else
            MoveCaretHorz(1);
        end;
      ecPageLeft, ecSelPageLeft, ecColSelPageLeft:
        begin
          MoveCaretHorz(-Max(1, CharsInWindow));
        end;
      ecPageRight, ecSelPageRight, ecColSelPageRight:
        begin
          MoveCaretHorz(Max(1, CharsInWindow));
        end;
      ecLineStart, ecSelLineStart, ecColSelLineStart:
        begin
          DoHomeKey;
        end;
      ecLineTextStart, ecSelLineTextStart, ecColSelLineTextStart:
        begin
          DoHomeKey(synhmFirstWord);
        end;
      ecLineEnd, ecSelLineEnd, ecColSelLineEnd:
        begin
          DoEndKey;
        end;
      // vertical caret movement or selection
      ecUp, ecSelUp, ecColSelUp:
        begin
          MoveCaretVert(-1);
        end;
      ecDown, ecSelDown, ecColSelDown:
        begin
          MoveCaretVert(1);
        end;
      ecPageUp, ecSelPageUp, ecPageDown, ecSelPageDown, ecColSelPageUp, ecColSelPageDown:
        begin
          counter := LinesInWindow;
          if (eoHalfPageScroll in FOptions) then
            counter := counter div 2;
          if eoScrollByOneLess in FOptions then
            dec(counter);
          counter := Max(1, counter);
          if (Command in [ecPageUp, ecSelPageUp, ecColSelPageUp]) then
            counter := -counter;
          TopView := TopView + counter;
          MoveCaretVert(counter);
        end;
      ecPageTop, ecSelPageTop, ecColSelPageTop:
        begin
          FCaret.LinePos := TopLine;
        end;
      ecPageBottom, ecSelPageBottom, ecColSelPageBottom:
        begin
          FCaret.LinePos := ScreenRowToRow(LinesInWindow - 1);
        end;
      ecEditorTop, ecSelEditorTop:
        begin
          FCaret.LineCharPos := Point(1, FFoldedLinesView.ViewPosToTextIndex(1) + 1);
        end;
      ecEditorBottom, ecSelEditorBottom:
        begin
          CaretNew := Point(1, FFoldedLinesView.ViewPosToTextIndex(FFoldedLinesView.Count) + 1);
          if (CaretNew.Y > 0) then
            CaretNew.X := length(FTheLinesView[CaretNew.Y - 1]) + 1;
          FCaret.LineCharPos := CaretNew;
        end;
      ecColSelEditorTop:
        begin
          FCaret.LinePos := FFoldedLinesView.ViewPosToTextIndex(1) + 1;
        end;
      ecColSelEditorBottom:
        begin
          FCaret.LinePos := FFoldedLinesView.ViewPosToTextIndex(FFoldedLinesView.Count) + 1;
        end;

      // goto special line / column position
      ecGotoXY, ecSelGotoXY:
        if assigned(Data) then
        begin
          FCaret.LineCharPos := PPoint(Data)^;
        end;
      // word selection
      ecWordLeft, ecSelWordLeft, ecColSelWordLeft, ecWordEndLeft, ecSelWordEndLeft, ecHalfWordLeft, ecSelHalfWordLeft:
        begin
          case Command of
            ecWordEndLeft, ecSelWordEndLeft:
              CaretNew := PrevWordLogicalPos(swbWordEnd);
            ecHalfWordLeft, ecSelHalfWordLeft:
              CaretNew := PrevWordLogicalPos(swbCaseChange);
          else
            CaretNew := PrevWordLogicalPos;
          end;
          if FFoldedLinesView.FoldedAtTextIndex[CaretNew.Y - 1] then
          begin
            CY := FindNextUnfoldedLine(CaretNew.Y, False);
            CaretNew := Point(1 + length(FTheLinesView[CY - 1]), CY);
          end;
          FCaret.LineBytePos := CaretNew;
        end;
      ecWordRight, ecSelWordRight, ecColSelWordRight, ecWordEndRight, ecSelWordEndRight, ecHalfWordRight,
        ecSelHalfWordRight:
        begin
          case Command of
            ecWordEndRight, ecSelWordEndRight:
              CaretNew := NextWordLogicalPos(swbWordEnd);
            ecHalfWordRight, ecSelHalfWordRight:
              CaretNew := NextWordLogicalPos(swbCaseChange);
          else
            CaretNew := NextWordLogicalPos;
          end;
          if FFoldedLinesView.FoldedAtTextIndex[CaretNew.Y - 1] then
            CaretNew := Point(1, FindNextUnfoldedLine(CaretNew.Y, True));
          FCaret.LineBytePos := CaretNew;
        end;
      ecSelectAll:
        begin
          SelectAll;
        end;
      { begin }                                                                         // mh 2000-10-30
      ecDeleteLastChar:
        if not ReadOnly then
        begin
          if SelAvail and (not FBlockSelection.Persistent) and (eoOverwriteBlock in FOptions2) then
            SetSelTextExternal('')
          else
          begin
            Temp := LineText;
            Len := length(Temp);
            LogCaretXY := LogicalCaretXY;
            Caret := CaretXY;
            // debugln('ecDeleteLastChar B Temp="',DbgStr(Temp),'" CaretX=',dbgs(CaretX),' LogCaretXY=',dbgs(LogCaretXY));
            if LogCaretXY.X > Len + 1 then
            begin
              // past EOL; only move caret one column
              FCaret.IncForcePastEOL;
              CaretX := CaretX - 1;
              FCaret.DecForcePastEOL;
            end
            else if CaretX = 1 then
            begin
              // join this line with the last line if possible
              if CaretY > 1 then
              begin
                CaretY := CaretY - 1;
                CaretX := PhysicalLineLength(FTheLinesView[CaretY - 1], CaretY - 1) + 1;
                FTheLinesView.EditLineJoin(CaretY);
              end;
            end
            else
            begin
              // delete char
{$IFDEF USE_UTF8BIDI_LCL}
              CaretX := CaretX - 1;
              FTheLinesView.EditDelete(CaretX, LogCaretXY.Y, 1);
{$ELSE USE_UTF8BIDI_LCL}
              LogCaretXY.X := PhysicalToLogicalCol(Temp, CaretY - 1, CaretX - 1);
              LogCounter := GetCharLen(Temp, LogCaretXY.X);
              CaretX := LogicalToPhysicalCol(Temp, CaretY - 1, LogCaretXY.X);
              FTheLinesView.EditDelete(FCaret.BytePos, LogCaretXY.Y, LogCounter);
{$ENDIF USE_UTF8BIDI_LCL}
              // end;
            end;

          end;
        end;
      ecDeleteChar:
        if not ReadOnly then
        begin
          if SelAvail and (not FBlockSelection.Persistent) and (eoOverwriteBlock in FOptions2) then
            SetSelTextExternal('')
          else
          begin
            Temp := LineText;
            Len := length(Temp);
            LogCaretXY := LogicalCaretXY;
            if LogCaretXY.X <= Len then
            begin
              // delete char
              counter := GetCharLen(Temp, LogCaretXY.X);
              FTheLinesView.EditDelete(LogCaretXY.X, CaretY, counter);
              SetLogicalCaretXY(LogCaretXY);
            end
            else
            begin
              // join line with the line after
              if CaretY < FTheLinesView.Count then
              begin
                Helper := StringOfChar(' ', LogCaretXY.X - 1 - Len);
                FTheLinesView.EditLineJoin(CaretY, Helper);
              end;
            end;
          end;
        end;
      ecDeleteWord, ecDeleteEOL:
        if not ReadOnly then
        begin
          Len := LogicalToPhysicalCol(LineText, CaretY - 1, length(LineText) + 1) - 1;
          Helper := '';
          Caret := CaretXY;
          if Command = ecDeleteWord then
          begin
            if CaretX > Len + 1 then
            begin
              Helper := StringOfChar(' ', CaretX - 1 - Len);
              CaretX := 1 + Len;
            end;
            // if we are not in a word, delete word + spaces (up to next token)
            if WordBreaker.IsAtWordStart(LineText, LogicalCaretXY.X) or
              WordBreaker.IsAtWordEnd(LineText, LogicalCaretXY.X) or
              (not WordBreaker.IsInWord(LineText, LogicalCaretXY.X)) or (LogicalCaretXY.X > length(LineText)) then
              WP := NextWordLogicalPos(swbTokenBegin, True)
            else
              // if we are inside a word, delete to word-end
              WP := NextWordLogicalPos(swbWordEnd, True);
          end
          else
            WP := Point(Len + 1, CaretY);
          if (WP.X <> FCaret.BytePos) or (WP.Y <> FCaret.LinePos) then
          begin
            FInternalBlockSelection.StartLineBytePos := WP;
            FInternalBlockSelection.EndLineBytePos := LogicalCaretXY;
            FInternalBlockSelection.ActiveSelectionMode := smNormal;
            FInternalBlockSelection.SetSelTextPrimitive(smNormal, nil);
            if Helper <> '' then
              FTabbedLinesView.EditInsert(CaretX, CaretY, Helper);
            FCaret.BytePos := FInternalBlockSelection.StartBytePos + length(Helper);
          end;
        end;
      ecDeleteLastWord, ecDeleteBOL:
        if not ReadOnly then
        begin
          if Command = ecDeleteLastWord then
            WP := PrevWordLogicalPos
          else
            WP := Point(1, CaretY);
          if (WP.X <> FCaret.BytePos) or (WP.Y <> FCaret.LinePos) then
          begin
            FInternalBlockSelection.StartLineBytePos := WP;
            FInternalBlockSelection.EndLineBytePos := LogicalCaretXY;
            FInternalBlockSelection.ActiveSelectionMode := smNormal;
            FInternalBlockSelection.SetSelTextPrimitive(smNormal, nil);
            FCaret.LineBytePos := WP;
          end;
        end;
      ecDeleteLine:
        if not ReadOnly then
        begin
          CY := FCaret.LinePos;
          if (CY < FTheLinesView.Count) then
            FTheLinesView.EditLinesDelete(CaretY, 1)
          else if (CY = FTheLinesView.Count) and (FTheLinesView[CY - 1] <> '') then
            FTheLinesView.EditDelete(1, CY, length(FTheLinesView[CY - 1]));
          CaretXY := Point(1, CaretY); // like seen in the Delphi editor
        end;
      ecClearAll:
        begin
          if not ReadOnly then
            ClearAll;
        end;
      ecInsertLine, ecLineBreak:
        if not ReadOnly then
        begin
          if SelAvail and (not FBlockSelection.Persistent) and (eoOverwriteBlock in FOptions2) then
            SetSelTextExternal('');
          Temp := LineText;
          LogCaretXY := LogicalCaretXY;
          Len := length(Temp);
          if LogCaretXY.X > Len + 1 then
            LogCaretXY.X := Len + 1;
          FTheLinesView.EditLineBreak(LogCaretXY.X, LogCaretXY.Y);
          if Command = ecLineBreak then
            CaretXY := Point(1, CaretY + 1)
          else
            CaretXY := CaretXY;
        end;
      ecTab:
        if not ReadOnly then
          try
            FCaret.IncForcePastEOL;
            DoTabKey;
          finally
            FCaret.DecForcePastEOL;
          end;
      ecShiftTab:
        if not ReadOnly then
          if SelAvail and (eoTabIndent in Options) then
            DoBlockUnindent;
      ecMatchBracket:
        FindMatchingBracket;
      ecChar:
        if not ReadOnly and (AChar >= #32) and (AChar <> #127) then
        begin
          if SelAvail and (not FBlockSelection.Persistent) and (eoOverwriteBlock in FOptions2) then
          begin
            SetSelTextExternal(AChar);
          end
          else
          begin
            try
              FCaret.IncForcePastEOL;
              FCaret.IncForceAdjustToNextChar;

              LogCaretXY := FCaret.LineBytePos;
              Temp := LineText;
              Len := length(Temp);
              if (not fInserting) and (LogCaretXY.X - 1 < Len) then
              begin
                counter := GetCharLen(Temp, LogCaretXY.X);
                FTheLinesView.EditDelete(LogCaretXY.X, LogCaretXY.Y, counter);
                Len := Len - counter;
              end;

{$IFDEF USE_UTF8BIDI_LCL}
              // TODO: improve utf8bidi for tabs
              Len := VLength(LineText, drLTR);
              (* if Len < CaretX then
                Temp := StringOfChar(' ', CaretX - Len)
                else
                Temp := '' *)
              FTheLinesView.EditInsert(CaretX, LogCaretXY.Y, (* Temp + *) AChar);
{$ELSE}
              (* if Len < LogCaretXY.X - 1 then begin
                Temp := StringOfChar(' ', LogCaretXY.X - 1 - Len);
                LogCaretXY.X := Len + 1;
                end
                else
                temp := ''; *)
              FTheLinesView.EditInsert(LogCaretXY.X, LogCaretXY.Y, (* Temp + *) AChar);
{$ENDIF}
              CaretX := CaretX + 1;
              if CaretX >= LeftChar + CharsInWindow then
                LeftChar := LeftChar + Min(25, CharsInWindow - 1);
            finally
              FCaret.DecForceAdjustToNextChar;
              FCaret.DecForcePastEOL;
            end;
          end;
        end
        else if not ReadOnly and (AChar = #13) then
        begin
          // ecLineBreak is not assigned
          // Insert a linebreak, but do not apply any other functionality (such as indent)
          if SelAvail and (not FBlockSelection.Persistent) and (eoOverwriteBlock in FOptions2) then
            SetSelTextExternal('');
          LogCaretXY := LogicalCaretXY;
          FTheLinesView.EditLineBreak(LogCaretXY.X, LogCaretXY.Y);
          CaretXY := Point(1, CaretY + 1);
          EnsureCursorPosVisible;
        end;
      ecUndo:
        begin
          if not ReadOnly then
            Undo;
        end;
      ecRedo:
        begin
          if not ReadOnly then
            Redo;
        end;
      ecGotoMarker0 .. ecGotoMarker9:
        begin
          if BookMarkOptions.EnableKeys then
            GotoBookMark(Command - ecGotoMarker0);
        end;
      ecSetMarker0 .. ecSetMarker9, ecToggleMarker0 .. ecToggleMarker9:
        begin
          if BookMarkOptions.EnableKeys then
          begin
            if (Command >= ecSetMarker0) and (Command <= ecSetMarker9) then
              CX := Command - ecSetMarker0
            else
              CX := Command - ecToggleMarker0;
            if assigned(FBookMarks[CX]) then
            begin
              moveBkm := ((Command >= ecSetMarker0) and (Command <= ecSetMarker9)) or (FBookMarks[CX].Line <> CaretY);
              ClearBookMark(CX);
              if moveBkm then
                SetBookMark(CX, CaretX, CaretY);
            end
            else
              SetBookMark(CX, CaretX, CaretY);
          end; // if BookMarkOptions.EnableKeys
        end;
      ecCut:
        begin
          if (not ReadOnly) and SelAvail then
            CutToClipboard;
        end;
      ecCopy:
        begin
          CopyToClipboard;
        end;
      ecPaste:
        begin
          if not ReadOnly then
            PasteFromClipboard;
        end;
      ecScrollUp:
        begin
          TopView := TopView - 1;
          if CaretY > ScreenRowToRow(LinesInWindow - 1) then
            CaretY := ScreenRowToRow(LinesInWindow - 1);
        end;
      ecScrollDown:
        begin
          TopView := TopView + 1;
          if CaretY < TopLine then
            CaretY := TopLine;
        end;
      ecScrollLeft:
        begin
          LeftChar := LeftChar - 1;
          if CaretX > LeftChar + CharsInWindow then
            CaretX := LeftChar + CharsInWindow;
//          Update;
        end;
      ecScrollRight:
        begin
          LeftChar := LeftChar + 1;
          if CaretX < LeftChar then
            CaretX := LeftChar;
          //Update;
        end;
      ecInsertMode:
        begin
          InsertMode := True;
        end;
      ecOverwriteMode:
        begin
          InsertMode := False;
        end;
      ecToggleMode:
        begin
          InsertMode := not InsertMode;
        end;
      ecBlockSetBegin:
        begin
          FBlockSelection.Hide := CompareCarets(FCaret.LineBytePos, FBlockSelection.EndLineBytePos) <= 0;
          FBlockSelection.StartLineBytePosAdjusted := FCaret.LineBytePos;
        end;
      ecBlockSetEnd:
        begin
          FBlockSelection.Hide := CompareCarets(FCaret.LineBytePos, FBlockSelection.StartLineBytePos) >= 0;
          FBlockSelection.EndLineBytePos := FCaret.LineBytePos;
        end;
      ecBlockToggleHide:
        begin
          FBlockSelection.Hide := not FBlockSelection.Hide;
        end;
      ecBlockHide:
        begin
          FBlockSelection.Hide := True;
        end;
      ecBlockShow:
        begin
          FBlockSelection.Hide := False;
        end;
      ecBlockMove:
        begin
          if SelAvail then
          begin
            helper := FBlockSelection.SelText;
            FInternalBlockSelection.AssignFrom(FBlockSelection);
            FBlockSelection.IncPersistentLock;
            FBlockSelection.StartLineBytePos := FCaret.LineBytePos; // Track the Adjustment of the insert position
            FInternalBlockSelection.SelText := '';
            FCaret.LineBytePos := FBlockSelection.StartLineBytePos;
            Caret := FCaret.LineBytePos;
            FBlockSelection.SelText := Helper;
            FBlockSelection.DecPersistentLock;
            CaretNew := FCaret.LineBytePos;
            FBlockSelection.StartLineBytePos := Caret;
            FBlockSelection.EndLineBytePos := CaretNew;
          end;
        end;
      ecBlockCopy:
        begin
          if SelAvail then
            InsertTextAtCaret(FBlockSelection.SelText, scamEnd);
        end;
      ecBlockDelete:
        begin
          if SelAvail then
            FBlockSelection.SelText := '';
        end;
      ecBlockGotoBegin:
        begin
          FCaret.LineBytePos := FBlockSelection.FirstLineBytePos;
        end;
      ecBlockGotoEnd:
        begin
          FCaret.LineBytePos := FBlockSelection.LastLineBytePos;
        end;

      ecBlockIndent:
        if not ReadOnly then
          DoBlockIndent;
      ecBlockUnindent:
        if not ReadOnly then
          DoBlockUnindent;
      ecNormalSelect, ecColumnSelect, ecLineSelect:
        begin
          DefaultSelectionMode := SEL_MODE[Command];
        end;
{$IFDEF SYN_MBCSSUPPORT}
      ecImeStr:
        if not ReadOnly then
        begin
          SetString(s, PChar(Data), StrLen(Data));
          if SelAvail then
          begin
            SetSelTextExternal(s);
          end
          else
          begin
            Temp := LineText;
            Len := length(Temp);
            if Len < CaretX then
              Temp := Temp + StringOfChar(' ', CaretX - Len);
            try
              FCaret.IncForcePastEOL;
              StartOfBlock := CaretXY;
              // Processing of case character covers on LeadByte.
              Len := length(s);
              if not fInserting then
              begin
                i := (CaretX + Len);
                if (ByteType(Temp, i) = mbTrailByte) then
                begin
                  s := s + Temp[i - 1];
                  Helper := copy(Temp, CaretX, Len - 1);
                end
                else
                  Helper := copy(Temp, CaretX, Len);
                Delete(Temp, CaretX, Len);
              end;
              Insert(s, Temp, CaretX);
              CaretX := (CaretX + Len);
              FTheLinesView[CaretY - 1] := Temp;
              if fInserting then
                Helper := '';
              fUndoList.AddChange(crInsert, StartOfBlock, LogicalCaretXY, Helper, smNormal);
              if CaretX >= LeftChar + CharsInWindow then
                LeftChar := LeftChar + Min(25, CharsInWindow - 1);
            finally
              FCaret.DecForcePastEOL;
            end;
          end;
        end;
{$ENDIF}
      EcFoldLevel1 .. EcFoldLevel9:
        FoldAll(Command - EcFoldLevel1);
      EcFoldLevel0:
        UnfoldAll;
      EcFoldCurrent:
        begin
          CY := FFoldedLinesView.ExpandedLineForBlockAtLine(CaretY);
          if CY > 0 then
          begin
            FFoldedLinesView.FoldAtTextIndex(CY - 1);
            SetCaretXY(Point(1, CY));
          end;
        end;
      EcUnFoldCurrent:
        FFoldedLinesView.UnFoldAtTextIndex(CaretY - 1);
      EcToggleMarkupWord:
        fMarkupHighCaret.ToggleCurrentWord;
    end;
  finally
    DecPaintLock;
  end;
end;

procedure TCustomSynEdit.DoOnCommandProcessed(Command: TSynEditorCommand; AChar: Char; Data: pointer);
begin
  if assigned(fOnCommandProcessed) then
    fOnCommandProcessed(self, Command, AChar, Data);
end;

procedure TCustomSynEdit.DoOnProcessCommand(var Command: TSynEditorCommand; var AChar: Char; Data: pointer);
begin
  // DebugLn(['TCustomSynEdit.DoOnProcessCommand Command=',Command]);
  if Command < ecUserFirst then
  begin
    if assigned(fOnProcessCommand) then
      fOnProcessCommand(self, Command, AChar, Data);
  end
  else
  begin
    if assigned(fOnProcessUserCommand) then
      fOnProcessUserCommand(self, Command, AChar, Data);
  end;
end;

procedure TCustomSynEdit.ClearAll;
begin
  InternalBeginUndoBlock;
  try
    SelectAll;
    SelText := '';
  finally
    InternalEndUndoBlock;
  end;
end;

procedure TCustomSynEdit.ClearSelection;
begin
  if SelAvail then
    SelText := '';
end;

function TCustomSynEdit.GetSelectionMode: TSynSelectionMode;
begin
  Result := FBlockSelection.ActiveSelectionMode;
end;

procedure TCustomSynEdit.SetSelectionMode(const Value: TSynSelectionMode);
begin
  FBlockSelection.ActiveSelectionMode := Value;
end;

procedure TCustomSynEdit.InternalBeginUndoBlock(aList: TSynEditUndoList);
begin
  if aList = nil then
    aList := fUndoList;
{$IFDEF SynUndoDebugBeginEnd}
  DebugLnEnter(['>> TCustomSynEdit.InternalBeginUndoBlock', DbgSName(self), ' ', dbgs(self), ' aList=', aList,
    ' FPaintLock=', FPaintLock, ' InGroupCount=', aList.InGroupCount]);
{$ENDIF}
  aList.OnNeedCaretUndo := {$IFDEF FPC}@{$ENDIF}GetCaretUndo;
  aList.BeginBlock;
  IncPaintLock;
  FFoldedLinesView.Lock;
end;

procedure TCustomSynEdit.InternalEndUndoBlock(aList: TSynEditUndoList);
begin
  if aList = nil then
    aList := fUndoList;
  // Write all trimming info to the end of the undo block,
  // so it will be undone first, and other UndoItems do see the expected spaces
  FFoldedLinesView.Unlock;
  // must be last => May call MoveCaretToVisibleArea, which must only happen
  // after unfold
  DecPaintLock;
  aList.EndBlock; // Todo: Doing this after DecPaintLock, can cause duplicate calls to StatusChanged(scModified)
{$IFDEF SynUndoDebugBeginEnd}
  DebugLnEnter(['<< TCustomSynEdit.InternalEndUndoBlock', DbgSName(self), ' ', dbgs(self), ' aList=', aList,
    ' FPaintLock=', FPaintLock, ' InGroupCount=', aList.InGroupCount]);
{$ENDIF}
end;

procedure TCustomSynEdit.BeginUndoBlock{$IFDEF SynUndoDebugBeginEnd}(ACaller: String = ''){$ENDIF};
begin
{$IFDEF SynUndoDebugBeginEnd}
  DebugLnEnter(['>> TCustomSynEdit.BeginUndoBlock ', DbgSName(self), ' ', dbgs(self), ' Caller=', ACaller,
    ' FPaintLock=', FPaintLock, ' InGroupCount=', fUndoList.InGroupCount, '  FIsInDecPaintLock=',
    dbgs(FIsInDecPaintLock)]);
  if ACaller = '' then
    DumpStack;
{$ENDIF}
  fUndoList.OnNeedCaretUndo := {$IFDEF FPC}@{$ENDIF}GetCaretUndo;
  fUndoList.BeginBlock;
  /// /FFoldedLinesView.Lock;
  // FTrimmedLinesView.Lock;
end;

procedure TCustomSynEdit.BeginUpdate(WithUndoBlock: boolean = True);
begin
  IncPaintLock;
{$IFDEF SynUndoDebugBeginEnd}
  if WithUndoBlock and (FPaintLock = 0) and (FUndoBlockAtPaintLock = 0) then
    debugln(['************** TCustomSynEdit.BeginUpdate  PAINTLOCK NOT INCREASED  ', DbgSName(self), ' ', dbgs(self),
      ' FPaintLock=', FPaintLock, ' InGroupCount=', fUndoList.InGroupCount, '  FIsInDecPaintLock=',
      dbgs(FIsInDecPaintLock)]);
{$ENDIF}
  if WithUndoBlock and (FPaintLock > 0) and (FUndoBlockAtPaintLock = 0) then
  begin
    FUndoBlockAtPaintLock := FPaintLock;
    BeginUndoBlock{$IFDEF SynUndoDebugBeginEnd}('SynEdit.BeginUpdate'){$ENDIF};
  end;
end;

procedure TCustomSynEdit.EndUndoBlock{$IFDEF SynUndoDebugBeginEnd}(ACaller: String = ''){$ENDIF};
begin
  // Write all trimming info to the end of the undo block,
  // so it will be undone first, and other UndoItems do see the expected spaces
  // FTrimmedLinesView.UnLock;
  /// /FFoldedLinesView.UnLock;
  fUndoList.EndBlock;
{$IFDEF SynUndoDebugBeginEnd}
  DebugLnEnter(['<< TCustomSynEdit.EndUndoBlock', DbgSName(self), ' ', dbgs(self), ' Caller=', ACaller, ' FPaintLock=',
    FPaintLock, ' InGroupCount=', fUndoList.InGroupCount, '  FIsInDecPaintLock=', dbgs(FIsInDecPaintLock)]);
  // if ACaller = '' then DumpStack;
{$ENDIF}
end;

procedure TCustomSynEdit.EndUpdate;
begin
  DecPaintLock;
end;

procedure TCustomSynEdit.AddKey(Command: TSynEditorCommand; Key1: word; SS1: TShiftState; Key2: word; SS2: TShiftState);
var
  Key: TSynEditKeyStroke;
begin
  Key := Keystrokes.Add;
  Key.Command := Command;
  Key.Key := Key1;
  Key.Shift := SS1;
  Key.Key2 := Key2;
  Key.Shift2 := SS2;
end;

procedure TCustomSynEdit.AfterLoadFromFile;
begin
  if {(not HandleAllocated) or }((FPaintLock > 0) and not((FPaintLock = 1) and FIsInDecPaintLock)) then
  begin
    Include(fStateFlags, sfAfterLoadFromFileNeeded);
    exit;
  end;
  Exclude(fStateFlags, sfAfterLoadFromFileNeeded);
  if assigned(FFoldedLinesView) then
  begin
    ScanRanges;
    FFoldedLinesView.UnfoldAll;
    FFoldedLinesView.CollapseDefaultFolds;
    if FPendingFoldState <> '' then
      SetFoldState(FPendingFoldState);
    TopView := TopView;
  end;
end;

procedure TCustomSynEdit.MarkListChange(Sender: TSynEditMark; Changes: TSynEditMarkChangeReasons);
begin
  if (smcrAdded in Changes) and Sender.IsBookmark then
  begin
    FBookMarks[Sender.BookmarkNumber] := Sender;
    if assigned(FOnPlaceMark) then
      FOnPlaceMark(self, Sender);
  end;
  if (smcrRemoved in Changes) and Sender.IsBookmark then
  begin
    FBookMarks[Sender.BookmarkNumber] := nil;
    if assigned(FOnPlaceMark) then
      FOnClearMark(self, Sender);
  end;

  if (not Sender.Visible) and (not(smcrVisible in Changes)) then
    exit;

  if smcrLine in Changes then
  begin
    InvalidateLine(Sender.OldLine); // TODO: only if mark has special line color, or other code markup
    InvalidateGutterLines(Sender.OldLine, Sender.OldLine);
  end;
  InvalidateLine(Sender.Line); // TODO: only if mark has special line color, or other code markup
  InvalidateGutterLines(Sender.Line, Sender.Line);
end;

function TCustomSynEdit.GetSelStart: Integer; // L505 begin

  function llen(const Data: string): Integer;
  begin
    Result := length(Data) + length(LineEnding);
  end;

var
  loop: Integer;
  p: TPoint;
begin
  if SelAvail then
  begin
    p := BlockBegin;
  end
  else
  begin
    p := LogicalCaretXY;
  end;

  Result := 0;
  loop := 0;
  while (loop < (p.Y - 1)) and (loop < FTheLinesView.Count) do
  begin
    Result := Result + llen(FTheLinesView[loop]);
    inc(loop);
  end;
  if loop < FTheLinesView.Count then
    Result := Result + Min(p.X, length(FTheLinesView[loop]) + 1);
end;

procedure TCustomSynEdit.SetSelStart(const Value: Integer);

  function llen(const Data: string): Integer;
  begin
    Result := length(Data) + length(LineEnding);
  end;

var
  loop: Integer;
  Count: Integer;
begin
  loop := 0;
  Count := 0;
  while (loop < FTheLinesView.Count) and (Count + llen(FTheLinesView[loop]) < Value) do
  begin
    Count := Count + llen(FTheLinesView[loop]);
    inc(loop);
  end;
  { CaretX := value - count;
    CaretY := loop + 1;

    fBlockBegin.X := CaretX;
    fBlockBegin.Y := CaretY; }

  // This seems the same as above, but uses the other fixes inside of SetCaretXY
  // to adjust the cursor pos correctly.
  FCaret.LineBytePos := Point(Value - Count, loop + 1);
  BlockBegin := Point(Value - Count, loop + 1);
end;

function TCustomSynEdit.GetSelEnd: Integer;

  function llen(const Data: string): Integer;
  begin
    Result := length(Data) + length(LineEnding);
  end;

var
  loop: Integer;
  p: TPoint;
begin
  if SelAvail then
  begin
    p := BlockEnd;
  end
  else
  begin
    p := LogicalCaretXY;
  end;

  Result := 0;
  loop := 0;
  while (loop < (p.Y - 1)) and (loop < FTheLinesView.Count) do
  begin
    Result := Result + llen(FTheLinesView[loop]);
    inc(loop);
  end;
  if loop < FTheLinesView.Count then
    Result := Result + p.X;
end;

procedure TCustomSynEdit.SetSelEnd(const Value: Integer);

  function llen(const Data: string): Integer;
  begin
    Result := length(Data) + length(LineEnding);
  end;

var
  p: TPoint;
  loop: Integer;
  Count: Integer;
begin
  loop := 0;
  Count := 0;
  while (loop < FTheLinesView.Count) and (Count + llen(FTheLinesView[loop]) < Value) do
  begin
    Count := Count + llen(FTheLinesView.strings[loop]);
    inc(loop);
  end;
  p.X := Value - Count;
  p.Y := loop + 1;
  BlockEnd := p;
end;

procedure TCustomSynEdit.SetExtraLineSpacing(const Value: Integer);
begin
  if fExtraLineSpacing = Value then
    exit;
  fExtraLineSpacing := Value;
  FTextArea.ExtraLineSpacing := Value;
  //FontChanged(self);
end;

function TCustomSynEdit.GetBookMark(BookMark: Integer; var X, Y: Integer): boolean;
var
  i: Integer;
begin
  Result := False;
  if assigned(Marks) then
    for i := 0 to Marks.Count - 1 do
      if Marks[i].IsBookmark and (Marks[i].BookmarkNumber = BookMark) then
      begin
        X := Marks[i].Column;
        Y := Marks[i].Line;
        X := LogicalToPhysicalPos(Point(X, Y)).X;
        Result := True;
        exit;
      end;
end;

function TCustomSynEdit.IsBookmark(BookMark: Integer): boolean;
var
  X, Y: Integer;
begin
  Result := GetBookMark(BookMark, X, Y);
end;

procedure TCustomSynEdit.MarkTextAsSaved;
begin
  TSynEditStringList(FLines).MarkSaved;
  if FLeftGutter.Visible and FLeftGutter.ChangesPart(0).Visible then
    InvalidateGutter; // Todo: Make the ChangeGutterPart an observer
end;

procedure TCustomSynEdit.ClearUndo;
begin
  fUndoList.Clear;
  fRedoList.Clear;
end;

procedure TCustomSynEdit.SetGutter(const Value: TSynGutter);
begin
  FLeftGutter.Assign(Value);
end;

procedure TCustomSynEdit.SetRightGutter(const AValue: TSynGutter);
begin
  FRightGutter.Assign(AValue);
end;

procedure TCustomSynEdit.GutterChanged(Sender: TObject);
begin
  if (csLoading in ComponentState) then
    exit;
  InvalidateGutter; // Todo: move to gutter
end;

procedure TCustomSynEdit.GutterResized(Sender: TObject);
begin
  if (csLoading in ComponentState) then
    exit;

  GutterChanged(Sender);
{
  if HandleAllocated then
  begin
    RecalcCharsAndLinesInWin(False);
    UpdateScrollBars;
    Invalidate;
  end;}
end;

function TCustomSynEdit.TextLeftPixelOffset(IncludeGutterTextDist: boolean): Integer;
begin
  if FLeftGutter.Visible then
  begin
    Result := FLeftGutter.Width;
    if IncludeGutterTextDist then
      inc(Result, GutterTextDist);
  end
  else
  begin
    Result := 0;
    if IncludeGutterTextDist then
      inc(Result, 1); // include space for caret at pos.x=1 (if FOffsetX = -1)
  end;
end;

function TCustomSynEdit.TextRightPixelOffset: Integer;
begin
  if FRightGutter.Visible then
    Result := FRightGutter.Width
  else
    Result := 0;
end;

procedure TCustomSynEdit.LockUndo;
begin
  fUndoList.Lock;
  fRedoList.Lock
end;

procedure TCustomSynEdit.UnlockUndo;
begin
  fUndoList.Unlock;
  fRedoList.Unlock;
end;

procedure TCustomSynEdit.WMMouseWheel(var Message: TWMMouseWheel);
var
  lState: TShiftState;
begin
  if ((sfHorizScrollbarVisible in fStateFlags) and (Message.Pos.x > Height)) or
    ((sfVertScrollbarVisible in fStateFlags) and (Message.Pos.X > Width)) then
  begin
    inherited;
    exit;
  end;

  lState := TShiftState(Message.Keys) {- [ssCaps, ssNum, ssScroll]};
  // Remove unreliable states, see http://bugs.freepascal.org/view.php?id=20065

  FMouseClickDoPopUp := False;
  IncPaintLock;
  try
    if Message.WheelDelta > 0 then
    begin
      FindAndHandleMouseAction(mbXWheelUp, lState, Message.Xpos, Message.Ypos, ccSingle, cdDown, Message.WheelDelta);
    end
    else
    begin
      // send megative delta
      FindAndHandleMouseAction(mbXWheelDown, lState, Message.Xpos, Message.Ypos, ccSingle, cdDown, Message.WheelDelta);
    end;
  finally
    DecPaintLock;
  end;

  if FMouseClickDoPopUp and (PopupMenu <> nil) then
  begin
    PopupMenu.PopupComponent := self;
//    PopupMenu.PopUp;
  end;

  Message.Result := 1 // handled, skip further handling by interface
end;

procedure TCustomSynEdit.SetWantTabs(const Value: boolean);
begin
  fWantTabs := Value;
end;

procedure TCustomSynEdit.SetTabWidth(Value: Integer);
begin
  Value := Trunc(MinMax(Value, 1 { 0 } , 256));
  if (Value <> fTabWidth) then
  begin
    fTabWidth := Value;
    FTabbedLinesView.TabWidth := Value;
//    Invalidate; // to redraw text containing tab chars
  end;
end;

// find / replace
function TCustomSynEdit.SearchReplace(const ASearch, AReplace: string; AOptions: TSynSearchOptions): Integer;
var
  StartPos: TPoint;
begin
  if (ssoFindContinue in AOptions) and SelAvail then
  begin
    if ssoBackwards in AOptions then
      StartPos := BlockBegin
    else
      StartPos := BlockEnd;
  end
  else
    StartPos := LogicalCaretXY;
  Result := SearchReplaceEx(ASearch, AReplace, AOptions, StartPos);
end;

function TCustomSynEdit.SearchReplaceEx(const ASearch, AReplace: string; AOptions: TSynSearchOptions;
  AStart: TPoint): Integer;
var
  ptStart, ptEnd: TPoint; // start and end of the search range
  ptCurrent: TPoint; // current search position
  nFound: Integer;
  bBackward, bFromCursor: boolean;
  bPrompt: boolean;
  bReplace, bReplaceAll: boolean;
  nAction: TSynReplaceAction;
  CurReplace: string;
  ptFoundStart, ptFoundEnd: TPoint;

  function InValidSearchRange(First, Last: Integer): boolean;
  begin
    Result := True;
    case FBlockSelection.ActiveSelectionMode of
      smNormal:
        if ((ptCurrent.Y = ptStart.Y) and (First < ptStart.X)) or ((ptCurrent.Y = ptEnd.Y) and (Last > ptEnd.X)) then
          Result := False;
      smColumn:
        Result := (First >= ptStart.X) and (Last <= ptEnd.X);
    end;
  end;

begin
  Result := 0;
  // can't search for or replace an empty string
  if length(ASearch) = 0 then
    exit;
  // get the text range to search in, ignore the "Search in selection only"
  // option if nothing is selected
  bBackward := (ssoBackwards in AOptions);
  bPrompt := (ssoPrompt in AOptions);
  bReplace := (ssoReplace in AOptions);
  bReplaceAll := (ssoReplaceAll in AOptions);
  bFromCursor := not(ssoEntireScope in AOptions);
  if not SelAvail then
    Exclude(AOptions, ssoSelectedOnly);
  if (ssoSelectedOnly in AOptions) then
  begin
    ptStart := BlockBegin;
    ptEnd := BlockEnd;
    // search the whole line in the line selection mode
    if (FBlockSelection.ActiveSelectionMode = smLine) then
    begin
      ptStart.X := 1;
      ptEnd.X := length(FTheLinesView[ptEnd.Y - 1]) + 1;
    end
    else if (FBlockSelection.ActiveSelectionMode = smColumn) then
      // make sure the start column is smaller than the end column
      if (ptStart.X > ptEnd.X) then
      begin
        nFound := ptStart.X;
        ptStart.X := ptEnd.X;
        ptEnd.X := nFound;
      end;
    // ignore the cursor position when searching in the selection
    if bBackward then
      ptCurrent := ptEnd
    else
      ptCurrent := ptStart;
  end
  else
  begin
    ptStart := Point(1, 1);
    ptEnd.Y := FTheLinesView.Count;
    ptEnd.X := length(FTheLinesView[ptEnd.Y - 1]) + 1;
    if bFromCursor then
      if bBackward then
        ptEnd := AStart
      else
        ptStart := AStart;
    if bBackward then
      ptCurrent := ptEnd
    else
      ptCurrent := ptStart;
  end;
  // initialize the search engine
  fTSearch.Sensitive := ssoMatchCase in AOptions;
  fTSearch.Whole := ssoWholeWord in AOptions;
  fTSearch.Pattern := ASearch;
//  fTSearch.RegularExpressions := ssoRegExpr in AOptions;
//  fTSearch.RegExprMultiLine := ssoRegExprMultiLine in AOptions;
//  fTSearch.Replacement := AReplace;
//  fTSearch.Backwards := bBackward;
  // search while the current search position is inside of the search range
  IncPaintLock;
  try
    // DebugLn(['TCustomSynEdit.SearchReplace ptStart=',dbgs(ptStart),' ptEnd=',dbgs(ptEnd),' ASearch="',dbgstr(ASearch),'" AReplace="',dbgstr(AReplace),'"']);
//    while fTSearch.FindNextOne(FTheLinesView, ptStart, ptEnd, ptFoundStart, ptFoundEnd, True) do
//    begin
//      // DebugLn(['TCustomSynEdit.SearchReplace FOUND ptStart=',dbgs(ptStart),' ptEnd=',dbgs(ptEnd),' ptFoundStart=',dbgs(ptFoundStart),' ptFoundEnd=',dbgs(ptFoundEnd)]);
//      // check if found place is entirely in range
//      if (FBlockSelection.ActiveSelectionMode <> smColumn) or
//        ((ptFoundStart.Y = ptFoundEnd.Y) and (ptFoundStart.X >= ptStart.X) and (ptFoundEnd.X <= ptEnd.X)) then
//      begin
//        // pattern found
//        inc(Result);
//        // Select the text, so the user can see it in the OnReplaceText event
//        // handler or as the search result.
//        BlockBegin := ptFoundStart;
//        if bBackward then
//          LogicalCaretXY := BlockBegin;
//        BlockEnd := ptFoundEnd;
//        if not bBackward then
//          LogicalCaretXY := ptFoundEnd;
//        // If it's a 'search' only we can leave the procedure now.
//        if not(bReplace or bReplaceAll) then
//          exit;
//        // Prompt and replace or replace all.  If user chooses to replace
//        // all after prompting, turn off prompting.
//        CurReplace := AReplace;
//        if ssoRegExpr in AOptions then
//          CurReplace := fTSearch.RegExprReplace;
//        if bPrompt and assigned(fOnReplaceText) then
//        begin
//          EnsureCursorPosVisible;
//          try
//            DecPaintLock;
//            nAction := DoOnReplaceText(ASearch, CurReplace, ptFoundStart.Y, ptFoundStart.X);
//          finally
//            IncPaintLock;
//          end;
//          if nAction = raCancel then
//            exit;
//        end
//        else
//          nAction := raReplace;
//        if not(nAction = raSkip) then
//        begin
//          // user has been prompted and has requested to silently replace all
//          // so turn off prompting
//          if nAction = raReplaceAll then
//          begin
//            bReplaceAll := True;
//            bPrompt := False;
//          end;
//          // replace text
//          // DebugLn(['TCustomSynEdit.SearchReplace OldSel="',dbgstr(SelText),'"']);
//          SetSelTextExternal(CurReplace);
//          // DebugLn(['TCustomSynEdit.SearchReplace NewSel="',dbgstr(SelText),'"']);
//          // adjust positions
//          ptEnd := AdjustPositionAfterReplace(ptEnd, ptFoundStart, ptFoundEnd, CurReplace);
//          ptFoundEnd := AdjustPositionAfterReplace(ptFoundEnd, ptFoundStart, ptFoundEnd, CurReplace);
//        end;
//        if not bReplaceAll then
//          exit;
//      end;
//      // shrink search range for next search
//      if ssoSearchInReplacement in AOptions then
//      begin
//        if bBackward then
//        begin
//          ptEnd := ptFoundEnd;
//        end
//        else
//        begin
//          ptStart := ptFoundStart;
//        end;
//      end
//      else
//      begin
//        if bBackward then
//        begin
//          ptEnd := ptFoundStart;
//        end
//        else
//        begin
//          ptStart := ptFoundEnd;
//        end;
//      end;
//       //DebugLn(['TCustomSynEdit.SearchReplace FIND NEXT ptStart=',dbgs(ptStart),' ptEnd=',dbgs(ptEnd)]);
//    end;
  finally
    DecPaintLock;
  end;
end;

{$IFDEF SYN_MBCSSUPPORT}

procedure TCustomSynEdit.MBCSGetSelRangeInLineWhenColumnSelectionMode(const s: string; var ColFrom, ColTo: Integer);
// --ColFrom and ColTo are in/out parameter. their range
// will be from 1 to MaxInt.
// --a range of selection means:  Copy(s, ColFrom, ColTo - ColFrom);
// be careful what ColTo means.
var
  Len: Integer;
begin
  Len := length(s);
  if (0 < ColFrom) and (ColFrom <= Len) then
    if mbTrailByte = ByteType(s, ColFrom) then
      inc(ColFrom);
  if (0 < ColTo) and (ColTo <= Len) then
    if mbTrailByte = ByteType(s, ColTo) then
      inc(ColTo);
end;

{$ENDIF}

function TCustomSynEdit.IsPointInSelection(Value: TPoint): boolean;
var
  ptBegin, ptEnd: TPoint;
begin
  ptBegin := BlockBegin;
  ptEnd := BlockEnd;
  if (Value.Y >= ptBegin.Y) and (Value.Y <= ptEnd.Y) and ((ptBegin.Y <> ptEnd.Y) or (ptBegin.X <> ptEnd.X)) then
  begin
    if FBlockSelection.SelectionMode = smLine then
      Result := True
    else if (FBlockSelection.ActiveSelectionMode = smColumn) then
    begin
      if (ptBegin.X > ptEnd.X) then
        Result := (Value.X >= ptEnd.X) and (Value.X < ptBegin.X)
      else if (ptBegin.X < ptEnd.X) then
        Result := (Value.X >= ptBegin.X) and (Value.X < ptEnd.X)
      else
        Result := False;
    end
    else
      Result := ((Value.Y > ptBegin.Y) or (Value.X >= ptBegin.X)) and ((Value.Y < ptEnd.Y) or (Value.X < ptEnd.X));
  end
  else
    Result := False;
end;

procedure TCustomSynEdit.BookMarkOptionsChanged(Sender: TObject);
begin
  InvalidateGutter;
end;

procedure TCustomSynEdit.SetOptions(Value: TSynEditorOptions);
var
  ChangedOptions: TSynEditorOptions;
  m: TSynEditorOption;
  MOpt: TSynEditorMouseOptions;
  f: boolean;
begin
  Value := Value - SYNEDIT_UNIMPLEMENTED_OPTIONS;
  if (Value = FOptions) then
    exit;

  ChangedOptions := (FOptions - Value) + (Value - FOptions);
  FOptions := Value;
  UpdateOptions;

  if not(eoScrollPastEol in Options) then
    LeftChar := LeftChar;
  if (eoScrollPastEol in Options) or (eoScrollPastEof in Options) then
  begin
    UpdateScrollBars;
    TopView := TopView;
  end;
  // (un)register HWND as drop target
  if (eoDropFiles in ChangedOptions) and not(csDesigning in ComponentState) {and HandleAllocated }then;
  // ToDo DragAcceptFiles
  if (eoPersistentCaret in ChangedOptions) {and HandleAllocated }then
  begin
    UpdateCaret;
    if not isFocused then
      FScreenCaret.Visible := (eoPersistentCaret in FOptions);
  end;
  if (eoShowSpecialChars in ChangedOptions) then
  begin
    if eoShowSpecialChars in FOptions then
      FTextArea.VisibleSpecialChars := VisibleSpecialChars
    else
      FTextArea.VisibleSpecialChars := [];
    {if HandleAllocated then
      Invalidate;}
  end;
  fMarkupSpecialChar.Enabled := (eoShowSpecialChars in FOptions);

  if (eoHideRightMargin in ChangedOptions) then
    FTextArea.RightEdgeVisible := not(eoHideRightMargin in FOptions);

  (* Deal with deprecated Mouse values
    Those are all controlled by mouse-actions.
    As long as the default mouse actions are set, the below will act as normal
  *)

  MOpt := FMouseOptions;
  f := False;
  for m := low(SYNEDIT_OLD_MOUSE_OPTIONS_MAP) to high(SYNEDIT_OLD_MOUSE_OPTIONS_MAP) do
    if (m in SYNEDIT_OLD_MOUSE_OPTIONS) and (m in ChangedOptions) then
    begin
      f := True;
      if (m in FOptions) then
        MOpt := MOpt + [SYNEDIT_OLD_MOUSE_OPTIONS_MAP[m]]
      else
        MOpt := MOpt - [SYNEDIT_OLD_MOUSE_OPTIONS_MAP[m]];
    end;
  if f then
    MouseOptions := MOpt;

  FOptions := Value; // undo changes applied by MouseOptions

end;

procedure TCustomSynEdit.UpdateOptions;
begin
  FTrimmedLinesView.Enabled := eoTrimTrailingSpaces in FOptions;
  FCaret.AllowPastEOL := (eoScrollPastEol in FOptions);
  FCaret.KeepCaretX := (eoKeepCaretX in FOptions);
  FBlockSelection.Enabled := not(eoNoSelection in FOptions);
  fUndoList.GroupUndo := eoGroupUndo in FOptions;
end;

procedure TCustomSynEdit.SetOptions2(const Value: TSynEditorOptions2);
var
  ChangedOptions: TSynEditorOptions2;
begin
  if (Value <> FOptions2) then
  begin
    ChangedOptions := (FOptions2 - Value) + (Value - FOptions2);
    FOptions2 := Value;
    UpdateOptions2;
    if eoAlwaysVisibleCaret in FOptions2 then
      MoveCaretToVisibleArea;
    if (eoAutoHideCursor in ChangedOptions) and not(eoAutoHideCursor in FOptions2) then
      UpdateCursor;
  end;
end;

procedure TCustomSynEdit.UpdateOptions2;
begin
  FBlockSelection.Persistent := eoPersistentBlock in FOptions2;
  FCaret.SkipTabs := (eoCaretSkipTab in FOptions2);
end;

procedure TCustomSynEdit.SetMouseOptions(AValue: TSynEditorMouseOptions);
var
  ChangedOptions: TSynEditorMouseOptions;
  m: TSynEditorOption;
begin
  if FMouseOptions = AValue then
    exit;

  ChangedOptions := (FMouseOptions - AValue) + (AValue - FMouseOptions);
  FMouseOptions := AValue;
  // changes take effect when MouseActions are accessed

  for m := low(SYNEDIT_OLD_MOUSE_OPTIONS_MAP) to high(SYNEDIT_OLD_MOUSE_OPTIONS_MAP) do
    if (m in SYNEDIT_OLD_MOUSE_OPTIONS) and (SYNEDIT_OLD_MOUSE_OPTIONS_MAP[m] in ChangedOptions) and
      not(SYNEDIT_OLD_MOUSE_OPTIONS_MAP[m] in FMouseOptions) then
      FOptions := FOptions - [m];

  if (emShowCtrlMouseLinks in ChangedOptions) then
  begin
    if assigned(fMarkupCtrlMouse) then
      fMarkupCtrlMouse.UpdateCtrlMouse;
    UpdateCursor;
  end;
end;

procedure TCustomSynEdit.UpdateMouseOptions;
begin
  //
end;

procedure TCustomSynEdit.SetOptionFlag(Flag: TSynEditorOption; Value: boolean);
begin
  if (Value <> (Flag in FOptions)) then
  begin
    if Value then
      Options := Options + [Flag]
    else
      Options := Options - [Flag];
  end;
end;

procedure TCustomSynEdit.SizeOrFontChanged(bFont: boolean);
begin
{  if HandleAllocated then
  begin
    LastMouseCaret := Point(-1, -1);
    RecalcCharsAndLinesInWin(False);

    // DebugLn('TCustomSynEdit.SizeOrFontChanged LinesInWindow=',dbgs(LinesInWindow),' ClientHeight=',dbgs(ClientHeight),' ',dbgs(LineHeight));
    // debugln('TCustomSynEdit.SizeOrFontChanged A ClientWidth=',dbgs(ClientWidth),' FLeftGutter.Width=',dbgs(FLeftGutter.Width),' ScrollBarWidth=',dbgs(ScrollBarWidth),' CharWidth=',dbgs(CharWidth),' CharsInWindow=',dbgs(CharsInWindow),' Width=',dbgs(Width));
    if bFont then
    begin
      UpdateScrollBars;
      Exclude(fStateFlags, sfCaretChanged);
      Invalidate;
    end
    else
      UpdateScrollBars;
    Exclude(fStateFlags, sfScrollbarChanged); // TODO: Why?
    if not(eoScrollPastEol in Options) then
      LeftChar := LeftChar;
    if not(eoScrollPastEof in Options) then
      TopView := TopView;
  end;}
end;

procedure TCustomSynEdit.RecalcCharsAndLinesInWin(CheckCaret: boolean);
var
  OldLinesInWindow: Integer;
  l, r: Integer;
begin
  if FLeftGutter.Visible then
    l := FLeftGutter.Width
  else
    l := 0;
  if FRightGutter.Visible then
    r := FRightGutter.Width
  else
    r := 0;

  OldLinesInWindow := FTextArea.LinesInWindow;

  // TODO: lock FTextArea, so size re-calc is done once only
  FPaintArea.SetBounds(0, 0, Round(Height), Round(Width));
  FPaintArea.LeftGutterWidth := l;
  FPaintArea.RightGutterWidth := r;

  if FLeftGutter.Visible then
    FTextArea.Padding[bsLeft] := GutterTextDist
  else
    FTextArea.Padding[bsLeft] := 1;
  if FRightGutter.Visible then
    FTextArea.Padding[bsRight] := 0 // GutterTextDist
  else
    FTextArea.Padding[bsRight] := 0;

  // CharsInWindow := Max(1, w div CharWidth);
  if OldLinesInWindow <> FTextArea.LinesInWindow then
    StatusChanged([scLinesInWindow]);

  FFoldedLinesView.LinesInWindow := LinesInWindow;
  fMarkupManager.LinesInWindow := LinesInWindow;

  FScreenCaret.Lock;
  FScreenCaret.ClipRect := FTextArea.Rect;
  // FScreenCaret.ClipRect := Rect(TextLeftPixelOffset(False), 0,
  // ClientWidth - TextRightPixelOffset - ScrollBarWidth + 1,
  // ClientHeight - ScrollBarWidth);
  FScreenCaret.ClipExtraPixel := Trunc(FTextArea.Bounds.Right - FTextArea.Bounds.Left - CharsInWindow * Width);
  FScreenCaret.Unlock;

  if CheckCaret then
  begin
    if not(eoScrollPastEol in Options) then
      LeftChar := LeftChar;
    if not(eoScrollPastEof in Options) then
      TopView := TopView;
  end;
end;

procedure TCustomSynEdit.MoveCaretHorz(DX: Integer);
var
  NewCaret: TPoint;
  s: String;
  PhysicalLineLen: Integer;
begin
  NewCaret := Point(CaretX + DX, CaretY);
  if NewCaret.X < 1 then
  begin
    if (eoScrollPastEol in FOptions) or (NewCaret.Y = 1) then
      NewCaret.X := 1
    else
    begin
      // move to end of prev line
      NewCaret.Y := FFoldedLinesView.TextPosAddLines(NewCaret.Y, -1);
      s := FTheLinesView[NewCaret.Y - 1];
      PhysicalLineLen := LogicalToPhysicalPos(Point(length(s) + 1, NewCaret.Y)).X - 1;
      NewCaret.X := PhysicalLineLen + 1;
    end;
  end
  else if not(eoScrollPastEol in FOptions) then
  begin
    s := LineText;
    PhysicalLineLen := LogicalToPhysicalPos(Point(length(s) + 1, CaretY)).X - 1;
    if (NewCaret.X > PhysicalLineLen + 1) and (DX > 0) then
    begin
      // move to start of next line (if it was a move to the right)
      NewCaret.X := 1;
      NewCaret.Y := FFoldedLinesView.TextPosAddLines(NewCaret.Y, +1);
    end;
  end;
  DoIncPaintLock(self); // No editing is taking place
  FCaret.IncForcePastEOL;
  if DX > 0 then
    FCaret.IncForceAdjustToNextChar;
  FCaret.LineCharPos := NewCaret;
  FCaret.DecForcePastEOL;
  if DX > 0 then
    FCaret.DecForceAdjustToNextChar;
  DoDecPaintLock(self);
end;

procedure TCustomSynEdit.MoveCaretVert(DY: Integer);
// moves Caret vertical DY unfolded lines
var
  NewCaret: TPoint;
  OldCaret: TPoint;
begin
  OldCaret := CaretXY;
  NewCaret := OldCaret;
  NewCaret.Y := FFoldedLinesView.TextPosAddLines(NewCaret.Y, DY);
  DoIncPaintLock(self); // No editing is taking place
  FCaret.LinePos := NewCaret.Y;
  DoDecPaintLock(self);
end;

procedure TCustomSynEdit.SetCaretAndSelection(const ptCaret, ptBefore, ptAfter: TPoint;
  Mode: TSynSelectionMode = smCurrent; MakeSelectionVisible: boolean = False);
// caret is physical (screen)
// Before, After is logical (byte)
var
  L1, L2, LBottomLine, LCaretFirst, LCaretLast: Integer;
begin
  DoIncPaintLock(self); // No editing is taking place

  CaretXY := ptCaret;
  SetBlockBegin(ptBefore);
  SetBlockEnd(ptAfter);
  if Mode <> smCurrent then
    FBlockSelection.ActiveSelectionMode := Mode;

  if MakeSelectionVisible then
  begin
    // l1 := FBlockSelection.FirstLineBytePos;;
    LBottomLine := FFoldedLinesView.TextPosAddLines(TopLine, LinesInWindow);

    LCaretFirst := CaretY;
    LCaretLast := Max(1, FFoldedLinesView.TextPosAddLines(CaretY, 1 - LinesInWindow));
    // Will have caret on last visible line

    L1 := Min(LCaretFirst, FBlockSelection.FirstLineBytePos.Y);
    L2 := Max(LCaretFirst, FBlockSelection.LastLineBytePos.Y);

    if CaretY < TopLine then
    begin
      // Scrolling up,  Topline = L1 ; but ensure Caret
      TopLine := Max(LCaretLast, Min(LCaretFirst, L1));
    end
    else if CaretY > LBottomLine then
    begin
      // Scrolling down, LastLine = L2
      TopLine := Max(LCaretLast, Min(LCaretFirst, FFoldedLinesView.TextPosAddLines(L2, 1 - LinesInWindow)));
    end
    else
    begin
      // Caret alreayd visible, check block
      if L1 < TopLine then
        TopLine := Max(LCaretLast, Min(LCaretFirst, L1))
      else if L2 > LBottomLine then
        TopLine := Max(LCaretLast, Min(LCaretFirst, FFoldedLinesView.TextPosAddLines(L2, 1 - LinesInWindow)));
    end;
  end;

  DoDecPaintLock(self);
end;

procedure TCustomSynEdit.RecalcCharExtent;
var
  i: Integer;
begin
  (* Highlighter or Font changed *)

//  fFontDummy.Assign(Font);
  with fFontDummy do
  begin
    // Keep GTK happy => By ensuring a change the XFLD fontname gets cleared
//    Pitch := fpVariable;
    Style := [TFontStyle.fsBold];
//    Pitch := fpDefault; // maybe Fixed
    // TODO: Clear style only, if Highlighter uses styles
    Style := []; // Reserved for Highlighter
  end;
  // debugln(['TCustomSynEdit.RecalcCharExtent ',fFontDummy.Name,' ',fFontDummy.Size]);
  // debugln('TCustomSynEdit.RecalcCharExtent A UseUTF8=',dbgs(UseUTF8),
  // ' Font.CanUTF8='+dbgs(Font.CanUTF8)+' CharHeight=',dbgs(CharHeight));

  fTextDrawer.BaseFont := fFontDummy;
  if assigned(fHighlighter) then
    for i := 0 to Pred(fHighlighter.AttrCount) do
      fTextDrawer.BaseStyle := fHighlighter.Attribute[i].Style;
  fTextDrawer.CharExtra := fExtraCharSpacing;

  FUseUTF8 := fTextDrawer.UseUTF8;
  FLines.IsUtf8 := FUseUTF8;

  FScreenCaret.Lock;
  try
    FScreenCaret.CharWidth := CharWidth;
    FScreenCaret.CharHeight := LineHeight - Max(0, ExtraLineSpacing);
    SizeOrFontChanged(True);
  finally
    FScreenCaret.Unlock;
  end;
  UpdateScrollBars;

  // debugln('TCustomSynEdit.RecalcCharExtent UseUTF8=',dbgs(UseUTF8),' Font.CanUTF8=',dbgs(Font.CanUTF8));
end;

procedure TCustomSynEdit.HighlighterAttrChanged(Sender: TObject);
begin
  RecalcCharExtent;
//  Invalidate;
  // TODO: obey paintlock
  if fHighlighter.AttributeChangeNeedScan then
  begin
    fHighlighter.CurrentLines := FTheLinesView;
    fHighlighter.ScanAllRanges;
    fMarkupManager.TextChanged(0, FTheLinesView.Count - 1);
    TopView := TopView;
  end;
end;

procedure TCustomSynEdit.StatusChanged(AChanges: TSynStatusChanges);
begin
  fStatusChanges := fStatusChanges + AChanges;
  if PaintLock = 0 then
    DoOnStatusChange(fStatusChanges);
end;

procedure TCustomSynEdit.DoTabKey;
var
  i, iLine: Integer;
  PrevLine, Spaces: string;
  p: PChar;
  OldCaretX: Integer;
begin
  if (eoTabIndent in Options) and SelAvail then
  begin
    DoBlockIndent;
    exit;
  end;

  InternalBeginUndoBlock;
  try
    i := 0;
    OldCaretX := CaretX;
    SelText := '';
    // With a multi-line block the caret may have advanced, avoid negative spaces
    if CaretX > OldCaretX then
      OldCaretX := CaretX;
    if eoSmartTabs in FOptions then
    begin
      iLine := CaretY - 1;
      if (iLine > 0) and (iLine < FTheLinesView.Count) then
      begin
        repeat
          dec(iLine);
          if iLine < 0 then
            break;
          PrevLine := FTheLinesView[iLine];
        until PhysicalLineLength(PrevLine, iLine) > OldCaretX - 1;

        if iLine >= 0 then
        begin
          p := @PrevLine[PhysicalToLogicalCol(PrevLine, iLine, OldCaretX)];
          // scan over non-whitespaces
          while not(p^ in [#0, #9, #32]) do
            inc(p);
          // scan over whitespaces
          while (p^ in [#9, #32]) do
            inc(p);
          i := LogicalToPhysicalCol(PrevLine, iLine, p - @PrevLine[1] + 1) - CaretX;
        end;
      end;
    end;
    if i <= 0 then
    begin
      i := TabWidth - (CaretX - 1) mod TabWidth;
      if i = 0 then
        i := TabWidth;
    end;
    // i now contains the needed spaces
    Spaces := CreateTabsAndSpaces(CaretX, i, TabWidth, not(eoTabsToSpaces in Options));
    // debugln('TCustomSynEdit.DoTabKey Spaces="',DbgStr(Spaces),'" TabChar=',DbgStr(TabChar));
    OldCaretX := CaretX;
    // debugln('TCustomSynEdit.DoTabKey Before SetSelText Line="',DbgStr(GetLineText),'"');
    SetSelTextExternal(Spaces);
    // debugln('TCustomSynEdit.DoTabKey After SetSelText Line="',DbgStr(GetLineText),'"');
    CaretX := OldCaretX + i;
    // debugln('TCustomSynEdit.DoTabKey StartOfBlock=',dbgs(StartOfBlock),' fBlockEnd=',dbgs(fBlockEnd),' Spaces="',Spaces,'"');
  finally
    InternalEndUndoBlock;
  end;
  EnsureCursorPosVisible;
end;

(*procedure TCustomSynEdit.CreateWnd;
begin
  inherited;
  if (eoDropFiles in FOptions) and not(csDesigning in ComponentState) then
{$IFDEF SYN_LAZARUS}
    // ToDo DragAcceptFiles
      ;
{$ELSE}
    DragAcceptFiles(Handle, True);
{$ENDIF}
  SizeOrFontChanged(True);
end;

procedure TCustomSynEdit.DestroyWnd;
begin
  if (eoDropFiles in FOptions) and not(csDesigning in ComponentState) then
  begin
{$IFDEF SYN_LAZARUS}
    // ToDo DragAcceptFiles
    ;
{$ELSE}
    DragAcceptFiles(Handle, False);
{$ENDIF}
  end;
{$IFDEF EnableDoubleBuf}
  FreeAndNil(BufferBitmap);
{$ENDIF}
  SurrenderPrimarySelection;
  inherited DestroyWnd;
end;*)

procedure TCustomSynEdit.DoBlockIndent;
var
  BB, BE: TPoint;
  Line: PChar;
  Len, e, Y: Integer;
  Spaces, Tabs: String;

  function GetLeadWSLen: Integer;
  var
    Run: PChar;
  begin
    Run := Line;
    while (Run[0] in [' ', #9]) do
      inc(Run);
    Result := Run - Line;
  end;

begin
  IncPaintLock;
  FBlockSelection.IncPersistentLock;
  try
    // build text to insert
    if not SelAvail then
    begin
      BB := CaretXY;
      BE := CaretXY;
      e := BE.Y;
    end
    else
    begin
      BB := BlockBegin;
      BE := BlockEnd;
      if (BE.X = 1) then
        e := BE.Y - 1
      else
        e := BE.Y;
    end;

    Spaces := StringOfChar(#32, FBlockIndent);
    Tabs := StringOfChar(#9, FBlockTabIndent);
    fUndoList.Lock;
    fRedoList.Lock;
    try
      for Y := BB.Y to e do
      begin
        Line := PChar(FTheLinesView[Y - 1]);
        Len := GetLeadWSLen;
        FTheLinesView.EditInsert(Len + 1, Y, Spaces);
        FTheLinesView.EditInsert(1, Y, Tabs);
      end;
    finally
      fUndoList.Unlock;
      fRedoList.Unlock;
    end;

    fUndoList.AddChange(TSynEditUndoIndent.Create(BB.Y, e, FBlockIndent, FBlockTabIndent));
  finally
    FTrimmedLinesView.ForceTrim; // Otherwise it may reset the block
    FCaret.LineBytePos := FBlockSelection.EndLineBytePos;
    FBlockSelection.DecPersistentLock;
    DecPaintLock;
  end;
end;

procedure TCustomSynEdit.DoBlockUnindent;
const
  LineEnd = #10;
var
  BB, BE: TPoint;
  FullStrToDelete: PChar;
  Line: PChar;
  Len, LogP1, PhyP1, LogP2, PhyP2, Y, StrToDeleteLen, e: Integer;
  i, i2, j: Integer;
  SomethingDeleted: boolean;
  HasTab: boolean;

  function GetLeadWSLen: Integer;
  var
    Run: PChar;
  begin
    Run := Line;
    HasTab := False;
    while (Run[0] in [' ', #9]) do
    begin
      HasTab := HasTab or (Run[0] = #9);
      inc(Run);
    end;
    Result := Run - Line;
  end;

begin
  if not SelAvail then
  begin
    BB := CaretXY;
    BE := CaretXY;
    e := BE.Y;
  end
  else
  begin
    BB := BlockBegin;
    BE := BlockEnd;
    // convert selection to complete lines
    if BE.X = 1 then
      e := BE.Y - 1
    else
      e := BE.Y;
  end;

  IncPaintLock;
  FBlockSelection.IncPersistentLock;
  // build string to delete
  StrToDeleteLen := (FBlockIndent + length(LineEnd)) * (e - BB.Y + 1) + 1;
  // chars per line * lines-1    + last line + null char
  FullStrToDelete := StrAlloc(StrToDeleteLen);
  try
    FullStrToDelete[0] := #0;
    SomethingDeleted := False;

    fUndoList.Lock;
    fRedoList.Lock;

    // before locking the undo list
    for Y := BB.Y to e do
    begin
      Line := PChar(FTheLinesView[Y - 1]);
      Len := GetLeadWSLen;
      LogP1 := Len + 1;
      if HasTab and (Len > 0) then
      begin
        // LogP1, PhyP1 log and phys of the first none-whitespace
        PhyP1 := LogicalToPhysicalPos(Point(LogP1, Y)).X;
        // LogP2, PhyP2 log and phys of the point to which to delete back
        LogP2 := PhysicalToLogicalPos(Point(Max(PhyP1 - FBlockIndent, 1), Y)).X;
        PhyP2 := LogicalToPhysicalPos(Point(LogP2, Y)).X;

        if PhyP1 - PhyP2 <> FBlockIndent then
        begin
          // need tab to space
          StrCat(FullStrToDelete, PChar(copy(Line, LogP2, LogP1 - LogP2)));
          StrCat(FullStrToDelete, PChar(LineEnd));
          FTheLinesView.EditDelete(LogP2, Y, LogP1 - LogP2);
          SomethingDeleted := True;

          fUndoList.Unlock;
          fRedoList.Unlock;
          FTheLinesView.EditInsert(LogP2, Y, StringOfChar(' ', PhyP1 - PhyP2 - FBlockIndent));
          fUndoList.Lock;
          fRedoList.Lock;
          continue;
        end;
        // tabs present, but no replacement needed (LogP1, LogP2 are correct
      end
      else
      begin
        // no tabs present
        LogP2 := Max(LogP1 - FBlockIndent, 1);
      end;

      // Remove spaces (or tab)
      if LogP1 - LogP2 > 0 then
        StrCat(FullStrToDelete, PChar(copy(Line, LogP2, LogP1 - LogP2)));
      StrCat(FullStrToDelete, PChar(LineEnd));
      if LogP1 - LogP2 > 0 then
        FTheLinesView.EditDelete(LogP2, Y, LogP1 - LogP2);
      SomethingDeleted := SomethingDeleted or (LogP1 - LogP2 > 0);

      // Todo: create FullTabStrToDelete for tabs
      fUndoList.Unlock;
      fRedoList.Unlock;
      Line := PChar(FTheLinesView[Y - 1]);
      j := 0;
      for i := 1 to FBlockTabIndent do
      begin
        i2 := fTabWidth;
        while (i2 > 0) and (Line[j] = #32) do
        begin
          dec(i2);
          inc(j);
        end;
        if (i2 > 0) and (Line[j] = #9) then
          inc(j);
      end;
      if j > 0 then
        FTheLinesView.EditDelete(1, Y, j);
      fUndoList.Lock;
      fRedoList.Lock;

    end;

    fUndoList.Unlock;
    fRedoList.Unlock;

    if SomethingDeleted then
      fUndoList.AddChange(TSynEditUndoUnIndent.Create(BB.Y, e, FullStrToDelete));

    FTrimmedLinesView.ForceTrim; // Otherwise it may reset the block
  finally
    StrDispose(FullStrToDelete);
    FCaret.LineBytePos := FBlockSelection.EndLineBytePos;
    FBlockSelection.DecPersistentLock;
    DecPaintLock;
  end;
end;

procedure TCustomSynEdit.DoHomeKey(AMode: TSynHomeMode = synhmDefault);
// jump to start of line (x=1),
// or if already there, jump to first non blank char
// or if blank line, jump to line indent position
// if eoEnhanceHomeKey and behind alternative point then jump first
var
  s: string;
  FirstNonBlank: Integer;
  LineStart: Longint;
  OldPos: TPoint;
  NewPos: TPoint;
begin
  OldPos := CaretXY;
  NewPos := OldPos;

  if not(eoEnhanceHomeKey in FOptions) and (CaretX > 1) and (AMode in [synhmDefault]) then
  begin
    // not at start of line -> jump to start of line
    NewPos.X := 1;
  end
  else
  begin
    // calculate line start position
    FirstNonBlank := -1;
    if CaretY <= FTheLinesView.Count then
    begin
      s := FTheLinesView[CaretXY.Y - 1];

      // search first non blank char pos
      FirstNonBlank := 1;
      while (FirstNonBlank <= length(s)) and (s[FirstNonBlank] in [#32, #9]) do
        inc(FirstNonBlank);
      if FirstNonBlank > length(s) then
        FirstNonBlank := -1;
    end
    else
      s := '';

    if (FirstNonBlank >= 1) or (AMode in [synhmFirstWord]) then
    begin
      // this line is not blank
      if FirstNonBlank < 1 then
        FirstNonBlank := 1;
      LineStart := LogicalToPhysicalPos(Point(FirstNonBlank, CaretY)).X;
    end
    else
    begin
      // this line is blank
      // -> use automatic line indent
      LineStart := FBeautifier.GetDesiredIndentForLine(self, FTheLinesView, FCaret);
    end;

    NewPos.X := LineStart;
    if (eoEnhanceHomeKey in FOptions) and (AMode in [synhmDefault]) and (OldPos.X > 1) and (OldPos.X <= NewPos.X) then
    begin
      NewPos.X := 1;
    end;
  end;
  FCaret.LineCharPos := NewPos;
end;

procedure TCustomSynEdit.DoEndKey;
// jump to start of line (x=1),
// or if already there, jump to first non blank char
// or if blank line, jump to line indent position
// if eoEnhanceHomeKey and behind alternative point then jump first
var
  s: string;
  LastNonBlank: Integer;
  LineEnd: Longint;
  OldPos: TPoint;
  NewPos: TPoint;
begin
  OldPos := CaretXY;
  NewPos := OldPos;
  s := LineText;

  if not(eoEnhanceEndKey in FOptions2) and (FCaret.BytePos <> length(s) + 1) then
  begin
    // not at end of real line -> jump to end of line
    FCaret.BytePos := length(s) + 1;
  end
  else
  begin
    // calculate line end position
    LastNonBlank := -1;
    if s <> '' then
    begin
      // search first non blank char pos
      LastNonBlank := length(s);
      while (LastNonBlank > 0) and (s[LastNonBlank] in [#32, #9]) do
        dec(LastNonBlank);
    end;
    if LastNonBlank >= 1 then
    begin
      // this line is not blank
      LineEnd := LogicalToPhysicalPos(Point(LastNonBlank + 1, CaretY)).X;
    end
    else
    begin
      // this line is blank
      // -> use automatic line indent
      LineEnd := FBeautifier.GetDesiredIndentForLine(self, FTheLinesView, FCaret);
    end;

    NewPos.X := LineEnd;
    if (eoEnhanceEndKey in FOptions2) and (FCaret.BytePos <> length(s) + 1) and (OldPos.X >= NewPos.X) then
    begin
      FCaret.BytePos := length(s) + 1;
    end
    else
      FCaret.LineCharPos := NewPos;
  end;
end;

{$IFDEF SYN_COMPILER_4_UP}

function TCustomSynEdit.ExecuteAction(ExeAction: TBasicAction): boolean;
begin
  if ExeAction is TEditAction then
  begin
    Result := True;
    if ExeAction is TEditCut then
      CutToClipboard
    else if ExeAction is TEditCopy then
      CopyToClipboard
    else if ExeAction is TEditPaste then
      PasteFromClipboard
{$IFDEF SYN_COMPILER_5_UP}
    else if ExeAction is TEditDelete then
      ClearSelection
    else if ExeAction is TEditUndo then
      Undo
    else if ExeAction is TEditSelectAll then
      SelectAll;
{$ENDIF}
  end
  else
    Result := inherited ExecuteAction(ExeAction);
end;

function TCustomSynEdit.UpdateAction(TheAction: TBasicAction): boolean;
begin
  if TheAction is TEditAction then
  begin
    Result := Focused;
    if Result then
    begin
      if (TheAction is TEditCut) or (TheAction is TEditCopy) then
        TEditAction(TheAction).Enabled := SelAvail
      else if TheAction is TEditPaste then
        TEditAction(TheAction).Enabled := CanPaste
{$IFDEF SYN_COMPILER_5_UP}
      else if TheAction is TEditDelete then
        TEditAction(TheAction).Enabled := True
      else if TheAction is TEditUndo then
        TEditAction(TheAction).Enabled := CanUndo
      else if TheAction is TEditSelectAll then
        TEditAction(TheAction).Enabled := True;
{$ENDIF}
    end;
  end
  else
    Result := inherited UpdateAction(TheAction);
end;
{$ENDIF}

procedure TCustomSynEdit.SetModified(Value: boolean);
begin
  TSynEditStringList(FLines).Modified := Value;
end;

procedure TCustomSynEdit.InvalidateLine(Line: Integer);
begin
  InvalidateLines(Line, Line);
  InvalidateGutterLines(Line, Line);
end;

function TCustomSynEdit.GetReadOnly: boolean;
begin
  Result := fReadOnly;
end;

procedure TCustomSynEdit.SetReadOnly(Value: boolean);
begin
  if fReadOnly <> Value then
  begin
    fReadOnly := Value;
    StatusChanged([scReadOnly]);
  end;
end;

procedure TCustomSynEdit.FindMatchingBracket;
begin
  FindMatchingBracket(CaretXY, False, True, False, False);
end;

function TCustomSynEdit.FindMatchingBracket(PhysStartBracket: TPoint;
  StartIncludeNeighborChars, MoveCaret, SelectBrackets, OnlyVisible: boolean): TPoint;
// returns physical (screen) position of end bracket
const
  // keep the ' last
  Brackets: array [0 .. 7] of Char = ('(', ')', '[', ']', '{', '}', '''', '"');
type
  TokenPos = Record
    X: Integer;
    Attr: Integer;
  end;
var
  Line, s1: string;
  PosX, PosY: Integer;
  StartPt: TPoint;
  LogicalStart: TPoint;
  // for ContextMatch
  BracketKind, TmpStart: Integer;
  TmpAttr: TSynHighlighterAttributes;
  // for IsContextBracket
  MaxKnownTokenPos, TokenListCnt: Integer;
  TokenPosList: Array of TokenPos;

  // remove all text, that is not of desired attribute
  function IsContextBracket: boolean;
  var
    i, l: Integer;
  begin
    if not assigned(fHighlighter) then
      exit(True);
    if PosX > MaxKnownTokenPos then
    begin
      // Token is not yet known
      l := length(TokenPosList);
      if l < Max(CharsInWindow * 2, 32) then
      begin
        l := Max(CharsInWindow * 2, 32);
        SetLength(TokenPosList, l);
      end;
      // Init the Highlighter only once per line
      if MaxKnownTokenPos < 1 then
      begin
        fHighlighter.CurrentLines := FTheLinesView;
        fHighlighter.StartAtLineIndex(PosY - 1);
        TokenListCnt := 0;
      end
      else
        fHighlighter.Next;
      i := TokenListCnt;
      while not fHighlighter.GetEOL do
      begin
        TokenPosList[i].X := fHighlighter.GetTokenPos + 1;
        TokenPosList[i].Attr := fHighlighter.GetTokenKind;
        if TokenPosList[i].X > PosX then
        begin
          TokenListCnt := i + 1;
          MaxKnownTokenPos := TokenPosList[i].X;
          Result := TokenPosList[i - 1].Attr = BracketKind;
          exit;
        end;
        inc(i);
        if i >= l then
        begin
          l := l * 4;
          SetLength(TokenPosList, l);
        end;
        fHighlighter.Next;
      end;
      MaxKnownTokenPos := length(Line);
      TokenPosList[i].X := MaxKnownTokenPos;
      TokenListCnt := i + 1;
      Result := TokenPosList[i - 1].Attr = BracketKind;
      exit;
    end;
    // Token is in previously retrieved values
    i := 1;
    while (i < TokenListCnt) and (TokenPosList[i].X <= PosX) do
      inc(i);
    Result := TokenPosList[i - 1].Attr = BracketKind;
  end;

  procedure DoMatchingBracketFound;
  var
    EndPt, DummyPt: TPoint;
  begin
    // matching bracket found, set caret and bail out
    Result := Point(PosX, PosY); // start with logical (byte) position
    if SelectBrackets then
    begin
      EndPt := Result;
      if (EndPt.Y < StartPt.Y) or ((EndPt.Y = StartPt.Y) and (EndPt.X < StartPt.X)) then
      begin
        DummyPt := StartPt;
        StartPt := EndPt;
        EndPt := DummyPt;
      end;
      inc(EndPt.X);
      SetCaretAndSelection(CaretXY, StartPt, EndPt);
    end
    else if MoveCaret then
      LogicalCaretXY := Result;
  end;

  procedure DoFindMatchingQuote(q: Char);
  var
    Test: Char;
    Len, PrevPosX, PrevCnt: Integer;
  begin
    StartPt := Point(PosX, PosY);
    GetHighlighterAttriAtRowColEx(StartPt, s1, BracketKind, TmpStart, TmpAttr);
    // Checck if we have a complete token, e.g. Highlightec returned entire "string"
    if (TmpStart = PosX) and (length(s1) > 1) and (s1[length(s1)] = q) then
    begin
      PosX := PosX + length(s1) - 1;
      DoMatchingBracketFound;
      exit;
    end;
    if (TmpStart + length(s1) - 1 = PosX) and (length(s1) > 1) and (s1[1] = q) then
    begin
      PosX := PosX - length(s1) + 1;
      DoMatchingBracketFound;
      exit;
    end;

    MaxKnownTokenPos := 0;
    Len := PosX;
    PrevPosX := -1;
    PrevCnt := 0;
    // search until start of line
    while PosX > 1 do
    begin
      dec(PosX);
      Test := Line[PosX];
      if (Test = q) and IsContextBracket then
      begin
        inc(PrevCnt);
        if PrevPosX < 0 then
          PrevPosX := PosX;
      end;
    end;
    // 1st, 3rd, 5th, ... are opening
    if (PrevPosX > 0) and (PrevCnt mod 2 = 1) then
    begin
      PosX := PrevPosX;
      DoMatchingBracketFound;
      exit;
    end;

    PosX := Len;
    Len := length(Line);
    while PosX < Len do
    begin
      inc(PosX);
      Test := Line[PosX];
      if (Test = q) and IsContextBracket then
      begin
        DoMatchingBracketFound;
        exit;
      end;
    end;

    if (PrevPosX > 0) then
    begin
      PosX := PrevPosX;
      DoMatchingBracketFound;
      exit;
    end;
  end;

  procedure DoFindMatchingBracket(i: Integer);
  var
    Test, BracketInc, BracketDec: Char;
    NumBrackets, Len: Integer;
  begin
    StartPt := Point(PosX, PosY);
    GetHighlighterAttriAtRowColEx(StartPt, s1, BracketKind, TmpStart, TmpAttr);
    MaxKnownTokenPos := 0;
    BracketInc := Brackets[i];
    BracketDec := Brackets[i xor 1]; // 0 -> 1, 1 -> 0, ...
    // search for the matching bracket (that is until NumBrackets = 0)
    NumBrackets := 1;
    if Odd(i) then
    begin
      // closing bracket -> search opening bracket
      repeat
        // search until start of line
        while PosX > 1 do
        begin
          dec(PosX);
          Test := Line[PosX];
          if (Test = BracketInc) and IsContextBracket then
            inc(NumBrackets)
          else if (Test = BracketDec) and IsContextBracket then
          begin
            dec(NumBrackets);
            if NumBrackets = 0 then
            begin
              DoMatchingBracketFound;
              exit;
            end;
          end;
        end;
        // get previous line if possible
        if PosY = 1 then
          break;
        dec(PosY);
        if OnlyVisible and ((PosY < TopLine) or (PosY >= ScreenRowToRow(LinesInWindow))) then
          break;
        Line := FTheLinesView[PosY - 1];
        MaxKnownTokenPos := 0;
        PosX := length(Line) + 1;
      until False;
    end
    else
    begin
      // opening bracket -> search closing bracket
      repeat
        // search until end of line
        Len := length(Line);
        while PosX < Len do
        begin
          inc(PosX);
          Test := Line[PosX];
          if (Test = BracketInc) and IsContextBracket then
            inc(NumBrackets)
          else if (Test = BracketDec) and IsContextBracket then
          begin
            dec(NumBrackets);
            if NumBrackets = 0 then
            begin
              DoMatchingBracketFound;
              exit;
            end;
          end;
        end;
        // get next line if possible
        if PosY = FTheLinesView.Count then
          break;
        inc(PosY);
        if OnlyVisible and ((PosY < TopLine) or (PosY >= ScreenRowToRow(LinesInWindow))) then
          break;
        Line := FTheLinesView[PosY - 1];
        MaxKnownTokenPos := 0;
        PosX := 0;
      until False;
    end;
  end;

  procedure DoCheckBracket;
  var
    i: Integer;
    Test: Char;
  begin
    if length(Line) >= PosX then
    begin
      Test := Line[PosX];
      // is it one of the recognized brackets?
      for i := Low(Brackets) to High(Brackets) do
      begin
        if Test = Brackets[i] then
        begin
          // this is the bracket, get the matching one and the direction
          if Brackets[i] in ['''', '"'] then
            DoFindMatchingQuote(Brackets[i])
          else
            DoFindMatchingBracket(i);
          exit;
        end;
      end;
    end;
  end;

begin
  Result.X := -1;
  Result.Y := -1;

  // get char at caret
  LogicalStart := PhysicalToLogicalPos(PhysStartBracket);
  PosX := LogicalStart.X;
  PosY := LogicalStart.Y;
  if (PosY < 1) or (PosY > FTheLinesView.Count) then
    exit;
  if OnlyVisible and ((PosY < TopLine) or (PosY >= ScreenRowToRow(LinesInWindow))) then
    exit;

  Line := FTheLinesView[PosY - 1];
  try
    DoCheckBracket;
    if Result.Y > 0 then
      exit;
    if StartIncludeNeighborChars then
    begin
      if PosX > 1 then
      begin
        // search in front
        dec(PosX);
        DoCheckBracket;
        if Result.Y > 0 then
          exit;
        inc(PosX);
      end;
      if PosX < length(Line) then
      begin
        // search behind
        inc(PosX);
        DoCheckBracket;
        if Result.Y > 0 then
          exit;
      end;
    end;
  finally
    if Result.Y > 0 then
    begin
      Result := LogicalToPhysicalPos(Result);
    end;
  end;
end;

// L505 begin
function TCustomSynEdit.GetHighlighterAttriAtRowCol(XY: TPoint; out Token: string;
  out Attri: TSynHighlighterAttributes): boolean;
var
  TmpType, TmpStart: Integer;
begin
  Result := GetHighlighterAttriAtRowColEx(XY, Token, TmpType, TmpStart, Attri);
end;

function TCustomSynEdit.GetHighlighterAttriAtRowColEx(XY: TPoint; out Token: string; out TokenType, Start: Integer;
  out Attri: TSynHighlighterAttributes): boolean;
var
  PosX, PosY: Integer;
  Line: string;
begin
  PosY := XY.Y - 1;
  if assigned(Highlighter) and (PosY >= 0) and (PosY < FTheLinesView.Count) then
  begin
    Line := FTheLinesView[PosY];
    fHighlighter.CurrentLines := FTheLinesView;
    Highlighter.StartAtLineIndex(PosY);
    PosX := XY.X;
    if (PosX > 0) and (PosX <= length(Line)) then
    begin
      while not Highlighter.GetEOL do
      begin
        Start := Highlighter.GetTokenPos + 1;
        Token := Highlighter.GetToken;
        if (PosX >= Start) and (PosX < Start + length(Token)) then
        begin
          Attri := Highlighter.GetTokenAttribute;
          TokenType := Highlighter.GetTokenKind;
          Result := True;
          exit;
        end;
        Highlighter.Next;
      end;
    end;
  end;
  Token := '';
  Attri := nil;
  TokenType := -1;
  Result := False;
end;

function TCustomSynEdit.IdentChars: TSynIdentChars;
begin
  Result := FWordBreaker.IdentChars; // Maybe WordChars?
end;

function TCustomSynEdit.IsIdentChar(const c: Char): boolean;
begin
  Result := (length(c) = 1) and (c in IdentChars);
end;

procedure TCustomSynEdit.GetWordBoundsAtRowCol(const XY: TPoint; out StartX, EndX: Integer);
// all params are logical (byte) positions
var
  Line: string;
begin
  StartX := XY.X;
  EndX := XY.X;
  Line := FTheLinesView[XY.Y - 1];
  if WordBreaker.IsInWord(Line, XY.X) then
  begin
    StartX := WordBreaker.PrevWordStart(Line, XY.X, True);
    EndX := WordBreaker.NextWordEnd(Line, XY.X, True);
  end;
end;

function TCustomSynEdit.GetWordAtRowCol(XY: TPoint): string;
var
  StartX, EndX: Integer;
  Line: string;
begin
  GetWordBoundsAtRowCol(XY, StartX, EndX);
  Line := FTheLinesView[XY.Y - 1];
  Result := copy(Line, StartX, EndX - StartX);
end;

function TCustomSynEdit.NextTokenPos: TPoint;
var
  CX, CY, LineLen: Integer;
  Line: string;
  CurIdentChars, WhiteChars: TSynIdentChars;
  nTokenPos, nTokenLen: Integer;
  sToken: PChar;
  LogCaret: TPoint;

  procedure FindFirstNonWhiteSpaceCharInNextLine;
  begin
    if CY < FTheLinesView.Count then
    begin
      Line := FTheLinesView[CY];
      LineLen := length(Line);
      inc(CY);
      CX := 1;
      while (CX <= LineLen) and (Line[CX] in WhiteChars) do
        inc(CX);
      if CX > LineLen then
        CX := 1;
    end;
  end;

begin
  LogCaret := LogicalCaretXY;
  CX := LogCaret.X;
  CY := LogCaret.Y;
  // valid line?
  if (CY >= 1) and (CY <= FTheLinesView.Count) then
  begin
    Line := FTheLinesView[CY - 1];
    LineLen := length(Line);
    WhiteChars := FWordBreaker.WhiteChars;
    if CX > LineLen then
    begin
      FindFirstNonWhiteSpaceCharInNextLine;
    end
    else
    begin
      if fHighlighter <> nil then
      begin
        fHighlighter.CurrentLines := FTheLinesView;
        fHighlighter.StartAtLineIndex(CY - 1);
        while not fHighlighter.GetEOL do
        begin
          nTokenPos := fHighlighter.GetTokenPos; // zero-based
          fHighlighter.GetTokenEx(sToken, nTokenLen);
          if (CX > nTokenPos) and (CX <= nTokenPos + nTokenLen) then
          begin
            CX := nTokenPos + nTokenLen + 1;
            break;
          end;
          // Let the highlighter scan the next token.
          fHighlighter.Next;
        end;
        if fHighlighter.GetEOL then
          FindFirstNonWhiteSpaceCharInNextLine;
      end
      else
      begin
        // no highlighter
        CurIdentChars := IdentChars;
        // find first "whitespace" if next char is not a "whitespace"
        if (Line[CX] in CurIdentChars) then
        begin
          // in a word -> move to end of word
          while (CX <= LineLen) and (Line[CX] in CurIdentChars) do
            inc(CX);
        end;
        if (Line[CX] in WhiteChars) then
        begin
          // skip white space
          while (CX <= LineLen) and (Line[CX] in WhiteChars) do
            inc(CX);
        end;
        // delete at least one char
        if (CX = CaretX) then
          inc(CX);
      end;
    end;
  end;
  Result := LogicalToPhysicalPos(Point(CX, CY));
end;

function TCustomSynEdit.NextWordPos: TPoint;
begin
  Result := LogicalToPhysicalPos(NextWordLogicalPos);
end;

function TCustomSynEdit.PrevWordPos: TPoint;
begin
  Result := LogicalToPhysicalPos(PrevWordLogicalPos);
end;

function TCustomSynEdit.FindHookedCmdEvent(AHandlerProc: THookedCommandEvent): Integer;
var
  Entry: THookedCommandHandlerEntry;
begin
  Result := GetHookedCommandHandlersCount - 1;
  while Result >= 0 do
  begin
    Entry := THookedCommandHandlerEntry(fHookedCommandHandlers[Result]);
    if Entry.Equals(AHandlerProc) then
      break;
    dec(Result);
  end;
end;

function TCustomSynEdit.GetHookedCommandHandlersCount: Integer;
begin
  if assigned(fHookedCommandHandlers) then
    Result := fHookedCommandHandlers.Count
  else
    Result := 0;
end;

procedure TCustomSynEdit.RegisterCommandHandler(AHandlerProc: THookedCommandEvent; AHandlerData: pointer;
  aFlags: THookedCommandFlags);
begin
  if not assigned(AHandlerProc) then
  begin
{$IFDEF SYN_DEVELOPMENT_CHECKS}
    raise Exception.Create('Event handler is NIL in RegisterCommandHandler');
{$ENDIF}
    exit;
  end;
  if not assigned(fHookedCommandHandlers) then
    fHookedCommandHandlers := TList.Create;
  if FindHookedCmdEvent(AHandlerProc) = -1 then
    fHookedCommandHandlers.Add(THookedCommandHandlerEntry.Create(AHandlerProc, AHandlerData, aFlags))
  else
{$IFDEF SYN_DEVELOPMENT_CHECKS}
    raise Exception.CreateFmt('Event handler (%p, %p) already registered',
      [TMethod(AHandlerProc).Data, TMethod(AHandlerProc).Code]);
{$ENDIF}
end;

procedure TCustomSynEdit.UnregisterCommandHandler(AHandlerProc: THookedCommandEvent);
var
  i: Integer;
begin
  if not assigned(AHandlerProc) then
  begin
{$IFDEF SYN_DEVELOPMENT_CHECKS}
    raise Exception.Create('Event handler is NIL in UnregisterCommandHandler');
{$ENDIF}
    exit;
  end;
  i := FindHookedCmdEvent(AHandlerProc);
  if i > -1 then
  begin
    THookedCommandHandlerEntry(fHookedCommandHandlers[i]).Free;
    fHookedCommandHandlers.Delete(i);
  end
  else
{$IFDEF SYN_DEVELOPMENT_CHECKS}
    raise Exception.CreateFmt('Event handler (%p, %p) is not registered',
      [TMethod(AHandlerProc).Data, TMethod(AHandlerProc).Code]);
{$ENDIF}
end;

procedure TCustomSynEdit.RegisterMouseActionSearchHandler(AHandlerProc: TSynEditMouseActionSearchProc);
begin
  FMouseActionSearchHandlerList.Add(TMethod(AHandlerProc));
end;

procedure TCustomSynEdit.UnregisterMouseActionSearchHandler(AHandlerProc: TSynEditMouseActionSearchProc);
begin
  FMouseActionSearchHandlerList.Remove(TMethod(AHandlerProc));
end;

procedure TCustomSynEdit.RegisterMouseActionExecHandler(AHandlerProc: TSynEditMouseActionExecProc);
begin
  FMouseActionExecHandlerList.Add(TMethod(AHandlerProc));
end;

procedure TCustomSynEdit.UnregisterMouseActionExecHandler(AHandlerProc: TSynEditMouseActionExecProc);
begin
  FMouseActionExecHandlerList.Remove(TMethod(AHandlerProc));
end;

procedure TCustomSynEdit.RegisterKeyTranslationHandler(AHandlerProc: THookedKeyTranslationEvent);
begin
  FHookedKeyTranslationList.Add(TMethod(AHandlerProc));
end;

procedure TCustomSynEdit.UnRegisterKeyTranslationHandler(AHandlerProc: THookedKeyTranslationEvent);
begin
  FHookedKeyTranslationList.Remove(TMethod(AHandlerProc));
end;

procedure TCustomSynEdit.RegisterStatusChangedHandler(AStatusChangeProc: TStatusChangeEvent;
  AChanges: TSynStatusChanges);
begin
  TSynStatusChangedHandlerList(FStatusChangedList).Add(AStatusChangeProc, AChanges);
end;

procedure TCustomSynEdit.UnRegisterStatusChangedHandler(AStatusChangeProc: TStatusChangeEvent);
begin
  TSynStatusChangedHandlerList(FStatusChangedList).Remove(AStatusChangeProc);
end;

procedure TCustomSynEdit.NotifyHookedCommandHandlers(var Command: TSynEditorCommand; var AChar: Char;
  Data: pointer; ATime: THookedCommandFlag);
var
  Handled: boolean;
  i: Integer;
  Entry: THookedCommandHandlerEntry;
begin
  Handled := False;
  for i := 0 to GetHookedCommandHandlersCount - 1 do
  begin
    Entry := THookedCommandHandlerEntry(fHookedCommandHandlers[i]);
    if not(ATime in Entry.FFlags) then
      continue;
    // NOTE: Command should NOT be set to ecNone, because this might interfere
    // with other handlers.  Set Handled to False instead (and check its value
    // to not process the command twice).
    Entry.FEvent(self, ATime in [hcfPostExec, hcfFinish], Handled, Command, AChar, Data, Entry.FData);
  end;
  if Handled then
    Command := ecNone;
end;

procedure TCustomSynEdit.DoOnPaint;
begin
  if assigned(fOnPaint) then
  begin
//    Canvas.Font.Assign(Font);
//    Canvas.Brush.Color := Color;
    fOnPaint(self, Canvas);
  end;
end;

function TCustomSynEdit.DoOnReplaceText(const ASearch, AReplace: string; Line, Column: Integer): TSynReplaceAction;
begin
  Result := raCancel;
  if assigned(fOnReplaceText) then
    fOnReplaceText(self, ASearch, AReplace, Line, Column, Result);
end;

procedure TCustomSynEdit.DoOnStatusChange(Changes: TSynStatusChanges);
begin
  TSynStatusChangedHandlerList(FStatusChangedList).CallStatusChangedHandlers(self, Changes);
  if assigned(fOnStatusChange) then
  begin
    fOnStatusChange(self, fStatusChanges);
    fStatusChanges := [];
  end;
end;

procedure TCustomSynEdit.UndoRedoAdded(Sender: TObject);
begin
  // Todo: Check Paintlock, otherwise move to LinesChanged, LineCountChanged
  if assigned(fOnChange) then
    fOnChange(self);
end;

procedure TCustomSynEdit.ModifiedChanged(Sender: TObject);
begin
  StatusChanged([scModified]);
end;

function TCustomSynEdit.LogicalToPhysicalPos(const p: TPoint): TPoint;
begin
  Result := FTheLinesView.LogicalToPhysicalPos(p);
end;

function TCustomSynEdit.LogicalToPhysicalCol(const Line: String; Index, LogicalPos: Integer): Integer;
// LogicalPos is 1-based
// Index 0-based LineNumber
begin
  Result := FTheLinesView.LogicalToPhysicalCol(Line, Index, LogicalPos);
end;

function TCustomSynEdit.PhysicalLineLength(Line: String; Index: Integer): Integer;
begin
  Result := LogicalToPhysicalCol(Line, Index, length(Line) + 1) - 1
end;

function TCustomSynEdit.PhysicalToLogicalPos(const p: TPoint): TPoint;
begin
  Result := FTheLinesView.PhysicalToLogicalPos(p);
end;

function TCustomSynEdit.PhysicalToLogicalCol(const Line: string; Index, PhysicalPos: Integer): Integer;
begin
  Result := FTheLinesView.PhysicalToLogicalCol(Line, Index, PhysicalPos);
end;

function TCustomSynEdit.ScreenColumnToXValue(Col: Integer): Integer;
begin
  Result := FTextArea.ScreenColumnToXValue(Col);
end;

(*procedure TCustomSynEdit.PrimarySelectionRequest(const RequestedFormatID: TClipboardFormat; Data: TStream);
var
  s: string;
  ClipHelper: TSynClipboardStream;
begin
  if (not SelAvail) then
    exit;
  s := SelText;
  if s = '' then
    exit;
  if RequestedFormatID = CF_TEXT then
  begin
    Data.Write(s[1], length(s));
  end
  else if RequestedFormatID = TSynClipboardStream.ClipboardFormatId then
  begin
    ClipHelper := TSynClipboardStream.Create;
    try
      ClipHelper.SelectionMode := SelectionMode;
      // InternalText, so we don't need a 2nd call for CF_TEXT
      ClipHelper.InternalText := s;
      // Fold
      if eoFoldedCopyPaste in FOptions2 then
        s := FFoldedLinesView.GetFoldDescription(FBlockSelection.FirstLineBytePos.Y - 1,
          FBlockSelection.FirstLineBytePos.X, FBlockSelection.LastLineBytePos.Y - 1, FBlockSelection.LastLineBytePos.X);
      if length(s) > 0 then
        ClipHelper.AddTag(synClipTagFold, @s[1], length(s));
      Data.Write(ClipHelper.Memory^, ClipHelper.Size);
    finally
      ClipHelper.Free;
    end;
  end;
end;
   *)
{ TLazSynEditPlugin }

constructor TLazSynEditPlugin.Create(AOwner: TComponent);
begin
  if AOwner is TCustomSynEdit then
  begin
    inherited Create(nil);
    Editor := TCustomSynEdit(AOwner);
  end
  else
    inherited Create(AOwner);
end;

destructor TLazSynEditPlugin.Destroy;
begin
  Editor := nil;
  inherited Destroy;
end;

procedure TLazSynEditPlugin.RegisterToEditor(AValue: TCustomSynEdit);
begin
  if AValue.FPlugins <> nil then
    AValue.FPlugins.Add(self);
end;

procedure TLazSynEditPlugin.UnRegisterFromEditor(AValue: TCustomSynEdit);
begin
  if AValue.FPlugins <> nil then
    AValue.FPlugins.Remove(self);
end;

procedure TLazSynEditPlugin.SetEditor(const AValue: TCustomSynEdit);
begin
  if AValue = FriendEdit then
    exit;

  if (FriendEdit <> nil) then
  begin
    DoRemoveEditor(Editor);
    UnRegisterFromEditor(Editor);
  end;

  FriendEdit := AValue;

  if FriendEdit <> nil then
  begin
    RegisterToEditor(Editor);
    DoAddEditor(Editor);
  end;
end;

function TLazSynEditPlugin.GetEditor: TCustomSynEdit;
begin
  Result := FriendEdit as TCustomSynEdit;
end;

function TLazSynEditPlugin.OwnedByEditor: boolean;
begin
  Result := Owner = nil;
end;

procedure TLazSynEditPlugin.DoEditorDestroyed(const AValue: TCustomSynEdit);
begin
  if Editor <> AValue then
    exit;
  if OwnedByEditor then
    Free
  else
    Editor := nil;
end;

procedure TLazSynEditPlugin.DoAddEditor(AValue: TCustomSynEdit);
begin
  //
end;

procedure TLazSynEditPlugin.DoRemoveEditor(AValue: TCustomSynEdit);
begin
  //
end;

{ TSynHookedKeyTranslationList }

procedure TSynHookedKeyTranslationList.CallHookedKeyTranslationHandlers(Sender: TObject; Code: word;
  SState: TShiftState; var Data: pointer; var IsStartOfCombo: boolean; var Handled: boolean;
  var Command: TSynEditorCommand; var ComboKeyStrokes: TSynEditKeyStrokes);
var
  i: Integer;
begin
  if ComboKeyStrokes <> nil then
  begin
    // Finish Combo
    for i := 0 to Count - 1 do
      THookedKeyTranslationEvent(Items[i])(Sender, Code, SState, Data, IsStartOfCombo, Handled, Command, True,
        ComboKeyStrokes);
  end
  else
  begin
    // New Stroke
    for i := 0 to Count - 1 do
      THookedKeyTranslationEvent(Items[i])(Sender, Code, SState, Data, IsStartOfCombo, Handled, Command, False,
        ComboKeyStrokes);
  end;
end;

{ TSynStatusChangedHandlerList }
procedure TSynStatusChangedHandlerList.Add(AHandler: TStatusChangeEvent; Changes: TSynStatusChanges);
begin
  AddBitFilter(TMethod(AHandler), Longint(Pointer(@Changes)^));
end;

procedure TSynStatusChangedHandlerList.Remove(AHandler: TStatusChangeEvent);
begin
  inherited Remove(TMethod(AHandler));
end;

procedure TSynStatusChangedHandlerList.CallStatusChangedHandlers(Sender: TObject; Changes: TSynStatusChanges);
var
  i: Integer;
begin
  i := Count;
  while NextDownIndexBitFilter(i, Longint(Pointer(@Changes)^)) do
    TStatusChangeEvent(FItems[i].FHandler)(Sender, Changes);
end;

{ TSynEditMarkListInternal }

function TSynEditMarkListInternal.GetLinesView: TSynEditStrings;
begin
  Result := FLines;
end;

procedure TSynEditMarkListInternal.SetLinesView(const AValue: TSynEditStrings);
begin
  FLines := AValue;
end;

procedure TSynEditMarkListInternal.AddOwnerEdit(AEdit: TSynEditBase);
begin
  FOwnerList.Add(AEdit);
end;

procedure TSynEditMarkListInternal.RemoveOwnerEdit(AEdit: TSynEditBase);
begin
  FOwnerList.Remove(AEdit);
end;

procedure Register;
begin
  RegisterComponents('SynEdit', [TSynEdit]);
end;

initialization
  InitSynDefaultFont;

  RegisterFmxClasses([TSynEdit, TSynGutterPartList]);
  RegisterFmxClasses([TSynGutterPartList, TSynRightGutterPartList, TSynGutterSeparator, TSynGutterCodeFolding,
    TSynGutterLineNumber, TSynGutterChanges, TSynGutterMarks, TSynGutterLineOverview]);
end.
