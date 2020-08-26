/////////////////////////////////////////////////////////////////////////////
// Name:        wx/defs.h
// Purpose:     interface of global functions
// Author:      wxWidgets team
// Licence:     wxWindows licence
/////////////////////////////////////////////////////////////////////////////


// ----------------------------------------------------------------------------
// enumerations
// ----------------------------------------------------------------------------

/**
    Generic flags.
*/
enum wxGeometryCentre
{
    wxCENTRE                  = 0x0001,
    wxCENTER                  = wxCENTRE
};

/**
    A generic orientation value.
*/
enum wxOrientation
{
    wxHORIZONTAL              = 0x0004,
    wxVERTICAL                = 0x0008,

    /**
        A mask value to indicate both vertical and horizontal orientations.
    */
    wxBOTH                    = wxVERTICAL | wxHORIZONTAL,

    /// A synonym for @c wxBOTH.
    wxORIENTATION_MASK        = wxBOTH
};

/**
    A generic direction value.
*/
enum wxDirection
{
    wxLEFT                    = 0x0010,
    wxRIGHT                   = 0x0020,
    wxUP                      = 0x0040,
    wxDOWN                    = 0x0080,

    wxTOP                     = wxUP,
    wxBOTTOM                  = wxDOWN,

    wxNORTH                   = wxUP,
    wxSOUTH                   = wxDOWN,
    wxWEST                    = wxLEFT,
    wxEAST                    = wxRIGHT,

    wxALL                     = (wxUP | wxDOWN | wxRIGHT | wxLEFT),

    /** A mask to extract direction from the combination of flags. */
    wxDIRECTION_MASK           = wxALL
};

/**
    Generic alignment values. Can be combined together.
*/
enum wxAlignment
{
    /**
        A value different from any valid alignment value.

        Note that you shouldn't use 0 for this as it's the value of (valid)
        alignments wxALIGN_LEFT and wxALIGN_TOP.

        @since 2.9.1
     */
    wxALIGN_INVALID           = -1,

    wxALIGN_NOT               = 0x0000,
    wxALIGN_CENTER_HORIZONTAL = 0x0100,
    wxALIGN_CENTRE_HORIZONTAL = wxALIGN_CENTER_HORIZONTAL,
    wxALIGN_LEFT              = wxALIGN_NOT,
    wxALIGN_TOP               = wxALIGN_NOT,
    wxALIGN_RIGHT             = 0x0200,
    wxALIGN_BOTTOM            = 0x0400,
    wxALIGN_CENTER_VERTICAL   = 0x0800,
    wxALIGN_CENTRE_VERTICAL   = wxALIGN_CENTER_VERTICAL,

    wxALIGN_CENTER            = (wxALIGN_CENTER_HORIZONTAL | wxALIGN_CENTER_VERTICAL),
    wxALIGN_CENTRE            = wxALIGN_CENTER,

    /** A mask to extract alignment from the combination of flags. */
    wxALIGN_MASK              = 0x0f00
};

/**
    Miscellaneous flags for wxSizer items.
*/
enum wxSizerFlagBits
{
    wxFIXED_MINSIZE                = 0x8000,
    wxRESERVE_SPACE_EVEN_IF_HIDDEN = 0x0002,

    /*  a mask to extract wxSizerFlagBits from combination of flags */
    wxSIZER_FLAG_BITS_MASK         = 0x8002
};

/**
    Generic stretch values.
*/
enum wxStretch
{
    wxSTRETCH_NOT             = 0x0000,
    wxSHRINK                  = 0x1000,
    wxGROW                    = 0x2000,
    wxEXPAND                  = wxGROW,
    wxSHAPED                  = 0x4000,
    wxTILE                    = wxSHAPED | wxFIXED_MINSIZE,

    /*  a mask to extract stretch from the combination of flags */
    wxSTRETCH_MASK            = 0x7000 /* sans wxTILE */
};

/**
    Border flags for wxWindow.
*/
enum wxBorder
{
    /**
        This is different from wxBORDER_NONE as by default the controls do have
        a border.
    */
    wxBORDER_DEFAULT = 0,

    wxBORDER_NONE   = 0x00200000,
    wxBORDER_STATIC = 0x01000000,
    wxBORDER_SIMPLE = 0x02000000,
    wxBORDER_RAISED = 0x04000000,
    wxBORDER_SUNKEN = 0x08000000,
    wxBORDER_DOUBLE = 0x10000000, /* deprecated */
    wxBORDER_THEME  = wxBORDER_DOUBLE,

    /*  a mask to extract border style from the combination of flags */
    wxBORDER_MASK   = 0x1f200000
};

/*  ---------------------------------------------------------------------------- */
/*  Possible SetSize flags */
/*  ---------------------------------------------------------------------------- */

/*  Use internally-calculated width if -1 */
#define wxSIZE_AUTO_WIDTH       0x0001
/*  Use internally-calculated height if -1 */
#define wxSIZE_AUTO_HEIGHT      0x0002
/*  Use internally-calculated width and height if each is -1 */
#define wxSIZE_AUTO             (wxSIZE_AUTO_WIDTH|wxSIZE_AUTO_HEIGHT)
/*  Ignore missing (-1) dimensions (use existing). */
/*  For readability only: test for wxSIZE_AUTO_WIDTH/HEIGHT in code. */
#define wxSIZE_USE_EXISTING     0x0000
/*  Allow -1 as a valid position */
#define wxSIZE_ALLOW_MINUS_ONE  0x0004
/*  Don't do parent client adjustments (for implementation only) */
#define wxSIZE_NO_ADJUSTMENTS   0x0008
/*  Change the window position even if it seems to be already correct */
#define wxSIZE_FORCE            0x0010
/*  Emit size event even if size didn't change */
#define wxSIZE_FORCE_EVENT      0x0020

/*  ---------------------------------------------------------------------------- */
/*  Window style flags */
/*  ---------------------------------------------------------------------------- */

/*
 * Values are chosen so they can be |'ed in a bit list.
 * Some styles are used across more than one group,
 * so the values mustn't clash with others in the group.
 * Otherwise, numbers can be reused across groups.
 */

/*
    Summary of the bits used by various styles.

    High word, containing styles which can be used with many windows:

    +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
    |31|30|29|28|27|26|25|24|23|22|21|20|19|18|17|16|
    +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
      |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |
      |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  \_ wxFULL_REPAINT_ON_RESIZE
      |  |  |  |  |  |  |  |  |  |  |  |  |  |  \____ wxPOPUP_WINDOW
      |  |  |  |  |  |  |  |  |  |  |  |  |  \_______ wxWANTS_CHARS
      |  |  |  |  |  |  |  |  |  |  |  |  \__________ wxTAB_TRAVERSAL
      |  |  |  |  |  |  |  |  |  |  |  \_____________ wxTRANSPARENT_WINDOW
      |  |  |  |  |  |  |  |  |  |  \________________ wxBORDER_NONE
      |  |  |  |  |  |  |  |  |  \___________________ wxCLIP_CHILDREN
      |  |  |  |  |  |  |  |  \______________________ wxALWAYS_SHOW_SB
      |  |  |  |  |  |  |  \_________________________ wxBORDER_STATIC
      |  |  |  |  |  |  \____________________________ wxBORDER_SIMPLE
      |  |  |  |  |  \_______________________________ wxBORDER_RAISED
      |  |  |  |  \__________________________________ wxBORDER_SUNKEN
      |  |  |  \_____________________________________ wxBORDER_{DOUBLE,THEME}
      |  |  \________________________________________ wxCAPTION/wxCLIP_SIBLINGS
      |  \___________________________________________ wxHSCROLL
      \______________________________________________ wxVSCROLL


    Low word style bits is class-specific meaning that the same bit can have
    different meanings for different controls (e.g. 0x10 is wxCB_READONLY
    meaning that the control can't be modified for wxComboBox but wxLB_SORT
    meaning that the control should be kept sorted for wxListBox, while
    wxLB_SORT has a different value -- and this is just fine).
 */

/*
 * Window (Frame/dialog/subwindow/panel item) style flags
 */
#define wxVSCROLL               0x80000000
#define wxHSCROLL               0x40000000
#define wxCAPTION               0x20000000

/*  New styles (border styles are now in their own enum) */
#define wxDOUBLE_BORDER         wxBORDER_DOUBLE
#define wxSUNKEN_BORDER         wxBORDER_SUNKEN
#define wxRAISED_BORDER         wxBORDER_RAISED
#define wxBORDER                wxBORDER_SIMPLE
#define wxSIMPLE_BORDER         wxBORDER_SIMPLE
#define wxSTATIC_BORDER         wxBORDER_STATIC
#define wxNO_BORDER             wxBORDER_NONE

/*  wxALWAYS_SHOW_SB: instead of hiding the scrollbar when it is not needed, */
/*  disable it - but still show (see also wxLB_ALWAYS_SB style) */
/*  */
/*  NB: as this style is only supported by wxUniversal and wxMSW so far */
#define wxALWAYS_SHOW_SB        0x00800000

/*  Clip children when painting, which reduces flicker in e.g. frames and */
/*  splitter windows, but can't be used in a panel where a static box must be */
/*  'transparent' (panel paints the background for it) */
#define wxCLIP_CHILDREN         0x00400000

/*  Note we're reusing the wxCAPTION style because we won't need captions */
/*  for subwindows/controls */
#define wxCLIP_SIBLINGS         0x20000000

#define wxTRANSPARENT_WINDOW    0x00100000

/*  Add this style to a panel to get tab traversal working outside of dialogs */
/*  (on by default for wxPanel, wxDialog, wxScrolledWindow) */
#define wxTAB_TRAVERSAL         0x00080000

/*  Add this style if the control wants to get all keyboard messages (under */
/*  Windows, it won't normally get the dialog navigation key events) */
#define wxWANTS_CHARS           0x00040000

/*  Make window retained (Motif only, see src/generic/scrolwing.cpp)
 *  This is non-zero only under wxMotif, to avoid a clash with wxPOPUP_WINDOW
 *  on other platforms
 */

#ifdef __WXMOTIF__
#define wxRETAINED              0x00020000
#else
#define wxRETAINED              0x00000000
#endif
#define wxBACKINGSTORE          wxRETAINED

/*  set this flag to create a special popup window: it will be always shown on */
/*  top of other windows, will capture the mouse and will be dismissed when the */
/*  mouse is clicked outside of it or if it loses focus in any other way */
#define wxPOPUP_WINDOW          0x00020000

/*  force a full repaint when the window is resized (instead of repainting just */
/*  the invalidated area) */
#define wxFULL_REPAINT_ON_RESIZE 0x00010000

/*  obsolete: now this is the default behaviour */
/*  */
/*  don't invalidate the whole window (resulting in a PAINT event) when the */
/*  window is resized (currently, makes sense for wxMSW only) */
#define wxNO_FULL_REPAINT_ON_RESIZE 0

/* A mask which can be used to filter (out) all wxWindow-specific styles.
 */
#define wxWINDOW_STYLE_MASK     \
    (wxVSCROLL|wxHSCROLL|wxBORDER_MASK|wxALWAYS_SHOW_SB|wxCLIP_CHILDREN| \
     wxCLIP_SIBLINGS|wxTRANSPARENT_WINDOW|wxTAB_TRAVERSAL|wxWANTS_CHARS| \
     wxRETAINED|wxPOPUP_WINDOW|wxFULL_REPAINT_ON_RESIZE)

/*
 * Extra window style flags (use wxWS_EX prefix to make it clear that they
 * should be passed to wxWindow::SetExtraStyle(), not SetWindowStyle())
 */

/*  wxCommandEvents and the objects of the derived classes are forwarded to the */
/*  parent window and so on recursively by default. Using this flag for the */
/*  given window allows blocking this propagation at this window, i.e. preventing */
/*  the events from being propagated further upwards. The dialogs have this */
/*  flag on by default. */
#define wxWS_EX_BLOCK_EVENTS            0x00000002

/*  don't use this window as an implicit parent for the other windows: this must */
/*  be used with transient windows as otherwise there is the risk of creating a */
/*  dialog/frame with this window as a parent which would lead to a crash if the */
/*  parent is destroyed before the child */
#define wxWS_EX_TRANSIENT               0x00000004

/*  don't paint the window background, we'll assume it will */
/*  be done by a theming engine. This is not yet used but could */
/*  possibly be made to work in the future, at least on Windows */
#define wxWS_EX_THEMED_BACKGROUND       0x00000008

/*  this window should always process idle events */
#define wxWS_EX_PROCESS_IDLE            0x00000010

/*  this window should always process UI update events */
#define wxWS_EX_PROCESS_UI_UPDATES      0x00000020

/*  Draw the window in a metal theme on Mac */
#define wxFRAME_EX_METAL                0x00000040
#define wxDIALOG_EX_METAL               0x00000040

/*  Use this style to add a context-sensitive help to the window (currently for */
/*  Win32 only and it doesn't work if wxMINIMIZE_BOX or wxMAXIMIZE_BOX are used) */
#define wxWS_EX_CONTEXTHELP             0x00000080

/* synonyms for wxWS_EX_CONTEXTHELP for compatibility */
#define wxFRAME_EX_CONTEXTHELP          wxWS_EX_CONTEXTHELP
#define wxDIALOG_EX_CONTEXTHELP         wxWS_EX_CONTEXTHELP

/*  Create a window which is attachable to another top level window */
#define wxFRAME_DRAWER          0x0020

/*
 * MDI parent frame style flags
 * Can overlap with some of the above.
 */

#define wxFRAME_NO_WINDOW_MENU  0x0100

/*
 * wxMenuBar style flags
 */
/*  use native docking */
#define wxMB_DOCKABLE       0x0001

/*
 * wxMenu style flags
 */
#define wxMENU_TEAROFF      0x0001

/*
 * Apply to all panel items
 */
#define wxCOLOURED          0x0800
#define wxFIXED_LENGTH      0x0400

/*
 * Styles for wxListBox
 */
#define wxLB_SORT           0x0010
#define wxLB_SINGLE         0x0020
#define wxLB_MULTIPLE       0x0040
#define wxLB_EXTENDED       0x0080
/*  wxLB_OWNERDRAW is Windows-only */
#define wxLB_NEEDED_SB      0x0000
#define wxLB_OWNERDRAW      0x0100
#define wxLB_ALWAYS_SB      0x0200
#define wxLB_NO_SB          0x0400
#define wxLB_HSCROLL        wxHSCROLL
/*  always show an entire number of rows */
#define wxLB_INT_HEIGHT     0x0800

/*
 * wxComboBox style flags
 */
#define wxCB_SIMPLE         0x0004
#define wxCB_SORT           0x0008
#define wxCB_READONLY       0x0010
#define wxCB_DROPDOWN       0x0020

/*
 * wxRadioBox style flags
 * These styles are not used in any port.
 */
#define wxRA_LEFTTORIGHT    0x0001
#define wxRA_TOPTOBOTTOM    0x0002

/*  New, more intuitive names to specify majorDim argument */
#define wxRA_SPECIFY_COLS   wxHORIZONTAL
#define wxRA_SPECIFY_ROWS   wxVERTICAL

/*  Old names for compatibility */
#define wxRA_HORIZONTAL     wxHORIZONTAL
#define wxRA_VERTICAL       wxVERTICAL

/*
 * wxRadioButton style flag
 */
#define wxRB_GROUP          0x0004
#define wxRB_SINGLE         0x0008

/*
 * wxScrollBar flags
 */
#define wxSB_HORIZONTAL      wxHORIZONTAL
#define wxSB_VERTICAL        wxVERTICAL

/*
 * wxSpinButton flags.
 * Note that a wxSpinCtrl is sometimes defined as a wxTextCtrl, and so the
 * flags shouldn't overlap with wxTextCtrl flags that can be used for a single
 * line controls (currently we reuse wxTE_CHARWRAP and wxTE_RICH2 neither of
 * which makes sense for them).
 */
#define wxSP_HORIZONTAL       wxHORIZONTAL /*  4 */
#define wxSP_VERTICAL         wxVERTICAL   /*  8 */
#define wxSP_ARROW_KEYS       0x4000
#define wxSP_WRAP             0x8000

/*
 * wxTabCtrl flags
 */
#define wxTC_RIGHTJUSTIFY     0x0010
#define wxTC_FIXEDWIDTH       0x0020
#define wxTC_TOP              0x0000    /*  default */
#define wxTC_LEFT             0x0020
#define wxTC_RIGHT            0x0040
#define wxTC_BOTTOM           0x0080
#define wxTC_MULTILINE        0x0200    /* == wxNB_MULTILINE */
#define wxTC_OWNERDRAW        0x0400

/*
 * wxStaticBitmap flags
 */
#define wxBI_EXPAND           wxEXPAND

/*
 * wxStaticLine flags
 */
#define wxLI_HORIZONTAL         wxHORIZONTAL
#define wxLI_VERTICAL           wxVERTICAL


/*
 * extended dialog specifiers. these values are stored in a different
 * flag and thus do not overlap with other style flags. note that these
 * values do not correspond to the return values of the dialogs (for
 * those values, look at the wxID_XXX defines).
 */

/*  wxCENTRE already defined as  0x00000001 */
#define wxYES                   0x00000002
#define wxOK                    0x00000004
#define wxNO                    0x00000008
#define wxYES_NO                (wxYES | wxNO)
#define wxCANCEL                0x00000010
#define wxAPPLY                 0x00000020
#define wxCLOSE                 0x00000040

#define wxOK_DEFAULT            0x00000000  /* has no effect (default) */
#define wxYES_DEFAULT           0x00000000  /* has no effect (default) */
#define wxNO_DEFAULT            0x00000080  /* only valid with wxYES_NO */
#define wxCANCEL_DEFAULT        0x80000000  /* only valid with wxCANCEL */

#define wxICON_EXCLAMATION      0x00000100
#define wxICON_HAND             0x00000200
#define wxICON_WARNING          wxICON_EXCLAMATION
#define wxICON_ERROR            wxICON_HAND
#define wxICON_QUESTION         0x00000400
#define wxICON_INFORMATION      0x00000800
#define wxICON_STOP             wxICON_HAND
#define wxICON_ASTERISK         wxICON_INFORMATION

#define wxHELP                  0x00001000
#define wxFORWARD               0x00002000
#define wxBACKWARD              0x00004000
#define wxRESET                 0x00008000
#define wxMORE                  0x00010000
#define wxSETUP                 0x00020000
#define wxICON_NONE             0x00040000
#define wxICON_AUTH_NEEDED      0x00080000

#define wxICON_MASK \
    (wxICON_EXCLAMATION|wxICON_HAND|wxICON_QUESTION|wxICON_INFORMATION|wxICON_NONE)


/*  symbolic constant used by all Find()-like functions returning positive */
/*  integer on success as failure indicator */
#define wxNOT_FOUND       (-1)

/**
    Background styles.

    @see wxWindow::SetBackgroundStyle()
*/
enum wxBackgroundStyle
{
    /**
        Default background style value indicating that the background may be
        erased in the user-defined EVT_ERASE_BACKGROUND handler.

        If no such handler is defined (or if it skips the event), the effect of
        this style is the same as wxBG_STYLE_SYSTEM. If an empty handler (@em
        not skipping the event) is defined, the effect is the same as
        wxBG_STYLE_PAINT, i.e. the background is not erased at all until
        EVT_PAINT handler is executed.

        This is the only background style value for which erase background
        events are generated at all.
     */
    wxBG_STYLE_ERASE,

    /**
        Use the default background, as determined by the system or the current
        theme.

        If the window has been assigned a non-default background colour, it
        will be used for erasing its background. Otherwise the default
        background (which might be a gradient or a pattern) will be used.

        EVT_ERASE_BACKGROUND event will not be generated at all for windows
        with this style.
     */
    wxBG_STYLE_SYSTEM,

    /**
        Indicates that the background is only erased in the user-defined
        EVT_PAINT handler.

        Using this style avoids flicker which would result from redrawing the
        background twice if the EVT_PAINT handler entirely overwrites it. It
        must not be used however if the paint handler leaves any parts of the
        window unpainted as their contents is then undetermined. Only use it if
        you repaint the whole window in your handler.

        EVT_ERASE_BACKGROUND event will not be generated at all for windows
        with this style.
     */
    wxBG_STYLE_PAINT,

    /* this style is deprecated and doesn't do anything, don't use */
    wxBG_STYLE_COLOUR,

    /**
        Indicates that the window background is not erased, letting the parent
        window show through.

        Currently this style is only supported in wxOSX and wxGTK with
        compositing available, see wxWindow::IsTransparentBackgroundSupported().
     */
    wxBG_STYLE_TRANSPARENT,
};


/**
    Standard IDs.

    Notice that some, but @em not all, of these IDs are also stock IDs, i.e.
    you can use them for the button or menu items without specifying the label
    which will be provided by the underlying platform itself. See
    @ref page_stockitems "the list of stock items" for the subset of standard
    IDs which are stock IDs as well.
*/
enum wxStandardID
{
    /**
       This id delimits the lower bound of the range used by automatically-generated ids
       (i.e.\ those used when wxID_ANY is specified during construction).

       It is defined as a relatively large negative number and its exact value
       is platform-dependent.
     */
    wxID_AUTO_LOWEST,

    /**
       This id delimits the upper bound of the range used by automatically-generated ids
       (i.e.\ those used when wxID_ANY is specified during construction).

       It is defined as a relatively small negative number and its exact value
       is platform-dependent.
     */
    wxID_AUTO_HIGHEST,

    /**
        No id matches this one when compared to it.
    */
    wxID_NONE = -3,

    /**
        Id for a separator line in the menu (invalid for normal item).
    */
    wxID_SEPARATOR = -2,

    /**
        Any id: means that we don't care about the id, whether when installing
        an event handler or when creating a new window.
    */
    wxID_ANY = -1,

    /**
        Start of the range reserved for wxWidgets-defined IDs.

        Don't define custom IDs in the range from wxID_LOWEST to wxID_HIGHEST.
     */
    wxID_LOWEST = 4999,

    wxID_OPEN,
    wxID_CLOSE,
    wxID_NEW,
    wxID_SAVE,
    wxID_SAVEAS,
    wxID_REVERT,
    wxID_EXIT,
    wxID_UNDO,
    wxID_REDO,
    wxID_HELP,
    wxID_PRINT,
    wxID_PRINT_SETUP,
    wxID_PAGE_SETUP,
    wxID_PREVIEW,
    wxID_ABOUT,
    wxID_HELP_CONTENTS,
    wxID_HELP_INDEX,
    wxID_HELP_SEARCH,
    wxID_HELP_COMMANDS,
    wxID_HELP_PROCEDURES,
    wxID_HELP_CONTEXT,
    wxID_CLOSE_ALL,
    wxID_PREFERENCES,

    wxID_EDIT = 5030,
    wxID_CUT,
    wxID_COPY,
    wxID_PASTE,
    wxID_CLEAR,
    wxID_FIND,
    wxID_DUPLICATE,
    wxID_SELECTALL,
    wxID_DELETE,
    wxID_REPLACE,
    wxID_REPLACE_ALL,
    wxID_PROPERTIES,

    wxID_VIEW_DETAILS,
    wxID_VIEW_LARGEICONS,
    wxID_VIEW_SMALLICONS,
    wxID_VIEW_LIST,
    wxID_VIEW_SORTDATE,
    wxID_VIEW_SORTNAME,
    wxID_VIEW_SORTSIZE,
    wxID_VIEW_SORTTYPE,

    wxID_FILE = 5050,
    wxID_FILE1,
    wxID_FILE2,
    wxID_FILE3,
    wxID_FILE4,
    wxID_FILE5,
    wxID_FILE6,
    wxID_FILE7,
    wxID_FILE8,
    wxID_FILE9,

    /** Standard button and menu IDs */
    wxID_OK = 5100,
    wxID_CANCEL,
    wxID_APPLY,
    wxID_YES,
    wxID_NO,
    wxID_STATIC,
    wxID_FORWARD,
    wxID_BACKWARD,
    wxID_DEFAULT,
    wxID_MORE,
    wxID_SETUP,
    wxID_RESET,
    wxID_CONTEXT_HELP,
    wxID_YESTOALL,
    wxID_NOTOALL,
    wxID_ABORT,
    wxID_RETRY,
    wxID_IGNORE,
    wxID_ADD,
    wxID_REMOVE,

    wxID_UP,
    wxID_DOWN,
    wxID_HOME,
    wxID_REFRESH,
    wxID_STOP,
    wxID_INDEX,

    wxID_BOLD,
    wxID_ITALIC,
    wxID_JUSTIFY_CENTER,
    wxID_JUSTIFY_FILL,
    wxID_JUSTIFY_RIGHT,
    wxID_JUSTIFY_LEFT,
    wxID_UNDERLINE,
    wxID_INDENT,
    wxID_UNINDENT,
    wxID_ZOOM_100,
    wxID_ZOOM_FIT,
    wxID_ZOOM_IN,
    wxID_ZOOM_OUT,
    wxID_UNDELETE,
    wxID_REVERT_TO_SAVED,
    wxID_CDROM,
    wxID_CONVERT,
    wxID_EXECUTE,
    wxID_FLOPPY,
    wxID_HARDDISK,
    wxID_BOTTOM,
    wxID_FIRST,
    wxID_LAST,
    wxID_TOP,
    wxID_INFO,
    wxID_JUMP_TO,
    wxID_NETWORK,
    wxID_SELECT_COLOR,
    wxID_SELECT_FONT,
    wxID_SORT_ASCENDING,
    wxID_SORT_DESCENDING,
    wxID_SPELL_CHECK,
    wxID_STRIKETHROUGH,

    /** System menu IDs (used by wxUniv): */
    wxID_SYSTEM_MENU = 5200,
    wxID_CLOSE_FRAME,
    wxID_MOVE_FRAME,
    wxID_RESIZE_FRAME,
    wxID_MAXIMIZE_FRAME,
    wxID_ICONIZE_FRAME,
    wxID_RESTORE_FRAME,

    /** MDI window menu ids */
    wxID_MDI_WINDOW_FIRST = 5230,
    wxID_MDI_WINDOW_CASCADE = wxID_MDI_WINDOW_FIRST,
    wxID_MDI_WINDOW_TILE_HORZ,
    wxID_MDI_WINDOW_TILE_VERT,
    wxID_MDI_WINDOW_ARRANGE_ICONS,
    wxID_MDI_WINDOW_PREV,
    wxID_MDI_WINDOW_NEXT,
    wxID_MDI_WINDOW_LAST = wxID_MDI_WINDOW_NEXT,

    /** IDs used by generic file dialog (13 consecutive starting from this value) */
    wxID_FILEDLGG = 5900,

    /** IDs used by generic file ctrl (4 consecutive starting from this value) */
    wxID_FILECTRL = 5950,

    /**
        End of the range reserved for wxWidgets-defined IDs.

        Don't define custom IDs in the range from wxID_LOWEST to wxID_HIGHEST.

        When using an enum to define a number of custom IDs, assigning the
        value of @c wxID_HIGHEST+1 to the first element ensures that none of
        the enum elements will conflict with any standard IDs.
     */
    wxID_HIGHEST = 5999
};

/**
    Item kinds for use with wxMenu, wxMenuItem, and wxToolBar.

    @see wxMenu::Append(), wxMenuItem::wxMenuItem(), wxToolBar::AddTool()
*/
enum wxItemKind
{
    wxITEM_SEPARATOR = -1,

    /**
        Normal tool button / menu item.

        @see wxToolBar::AddTool(), wxMenu::AppendItem().
    */
    wxITEM_NORMAL,

    /**
        Check (or toggle) tool button / menu item.

        @see wxToolBar::AddCheckTool(), wxMenu::AppendCheckItem().
    */
    wxITEM_CHECK,

    /**
        Radio tool button / menu item.

        @see wxToolBar::AddRadioTool(), wxMenu::AppendRadioItem().
    */
    wxITEM_RADIO,

    /**
        Normal tool button with a dropdown arrow next to it. Clicking the
        dropdown arrow sends a @c wxEVT_TOOL_DROPDOWN event and may
        also display the menu previously associated with the item with
        wxToolBar::SetDropdownMenu(). Currently this type of tools is supported
        under MSW and GTK.
    */
    wxITEM_DROPDOWN,

    wxITEM_MAX
};

/**
    Generic hit test results.
*/
enum wxHitTest
{
    wxHT_NOWHERE,

    /*  scrollbar */
    wxHT_SCROLLBAR_FIRST = wxHT_NOWHERE,
    wxHT_SCROLLBAR_ARROW_LINE_1,    /**< left or upper arrow to scroll by line */
    wxHT_SCROLLBAR_ARROW_LINE_2,    /**< right or down */
    wxHT_SCROLLBAR_ARROW_PAGE_1,    /**< left or upper arrow to scroll by page */
    wxHT_SCROLLBAR_ARROW_PAGE_2,    /**< right or down */
    wxHT_SCROLLBAR_THUMB,           /**< on the thumb */
    wxHT_SCROLLBAR_BAR_1,           /**< bar to the left/above the thumb */
    wxHT_SCROLLBAR_BAR_2,           /**< bar to the right/below the thumb */
    wxHT_SCROLLBAR_LAST,

    /*  window */
    wxHT_WINDOW_OUTSIDE,            /**< not in this window at all */
    wxHT_WINDOW_INSIDE,             /**< in the client area */
    wxHT_WINDOW_VERT_SCROLLBAR,     /**< on the vertical scrollbar */
    wxHT_WINDOW_HORZ_SCROLLBAR,     /**< on the horizontal scrollbar */
    wxHT_WINDOW_CORNER,             /**< on the corner between 2 scrollbars */

    wxHT_MAX
};

/**
    Data format IDs used by wxDataFormat.
*/
enum wxDataFormatId
{
    wxDF_INVALID =          0,
    wxDF_TEXT =             1,  /* CF_TEXT */
    wxDF_BITMAP =           2,  /* CF_BITMAP */
    wxDF_METAFILE =         3,  /* CF_METAFILEPICT */
    wxDF_SYLK =             4,
    wxDF_DIF =              5,
    wxDF_TIFF =             6,
    wxDF_OEMTEXT =          7,  /* CF_OEMTEXT */
    wxDF_DIB =              8,  /* CF_DIB */
    wxDF_PALETTE =          9,
    wxDF_PENDATA =          10,
    wxDF_RIFF =             11,
    wxDF_WAVE =             12,
    wxDF_UNICODETEXT =      13,
    wxDF_ENHMETAFILE =      14,
    wxDF_FILENAME =         15, /* CF_HDROP */
    wxDF_LOCALE =           16,
    wxDF_PRIVATE =          20,
    wxDF_HTML =             30, /* Note: does not correspond to CF_ constant */
    wxDF_MAX
};

/**
    Virtual keycodes used by wxKeyEvent and some other wxWidgets functions.

    Note that the range <code>0..255</code> corresponds to the characters of
    the current locale, in particular the <code>32..127</code> subrange is for
    the ASCII symbols, and all the special key values such as @c WXK_END lie
    above this range.
*/
enum wxKeyCode
{
    /**
        No key.

        This value is returned by wxKeyEvent::GetKeyCode() if there is no
        non-Unicode representation for the pressed key (e.g. a Cyrillic letter
        was entered when not using a Cyrillic locale) and by
        wxKeyEvent::GetUnicodeKey() if there is no Unicode representation for
        the key (this happens for the special, non printable, keys only, e.g.
        WXK_HOME).

        @since 2.9.2 (you can simply use 0 with previous versions).
     */
    WXK_NONE    =    0,

    WXK_CONTROL_A = 1,
    WXK_CONTROL_B,
    WXK_CONTROL_C,
    WXK_CONTROL_D,
    WXK_CONTROL_E,
    WXK_CONTROL_F,
    WXK_CONTROL_G,
    WXK_CONTROL_H,
    WXK_CONTROL_I,
    WXK_CONTROL_J,
    WXK_CONTROL_K,
    WXK_CONTROL_L,
    WXK_CONTROL_M,
    WXK_CONTROL_N,
    WXK_CONTROL_O,
    WXK_CONTROL_P,
    WXK_CONTROL_Q,
    WXK_CONTROL_R,
    WXK_CONTROL_S,
    WXK_CONTROL_T,
    WXK_CONTROL_U,
    WXK_CONTROL_V,
    WXK_CONTROL_W,
    WXK_CONTROL_X,
    WXK_CONTROL_Y,
    WXK_CONTROL_Z,

    WXK_BACK    =    8,     //!< Backspace.
    WXK_TAB     =    9,
    WXK_RETURN  =    13,
    WXK_ESCAPE  =    27,
    WXK_SPACE   =    32,

    WXK_DELETE  =    127,

    /**
        Special key values.

        These are, by design, not compatible with Unicode characters.
        If you want to get a Unicode character from a key event, use
        wxKeyEvent::GetUnicodeKey() instead.
    */
    WXK_START   = 300,
    WXK_LBUTTON,
    WXK_RBUTTON,
    WXK_CANCEL,
    WXK_MBUTTON,
    WXK_CLEAR,
    WXK_SHIFT,
    WXK_ALT,
    /** Note that under macOS, to improve compatibility with other
      * systems, 'WXK_CONTROL' represents the 'Command' key. Use this
      * constant to work with keyboard shortcuts. See 'WXK_RAW_CONTROL'
      * to get the state of the actual 'Control' key.
      */
    WXK_CONTROL,
    /** Under macOS, where the 'Command' key is mapped to 'Control'
      * to improve compatibility with other systems, WXK_RAW_CONTROL may
      * be used to obtain the state of the actual 'Control' key
      * ('WXK_CONTROL' would obtain the status of the 'Command' key).
      * Under Windows/Linux/Others, this is equivalent to WXK_CONTROL
      */
    WXK_RAW_CONTROL,
    WXK_MENU,
    WXK_PAUSE,
    WXK_CAPITAL,
    WXK_END,
    WXK_HOME,
    WXK_LEFT,
    WXK_UP,
    WXK_RIGHT,
    WXK_DOWN,
    WXK_SELECT,
    WXK_PRINT,
    WXK_EXECUTE,
    WXK_SNAPSHOT,
    WXK_INSERT,
    WXK_HELP,
    WXK_NUMPAD0,
    WXK_NUMPAD1,
    WXK_NUMPAD2,
    WXK_NUMPAD3,
    WXK_NUMPAD4,
    WXK_NUMPAD5,
    WXK_NUMPAD6,
    WXK_NUMPAD7,
    WXK_NUMPAD8,
    WXK_NUMPAD9,
    WXK_MULTIPLY,
    WXK_ADD,
    WXK_SEPARATOR,
    WXK_SUBTRACT,
    WXK_DECIMAL,
    WXK_DIVIDE,
    WXK_F1,
    WXK_F2,
    WXK_F3,
    WXK_F4,
    WXK_F5,
    WXK_F6,
    WXK_F7,
    WXK_F8,
    WXK_F9,
    WXK_F10,
    WXK_F11,
    WXK_F12,
    WXK_F13,
    WXK_F14,
    WXK_F15,
    WXK_F16,
    WXK_F17,
    WXK_F18,
    WXK_F19,
    WXK_F20,
    WXK_F21,
    WXK_F22,
    WXK_F23,
    WXK_F24,
    WXK_NUMLOCK,
    WXK_SCROLL,
    WXK_PAGEUP,
    WXK_PAGEDOWN,

    WXK_NUMPAD_SPACE,
    WXK_NUMPAD_TAB,
    WXK_NUMPAD_ENTER,
    WXK_NUMPAD_F1,
    WXK_NUMPAD_F2,
    WXK_NUMPAD_F3,
    WXK_NUMPAD_F4,
    WXK_NUMPAD_HOME,
    WXK_NUMPAD_LEFT,
    WXK_NUMPAD_UP,
    WXK_NUMPAD_RIGHT,
    WXK_NUMPAD_DOWN,
    WXK_NUMPAD_PAGEUP,
    WXK_NUMPAD_PAGEDOWN,
    WXK_NUMPAD_END,
    WXK_NUMPAD_BEGIN,
    WXK_NUMPAD_INSERT,
    WXK_NUMPAD_DELETE,
    WXK_NUMPAD_EQUAL,
    WXK_NUMPAD_MULTIPLY,
    WXK_NUMPAD_ADD,
    WXK_NUMPAD_SEPARATOR,
    WXK_NUMPAD_SUBTRACT,
    WXK_NUMPAD_DECIMAL,
    WXK_NUMPAD_DIVIDE,

    /** The following key codes are only generated under Windows currently */
    WXK_WINDOWS_LEFT,
    WXK_WINDOWS_RIGHT,
    WXK_WINDOWS_MENU ,

    /** This special key code was used to represent the key used for keyboard shortcuts. Under macOS,
      * this key maps to the 'Command' (aka logo or 'Apple') key, whereas on Linux/Windows/others
      * this is the Control key, with the new semantic of WXK_CONTROL, WXK_COMMAND is not needed anymore
      */
    WXK_COMMAND,

    /** Hardware-specific buttons */
    WXK_SPECIAL1 = 193,
    WXK_SPECIAL2,
    WXK_SPECIAL3,
    WXK_SPECIAL4,
    WXK_SPECIAL5,
    WXK_SPECIAL6,
    WXK_SPECIAL7,
    WXK_SPECIAL8,
    WXK_SPECIAL9,
    WXK_SPECIAL10,
    WXK_SPECIAL11,
    WXK_SPECIAL12,
    WXK_SPECIAL13,
    WXK_SPECIAL14,
    WXK_SPECIAL15,
    WXK_SPECIAL16,
    WXK_SPECIAL17,
    WXK_SPECIAL18,
    WXK_SPECIAL19,
    WXK_SPECIAL20,

    WXK_BROWSER_BACK = 501,
    WXK_BROWSER_FORWARD,
    WXK_BROWSER_REFRESH,
    WXK_BROWSER_STOP,
    WXK_BROWSER_SEARCH,
    WXK_BROWSER_FAVORITES,
    WXK_BROWSER_HOME,
    WXK_VOLUME_MUTE,
    WXK_VOLUME_DOWN,
    WXK_VOLUME_UP,
    WXK_MEDIA_NEXT_TRACK,
    WXK_MEDIA_PREV_TRACK,
    WXK_MEDIA_STOP,
    WXK_MEDIA_PLAY_PAUSE,
    WXK_LAUNCH_MAIL,
    WXK_LAUNCH_APP1,
    WXK_LAUNCH_APP2
};

/**
    This enum contains bit mask constants used in wxKeyEvent.
*/
enum wxKeyModifier
{
    wxMOD_NONE      = 0x0000,
    wxMOD_ALT       = 0x0001,
    /** Ctlr Key, corresponds to Command key on macOS */
    wxMOD_CONTROL   = 0x0002,
    wxMOD_ALTGR     = wxMOD_ALT | wxMOD_CONTROL,
    wxMOD_SHIFT     = 0x0004,
    wxMOD_META      = 0x0008,
    wxMOD_WIN       = wxMOD_META,

    /** used to describe the true Ctrl Key under macOS,
    identic to @c wxMOD_CONTROL on other platforms */
    wxMOD_RAW_CONTROL,

    /** deprecated, identic to @c wxMOD_CONTROL on all platforms */
    wxMOD_CMD       = wxMOD_CONTROL,
    wxMOD_ALL       = 0xffff
};

/**
    Paper size types for use with the printing framework.

    @see overview_printing, wxPrintData::SetPaperId()
*/
enum wxPaperSize
{
    wxPAPER_10X11,              ///<  10 x 11 in
    wxPAPER_10X14,              ///<  10-by-14-inch sheet
    wxPAPER_11X17,              ///<  11-by-17-inch sheet
    wxPAPER_12X11,              ///< 12 x 11 in
    wxPAPER_15X11,              ///<  15 x 11 in
    wxPAPER_9X11,               ///<  9 x 11 in
    wxPAPER_A2,                 ///<  A2 420 x 594 mm
    wxPAPER_A3,                 ///<  A3 sheet, 297 by 420 millimeters
    wxPAPER_A3_EXTRA,           ///<  A3 Extra 322 x 445 mm
    wxPAPER_A3_EXTRA_TRANSVERSE, ///<  A3 Extra Transverse 322 x 445 mm
    wxPAPER_A3_ROTATED,         ///< A3 Rotated 420 x 297 mm
    wxPAPER_A3_TRANSVERSE,      ///<  A3 Transverse 297 x 420 mm
    wxPAPER_A4,                 ///<  A4 Sheet, 210 by 297 millimeters
    wxPAPER_A4SMALL,            ///<  A4 small sheet, 210 by 297 millimeters
    wxPAPER_A4_EXTRA,           ///<  A4 Extra 9.27 x 12.69 in
    wxPAPER_A4_PLUS,            ///<  A4 Plus 210 x 330 mm
    wxPAPER_A4_ROTATED,         ///< A4 Rotated 297 x 210 mm
    wxPAPER_A4_TRANSVERSE,      ///<  A4 Transverse 210 x 297 mm
    wxPAPER_A5,                 ///<  A5 sheet, 148 by 210 millimeters
    wxPAPER_A5_EXTRA,           ///<  A5 Extra 174 x 235 mm
    wxPAPER_A5_ROTATED,         ///< A5 Rotated 210 x 148 mm
    wxPAPER_A5_TRANSVERSE,      ///<  A5 Transverse 148 x 210 mm
    wxPAPER_A6,                 ///< A6 105 x 148 mm
    wxPAPER_A6_ROTATED,         ///< A6 Rotated 148 x 105 mm
    wxPAPER_A_PLUS,             ///<  SuperA/SuperA/A4 227 x 356 mm
    wxPAPER_B4,                 ///<  B4 sheet, 250 by 354 millimeters
    wxPAPER_B4_JIS_ROTATED,     ///< B4 (JIS) Rotated 364 x 257 mm
    wxPAPER_B5,                 ///<  B5 sheet, 182-by-257-millimeter paper
    wxPAPER_B5_EXTRA,           ///<  B5 (ISO) Extra 201 x 276 mm
    wxPAPER_B5_JIS_ROTATED,     ///< B5 (JIS) Rotated 257 x 182 mm
    wxPAPER_B5_TRANSVERSE,      ///<  B5 (JIS) Transverse 182 x 257 mm
    wxPAPER_B6_JIS,             ///< B6 (JIS) 128 x 182 mm
    wxPAPER_B6_JIS_ROTATED,     ///< B6 (JIS) Rotated 182 x 128 mm
    wxPAPER_B_PLUS,             ///<  SuperB/SuperB/A3 305 x 487 mm
    wxPAPER_CSHEET,             ///<  C Sheet, 17 by 22 inches
    wxPAPER_DBL_JAPANESE_POSTCARD, ///< Japanese Double Postcard 200 x 148 mm
    wxPAPER_DBL_JAPANESE_POSTCARD_ROTATED, ///< Double Japanese Postcard Rotated 148 x 200 mm
    wxPAPER_DSHEET,             ///<  D Sheet, 22 by 34 inches
    wxPAPER_ENV_10,             ///<  #10 Envelope, 4 1/8 by 9 1/2 inches
    wxPAPER_ENV_11,             ///<  #11 Envelope, 4 1/2 by 10 3/8 inches
    wxPAPER_ENV_12,             ///<  #12 Envelope, 4 3/4 by 11 inches
    wxPAPER_ENV_14,             ///<  #14 Envelope, 5 by 11 1/2 inches
    wxPAPER_ENV_9,              ///<  #9 Envelope, 3 7/8 by 8 7/8 inches
    wxPAPER_ENV_B4,             ///<  B4 Envelope, 250 by 353 millimeters
    wxPAPER_ENV_B5,             ///<  B5 Envelope, 176 by 250 millimeters
    wxPAPER_ENV_B6,             ///<  B6 Envelope, 176 by 125 millimeters
    wxPAPER_ENV_C3,             ///<  C3 Envelope, 324 by 458 millimeters
    wxPAPER_ENV_C4,             ///<  C4 Envelope, 229 by 324 millimeters
    wxPAPER_ENV_C5,             ///<  C5 Envelope, 162 by 229 millimeters
    wxPAPER_ENV_C6,             ///<  C6 Envelope, 114 by 162 millimeters
    wxPAPER_ENV_C65,            ///<  C65 Envelope, 114 by 229 millimeters
    wxPAPER_ENV_DL,             ///<  DL Envelope, 110 by 220 millimeters
    wxPAPER_ENV_INVITE,         ///<  Envelope Invite 220 x 220 mm
    wxPAPER_ENV_ITALY,          ///<  Italy Envelope, 110 by 230 millimeters
    wxPAPER_ENV_MONARCH,        ///<  Monarch Envelope, 3 7/8 by 7 1/2 inches
    wxPAPER_ENV_PERSONAL,       ///<  6 3/4 Envelope, 3 5/8 by 6 1/2 inches
    wxPAPER_ESHEET,             ///<  E Sheet, 34 by 44 inches
    wxPAPER_EXECUTIVE,          ///<  Executive, 7 1/4 by 10 1/2 inches
    wxPAPER_FANFOLD_LGL_GERMAN, ///<  German Legal Fanfold, 8 1/2 by 13 inches
    wxPAPER_FANFOLD_STD_GERMAN, ///<  German Std Fanfold, 8 1/2 by 12 inches
    wxPAPER_FANFOLD_US,         ///<  US Std Fanfold, 14 7/8 by 11 inches
    wxPAPER_FOLIO,              ///<  Folio, 8-1/2-by-13-inch paper
    wxPAPER_ISO_B4,             ///<  B4 (ISO) 250 x 353 mm
    wxPAPER_JAPANESE_POSTCARD,  ///<  Japanese Postcard 100 x 148 mm
    wxPAPER_JAPANESE_POSTCARD_ROTATED, ///< Japanese Postcard Rotated 148 x 100 mm
    wxPAPER_JENV_CHOU3,         ///< Japanese Envelope Chou #3
    wxPAPER_JENV_CHOU3_ROTATED, ///< Japanese Envelope Chou #3 Rotated
    wxPAPER_JENV_CHOU4,         ///< Japanese Envelope Chou #4
    wxPAPER_JENV_CHOU4_ROTATED, ///< Japanese Envelope Chou #4 Rotated
    wxPAPER_JENV_KAKU2,         ///< Japanese Envelope Kaku #2
    wxPAPER_JENV_KAKU2_ROTATED, ///< Japanese Envelope Kaku #2 Rotated
    wxPAPER_JENV_KAKU3,         ///< Japanese Envelope Kaku #3
    wxPAPER_JENV_KAKU3_ROTATED, ///< Japanese Envelope Kaku #3 Rotated
    wxPAPER_JENV_YOU4,          ///< Japanese Envelope You #4
    wxPAPER_JENV_YOU4_ROTATED,  ///< Japanese Envelope You #4 Rotated
    wxPAPER_LEDGER,             ///<  Ledger, 17 by 11 inches
    wxPAPER_LEGAL,              ///<  Legal, 8 1/2 by 14 inches
    wxPAPER_LEGAL_EXTRA,        ///<  Legal Extra 9.5 x 15 in
    wxPAPER_LETTER,             ///<  Letter, 8 1/2 by 11 inches
    wxPAPER_LETTERSMALL,        ///<  Letter Small, 8 1/2 by 11 inches
    wxPAPER_LETTER_EXTRA,       ///<  Letter Extra 9.5 x 12 in
    wxPAPER_LETTER_EXTRA_TRANSVERSE, ///<  Letter Extra Transverse 9.5 x 12 in
    wxPAPER_LETTER_PLUS,        ///<  Letter Plus 8.5 x 12.69 in
    wxPAPER_LETTER_ROTATED,     ///< Letter Rotated 11 x 8 1/2 in
    wxPAPER_LETTER_TRANSVERSE,  ///<  Letter Transverse 8.5 x 11 in
    wxPAPER_NONE,               ///<  Use specific dimensions
    wxPAPER_NOTE,               ///<  Note, 8 1/2 by 11 inches
    wxPAPER_P16K,               ///< PRC 16K 146 x 215 mm
    wxPAPER_P16K_ROTATED,       ///< PRC 16K Rotated
    wxPAPER_P32K,               ///< PRC 32K 97 x 151 mm
    wxPAPER_P32KBIG,            ///< PRC 32K(Big) 97 x 151 mm
    wxPAPER_P32KBIG_ROTATED,    ///< PRC 32K(Big) Rotated
    wxPAPER_P32K_ROTATED,       ///< PRC 32K Rotated
    wxPAPER_PENV_1,             ///< PRC Envelope #1 102 x 165 mm
    wxPAPER_PENV_10,            ///< PRC Envelope #10 324 x 458 mm
    wxPAPER_PENV_10_ROTATED,    ///< PRC Envelope #10 Rotated 458 x 324 m
    wxPAPER_PENV_1_ROTATED,     ///< PRC Envelope #1 Rotated 165 x 102 mm
    wxPAPER_PENV_2,             ///< PRC Envelope #2 102 x 176 mm
    wxPAPER_PENV_2_ROTATED,     ///< PRC Envelope #2 Rotated 176 x 102 mm
    wxPAPER_PENV_3,             ///< PRC Envelope #3 125 x 176 mm
    wxPAPER_PENV_3_ROTATED,     ///< PRC Envelope #3 Rotated 176 x 125 mm
    wxPAPER_PENV_4,             ///< PRC Envelope #4 110 x 208 mm
    wxPAPER_PENV_4_ROTATED,     ///< PRC Envelope #4 Rotated 208 x 110 mm
    wxPAPER_PENV_5,             ///< PRC Envelope #5 110 x 220 mm
    wxPAPER_PENV_5_ROTATED,     ///< PRC Envelope #5 Rotated 220 x 110 mm
    wxPAPER_PENV_6,             ///< PRC Envelope #6 120 x 230 mm
    wxPAPER_PENV_6_ROTATED,     ///< PRC Envelope #6 Rotated 230 x 120 mm
    wxPAPER_PENV_7,             ///< PRC Envelope #7 160 x 230 mm
    wxPAPER_PENV_7_ROTATED,     ///< PRC Envelope #7 Rotated 230 x 160 mm
    wxPAPER_PENV_8,             ///< PRC Envelope #8 120 x 309 mm
    wxPAPER_PENV_8_ROTATED,     ///< PRC Envelope #8 Rotated 309 x 120 mm
    wxPAPER_PENV_9,             ///< PRC Envelope #9 229 x 324 mm
    wxPAPER_PENV_9_ROTATED,     ///< PRC Envelope #9 Rotated 324 x 229 mm
    wxPAPER_QUARTO,             ///<  Quarto, 215-by-275-millimeter paper
    wxPAPER_STATEMENT,          ///<  Statement, 5 1/2 by 8 1/2 inches
    wxPAPER_TABLOID,            ///<  Tabloid, 11 by 17 inches
    wxPAPER_TABLOID_EXTRA       ///<  Tabloid Extra 11.69 x 18 in
};

/**
    Printing orientation
*/

enum wxPrintOrientation
{
   wxPORTRAIT,
   wxLANDSCAPE
};

/**
    Duplex printing modes.
*/
enum wxDuplexMode
{
    wxDUPLEX_SIMPLEX, /**< Non-duplex */
    wxDUPLEX_HORIZONTAL,
    wxDUPLEX_VERTICAL
};

/**
    Predefined print quality constants.

    @see ::wxPrintQuality
 */
#define wxPRINT_QUALITY_HIGH    -1
#define wxPRINT_QUALITY_MEDIUM  -2
#define wxPRINT_QUALITY_LOW     -3
#define wxPRINT_QUALITY_DRAFT   -4

/**
    Specifies the print quality as either a predefined level or explicit
    resolution.

    The print quality may be one of ::wxPRINT_QUALITY_HIGH,
    ::wxPRINT_QUALITY_MEDIUM, ::wxPRINT_QUALITY_LOW or ::wxPRINT_QUALITY_DRAFT
    (which are all negative) or express the desired resolution, in DPI, e.g.
    600.
 */
typedef int wxPrintQuality;

/**
    Print mode (currently PostScript only).
*/
enum wxPrintMode
{
    wxPRINT_MODE_NONE =    0,
    wxPRINT_MODE_PREVIEW = 1,   /**< Preview in external application */
    wxPRINT_MODE_FILE =    2,   /**< Print to file */
    wxPRINT_MODE_PRINTER = 3,   /**< Send to printer */
    wxPRINT_MODE_STREAM =  4    /**< Send postscript data into a stream */
};

/**
    Flags which can be used in wxWindow::UpdateWindowUI().
*/
enum wxUpdateUI
{
    wxUPDATE_UI_NONE,
    wxUPDATE_UI_RECURSE,
    wxUPDATE_UI_FROMIDLE  /**<  Invoked from On(Internal)Idle */
};


// ----------------------------------------------------------------------------
// constants
// ----------------------------------------------------------------------------

/**
    Top level window styles common to wxFrame and wxDialog
*/

#define wxSTAY_ON_TOP           0x8000
#define wxICONIZE               0x4000
#define wxMINIMIZE              wxICONIZE
#define wxMAXIMIZE              0x2000
#define wxCLOSE_BOX             0x1000

#define wxSYSTEM_MENU           0x0800
#define wxMINIMIZE_BOX          0x0400
#define wxMAXIMIZE_BOX          0x0200

#define wxTINY_CAPTION          0x0080  // clashes with wxNO_DEFAULT
#define wxRESIZE_BORDER         0x0040


/**
    C99-like sized MIN/MAX constants for all integer types.

    For each @c n in the set 8, 16, 32, 64 we define @c wxINTn_MIN, @c
    wxINTn_MAX and @c wxUINTc_MAX (@c wxUINTc_MIN is always 0 and so is not
    defined).
 */
//@{
#define wxINT8_MIN CHAR_MIN
#define wxINT8_MAX CHAR_MAX
#define wxUINT8_MAX UCHAR_MAX

#define wxINT16_MIN SHRT_MIN
#define wxINT16_MAX SHRT_MAX
#define wxUINT16_MAX USHRT_MAX

#define wxINT32_MIN INT_MIN-or-LONG_MIN
#define wxINT32_MAX INT_MAX-or-LONG_MAX
#define wxUINT32_MAX UINT_MAX-or-LONG_MAX

#define wxINT64_MIN LLONG_MIN
#define wxINT64_MAX LLONG_MAX
#define wxUINT64_MAX ULLONG_MAX
//@}

// ----------------------------------------------------------------------------
// types
// ----------------------------------------------------------------------------

/** The type for screen and DC coordinates. */
typedef int wxCoord;

/** A special value meaning "use default coordinate". */
wxCoord wxDefaultCoord = -1;

//@{
/** 8 bit type (the mapping is more complex than a simple @c typedef and is not shown here). */
typedef signed char wxInt8;
typedef unsigned char wxUint8;
typedef wxUint8 wxByte;
//@}

//@{
/** 16 bit type (the mapping is more complex than a simple @c typedef and is not shown here). */
typedef signed short wxInt16;
typedef unsigned short wxUint16;
typedef wxUint16 wxWord;
typedef wxUint16 wxChar16;
//@}

//@{
/** 32 bit type (the mapping is more complex than a simple @c typedef and is not shown here). */
typedef int wxInt32;
typedef unsigned int wxUint32;
typedef wxUint32 wxDword;
typedef wxUint32 wxChar32;
//@}

//@{
/** 64 bit type (the mapping is more complex than a simple @c typedef and is not shown here). */
typedef wxLongLong_t wxInt64;
typedef wxULongLong_t wxUint64;
//@}

//@{
/**
    Signed and unsigned integral types big enough to contain all of @c long,
    @c size_t and @c void*.
    (The mapping is more complex than a simple @c typedef and is not shown here).
*/
typedef ssize_t wxIntPtr;
typedef size_t wxUIntPtr;
//@}


/**
    32 bit IEEE float ( 1 sign, 8 exponent bits, 23 fraction bits ).
    (The mapping is more complex than a simple @c typedef and is not shown here).
*/
typedef float wxFloat32;


/**
    64 bit IEEE float ( 1 sign, 11 exponent bits, 52 fraction bits ).
    (The mapping is more complex than a simple @c typedef and is not shown here).
*/
typedef double wxFloat64;

/**
    Native fastest representation that has at least wxFloat64 precision, so use
    the IEEE types for storage, and this for calculations.
    (The mapping is more complex than a simple @c typedef and is not shown here).
*/
typedef double wxDouble;



// ----------------------------------------------------------------------------
// macros
// ----------------------------------------------------------------------------


/** @addtogroup group_funcmacro_byteorder */
//@{

/**
    This macro will swap the bytes of the @a value variable from little endian
    to big endian or vice versa unconditionally, i.e. independently of the
    current platform.

    @header{wx/defs.h}
*/
#define wxINT32_SWAP_ALWAYS( wxInt32_value )
#define wxUINT32_SWAP_ALWAYS( wxUint32_value )
#define wxINT16_SWAP_ALWAYS( wxInt16_value )
#define wxUINT16_SWAP_ALWAYS( wxUint16_value )

//@}

/** @addtogroup group_funcmacro_byteorder */
//@{

/**
    This macro will swap the bytes of the @a value variable from little endian
    to big endian or vice versa if the program is compiled on a big-endian
    architecture (such as Sun work stations). If the program has been compiled
    on a little-endian architecture, the value will be unchanged.

    Use these macros to read data from and write data to a file that stores
    data in little-endian (for example Intel i386) format.

    @header{wx/defs.h}
*/
#define wxINT32_SWAP_ON_BE( wxInt32_value )
#define wxUINT32_SWAP_ON_BE( wxUint32_value )
#define wxINT16_SWAP_ON_BE( wxInt16_value )
#define wxUINT16_SWAP_ON_BE( wxUint16_value )

//@}

/** @addtogroup group_funcmacro_byteorder */
//@{

/**
    This macro will swap the bytes of the @a value variable from little endian
    to big endian or vice versa if the program is compiled on a little-endian
    architecture (such as Intel PCs). If the program has been compiled on a
    big-endian architecture, the value will be unchanged.

    Use these macros to read data from and write data to a file that stores
    data in big-endian format.

    @header{wx/defs.h}
*/
#define wxINT32_SWAP_ON_LE( wxInt32_value )
#define wxUINT32_SWAP_ON_LE( wxUint32_value )
#define wxINT16_SWAP_ON_LE( wxInt16_value )
#define wxUINT16_SWAP_ON_LE( wxUint16_value )

//@}



/** @addtogroup group_funcmacro_misc */
//@{

/**
    This macro can be used in a class declaration to disable the generation of
    default assignment operator.

    Some classes have a well-defined copy constructor but cannot have an
    assignment operator, typically because they can't be modified once created.
    In such case, this macro can be used to disable the automatic assignment
    operator generation.

    @see wxDECLARE_NO_COPY_CLASS()
 */
#define wxDECLARE_NO_ASSIGN_CLASS(classname)

/**
    This macro can be used in a class declaration to disable the generation of
    default copy ctor and assignment operator.

    Some classes don't have a well-defined copying semantics. In this case the
    standard C++ convention is to not allow copying them. One way of achieving
    it is to use this macro which simply defines a private copy constructor and
    assignment operator.

    Beware that simply not defining copy constructor and assignment operator is
    @em not enough as the compiler would provide its own automatically-generated
    versions of them -- hence the usefulness of this macro.

    Example of use:
    @code
    class FooWidget
    {
    public:
        FooWidget();
        ...

    private:
        // widgets can't be copied
        wxDECLARE_NO_COPY_CLASS(FooWidget);
    };
    @endcode

    Notice that a semicolon must be used after this macro and that it changes
    the access specifier to private internally so it is better to use it at the
    end of the class declaration.

    @see wxDECLARE_NO_ASSIGN_CLASS(), wxDECLARE_NO_COPY_TEMPLATE_CLASS()
 */
#define wxDECLARE_NO_COPY_CLASS(classname)

/**
    Analog of wxDECLARE_NO_COPY_CLASS() for template classes.

    This macro can be used for template classes (with a single template
    parameter) for the same purpose as wxDECLARE_NO_COPY_CLASS() is used with the
    non-template classes.

    @param classname The name of the template class.
    @param arg The name of the template parameter.

    @see wxDECLARE_NO_COPY_TEMPLATE_CLASS_2
 */
#define wxDECLARE_NO_COPY_TEMPLATE_CLASS(classname, arg)

/**
    Analog of wxDECLARE_NO_COPY_TEMPLATE_CLASS() for templates with 2
    parameters.

    This macro can be used for template classes with two template
    parameters for the same purpose as wxDECLARE_NO_COPY_CLASS() is used with
    the non-template classes.

    @param classname The name of the template class.
    @param arg1 The name of the first template parameter.
    @param arg2 The name of the second template parameter.

    @see wxDECLARE_NO_COPY_TEMPLATE_CLASS
 */
#define wxDECLARE_NO_COPY_TEMPLATE_CLASS_2(classname, arg1, arg2)

/**
    A function which deletes and nulls the pointer.

    This function uses operator delete to free the pointer and also sets it to
    @NULL. Notice that this does @em not work for arrays, use wxDELETEA() for
    them.

    @code
        MyClass *ptr = new MyClass;
        ...
        wxDELETE(ptr);
        wxASSERT(!ptr);
    @endcode

    @header{wx/defs.h}
*/
template <typename T> wxDELETE(T*& ptr);

/**
    A function which deletes and nulls the pointer.

    This function uses vector operator delete (@c delete[]) to free the array
    pointer and also sets it to @NULL. Notice that this does @em not work for
    non-array pointers, use wxDELETE() for them.

    @code
        MyClass *array = new MyClass[17];
        ...
        wxDELETEA(array);
        wxASSERT(!array);
    @endcode

    @see wxDELETE()

    @header{wx/defs.h}
*/
template <typename T> wxDELETEA(T*& array);

/**
    Generate deprecation warning with the given message when a function is
    used.

    This macro can be used to generate a warning indicating that a function is
    deprecated (i.e. scheduled for removal in the future) and explaining why is
    it so and/or what should it be replaced with. It applies to the declaration
    following it, for example:
    @code
    wxDEPRECATED_MSG("use safer overload returning wxString instead")
    void wxGetSomething(char* buf, size_t len);

    wxString wxGetSomething();
    @endcode

    For compilers other than clang, g++ 4.5 or later and MSVC 8 (MSVS 2005) or
    later, the message is ignored and a generic deprecation warning is given if
    possible, i.e. if the compiler is g++ (any supported version) or MSVC 7
    (MSVS 2003) or later.

    @since 3.0

    @header{wx/defs.h}
 */
#define wxDEPRECATED_MSG(msg)

/**
    This macro can be used around a function declaration to generate warnings
    indicating that this function is deprecated (i.e. obsolete and planned to
    be removed in the future) when it is used.

    Notice that this macro itself is deprecated in favour of wxDEPRECATED_MSG()!

    Only Visual C++ 7 and higher and g++ compilers currently support this
    functionality.

    Example of use:

    @code
    // old function, use wxString version instead
    wxDEPRECATED( void wxGetSomething(char *buf, size_t len) );

    // ...
    wxString wxGetSomething();
    @endcode

    @header{wx/defs.h}
*/
#define wxDEPRECATED(function)

/**
    This is a special version of wxDEPRECATED() macro which only does something
    when the deprecated function is used from the code outside wxWidgets itself
    but doesn't generate warnings when it is used from wxWidgets.

    It is used with the virtual functions which are called by the library
    itself -- even if such function is deprecated the library still has to call
    it to ensure that the existing code overriding it continues to work, but
    the use of this macro ensures that a deprecation warning will be generated
    if this function is used from the user code or, in case of Visual C++, even
    when it is simply overridden.

    @header{wx/defs.h}
*/
#define wxDEPRECATED_BUT_USED_INTERNALLY(function)

/**
    This macro is similar to wxDEPRECATED() but can be used to not only declare
    the function @a function as deprecated but to also provide its (inline)
    implementation @a body.

    It can be used as following:

    @code
    class wxFoo
    {
    public:
        // OldMethod() is deprecated, use NewMethod() instead
        void NewMethod();
        wxDEPRECATED_INLINE( void OldMethod(), NewMethod(); )
    };
    @endcode

    @header{wx/defs.h}
*/
#define wxDEPRECATED_INLINE(func, body)

/**
    A helper macro allowing to easily define a simple deprecated accessor.

    Compared to wxDEPRECATED_INLINE() it saves a @c return statement and,
    especially, a strangely looking semicolon inside a macro.

    Example of use
    @code
    class wxFoo
    {
    public:
        int GetValue() const { return m_value; }

        // this one is deprecated because it was erroneously non-const
        wxDEPRECATED_ACCESSOR( int GetValue(), m_value )

    private:
        int m_value;
    };
    @endcode
 */
#define wxDEPRECATED_ACCESSOR(func, what)

/**
    Combination of wxDEPRECATED_BUT_USED_INTERNALLY() and wxDEPRECATED_INLINE().

    This macro should be used for deprecated functions called by the library
    itself (usually for backwards compatibility reasons) and which are defined
    inline.

    @header{wx/defs.h}
*/
#define wxDEPRECATED_BUT_USED_INTERNALLY_INLINE(func, body)

/**
    @c wxOVERRIDE expands to the C++11 @c override keyword if it's supported by
    the compiler or nothing otherwise.

    This macro is useful for writing code which may be compiled by both C++11
    and non-C++11 compilers and still allow the use of @c override for the
    former.

    Example of using this macro:
    @code
        class MyApp : public wxApp {
        public:
            virtual bool OnInit() wxOVERRIDE;

            // This would result in an error from a C++11 compiler as the
            // method doesn't actually override the base class OnExit() due to
            // a typo in its name.
            //virtual int OnEzit() wxOVERRIDE;
        };
    @endcode

    @header{wx/defs.h}

    @since 3.1.0
 */
#define wxOVERRIDE

/**
    GNU C++ compiler gives a warning for any class whose destructor is private
    unless it has a friend. This warning may sometimes be useful but it doesn't
    make sense for reference counted class which always delete themselves
    (hence destructor should be private) but don't necessarily have any
    friends, so this macro is provided to disable the warning in such case. The
    @a name parameter should be the name of the class but is only used to
    construct a unique friend class name internally.

    Example of using the macro:

    @code
    class RefCounted
    {
    public:
        RefCounted() { m_nRef = 1; }
        void IncRef() { m_nRef++ ; }
        void DecRef() { if ( !--m_nRef ) delete this; }

    private:
        ~RefCounted() { }

        wxSUPPRESS_GCC_PRIVATE_DTOR(RefCounted)
    };
    @endcode

    Notice that there should be no semicolon after this macro.

    @header{wx/defs.h}
*/
#define wxSUPPRESS_GCC_PRIVATE_DTOR_WARNING(name)

/**
    Swaps the contents of two variables.

    This is similar to std::swap() but can be used even on the platforms where
    the standard C++ library is not available (if you don't target such
    platforms, please use std::swap() instead).

    The function relies on type T being copy constructible and assignable.

    Example of use:
    @code
        int x = 3,
            y = 4;
        wxSwap(x, y);
        wxASSERT( x == 4 && y == 3 );
    @endcode
 */
template <typename T> wxSwap(T& first, T& second);

/**
    This macro is the same as the standard C99 @c va_copy for the compilers
    which support it or its replacement for those that don't. It must be used
    to preserve the value of a @c va_list object if you need to use it after
    passing it to another function because it can be modified by the latter.

    As with @c va_start, each call to @c wxVaCopy must have a matching
    @c va_end.

    @header{wx/defs.h}
*/
void wxVaCopy(va_list argptrDst, va_list argptrSrc);

//@}


