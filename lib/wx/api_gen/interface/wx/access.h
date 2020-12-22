/////////////////////////////////////////////////////////////////////////////
// Name:        access.h
// Purpose:     interface of wxAccessible
// Author:      wxWidgets team
// Licence:     wxWindows licence
/////////////////////////////////////////////////////////////////////////////


/**
    wxAccessible functions return a wxAccStatus error code,
    which may be one of this enum's values.
*/
enum wxAccStatus
{
    wxACC_FAIL,            //!< The function failed.
    wxACC_FALSE,           //!< The function returned false.
    wxACC_OK,              //!< The function completed successfully.
    wxACC_NOT_IMPLEMENTED, //!< The function is not implemented.
    wxACC_NOT_SUPPORTED,   //!< The function is not supported.
    /**
        An argument is not valid (e.g. it does not make sense for the specified
        object).
    */
    wxACC_INVALID_ARG
};


/** Child ids are integer identifiers from 1 up.
    So zero represents 'this' object.
*/
#define wxACC_SELF 0

/**
    This enum represents directions of navigation used in
    wxAccessible::Navigate().
*/
enum wxNavDir
{
    /**
        Navigate to the first child of the object. When this flag is used, the
        @c fromId parameter must be @c wxACC_SELF.
    */
    wxNAVDIR_FIRSTCHILD,
    /**
        Navigate to the last child of the object. When this flag is used, the
        @c fromId parameter must be @c wxACC_SELF.
    */
    wxNAVDIR_LASTCHILD,
    wxNAVDIR_DOWN,
    wxNAVDIR_LEFT,
    wxNAVDIR_NEXT,
    wxNAVDIR_PREVIOUS,
    wxNAVDIR_RIGHT,
    wxNAVDIR_UP
};


/**
    The role of a user interface element is represented by the values of this enum.
*/
enum wxAccRole {
    wxROLE_NONE,
    wxROLE_SYSTEM_ALERT,
    wxROLE_SYSTEM_ANIMATION,
    wxROLE_SYSTEM_APPLICATION,
    wxROLE_SYSTEM_BORDER,
    wxROLE_SYSTEM_BUTTONDROPDOWN,
    wxROLE_SYSTEM_BUTTONDROPDOWNGRID,
    wxROLE_SYSTEM_BUTTONMENU,
    wxROLE_SYSTEM_CARET,
    wxROLE_SYSTEM_CELL,
    wxROLE_SYSTEM_CHARACTER,
    wxROLE_SYSTEM_CHART,
    wxROLE_SYSTEM_CHECKBUTTON,
    wxROLE_SYSTEM_CLIENT,
    wxROLE_SYSTEM_CLOCK,
    wxROLE_SYSTEM_COLUMN,
    wxROLE_SYSTEM_COLUMNHEADER,
    wxROLE_SYSTEM_COMBOBOX,
    wxROLE_SYSTEM_CURSOR,
    wxROLE_SYSTEM_DIAGRAM,
    wxROLE_SYSTEM_DIAL,
    wxROLE_SYSTEM_DIALOG,
    wxROLE_SYSTEM_DOCUMENT,
    wxROLE_SYSTEM_DROPLIST,
    wxROLE_SYSTEM_EQUATION,
    wxROLE_SYSTEM_GRAPHIC,
    wxROLE_SYSTEM_GRIP,
    wxROLE_SYSTEM_GROUPING,
    wxROLE_SYSTEM_HELPBALLOON,
    wxROLE_SYSTEM_HOTKEYFIELD,
    wxROLE_SYSTEM_INDICATOR,
    wxROLE_SYSTEM_LINK,
    wxROLE_SYSTEM_LIST,
    wxROLE_SYSTEM_LISTITEM,
    wxROLE_SYSTEM_MENUBAR,
    wxROLE_SYSTEM_MENUITEM,
    wxROLE_SYSTEM_MENUPOPUP,
    wxROLE_SYSTEM_OUTLINE,
    wxROLE_SYSTEM_OUTLINEITEM,
    wxROLE_SYSTEM_PAGETAB,
    wxROLE_SYSTEM_PAGETABLIST,
    wxROLE_SYSTEM_PANE,
    wxROLE_SYSTEM_PROGRESSBAR,
    wxROLE_SYSTEM_PROPERTYPAGE,
    wxROLE_SYSTEM_PUSHBUTTON,
    wxROLE_SYSTEM_RADIOBUTTON,
    wxROLE_SYSTEM_ROW,
    wxROLE_SYSTEM_ROWHEADER,
    wxROLE_SYSTEM_SCROLLBAR,
    wxROLE_SYSTEM_SEPARATOR,
    wxROLE_SYSTEM_SLIDER,
    wxROLE_SYSTEM_SOUND,
    wxROLE_SYSTEM_SPINBUTTON,
    wxROLE_SYSTEM_STATICTEXT,
    wxROLE_SYSTEM_STATUSBAR,
    wxROLE_SYSTEM_TABLE,
    wxROLE_SYSTEM_TEXT,
    wxROLE_SYSTEM_TITLEBAR,
    wxROLE_SYSTEM_TOOLBAR,
    wxROLE_SYSTEM_TOOLTIP,
    wxROLE_SYSTEM_WHITESPACE,
    wxROLE_SYSTEM_WINDOW
};

/**
    Objects are represented by a wxAccObject enum value.
*/
enum wxAccObject {
    /**
        @hideinitializer
    */
    wxOBJID_WINDOW =    0x00000000,
    /**
        @hideinitializer
    */
    wxOBJID_SYSMENU =   0xFFFFFFFF,
    /**
        @hideinitializer
    */
    wxOBJID_TITLEBAR =  0xFFFFFFFE,
    /**
        @hideinitializer
    */
    wxOBJID_MENU =      0xFFFFFFFD,
    /**
        @hideinitializer
    */
    wxOBJID_CLIENT =    0xFFFFFFFC,
    /**
        @hideinitializer
    */
    wxOBJID_VSCROLL =   0xFFFFFFFB,
    /**
        @hideinitializer
    */
    wxOBJID_HSCROLL =   0xFFFFFFFA,
    /**
        @hideinitializer
    */
    wxOBJID_SIZEGRIP =  0xFFFFFFF9,
    /**
        @hideinitializer
    */
    wxOBJID_CARET =     0xFFFFFFF8,
    /**
        @hideinitializer
    */
    wxOBJID_CURSOR =    0xFFFFFFF7,
    /**
        @hideinitializer
    */
    wxOBJID_ALERT =     0xFFFFFFF6,
    /**
        @hideinitializer
    */
    wxOBJID_SOUND =     0xFFFFFFF5
};


/**
    Selection actions are identified by the wxAccSelectionFlags values.
*/
enum wxAccSelectionFlags
{
    /**
        No action is performed. Neither the selection nor focus is changed.

        @hideinitializer
    */
    wxACC_SEL_NONE            = 0,
    /**
        The object is focused and becomes the selection anchor.

        @hideinitializer
    */
    wxACC_SEL_TAKEFOCUS       = 1,
    /**
        The object is selected and all other objects are removed from the
        current selection.

        @hideinitializer
    */
    wxACC_SEL_TAKESELECTION   = 2,
    /**
        All objects between the selection anchor and this object are added to
        the current selection if the anchor object's is selected or they are
        removed from the current selection otherwise.
        If this flag is combined with @c wxACC_SEL_ADDSELECTION, the objects
        are added to the current selection regardless of the anchor object's
        state.
        If this flag is combined with @c wxACC_SEL_REMOVESELECTION, the objects
        are removed from the current selection regardless of the anchor
        object's state.

        @hideinitializer
    */
    wxACC_SEL_EXTENDSELECTION = 4,
    /**
        The object is added to the current selection. A noncontiguous selection
        can be a result of this operation.

        @hideinitializer
    */
    wxACC_SEL_ADDSELECTION    = 8,
    /**
        The object is removed from the current selection. A noncontiguous
        selection can be a result of this operation.

        @hideinitializer
    */
    wxACC_SEL_REMOVESELECTION = 16
};

//@{
/**
    Represents a status of the system.
*/
#define wxACC_STATE_SYSTEM_ALERT_HIGH       0x00000001
#define wxACC_STATE_SYSTEM_ALERT_MEDIUM     0x00000002
#define wxACC_STATE_SYSTEM_ALERT_LOW        0x00000004
#define wxACC_STATE_SYSTEM_ANIMATED         0x00000008
#define wxACC_STATE_SYSTEM_BUSY             0x00000010
#define wxACC_STATE_SYSTEM_CHECKED          0x00000020
#define wxACC_STATE_SYSTEM_COLLAPSED        0x00000040
#define wxACC_STATE_SYSTEM_DEFAULT          0x00000080
#define wxACC_STATE_SYSTEM_EXPANDED         0x00000100
#define wxACC_STATE_SYSTEM_EXTSELECTABLE    0x00000200
#define wxACC_STATE_SYSTEM_FLOATING         0x00000400
#define wxACC_STATE_SYSTEM_FOCUSABLE        0x00000800
#define wxACC_STATE_SYSTEM_FOCUSED          0x00001000
#define wxACC_STATE_SYSTEM_HOTTRACKED       0x00002000
#define wxACC_STATE_SYSTEM_INVISIBLE        0x00004000
#define wxACC_STATE_SYSTEM_MARQUEED         0x00008000
#define wxACC_STATE_SYSTEM_MIXED            0x00010000
#define wxACC_STATE_SYSTEM_MULTISELECTABLE  0x00020000
#define wxACC_STATE_SYSTEM_OFFSCREEN        0x00040000
#define wxACC_STATE_SYSTEM_PRESSED          0x00080000
#define wxACC_STATE_SYSTEM_PROTECTED        0x00100000
#define wxACC_STATE_SYSTEM_READONLY         0x00200000
#define wxACC_STATE_SYSTEM_SELECTABLE       0x00400000
#define wxACC_STATE_SYSTEM_SELECTED         0x00800000
#define wxACC_STATE_SYSTEM_SELFVOICING      0x01000000
#define wxACC_STATE_SYSTEM_UNAVAILABLE      0x02000000
//@}

//@{
/**
    An event identifier that can be sent via wxAccessible::NotifyEvent.
*/
#define wxACC_EVENT_SYSTEM_SOUND              0x0001
#define wxACC_EVENT_SYSTEM_ALERT              0x0002
#define wxACC_EVENT_SYSTEM_FOREGROUND         0x0003
#define wxACC_EVENT_SYSTEM_MENUSTART          0x0004
#define wxACC_EVENT_SYSTEM_MENUEND            0x0005
#define wxACC_EVENT_SYSTEM_MENUPOPUPSTART     0x0006
#define wxACC_EVENT_SYSTEM_MENUPOPUPEND       0x0007
#define wxACC_EVENT_SYSTEM_CAPTURESTART       0x0008
#define wxACC_EVENT_SYSTEM_CAPTUREEND         0x0009
#define wxACC_EVENT_SYSTEM_MOVESIZESTART      0x000A
#define wxACC_EVENT_SYSTEM_MOVESIZEEND        0x000B
#define wxACC_EVENT_SYSTEM_CONTEXTHELPSTART   0x000C
#define wxACC_EVENT_SYSTEM_CONTEXTHELPEND     0x000D
#define wxACC_EVENT_SYSTEM_DRAGDROPSTART      0x000E
#define wxACC_EVENT_SYSTEM_DRAGDROPEND        0x000F
#define wxACC_EVENT_SYSTEM_DIALOGSTART        0x0010
#define wxACC_EVENT_SYSTEM_DIALOGEND          0x0011
#define wxACC_EVENT_SYSTEM_SCROLLINGSTART     0x0012
#define wxACC_EVENT_SYSTEM_SCROLLINGEND       0x0013
#define wxACC_EVENT_SYSTEM_SWITCHSTART        0x0014
#define wxACC_EVENT_SYSTEM_SWITCHEND          0x0015
#define wxACC_EVENT_SYSTEM_MINIMIZESTART      0x0016
#define wxACC_EVENT_SYSTEM_MINIMIZEEND        0x0017
#define wxACC_EVENT_OBJECT_CREATE                 0x8000
#define wxACC_EVENT_OBJECT_DESTROY                0x8001
#define wxACC_EVENT_OBJECT_SHOW                   0x8002
#define wxACC_EVENT_OBJECT_HIDE                   0x8003
#define wxACC_EVENT_OBJECT_REORDER                0x8004
#define wxACC_EVENT_OBJECT_FOCUS                  0x8005
#define wxACC_EVENT_OBJECT_SELECTION              0x8006
#define wxACC_EVENT_OBJECT_SELECTIONADD           0x8007
#define wxACC_EVENT_OBJECT_SELECTIONREMOVE        0x8008
#define wxACC_EVENT_OBJECT_SELECTIONWITHIN        0x8009
#define wxACC_EVENT_OBJECT_STATECHANGE            0x800A
#define wxACC_EVENT_OBJECT_LOCATIONCHANGE         0x800B
#define wxACC_EVENT_OBJECT_NAMECHANGE             0x800C
#define wxACC_EVENT_OBJECT_DESCRIPTIONCHANGE      0x800D
#define wxACC_EVENT_OBJECT_VALUECHANGE            0x800E
#define wxACC_EVENT_OBJECT_PARENTCHANGE           0x800F
#define wxACC_EVENT_OBJECT_HELPCHANGE             0x8010
#define wxACC_EVENT_OBJECT_DEFACTIONCHANGE        0x8011
#define wxACC_EVENT_OBJECT_ACCELERATORCHANGE      0x8012
//@}

/**
    @class wxAccessible

    The wxAccessible class allows wxWidgets applications, and wxWidgets itself,
    to return extended information about user interface elements to client
    applications such as screen readers. This is the main way in which wxWidgets
    implements accessibility features.

    At present, only Microsoft Active Accessibility is supported by this class.

    To use this class, derive from wxAccessible, implement appropriate
    functions, and associate an object of the class with a window using
    wxWindow::SetAccessible.

    All functions return an indication of success, failure, or not implemented
    using values of the wxAccStatus enum type.

    If you return @c wxACC_NOT_IMPLEMENTED from any function, the system will try
    to implement the appropriate functionality. However this will not work with
    all functions.

    Most functions work with an object @e id, which can be zero to refer to
    'this' UI element, or greater than zero to refer to the nth child element.
    This allows you to specify elements that don't have a corresponding wxWindow or
    wxAccessible; for example, the sash of a splitter window.

    For details on the semantics of functions and types, please refer to the
    Microsoft Active Accessibility 1.2 documentation.

    This class is compiled into wxWidgets only if the @c wxUSE_ACCESSIBILITY setup
    symbol is set to 1.

    @onlyfor{wxmsw}

    @library{wxcore}
    @category{misc}

    @see @sample{access}
*/
class wxAccessible : public wxObject
{
public:
    /**
        Constructor, taking an optional window. The object can be associated with
        a window later.
    */
    wxAccessible(wxWindow* win = NULL);

    /**
        Destructor.
    */
    ~wxAccessible();

    /**
        Performs the default action for the object.
        @a childId is 0 (the action for this object) or greater than 0 (the action
        for a child).

        @return wxACC_NOT_SUPPORTED if there is no default action for this
                window (e.g. an edit control).
    */
    virtual wxAccStatus DoDefaultAction(int childId);

    /**
        Gets the specified child (starting from 1). If @a child is @NULL and the return
        value is wxACC_OK, this means that the child is a simple element and not an
        accessible object.
    */
    virtual wxAccStatus GetChild(int childId, wxAccessible** child);

    /**
        Returns the number of children in @a childCount.
    */
    virtual wxAccStatus GetChildCount(int* childCount);

    /**
        Gets the default action for this object (0) or a child (greater than 0).

        Return wxACC_OK even if there is no action. @a actionName is the action, or the
        empty string if there is no action. The retrieved string describes the action that is
        performed on an object, not what the object does as a result. For example, a toolbar
        button that prints a document has a default action of "Press" rather than "Prints
        the current document."
    */
    virtual wxAccStatus GetDefaultAction(int childId,
                                         wxString* actionName);

    /**
        Returns the description for this object or a child.
    */
    virtual wxAccStatus GetDescription(int childId,
                                       wxString* description);

    /**
        Gets the window with the keyboard focus. If childId is 0 and child is @NULL, no
        object in this subhierarchy has the focus. If this object has the focus, child
        should be 'this'.
    */
    virtual wxAccStatus GetFocus(int* childId, wxAccessible** child);

    /**
        Returns help text for this object or a child, similar to tooltip text.
    */
    virtual wxAccStatus GetHelpText(int childId, wxString* helpText);

    /**
        Returns the keyboard shortcut for this object or child.
        Returns e.g. ALT+K.
    */
    virtual wxAccStatus GetKeyboardShortcut(int childId,
                                            wxString* shortcut);

    /**
        Returns the rectangle for this object (id is 0) or a child element (id is
        greater than 0).
        @a rect is in screen coordinates.
    */
    virtual wxAccStatus GetLocation(wxRect& rect, int elementId);

    /**
        Gets the name of the specified object.
    */
    virtual wxAccStatus GetName(int childId, wxString* name);

    /**
        Returns the parent of this object, or @NULL.
    */
    virtual wxAccStatus GetParent(wxAccessible** parent);

    /**
        Returns a role constant describing this object. See wxAccRole for a list
        of these roles.
    */
    virtual wxAccStatus GetRole(int childId, wxAccRole* role);

    /**
        Gets a variant representing the selected children of this object.

        Acceptable values are:
        @li a null variant (@c IsNull() returns @true) if no children
            are selected
        @li a @c void* pointer to a wxAccessible of selected child object
        @li an integer representing the selected child element,
            or 0 if this object is selected (@c GetType() @c == @c "long")
        @li a list variant (@c GetType() @c == @c "list") if multiple child
            objects are selected
    */
    virtual wxAccStatus GetSelections(wxVariant* selections);

    /**
        Returns a state constant. See wxAccStatus for a list of these states.
    */
    virtual wxAccStatus GetState(int childId, long* state);

    /**
        Returns a localized string representing the value for the object
        or child.
    */
    virtual wxAccStatus GetValue(int childId, wxString* strValue);

    /**
        Returns the window associated with this object.
    */
    wxWindow* GetWindow();

    /**
        Returns a status value and object id to indicate whether the given point
        was on this or a child object. Can return either a child object, or an
        integer representing the child element, starting from 1.

        @a pt is in screen coordinates.
    */
    virtual wxAccStatus HitTest(const wxPoint& pt, int* childId,
                                wxAccessible** childObject);

    /**
        Navigates from @a fromId to @a toId or to @a toObject.
    */
    virtual wxAccStatus Navigate(wxNavDir navDir, int fromId,
                                 int* toId,
                                 wxAccessible** toObject);

    /**
        Allows the application to send an event when something changes in
        an accessible object.
    */
    static void NotifyEvent(int eventType, wxWindow* window,
                            wxAccObject objectType,
                            int objectId);

    /**
        Selects the object or child. See wxAccSelectionFlags for a list
        of the selection actions.
    */
    virtual wxAccStatus Select(int childId,
                               wxAccSelectionFlags selectFlags);

    /**
        Sets the window associated with this object.
    */
    void SetWindow(wxWindow* window);
};

