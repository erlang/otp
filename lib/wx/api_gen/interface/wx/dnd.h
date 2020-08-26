/////////////////////////////////////////////////////////////////////////////
// Name:        dnd.h
// Purpose:     interface of wxDropSource and wx*DropTarget
// Author:      wxWidgets team
// Licence:     wxWindows licence
/////////////////////////////////////////////////////////////////////////////

/**
    Possible flags for drag and drop operations.
 */
enum
{
    wxDrag_CopyOnly    = 0, ///< Allow only copying.
    wxDrag_AllowMove   = 1, ///< Allow moving too (copying is always allowed).
    wxDrag_DefaultMove = 3  ///< Allow moving and make it default operation.
};

/**
    Result returned from a wxDropSource::DoDragDrop() call.
*/
enum wxDragResult
{
    wxDragError,    ///< Error prevented the D&D operation from completing.
    wxDragNone,     ///< Drag target didn't accept the data.
    wxDragCopy,     ///< The data was successfully copied.
    wxDragMove,     ///< The data was successfully moved (MSW only).
    wxDragLink,     ///< Operation is a drag-link.
    wxDragCancel    ///< The operation was cancelled by user (not an error).
};



/**
    @class wxDropTarget

    This class represents a target for a drag and drop operation. A
    wxDataObject can be associated with it and by default, this object will be
    filled with the data from the drag source, if the data formats supported by
    the data object match the drag source data format.

    There are various virtual handler functions defined in this class which may
    be overridden to give visual feedback or react in a more fine-tuned way,
    e.g. by not accepting data on the whole window area, but only a small
    portion of it. The normal sequence of calls is OnEnter(), OnDragOver()
    possibly many times, OnDrop() and finally OnData().

    @library{wxcore}
    @category{dnd}

    @see @ref overview_dnd, @ref overview_dataobject, wxDropSource,
         wxTextDropTarget, wxFileDropTarget, wxDataFormat, wxDataObject
*/
class wxDropTarget
{
public:
    /**
        Constructor. @a data is the data to be associated with the drop target.
    */
    wxDropTarget(wxDataObject* data = NULL);

    /**
        Destructor. Deletes the associated data object, if any.
    */
    virtual ~wxDropTarget();

    /**
        This method may only be called from within OnData(). By default, this
        method copies the data from the drop source to the wxDataObject
        associated with this drop target, calling its wxDataObject::SetData()
        method.
    */
    virtual bool GetData();

    /**
        Called after OnDrop() returns @true. By default this will usually
        GetData() and will return the suggested default value @a defResult.
    */
    virtual wxDragResult OnData(wxCoord x, wxCoord y, wxDragResult defResult) = 0;

    /**
        Called when the mouse is being dragged over the drop target. By
        default, this calls functions return the suggested return value @a defResult.

        @param x
            The x coordinate of the mouse.
        @param y
            The y coordinate of the mouse.
        @param defResult
            Suggested value for return value. Determined by SHIFT or CONTROL
            key states.

        @return The desired operation or wxDragNone. This is used for optical
                 feedback from the side of the drop source, typically in form
                 of changing the icon.
    */
    virtual wxDragResult OnDragOver(wxCoord x, wxCoord y, wxDragResult defResult);

    /**
        Called when the user drops a data object on the target. Return @false
        to veto the operation.

        @param x
            The x coordinate of the mouse.
        @param y
            The y coordinate of the mouse.

        @return @true to accept the data, or @false to veto the operation.
    */
    virtual bool OnDrop(wxCoord x, wxCoord y);

    /**
        Called when the mouse enters the drop target. By default, this calls
        OnDragOver().

        @param x
            The x coordinate of the mouse.
        @param y
            The y coordinate of the mouse.
        @param defResult
            Suggested default for return value. Determined by SHIFT or CONTROL
            key states.

        @return The desired operation or wxDragNone. This is used for optical
                 feedback from the side of the drop source, typically in form
                 of changing the icon.
    */
    virtual wxDragResult OnEnter(wxCoord x, wxCoord y, wxDragResult defResult);

    /**
        Called when the mouse leaves the drop target.
    */
    virtual void OnLeave();

    /**
        Returns the data wxDataObject associated with the drop target
    */
    wxDataObject *GetDataObject() const;

    /**
        Sets the data wxDataObject associated with the drop target and deletes
        any previously associated data object.
    */
    void SetDataObject(wxDataObject* data);


    /**
       Sets the default action for drag and drop.  Use wxDragMove or
       wxDragCopy to set default action to move or copy and use wxDragNone
       (default) to set default action specified by initialization of dragging
       (see wxDropSource::DoDragDrop())
    */
    void SetDefaultAction(wxDragResult action);

    /**
       Returns default action for drag and drop or wxDragNone if this not
       specified.
    */
    wxDragResult GetDefaultAction();

};



/**
    @class wxDropSource

    This class represents a source for a drag and drop operation.

    @library{wxcore}
    @category{dnd}

    @see @ref overview_dnd, @ref overview_dataobject, wxDropTarget,
         wxTextDropTarget, wxFileDropTarget
*/
class wxDropSource
{
public:
    /**
        This constructor requires that you must call SetData() later.

        Note that the type of @a iconCopy and subsequent parameters
        differs between different ports: these are cursors under Windows and OS
        X but icons for GTK. You should use the macro wxDROP_ICON() in portable
        programs instead of directly using either of these types.

        @onlyfor{wxmsw,wxosx}

        @param win
            The window which initiates the drag and drop operation.
        @param iconCopy
            The icon or cursor used for feedback for copy operation.
        @param iconMove
            The icon or cursor used for feedback for move operation.
        @param iconNone
            The icon or cursor used for feedback when operation can't be done.
    */
    wxDropSource(wxWindow* win = NULL,
                 const wxCursor& iconCopy = wxNullCursor,
                 const wxCursor& iconMove = wxNullCursor,
                 const wxCursor& iconNone = wxNullCursor);

    /**
        The constructor taking a wxDataObject.

        Note that the type of @a iconCopy and subsequent parameters
        differs between different ports: these are cursors under Windows and OS
        X but icons for GTK. You should use the macro wxDROP_ICON() in portable
        programs instead of directly using either of these types.

        @onlyfor{wxmsw,wxosx}

        @param data
            The data associated with the drop source.
        @param win
            The window which initiates the drag and drop operation.
        @param iconCopy
            The icon or cursor used for feedback for copy operation.
        @param iconMove
            The icon or cursor used for feedback for move operation.
        @param iconNone
            The icon or cursor used for feedback when operation can't be done.
    */
    wxDropSource(wxDataObject& data, wxWindow* win = NULL,
                 const wxCursor& iconCopy = wxNullCursor,
                 const wxCursor& iconMove = wxNullCursor,
                 const wxCursor& iconNone = wxNullCursor);

    /**
        This constructor requires that you must call SetData() later.

        This is the wxGTK-specific version of the constructor taking wxIcon
        instead of wxCursor as the other ports.

        @onlyfor{wxgtk}

        @param win
            The window which initiates the drag and drop operation.
        @param iconCopy
            The icon or cursor used for feedback for copy operation.
        @param iconMove
            The icon or cursor used for feedback for move operation.
        @param iconNone
            The icon or cursor used for feedback when operation can't be done.
    */
    wxDropSource(wxWindow* win = NULL,
                 const wxIcon& iconCopy = wxNullIcon,
                 const wxIcon& iconMove = wxNullIcon,
                 const wxIcon& iconNone = wxNullIcon);

    /**
        The constructor taking a wxDataObject.

        This is the wxGTK-specific version of the constructor taking wxIcon
        instead of wxCursor as the other ports.

        @onlyfor{wxgtk}

        @param data
            The data associated with the drop source.
        @param win
            The window which initiates the drag and drop operation.
        @param iconCopy
            The icon or cursor used for feedback for copy operation.
        @param iconMove
            The icon or cursor used for feedback for move operation.
        @param iconNone
            The icon or cursor used for feedback when operation can't be done.
    */
    wxDropSource(wxDataObject& data, wxWindow* win = NULL,
                 const wxIcon& iconCopy = wxNullIcon,
                 const wxIcon& iconMove = wxNullIcon,
                 const wxIcon& iconNone = wxNullIcon);

    /**
        Starts the drag-and-drop operation which will terminate when the user
        releases the mouse. Call this in response to a mouse button press, for
        example.

        @param flags
            If ::wxDrag_AllowMove is included in the flags, data may be moved
            and not only copied as is the case for the default
            ::wxDrag_CopyOnly. If ::wxDrag_DefaultMove is specified
            (which includes the previous flag), moving is not only possible but
            becomes the default operation.

        @return The operation requested by the user, may be ::wxDragCopy,
                 ::wxDragMove, ::wxDragLink, ::wxDragCancel or ::wxDragNone if
                 an error occurred.
    */
    virtual wxDragResult DoDragDrop(int flags = wxDrag_CopyOnly);

    /**
        Returns the wxDataObject object that has been assigned previously.
    */
    wxDataObject* GetDataObject();

    /**
        You may give some custom UI feedback during the drag and drop operation
        by overriding this function. It is called on each mouse move, so your
        implementation must not be too slow.

        @param effect
            The effect to implement. One of ::wxDragCopy, ::wxDragMove,
            ::wxDragLink and ::wxDragNone.

        @return @false if you want default feedback, or @true if you implement
                your own feedback. The return value is ignored under GTK.
    */
    virtual bool GiveFeedback(wxDragResult effect);

    /**
        Set the icon to use for a certain drag result.

        @param res
            The drag result to set the icon for.
        @param cursor
            The icon to show when this drag result occurs.

        @onlyfor{wxmsw,wxosx}
    */
    void SetCursor(wxDragResult res, const wxCursor& cursor);

    /**
        Set the icon to use for a certain drag result.

        @param res
            The drag result to set the icon for.
        @param icon
            The icon to show when this drag result occurs.

        @onlyfor{wxgtk}
    */
    void SetIcon(wxDragResult res, const wxIcon& icon);

    /**
        Sets the data wxDataObject associated with the drop source. This will
        not delete any previously associated data.
    */
    void SetData(wxDataObject& data);
};



/**
    @class wxTextDropTarget

    A predefined drop target for dealing with text data.

    @library{wxcore}
    @category{dnd}

    @see @ref overview_dnd, wxDropSource, wxDropTarget, wxFileDropTarget
*/
class wxTextDropTarget : public wxDropTarget
{
public:
    /**
        Constructor.
    */
    wxTextDropTarget();

    /**
        See wxDropTarget::OnDrop(). This function is implemented appropriately
        for text, and calls OnDropText().
    */
    virtual bool OnDrop(wxCoord x, wxCoord y);

    /**
        Override this function to receive dropped text.

        @param x
            The x coordinate of the mouse.
        @param y
            The y coordinate of the mouse.
        @param data
            The data being dropped: a wxString.

        Return @true to accept the data, or @false to veto the operation.
    */
    virtual bool OnDropText(wxCoord x, wxCoord y, const wxString& data) = 0;
};


/**
    @class wxFileDropTarget

    This is a drop target which accepts files (dragged from File Manager or
    Explorer).

    @library{wxcore}
    @category{dnd}

    @see @ref overview_dnd, wxDropSource, wxDropTarget, wxTextDropTarget
*/
class wxFileDropTarget : public wxDropTarget
{
public:
    /**
        Constructor.
    */
    wxFileDropTarget();

    /**
        See wxDropTarget::OnDrop(). This function is implemented appropriately
        for files, and calls OnDropFiles().
    */
    virtual bool OnDrop(wxCoord x, wxCoord y);

    /**
        Override this function to receive dropped files.

        @param x
            The x coordinate of the mouse.
        @param y
            The y coordinate of the mouse.
        @param filenames
            An array of filenames.

        Return @true to accept the data, or @false to veto the operation.
    */
    virtual bool OnDropFiles(wxCoord x, wxCoord y,
                             const wxArrayString& filenames) = 0;
};



// ============================================================================
// Global functions/macros
// ============================================================================

/** @addtogroup group_funcmacro_gdi */
//@{

/**
    This macro creates either a cursor (MSW) or an icon (elsewhere) with the
    given @a name (of type <tt>const char*</tt>). Under MSW, the cursor is
    loaded from the resource file and the icon is loaded from XPM file under
    other platforms.

    This macro should be used with wxDropSource::wxDropSource().

    @return wxCursor on MSW, otherwise returns a wxIcon

    @header{wx/dnd.h}
*/
#define wxDROP_ICON(name)

/**
   Returns true if res indicates that something was done during a DnD operation,
   i.e. is neither error nor none nor cancel.
*/
bool wxIsDragResultOk(wxDragResult res);

//@}

