/////////////////////////////////////////////////////////////////////////////
// Name:        vscroll.h
// Purpose:     interface of wxVarHScrollHelper
// Author:      wxWidgets team
// Licence:     wxWindows licence
/////////////////////////////////////////////////////////////////////////////

/**
    @class wxVarScrollHelperBase

    This class provides all common base functionality for scroll calculations
    shared among all variable scrolled window implementations as well as
    automatic scrollbar functionality, saved scroll positions, controlling
    target windows to be scrolled, as well as defining all required virtual
    functions that need to be implemented for any orientation specific work.

    Documentation of this class is provided specifically for referencing use
    of the functions provided by this class for use with the variable scrolled
    windows that derive from here. You will likely want to derive your window
    from one of the already implemented variable scrolled windows rather than
    from wxVarScrollHelperBase directly.

    @library{wxcore}
    @category{miscwnd}

    @see wxHScrolledWindow, wxHVScrolledWindow, wxVScrolledWindow
*/
class wxVarScrollHelperBase
{
public:
    /**
        Constructor taking the target window to be scrolled by this helper
        class. This will attach scroll event handlers to the target window to
        catch and handle scroll events appropriately.
    */
    wxVarScrollHelperBase(wxWindow* winToScroll);

    /**
        Virtual destructor for detaching scroll event handlers attached with
        this helper class.
    */
    virtual ~wxVarScrollHelperBase();

    /**
        Translates the logical coordinate given to the current device
        coordinate. For example, if the window is scrolled 10 units and each
        scroll unit represents 10 device units (which may not be the case since
        this class allows for variable scroll unit sizes), a call to this
        function with a coordinate of 15 will return -85.

        @see CalcUnscrolledPosition()
    */
    int CalcScrolledPosition(int coord) const;

    /**
        Translates the device coordinate given to the corresponding logical
        coordinate. For example, if the window is scrolled 10 units and each
        scroll unit represents 10 device units (which may not be the case since
        this class allows for variable scroll unit sizes), a call to this
        function with a coordinate of 15 will return 115.

        @see CalcScrolledPosition()
    */
    int CalcUnscrolledPosition(int coord) const;

    /**
        With physical scrolling on (when this is @true), the device origin is
        changed properly when a wxPaintDC is prepared, children are actually
        moved and laid out properly, and the contents of the window (pixels)
        are actually moved. When this is @false, you are responsible for
        repainting any invalidated areas of the window yourself to account for
        the new scroll position.
    */
    void EnablePhysicalScrolling(bool scrolling = true);

    /**
        This function needs to be overridden in the in the derived class to
        return the window size with respect to the opposing orientation. If
        this is a vertical scrolled window, it should return the height.

        @see GetOrientationTargetSize()
    */
    virtual int GetNonOrientationTargetSize() const = 0;

    /**
        This function need to be overridden to return the orientation that this
        helper is working with, either @c wxHORIZONTAL or @c wxVERTICAL.
    */
    virtual wxOrientation GetOrientation() const = 0;

    /**
        This function needs to be overridden in the in the derived class to
        return the window size with respect to the orientation this helper is
        working with. If this is a vertical scrolled window, it should return
        the width.

        @see GetNonOrientationTargetSize()
    */
    virtual int GetOrientationTargetSize() const = 0;

    /**
        This function will return the target window this helper class is
        currently scrolling.

        @see SetTargetWindow()
    */
    virtual wxWindow* GetTargetWindow() const;

    /**
        Returns the index of the first visible unit based on the scroll
        position.
    */
    size_t GetVisibleBegin() const;

    /**
        Returns the index of the last visible unit based on the scroll
        position. This includes the last unit even if it is only partially
        visible.
    */
    size_t GetVisibleEnd() const;

    /**
        Returns @true if the given scroll unit is currently visible (even if
        only partially visible) or @false otherwise.
    */
    bool IsVisible(size_t unit) const;

    /**
        Recalculate all parameters and repaint all units.
    */
    virtual void RefreshAll();

    /**
        Normally the window will scroll itself, but in some rare occasions you
        might want it to scroll (part of) another window (e.g. a child of it in
        order to scroll only a portion the area between the scrollbars like a
        spreadsheet where only the cell area will move).

        @see GetTargetWindow()
    */
    virtual void SetTargetWindow(wxWindow* target);

    /**
        Update the thumb size shown by the scrollbar.
    */
    virtual void UpdateScrollbar();

    /**
        Returns the virtual scroll unit under the device unit given accounting
        for scroll position or @c wxNOT_FOUND if none (i.e. if it is below the
        last item).
    */
    int VirtualHitTest(wxCoord coord) const;


protected:

    /**
        This function doesn't have to be overridden but it may be useful to do
        so if calculating the units' sizes is a relatively expensive operation
        as it gives your code a chance to calculate several of them at once and
        cache the result if necessary.

        OnGetUnitsSizeHint() is normally called just before OnGetUnitSize() but
        you shouldn't rely on the latter being called for all units in the
        interval specified here. It is also possible that OnGetUnitSize() will
        be called for units outside of this interval, so this is really just a
        hint, not a promise.

        Finally, note that @a unitMin is inclusive, while @a unitMax is
        exclusive.
    */
    virtual void OnGetUnitsSizeHint(size_t unitMin, size_t unitMax) const;

    /**
        When the number of scroll units change, we try to estimate the total
        size of all units when the full window size is needed (i.e. to
        calculate the scrollbar thumb size). This is a rather expensive
        operation in terms of unit access, so if the user code may estimate the
        average size better or faster than we do, it should override this
        function to implement its own logic. This function should return the
        best guess for the total virtual window size.

        @note Although returning a totally wrong value would still work, it
              risks resulting in very strange scrollbar behaviour so this
              function should really try to make the best guess possible.
    */
    virtual wxCoord EstimateTotalSize() const;

    /**
        This function must be overridden in the derived class, and should
        return the size of the given unit in pixels.
    */
    virtual wxCoord OnGetUnitSize(size_t unit) const = 0;
};



/**
    @class wxVarVScrollHelper

    This class provides functions wrapping the wxVarScrollHelperBase class,
    targeted for vertical-specific scrolling.

    Like wxVarScrollHelperBase, this class is mostly only useful to those
    classes built into wxWidgets deriving from here, and this documentation is
    mostly only provided for referencing the functions provided by this class.
    You will likely want to derive your window from wxVScrolledWindow rather
    than from here directly.

    @library{wxcore}
    @category{miscwnd}

    @see wxHScrolledWindow, wxHVScrolledWindow, wxVScrolledWindow
*/
class wxVarVScrollHelper : public wxVarScrollHelperBase
{
public:
    /**
        Constructor taking the target window to be scrolled by this helper
        class. This will attach scroll event handlers to the target window to
        catch and handle scroll events appropriately.
    */
    wxVarVScrollHelper(wxWindow* winToScroll);

    /**
        Returns the number of rows the target window contains.

        @see SetRowCount()
    */
    size_t GetRowCount() const;

    /**
        Returns the index of the first visible row based on the scroll
        position.
    */
    size_t GetVisibleRowsBegin() const;

    /**
        Returns the index of the last visible row based on the scroll position.
        This includes the last row even if it is only partially visible.
    */
    size_t GetVisibleRowsEnd() const;

    /**
        Returns @true if the given row is currently visible (even if only
        partially visible) or @false otherwise.
    */
    bool IsRowVisible(size_t row) const;

    /**
        Triggers a refresh for just the given row's area of the window if it's
        visible.
    */
    virtual void RefreshRow(size_t row);

    /**
        Triggers a refresh for the area between the specified range of rows
        given (inclusively).
    */
    virtual void RefreshRows(size_t from, size_t to);

    /**
        Scroll by the specified number of pages which may be positive (to
        scroll down) or negative (to scroll up).
    */
    virtual bool ScrollRowPages(int pages);

    /**
        Scroll by the specified number of rows which may be positive (to scroll
        down) or negative (to scroll up).

        @return @true if the window was scrolled, @false otherwise (for
                example, if we're trying to scroll down but we are already
                showing the last row).
    */
    virtual bool ScrollRows(int rows);

    /**
        Scroll to the specified row. It will become the first visible row in
        the window.

        @return @true if we scrolled the window, @false if nothing was done.
    */
    bool ScrollToRow(size_t row);

    /**
        Set the number of rows the window contains. The derived class must
        provide the heights for all rows with indices up to the one given here
        in it's OnGetRowHeight() implementation.

        @see GetRowCount()
    */
    void SetRowCount(size_t rowCount);

protected:

    /**
        This function doesn't have to be overridden but it may be useful to do
        so if calculating the rows' sizes is a relatively expensive operation
        as it gives your code a chance to calculate several of them at once and
        cache the result if necessary.

        OnGetRowsHeightHint() is normally called just before OnGetRowHeight()
        but you shouldn't rely on the latter being called for all rows in the
        interval specified here. It is also possible that OnGetRowHeight() will
        be called for units outside of this interval, so this is really just a
        hint, not a promise.

        Finally, note that @a rowMin is inclusive, while @a rowMax is
        exclusive.
    */
    virtual void OnGetRowsHeightHint(size_t rowMin, size_t rowMax) const;

    /**
        This class forwards calls from EstimateTotalSize() to this function so
        derived classes can override either just the height or the width
        estimation, or just estimate both differently if desired in any
        wxHVScrolledWindow derived class.

        @note This function will not be called if EstimateTotalSize() is
              overridden in your derived class.
    */
    virtual wxCoord EstimateTotalHeight() const;

    /**
        This function must be overridden in the derived class, and should
        return the height of the given row in pixels.
    */
    virtual wxCoord OnGetRowHeight(size_t row) const = 0;
};



/**
    @class wxVarHScrollHelper

    This class provides functions wrapping the wxVarScrollHelperBase class,
    targeted for horizontal-specific scrolling.

    Like wxVarScrollHelperBase, this class is mostly only useful to those
    classes built into wxWidgets deriving from here, and this documentation is
    mostly only provided for referencing the functions provided by this class.
    You will likely want to derive your window from wxHScrolledWindow rather
    than from here directly.

    @library{wxcore}
    @category{miscwnd}

    @see wxHScrolledWindow, wxHVScrolledWindow, wxVScrolledWindow
*/
class wxVarHScrollHelper : public wxVarScrollHelperBase
{
public:
    /**
        Constructor taking the target window to be scrolled by this helper
        class. This will attach scroll event handlers to the target window to
        catch and handle scroll events appropriately.
    */
    wxVarHScrollHelper(wxWindow* winToScroll);

    /**
        Returns the number of columns the target window contains.

        @see SetColumnCount()
    */
    size_t GetColumnCount() const;

    /**
        Returns the index of the first visible column based on the scroll
        position.
    */
    size_t GetVisibleColumnsBegin() const;

    /**
        Returns the index of the last visible column based on the scroll
        position. This includes the last column even if it is only partially
        visible.
    */
    size_t GetVisibleColumnsEnd() const;

    /**
        Returns @true if the given column is currently visible (even if only
        partially visible) or @false otherwise.
    */
    bool IsColumnVisible(size_t column) const;

    /**
        Triggers a refresh for just the given column's area of the window if
        it's visible.
    */
    virtual void RefreshColumn(size_t column);

    /**
        Triggers a refresh for the area between the specified range of columns
        given (inclusively).
    */
    virtual void RefreshColumns(size_t from, size_t to);

    /**
        Scroll by the specified number of pages which may be positive (to
        scroll right) or negative (to scroll left).
    */
    virtual bool ScrollColumnPages(int pages);

    /**
        Scroll by the specified number of columns which may be positive (to
        scroll right) or negative (to scroll left).

        @return @true if the window was scrolled, @false otherwise (for
                example, if we're trying to scroll right but we are already
                showing the last column).
    */
    virtual bool ScrollColumns(int columns);

    /**
        Scroll to the specified column. It will become the first visible column
        in the window.

        @return @true if we scrolled the window, @false if nothing was done.
    */
    bool ScrollToColumn(size_t column);

    /**
        Set the number of columns the window contains. The derived class must
        provide the widths for all columns with indices up to the one given
        here in it's OnGetColumnWidth() implementation.

        @see GetColumnCount()
    */
    void SetColumnCount(size_t columnCount);

protected:

    /**
        This class forwards calls from EstimateTotalSize() to this function so
        derived classes can override either just the height or the width
        estimation, or just estimate both differently if desired in any
        wxHVScrolledWindow derived class.

        @note This function will not be called if EstimateTotalSize() is
              overridden in your derived class.
    */
    virtual wxCoord EstimateTotalWidth() const;

    /**
        This function doesn't have to be overridden but it may be useful to do
        so if calculating the columns' sizes is a relatively expensive
        operation as it gives your code a chance to calculate several of them
        at once and cache the result if necessary.

        OnGetColumnsWidthHint() is normally called just before
        OnGetColumnWidth() but you shouldn't rely on the latter being called
        for all columns in the interval specified here. It is also possible
        that OnGetColumnWidth() will be called for units outside of this
        interval, so this is really just a hint, not a promise.

        Finally, note that @a columnMin is inclusive, while @a columnMax is
        exclusive.
    */
    virtual void OnGetColumnsWidthHint(size_t columnMin,
                                       size_t columnMax) const;

    /**
        This function must be overridden in the derived class, and should
        return the width of the given column in pixels.
    */
    virtual wxCoord OnGetColumnWidth(size_t column) const = 0;
};



/**
    @class wxVarHVScrollHelper

    This class provides functions wrapping the wxVarHScrollHelper and
    wxVarVScrollHelper classes, targeted for scrolling a window in both axis.
    Since this class is also the join class of the horizontal and vertical
    scrolling functionality, it also addresses some wrappers that help avoid
    the need to specify class scope in your wxHVScrolledWindow derived class
    when using wxVarScrollHelperBase functionality.

    Like all three of it's scroll helper base classes, this class is mostly
    only useful to those classes built into wxWidgets deriving from here, and
    this documentation is mostly only provided for referencing the functions
    provided by this class. You will likely want to derive your window from
    wxHVScrolledWindow rather than from here directly.

    @library{wxcore}
    @category{miscwnd}

    @see wxHScrolledWindow, wxHVScrolledWindow, wxVScrolledWindow
*/
class wxVarHVScrollHelper : public wxVarVScrollHelper,
                             public wxVarHScrollHelper
{
public:
    /**
        Constructor taking the target window to be scrolled by this helper
        class. This will attach scroll event handlers to the target window to
        catch and handle scroll events appropriately.
    */
    wxVarHVScrollHelper(wxWindow* winToScroll);

    /**
        With physical scrolling on (when this is @true), the device origin is
        changed properly when a wxPaintDC is prepared, children are actually
        moved and laid out properly, and the contents of the window (pixels)
        are actually moved. When this is @false, you are responsible for
        repainting any invalidated areas of the window yourself to account for
        the new scroll position.

        @param vscrolling
            Specifies if physical scrolling should be turned on when scrolling
            vertically.
        @param hscrolling
            Specifies if physical scrolling should be turned on when scrolling
            horizontally.
    */
    void EnablePhysicalScrolling(bool vscrolling = true,
                                 bool hscrolling = true);

    /**
        Returns the number of columns and rows the target window contains.

        @see SetRowColumnCount()
    */
    wxSize GetRowColumnCount() const;

    /**
        Returns the index of the first visible column and row based on the
        current scroll position.
    */
    wxPosition GetVisibleBegin() const;

    /**
        Returns the index of the last visible column and row based on the
        scroll position. This includes any partially visible columns or rows.
    */
    wxPosition GetVisibleEnd() const;

    //@{
    /**
        Returns @true if both the given row and column are currently visible
        (even if only partially visible) or @false otherwise.
    */
    bool IsVisible(size_t row, size_t column) const;
    bool IsVisible(const wxPosition& pos) const;
    //@}

    //@{
    /**
        Triggers a refresh for just the area shared between the given row and
        column of the window if it is visible.
    */
    virtual void RefreshRowColumn(size_t row, size_t column);
    virtual void RefreshRowColumn(const wxPosition& pos);
    //@}

    //@{
    /**
        Triggers a refresh for the visible area shared between all given rows
        and columns (inclusive) of the window. If the target window for both
        orientations is the same, the rectangle of cells is refreshed; if the
        target windows differ, the entire client size opposite the orientation
        direction is refreshed between the specified limits.
    */
    virtual void RefreshRowsColumns(size_t fromRow, size_t toRow,
                                    size_t fromColumn, size_t toColumn);
    virtual void RefreshRowsColumns(const wxPosition& from,
                                    const wxPosition& to);
    //@}

    //@{
    /**
        Scroll to the specified row and column. It will become the first
        visible row and column in the window. Returns @true if we scrolled the
        window, @false if nothing was done.
    */
    bool ScrollToRowColumn(size_t row, size_t column);
    bool ScrollToRowColumn(const wxPosition& pos);
    //@}

    /**
        Set the number of rows and columns the target window will contain. The
        derived class must provide the sizes for all rows and columns with
        indices up to the ones given here in it's OnGetRowHeight() and
        OnGetColumnWidth() implementations, respectively.

        @see GetRowColumnCount()
    */
    void SetRowColumnCount(size_t rowCount, size_t columnCount);

    //@{
    /**
        Returns the virtual scroll unit under the device unit given accounting
        for scroll position or @c wxNOT_FOUND (for the row, column, or possibly
        both values) if none.
    */
    wxPosition VirtualHitTest(wxCoord x, wxCoord y) const;
    wxPosition VirtualHitTest(const wxPoint& pos) const;
    //@}
};



/**
    @class wxVScrolledWindow

    In the name of this class, "V" may stand for "variable" because it can be
    used for scrolling rows of variable heights; "virtual", because it is not
    necessary to know the heights of all rows in advance -- only those which
    are shown on the screen need to be measured; or even "vertical", because
    this class only supports scrolling vertically.

    In any case, this is a generalization of wxScrolled which can be only used
    when all rows have the same heights. It lacks some other wxScrolled
    features however, notably it can't scroll specific pixel sizes of the
    window or its exact client area size.

    To use this class, you need to derive from it and implement the
    OnGetRowHeight() pure virtual method. You also must call SetRowCount() to
    let the base class know how many rows it should display, but from that
    moment on the scrolling is handled entirely by wxVScrolledWindow. You only
    need to draw the visible part of contents in your @c OnPaint() method as
    usual. You should use GetVisibleRowsBegin() and GetVisibleRowsEnd() to
    select the lines to display. Note that the device context origin is not
    shifted so the first visible row always appears at the point (0, 0) in
    physical as well as logical coordinates.

    @section vscrolledwindow_compat wxWidgets 2.8 Compatibility Functions

    The following functions provide backwards compatibility for applications
    originally built using wxVScrolledWindow in 2.6 or 2.8. Originally,
    wxVScrolledWindow referred to scrolling "lines". We now use "units" in
    wxVarScrollHelperBase to avoid implying any orientation (since the
    functions are used for both horizontal and vertical scrolling in derived
    classes). And in the new wxVScrolledWindow and wxHScrolledWindow classes,
    we refer to them as "rows" and "columns", respectively. This is to help
    clear some confusion in not only those classes, but also in
    wxHVScrolledWindow where functions are inherited from both.

    You are encouraged to update any existing code using these function to use
    the new replacements mentioned below, and avoid using these functions for
    any new code as they are deprecated.

    @beginTable
    @row2col{ <tt>size_t %GetFirstVisibleLine() const</tt>,
        Deprecated for GetVisibleRowsBegin(). }
    @row2col{ <tt>size_t %GetLastVisibleLine() const</tt>,
        Deprecated for GetVisibleRowsEnd(). This function originally had a
        slight design flaw in that it was possible to return
        <tt>(size_t)-1</tt> (ie: a large positive number) if the scroll
        position was 0 and the first line wasn't completely visible. }
    @row2col{ <tt>size_t %GetLineCount() const</tt>,
        Deprecated for GetRowCount(). }
    @row2col{ <tt>int %HitTest(wxCoord x\, wxCoord y) const
              @n  int %HitTest(const wxPoint& pt) const</tt>,
        Deprecated for VirtualHitTest(). }
    @row2col{ <tt>virtual wxCoord %OnGetLineHeight(size_t line) const</tt>,
        Deprecated for OnGetRowHeight(). }
    @row2col{ <tt>virtual void %OnGetLinesHint(size_t lineMin\, size_t lineMax) const</tt>,
        Deprecated for OnGetRowsHeightHint(). }
    @row2col{ <tt>virtual void %RefreshLine(size_t line)</tt>,
        Deprecated for RefreshRow(). }
    @row2col{ <tt>virtual void %RefreshLines(size_t from\, size_t to)</tt>,
        Deprecated for RefreshRows(). }
    @row2col{ <tt>virtual bool %ScrollLines(int lines)</tt>,
        Deprecated for ScrollRows(). }
    @row2col{ <tt>virtual bool %ScrollPages(int pages)</tt>,
        Deprecated for ScrollRowPages(). }
    @row2col{ <tt>bool %ScrollToLine(size_t line)</tt>,
        Deprecated for ScrollToRow(). }
    @row2col{ <tt>void %SetLineCount(size_t count)</tt>,
        Deprecated for SetRowCount(). }
    @endTable

    @library{wxcore}
    @category{miscwnd}

    @see wxHScrolledWindow, wxHVScrolledWindow
*/
class wxVScrolledWindow : public wxPanel, public wxVarVScrollHelper
{
public:
    /**
        Default constructor, you must call Create() later.
    */
    wxVScrolledWindow();
    /**
        This is the normal constructor, no need to call Create() after using
        this constructor.

        @note @c wxVSCROLL is always automatically added to the style, there is
              no need to specify it explicitly.

        @param parent
            The parent window, must not be @NULL.
        @param id
            The identifier of this window, wxID_ANY by default.
        @param pos
            The initial window position.
        @param size
            The initial window size.
        @param style
            The window style. There are no special style bits defined for this
            class.
        @param name
            The name for this window; usually not used.
    */
    wxVScrolledWindow(wxWindow* parent, wxWindowID id = wxID_ANY,
                      const wxPoint& pos = wxDefaultPosition,
                      const wxSize& size = wxDefaultSize, long style = 0,
                      const wxString& name = wxPanelNameStr);

    /**
        Same as the non-default constructor, but returns a status code: @true if
        ok, @false if the window couldn't be created.

        Just as with the constructor, the @c wxVSCROLL style is always used,
        there is no need to specify it explicitly.
    */
    bool Create(wxWindow* parent, wxWindowID id = wxID_ANY,
                const wxPoint& pos = wxDefaultPosition,
                const wxSize& size = wxDefaultSize, long style = 0,
                const wxString& name = wxPanelNameStr);
};



/**
    @class wxHScrolledWindow

    In the name of this class, "H" stands for "horizontal" because it can be
    used for scrolling columns of variable widths. It is not necessary to know
    the widths of all columns in advance -- only those which are shown on the
    screen need to be measured.

    In any case, this is a generalization of wxScrolled which can be only used
    when all columns have the same widths. It lacks some other wxScrolled
    features however, notably it can't scroll specific pixel sizes of the
    window or its exact client area size.

    To use this class, you need to derive from it and implement the
    OnGetColumnWidth() pure virtual method. You also must call SetColumnCount()
    to let the base class know how many columns it should display, but from
    that moment on the scrolling is handled entirely by wxHScrolledWindow. You
    only need to draw the visible part of contents in your @c OnPaint() method
    as usual. You should use GetVisibleColumnsBegin() and
    GetVisibleColumnsEnd() to select the lines to display. Note that the device
    context origin is not shifted so the first visible column always appears at
    the point (0, 0) in physical as well as logical coordinates.

    @library{wxcore}
    @category{miscwnd}

    @see wxHVScrolledWindow, wxVScrolledWindow
*/
class wxHScrolledWindow : public wxPanel, public wxVarHScrollHelper
{
public:
    /**
        Default constructor, you must call Create() later.
    */
    wxHScrolledWindow();
    /**
        This is the normal constructor, no need to call Create() after using
        this constructor.

        @note @c wxHSCROLL is always automatically added to the style, there is
              no need to specify it explicitly.

        @param parent
            The parent window, must not be @NULL.
        @param id
            The identifier of this window, wxID_ANY by default.
        @param pos
            The initial window position.
        @param size
            The initial window size.
        @param style
            The window style. There are no special style bits defined for this
            class.
        @param name
            The name for this window; usually not used.
    */
    wxHScrolledWindow(wxWindow* parent, wxWindowID id = wxID_ANY,
                      const wxPoint& pos = wxDefaultPosition,
                      const wxSize& size = wxDefaultSize, long style = 0,
                      const wxString& name = wxPanelNameStr);

    /**
        Same as the non-default constructor, but returns a status code: @true if
        ok, @false if the window couldn't be created.

        Just as with the constructor, the @c wxHSCROLL style is always used,
        there is no need to specify it explicitly.
    */
    bool Create(wxWindow* parent, wxWindowID id = wxID_ANY,
                const wxPoint& pos = wxDefaultPosition,
                const wxSize& size = wxDefaultSize, long style = 0,
                const wxString& name = wxPanelNameStr);
};



/**
    @class wxHVScrolledWindow

    This window inherits all functionality of both vertical and horizontal,
    variable scrolled windows. It automatically handles everything needed to
    scroll both axis simultaneously with both variable row heights and variable
    column widths.

    In any case, this is a generalization of wxScrolled which can be only used
    when all rows and columns are the same size. It lacks some other wxScrolled
    features however, notably it can't scroll specific pixel sizes of the
    window or its exact client area size.

    To use this class, you must derive from it and implement both the
    OnGetRowHeight() and OnGetColumnWidth() pure virtual methods to let the
    base class know how many rows and columns it should display. You also need
    to set the total rows and columns the window contains, but from that moment
    on the scrolling is handled entirely by wxHVScrolledWindow. You only need
    to draw the visible part of contents in your @c OnPaint() method as usual.
    You should use GetVisibleBegin() and GetVisibleEnd() to select the lines to
    display. Note that the device context origin is not shifted so the first
    visible row and column always appear at the point (0, 0) in physical as
    well as logical coordinates.

    @library{wxcore}
    @category{miscwnd}

    @see wxHScrolledWindow, wxVScrolledWindow
*/
class wxHVScrolledWindow : public wxPanel, public wxVarHVScrollHelper
{
public:
    /**
        Default constructor, you must call Create() later.
    */
    wxHVScrolledWindow();
    /**
        This is the normal constructor, no need to call Create() after using
        this constructor.

        @note @c wxHSCROLL and @c wxVSCROLL are always automatically added to
              the style, there is no need to specify it explicitly.

        @param parent
            The parent window, must not be @NULL.
        @param id
            The identifier of this window, wxID_ANY by default.
        @param pos
            The initial window position.
        @param size
            The initial window size.
        @param style
            The window style. There are no special style bits defined for this
            class.
        @param name
            The name for this window; usually not used.
    */
    wxHVScrolledWindow(wxWindow* parent, wxWindowID id = wxID_ANY,
                       const wxPoint& pos = wxDefaultPosition,
                       const wxSize& size = wxDefaultSize, long style = 0,
                       const wxString& name = wxPanelNameStr);

    /**
        Same as the non-default constructor, but returns a status code: @true if
        ok, @false if the window couldn't be created.

        Just as with the constructor, the @c wxHSCROLL and @c wxVSCROLL styles
        are always used, there is no need to specify them explicitly.
    */
    bool Create(wxWindow* parent, wxWindowID id = wxID_ANY,
                const wxPoint& pos = wxDefaultPosition,
                const wxSize& size = wxDefaultSize, long style = 0,
                const wxString& name = wxPanelNameStr);
};

