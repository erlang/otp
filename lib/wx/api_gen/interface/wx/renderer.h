/////////////////////////////////////////////////////////////////////////////
// Name:        renderer.h
// Purpose:     interface of wxRendererNative
// Author:      wxWidgets team
// Licence:     wxWindows licence
/////////////////////////////////////////////////////////////////////////////

/**
    @anchor wxCONTROL_FLAGS

    The following rendering flags are defined for wxRendererNative:
*/
enum
{
    /**
        Default state, no special flags.

        @since 3.1.0
     */
    wxCONTROL_NONE       = 0x00000000,

    /** Control is disabled. */
    wxCONTROL_DISABLED   = 0x00000001,

    /** Currently has keyboard focus. */
    wxCONTROL_FOCUSED    = 0x00000002,

    /** (Button) is pressed. */
    wxCONTROL_PRESSED    = 0x00000004,

    /** Control-specific bit. */
    wxCONTROL_SPECIAL    = 0x00000008,

    /** Only for the buttons. */
    wxCONTROL_ISDEFAULT  = wxCONTROL_SPECIAL,

    /** Only for the menu items. */
    wxCONTROL_ISSUBMENU  = wxCONTROL_SPECIAL,

    /** Only for the tree items and collapse buttons. */
    wxCONTROL_EXPANDED   = wxCONTROL_SPECIAL,

    /** Only for the status bar panes. */
    wxCONTROL_SIZEGRIP   = wxCONTROL_SPECIAL,

    /** Checkboxes only: flat border. */
    wxCONTROL_FLAT       = wxCONTROL_SPECIAL,

    /** Item selection rect only: cell inside selection. */
    wxCONTROL_CELL       = wxCONTROL_SPECIAL,

    /** Mouse is currently over the control. */
    wxCONTROL_CURRENT    = 0x00000010,

    /** Selected item in e.g.\ listbox. */
    wxCONTROL_SELECTED   = 0x00000020,

    /** (Check/radio button) is checked. */
    wxCONTROL_CHECKED    = 0x00000040,

    /** (Menu) item can be checked. */
    wxCONTROL_CHECKABLE  = 0x00000080,

    /** (Check) undetermined state. */
    wxCONTROL_UNDETERMINED = wxCONTROL_CHECKABLE
};

/**
    Title bar buttons supported by wxRendererNative::DrawTitleBarBitmap().
 */
enum wxTitleBarButton
{
    wxTITLEBAR_BUTTON_CLOSE    = 0x01000000,
    wxTITLEBAR_BUTTON_MAXIMIZE = 0x02000000,
    wxTITLEBAR_BUTTON_ICONIZE  = 0x04000000,
    wxTITLEBAR_BUTTON_RESTORE  = 0x08000000,
    wxTITLEBAR_BUTTON_HELP     = 0x10000000
};

/**
    @struct wxSplitterRenderParams

    This is just a simple @c struct used as a return value of
    wxRendererNative::GetSplitterParams().

    It doesn't have any methods and all of its fields are constant, so they can
    only be examined but not modified.

    @library{wxcore}
    @category{gdi}
*/
struct wxSplitterRenderParams
{
    /**
        The only way to initialize this struct is by using this ctor.
    */
    wxSplitterRenderParams(wxCoord widthSash_, wxCoord border_, bool isSens_);

    /**
        The width of the border drawn by the splitter inside it, may be 0.
    */
    const wxCoord border;

    /**
        @true if the sash changes appearance when the mouse passes over it, @false
        otherwise.
    */
    const bool isHotSensitive;

    /**
        The width of the splitter sash.
    */
    const wxCoord widthSash;
};

/**
    @struct wxHeaderButtonParams

    This @c struct can optionally be used with
    wxRendererNative::DrawHeaderButton() to specify custom values used to draw
    the text or bitmap label.

    @library{wxcore}
    @category{gdi}
*/
struct wxHeaderButtonParams
{
    wxHeaderButtonParams();

    wxColour    m_arrowColour;
    wxColour    m_selectionColour;
    wxString    m_labelText;
    wxFont      m_labelFont;
    wxColour    m_labelColour;
    wxBitmap    m_labelBitmap;
    int         m_labelAlignment;
};

/**
    Used to specify the type of sort arrow used with
    wxRendererNative::DrawHeaderButton().
*/
enum wxHeaderSortIconType
{
    wxHDR_SORT_ICON_NONE,    ///< Don't draw a sort arrow.
    wxHDR_SORT_ICON_UP,      ///< Draw a sort arrow icon pointing up.
    wxHDR_SORT_ICON_DOWN     ///< Draw a sort arrow icon pointing down.
};



/**
    @class wxDelegateRendererNative

    wxDelegateRendererNative allows reuse of renderers code by forwarding all the
    wxRendererNative methods to the given object and
    thus allowing you to only modify some of its methods -- without having to
    reimplement all of them.

    Note that the "normal", inheritance-based approach, doesn't work with the
    renderers as it is impossible to derive from a class unknown at compile-time
    and the renderer is only chosen at run-time. So suppose that you want to only
    add something to the drawing of the tree control buttons but leave all the
    other methods unchanged -- the only way to do it, considering that the renderer
    class which you want to customize might not even be written yet when you write
    your code (it could be written later and loaded from a DLL during run-time), is
    by using this class.

    Except for the constructor, it has exactly the same methods as
    wxRendererNative and their implementation is
    trivial: they are simply forwarded to the real renderer. Note that the "real"
    renderer may, in turn, be a wxDelegateRendererNative as well and that there may
    be arbitrarily many levels like this -- but at the end of the chain there must
    be a real renderer which does the drawing.

    @library{wxcore}
    @category{gdi}

    @see wxRendererNative
*/
class wxDelegateRendererNative : public wxRendererNative
{
public:
    /**
        The default constructor does the same thing as the other one except that it
        uses the @ref wxRendererNative::GetGeneric() "generic renderer" instead of the
        user-specified @a rendererNative.

        In any case, this sets up the delegate renderer object to follow all calls to
        the specified real renderer.
    */
    wxDelegateRendererNative();
    /**
        This constructor uses the user-specified @a rendererNative to set up the delegate
        renderer object to follow all calls to the specified real renderer.

        @note
        This object does not take ownership of (i.e. won't delete) @a rendererNative.
    */
    wxDelegateRendererNative(wxRendererNative& rendererNative);

    // The rest of these functions inherit the documentation from wxRendererNative

    virtual int DrawHeaderButton(wxWindow *win, wxDC& dc,
                                 const wxRect& rect, int flags = 0,
                                 wxHeaderSortIconType sortArrow = wxHDR_SORT_ICON_NONE,
                                 wxHeaderButtonParams* params = NULL);

    virtual int DrawHeaderButtonContents(wxWindow *win, wxDC& dc,
                                         const wxRect& rect, int flags = 0,
                                         wxHeaderSortIconType sortArrow = wxHDR_SORT_ICON_NONE,
                                         wxHeaderButtonParams* params = NULL);

    virtual int GetHeaderButtonHeight(wxWindow *win);

    virtual int GetHeaderButtonMargin(wxWindow *win);

    virtual void DrawTreeItemButton(wxWindow *win, wxDC& dc,
                                    const wxRect& rect, int flags = 0);

    virtual void DrawSplitterBorder(wxWindow *win, wxDC& dc,
                                    const wxRect& rect, int flags = 0);

    virtual void DrawSplitterSash(wxWindow *win, wxDC& dc,
                                  const wxSize& size, wxCoord position,
                                  wxOrientation orient, int flags = 0);

    virtual void DrawComboBoxDropButton(wxWindow *win, wxDC& dc,
                                        const wxRect& rect, int flags = 0);

    virtual void DrawDropArrow(wxWindow *win, wxDC& dc,
                               const wxRect& rect, int flags = 0);

    virtual void DrawCheckBox(wxWindow *win, wxDC& dc,
                              const wxRect& rect, int flags = 0 );

    virtual void DrawCheckMark(wxWindow *win, wxDC& dc,
                               const wxRect& rect, int flags = 0 );

    virtual wxSize GetCheckBoxSize(wxWindow *win, int flags = 0);

    virtual wxSize GetCheckMarkSize(wxWindow *win);

    virtual wxSize GetExpanderSize(wxWindow* win);

    virtual void DrawPushButton(wxWindow *win, wxDC& dc,
                                const wxRect& rect, int flags = 0 );

    virtual void DrawItemSelectionRect(wxWindow *win, wxDC& dc,
                                       const wxRect& rect, int flags = 0 );

    virtual void DrawFocusRect(wxWindow* win, wxDC& dc,
                               const wxRect& rect, int flags = 0);

    virtual wxSplitterRenderParams GetSplitterParams(const wxWindow *win);

    virtual wxRendererVersion GetVersion() const;
};



/**
    @class wxRendererNative

    First, a brief introduction to wxRendererNative and why it is needed.

    Usually wxWidgets uses the underlying low level GUI system to draw all the
    controls - this is what we mean when we say that it is a "native" framework.
    However not all controls exist under all (or even any) platforms and in this
    case wxWidgets provides a default, generic, implementation of them written in
    wxWidgets itself.

    These controls don't have the native appearance if only the standard
    line drawing and other graphics primitives are used, because the native
    appearance is different under different platforms while the lines are always
    drawn in the same way.

    This is why we have renderers: wxRendererNative is a class which virtualizes the
    drawing, i.e. it abstracts the drawing operations and allows you to draw say, a
    button, without caring about exactly how this is done. Of course, as we
    can draw the button differently in different renderers, this also allows us to
    emulate the native look and feel.

    So the renderers work by exposing a large set of high-level drawing functions
    which are used by the generic controls. There is always a default global
    renderer but it may be changed or extended by the user, see
    @ref page_samples_render.

    All drawing functions take some standard parameters:

    @li @a win - The window being drawn. It is normally not used and when
    it is it should only be used as a generic wxWindow
    (in order to get its low level handle, for example), but you should
    not assume that it is of some given type as the same renderer
    function may be reused for drawing different kinds of control.
    @li @a dc - The wxDC to draw on. Only this device
    context should be used for drawing. It is not necessary to restore
    pens and brushes for it on function exit but, on the other hand, you
    shouldn't assume that it is in any specific state on function entry:
    the rendering functions should always prepare it.
    @li @a rect - The bounding rectangle for the element to be drawn.
    @li @a flags - The optional flags (none by default) which can be a
    combination of the @ref wxCONTROL_FLAGS.

    Note that each drawing function restores the wxDC attributes if
    it changes them, so it is safe to assume that the same pen, brush and colours
    that were active before the call to this function are still in effect after it.

    @library{wxcore}
    @category{gdi}
*/
class wxRendererNative
{
public:
    /**
        Virtual destructor as for any base class.
    */
    virtual ~wxRendererNative();

    /**
        Draw a check box.

        @a flags may have the @c wxCONTROL_CHECKED, @c wxCONTROL_CURRENT or
        @c wxCONTROL_UNDETERMINED bit set, see @ref wxCONTROL_FLAGS.
    */
    virtual void DrawCheckBox(wxWindow* win, wxDC& dc, const wxRect& rect,
                              int flags = 0) = 0;

    /**
        Draw a button like the one used by wxComboBox to show a
        drop down window. The usual appearance is a downwards pointing arrow.

        @a flags may have the @c wxCONTROL_PRESSED or @c wxCONTROL_CURRENT bit set,
        see @ref wxCONTROL_FLAGS.
    */
    virtual void DrawComboBoxDropButton(wxWindow* win, wxDC& dc,
                                        const wxRect& rect, int flags = 0) = 0;

    /**
        Draw a drop down arrow that is suitable for use outside a combo box. Arrow will
        have transparent background.

        @a rect is not entirely filled by the arrow. Instead, you should use bounding
        rectangle of a drop down button which arrow matches the size you need.

        @a flags may have the @c wxCONTROL_PRESSED or @c wxCONTROL_CURRENT bit set,
        see @ref wxCONTROL_FLAGS.
    */
    virtual void DrawDropArrow(wxWindow* win, wxDC& dc, const wxRect& rect,
                               int flags = 0) = 0;

    /**
        Draw a focus rectangle using the specified rectangle.
        wxListCtrl.

        The only supported flags is @c wxCONTROL_SELECTED for items which are selected.
        see @ref wxCONTROL_FLAGS.
    */
    virtual void DrawFocusRect(wxWindow* win, wxDC& dc, const wxRect& rect,
                               int flags = 0) = 0;

    /**
        Draw a progress bar in the specified rectangle.

        The @a value and @a max arguments determine the part of the progress
        bar that is drawn as being filled in, @a max must be strictly positive
        and @a value must be between 0 and @a max.

        @c wxCONTROL_SPECIAL must be set in @a flags for the vertical gauges.

        @since 3.1.0
     */
    virtual void DrawGauge(wxWindow* win,
                           wxDC& dc,
                           const wxRect& rect,
                           int value,
                           int max,
                           int flags = 0) = 0;

    /**
        Draw the header control button (used, for example, by wxListCtrl).

        Depending on platforms the @a flags parameter may support the @c wxCONTROL_SELECTED
        @c wxCONTROL_DISABLED and @c wxCONTROL_CURRENT bits, see @ref wxCONTROL_FLAGS.

        @return
        The optimal width to contain the unabbreviated label text or
        bitmap, the sort arrow if present, and internal margins.
    */
    virtual int DrawHeaderButton(wxWindow* win, wxDC& dc, const wxRect& rect,
                                 int flags = 0,
                                 wxHeaderSortIconType sortArrow = wxHDR_SORT_ICON_NONE, wxHeaderButtonParams* params = NULL) = 0;

    /**
        Draw the contents of a header control button (label, sort arrows,
        etc.). This function is normally only called by DrawHeaderButton().

        Depending on platforms the @a flags parameter may support the @c wxCONTROL_SELECTED
        @c wxCONTROL_DISABLED and @c wxCONTROL_CURRENT bits, see @ref wxCONTROL_FLAGS.

        @return
        The optimal width to contain the unabbreviated label text or
        bitmap, the sort arrow if present, and internal margins.
    */
    virtual int DrawHeaderButtonContents(wxWindow* win, wxDC& dc,
                                         const wxRect& rect, int flags = 0,
                                         wxHeaderSortIconType sortArrow = wxHDR_SORT_ICON_NONE, wxHeaderButtonParams* params = NULL) = 0;

    /**
        Draw a selection rectangle underneath the text as used e.g. in a
        wxListCtrl.

        The supported @a flags are @c wxCONTROL_SELECTED for items
        which are selected (e.g. often a blue rectangle) and @c wxCONTROL_CURRENT
        for the item that has the focus (often a dotted line around the item's text).
        @c wxCONTROL_FOCUSED may be used to indicate if the control has the focus
        (otherwise the selection rectangle is e.g. often grey and not blue).
        This may be ignored by the renderer or deduced by the code directly from
        the @a win. Additionally @c wxCONTROL_CELL may be used to draw a cell inside
        a bigger selection area.

        @see DrawItemText()
    */
    virtual void DrawItemSelectionRect(wxWindow* win, wxDC& dc,
                                       const wxRect& rect, int flags = 0) = 0;


    /**
        Draw item text in the correct color based on selection status.

        Background of the text should be painted with DrawItemSelectionRect().

        The supported @a flags are @c wxCONTROL_SELECTED for items
        which are selected.
        @c wxCONTROL_FOCUSED may be used to indicate if the control has the focus.
        @c wxCONTROL_DISABLED may be used to indicate if the control is disabled.

        @since 3.1.0
        @see DrawItemSelectionRect()
    */
    virtual void DrawItemText(wxWindow* win,
                              wxDC& dc,
                              const wxString& text,
                              const wxRect& rect,
                              int align = wxALIGN_LEFT | wxALIGN_TOP,
                              int flags = 0,
                              wxEllipsizeMode ellipsizeMode = wxELLIPSIZE_END) = 0;

    /**
        Draw a blank push button that looks very similar to wxButton.

        @a flags may have the @c wxCONTROL_PRESSED, @c wxCONTROL_CURRENT or
        @c wxCONTROL_ISDEFAULT bit set, see @ref wxCONTROL_FLAGS.
    */
    virtual void DrawPushButton(wxWindow* win, wxDC& dc, const wxRect& rect,
                                int flags = 0) = 0;

    /**
        Draw a collapse button.

        @a flags may have the @c wxCONTROL_EXPANDED or @c wxCONTROL_CURRENT
        bit set, see @ref wxCONTROL_FLAGS.

        @since 3.1.0
    */
    virtual void DrawCollapseButton(wxWindow *win, wxDC& dc,
                                    const wxRect& rect, int flags = 0) = 0;

    /**
        Returns the size of a collapse button.

        @since 3.1.0
    */
    virtual wxSize GetCollapseButtonSize(wxWindow *win, wxDC& dc) = 0;

    /**
        Draw the border for sash window: this border must be such that the sash
        drawn by DrawSplitterSash() blends into it well.
    */
    virtual void DrawSplitterBorder(wxWindow* win, wxDC& dc, const wxRect& rect,
                                    int flags = 0) = 0;

    /**
        Draw a sash. The @a orient parameter defines whether the sash should be
        vertical or horizontal and how the @a position should be interpreted.
    */
    virtual void DrawSplitterSash(wxWindow* win, wxDC& dc, const wxSize& size,
                                  wxCoord position, wxOrientation orient,
                                  int flags = 0) = 0;

    /**
        Draw the expanded/collapsed icon for a tree control item.

        To draw an expanded button the @a flags parameter must contain @c wxCONTROL_EXPANDED bit,
        see @ref wxCONTROL_FLAGS.
    */
    virtual void DrawTreeItemButton(wxWindow* win, wxDC& dc, const wxRect& rect,
                                    int flags = 0) = 0;

    /**
        Draw a native wxChoice
    */
    virtual void DrawChoice(wxWindow* win, wxDC& dc, const wxRect& rect, int flags = 0) = 0;

    /**
        Draw a native wxComboBox
    */
    virtual void DrawComboBox(wxWindow* win, wxDC& dc, const wxRect& rect, int flags = 0) = 0;

    /**
        Draw a native wxTextCtrl frame
    */
    virtual void DrawTextCtrl(wxWindow* win, wxDC& dc, const wxRect& rect, int flags = 0) = 0;

    /**
        Draw a native wxRadioButton bitmap.
    */
    virtual void DrawRadioBitmap(wxWindow* win, wxDC& dc, const wxRect& rect, int flags = 0) = 0;

    /**
        Draw a title bar button in the given state.

        This function is currently only available under MSW and macOS (and only
        for wxTITLEBAR_BUTTON_CLOSE under the latter), its best replacement for
        the other platforms is to use wxArtProvider to retrieve the bitmaps for
        @c wxART_HELP and @c wxART_CLOSE (but not any other title bar buttons
        and not for any state but normal, i.e. not pressed and not current one).

        The presence of this function is indicated by @c
        wxHAS_DRAW_TITLE_BAR_BITMAP symbol being defined.

        @since 2.9.1
     */
    virtual void DrawTitleBarBitmap(wxWindow *win,
                                    wxDC& dc,
                                    const wxRect& rect,
                                    wxTitleBarButton button,
                                    int flags = 0) = 0;

    /**
        Draw a check mark.

        @a flags may have the @c wxCONTROL_DISABLED bit set, see
        @ref wxCONTROL_FLAGS.

        @since 3.1.3
    */
    virtual void DrawCheckMark(wxWindow* win, wxDC& dc, const wxRect& rect,
                               int flags = 0) = 0;

    /**
        Return the currently used renderer.
    */
    static wxRendererNative& Get();

    /**
        Return the default (native) implementation for this platform -- this is also
        the one used by default but this may be changed by calling
        Set() in which case the return value of this
        method may be different from the return value of Get().
    */
    static wxRendererNative& GetDefault();

    /**
        Return the generic implementation of the renderer. Under some platforms, this
        is the default renderer implementation, others have platform-specific default
        renderer which can be retrieved by calling GetDefault().
    */
    static wxRendererNative& GetGeneric();

    /**
        Returns the size of a check box.

        @param win A valid, i.e. non-null, window pointer which is used to get
            the theme defining the checkbox size under some platforms.

        @param flags The only acceptable flag is @c wxCONTROL_CELL which means
            that just the size of the checkbox itself is returned, without any
            margins that are included by default. This parameter is only
            available in wxWidgets 3.1.4 or later.
    */
    virtual wxSize GetCheckBoxSize(wxWindow* win, int flags = 0) = 0;

    /**
        Returns the size of a check mark.

        @param win A valid, i.e. non-null, window pointer which is used to get
            the theme defining the checkmark size under some platforms.

        @since 3.1.3
    */
    virtual wxSize GetCheckMarkSize(wxWindow* win) = 0;

    /**
        Returns the size of the expander used in tree-like controls.

        @param win A valid, i.e. non-null, window pointer which is used to get
            the theme defining the expander size under some platforms.

        @since 3.1.3
     */
    virtual wxSize GetExpanderSize(wxWindow* win) = 0;

    /**
        Returns the height of a header button, either a fixed platform height if
        available, or a generic height based on the @a win window's font.
    */
    virtual int GetHeaderButtonHeight(wxWindow* win) = 0;

    /**
        Returns the horizontal margin on the left and right sides of header
        button's label.

        @since 2.9.2
     */
    virtual int GetHeaderButtonMargin(wxWindow *win) = 0;

    /**
        Get the splitter parameters, see wxSplitterRenderParams.
        The @a win parameter should be a wxSplitterWindow.
    */
    virtual wxSplitterRenderParams GetSplitterParams(const wxWindow* win) = 0;

    /**
        This function is used for version checking: Load()
        refuses to load any shared libraries implementing an older or incompatible
        version.

        @remarks
        The implementation of this method is always the same in all renderers (simply
        construct wxRendererVersion using the @c wxRendererVersion::Current_XXX values),
        but it has to be in the derived, not base, class, to detect mismatches between
        the renderers versions and so you have to implement it anew in all renderers.
    */
    virtual wxRendererVersion GetVersion() const = 0;

    /**
        Load the renderer from the specified DLL, the returned pointer must be
        deleted by caller if not @NULL when it is not used any more.

        The @a name should be just the base name of the renderer and not the full
        name of the DLL file which is constructed differently (using
        wxDynamicLibrary::CanonicalizePluginName())
        on different systems.
    */
    static wxRendererNative* Load(const wxString& name);

    /**
        Set the renderer to use, passing @NULL reverts to using the default
        renderer (the global renderer must always exist).

        Return the previous renderer used with Set() or @NULL if none.
    */
    static wxRendererNative* Set(wxRendererNative* renderer);
};



/**
    @struct wxRendererVersion

    This simple struct represents the wxRendererNative
    interface version and is only used as the return value of
    wxRendererNative::GetVersion().

    The version has two components: the version itself and the age. If the main
    program and the renderer have different versions they are never compatible with
    each other because the version is only changed when an existing virtual
    function is modified or removed. The age, on the other hand, is incremented
    each time a new virtual method is added and so, at least for the compilers
    using a common C++ object model, the calling program is compatible with any
    renderer which has the age greater or equal to its age. This verification is
    done by IsCompatible() method.

    @library{wxcore}
    @category{gdi}
*/
struct wxRendererVersion
{
    wxRendererVersion(int version_, int age_);

    /**
        Checks if the main program is compatible with the renderer having the version
        @e ver, returns @true if it is and @false otherwise.

        This method is used by wxRendererNative::Load() to determine whether a
        renderer can be used.
    */
    static bool IsCompatible(const wxRendererVersion& ver);

    /**
        The age component.
    */
    const int age;

    /**
        The version component.
    */
    const int version;
};

