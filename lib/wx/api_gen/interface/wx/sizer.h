/////////////////////////////////////////////////////////////////////////////
// Name:        sizer.h
// Purpose:     interface of wxStdDialogButtonSizer
// Author:      wxWidgets team
// Licence:     wxWindows licence
/////////////////////////////////////////////////////////////////////////////


/**
    @class wxSizer

    wxSizer is the abstract base class used for laying out subwindows in a window.
    You cannot use wxSizer directly; instead, you will have to use one of the sizer
    classes derived from it. Currently there are wxBoxSizer, wxStaticBoxSizer,
    wxGridSizer, wxFlexGridSizer, wxWrapSizer and wxGridBagSizer.

    The layout algorithm used by sizers in wxWidgets is closely related to layout
    in other GUI toolkits, such as Java's AWT, the GTK toolkit or the Qt toolkit.
    It is based upon the idea of the individual subwindows reporting their minimal
    required size and their ability to get stretched if the size of the parent window
    has changed.

    This will most often mean that the programmer does not set the original size of
    a dialog in the beginning, rather the dialog will be assigned a sizer and this
    sizer will be queried about the recommended size. The sizer in turn will query
    its children, which can be normal windows, empty space or other sizers, so that
    a hierarchy of sizers can be constructed. Note that wxSizer does not derive
    from wxWindow and thus does not interfere with tab ordering and requires very little
    resources compared to a real window on screen.

    What makes sizers so well fitted for use in wxWidgets is the fact that every
    control reports its own minimal size and the algorithm can handle differences in
    font sizes or different window (dialog item) sizes on different platforms without
    problems. If e.g. the standard font as well as the overall design of Motif widgets
    requires more space than on Windows, the initial dialog size will automatically
    be bigger on Motif than on Windows.

    Sizers may also be used to control the layout of custom drawn items on the
    window. The wxSizer::Add(), wxSizer::Insert(), and wxSizer::Prepend() functions
    return a pointer to the newly added wxSizerItem.
    Just add empty space of the desired size and attributes, and then use the
    wxSizerItem::GetRect() method to determine where the drawing operations
    should take place.

    Please notice that sizers, like child windows, are owned by the library and
    will be deleted by it which implies that they must be allocated on the heap.
    However if you create a sizer and do not add it to another sizer or
    window, the library wouldn't be able to delete such an orphan sizer and in
    this, and only this, case it should be deleted explicitly.

    @section wxsizer_flags wxSizer flags

    The "flag" argument accepted by wxSizerItem constructors and other
    functions, e.g. wxSizer::Add(), is an OR-combination of the following flags.
    Two main behaviours are defined using these flags. One is the border around
    a window: the border parameter determines the border width whereas the
    flags given here determine which side(s) of the item that the border will
    be added.  The other flags determine how the sizer item behaves when the
    space allotted to the sizer changes, and is somewhat dependent on the
    specific kind of sizer used.

    @beginDefList
    @itemdef{wxTOP<br>
             wxBOTTOM<br>
             wxLEFT<br>
             wxRIGHT<br>
             wxALL,
             These flags are used to specify which side(s) of the sizer item
             the border width will apply to.}
    @itemdef{wxEXPAND,
             The item will be expanded to fill the space assigned to the item.
             When used for the items of wxBoxSizer, the expansion only happens
             in the transversal direction of the sizer as only the item
             proportion governs its behaviour in the principal sizer direction.
             But when it is used for the items of wxGridSizer, this flag can be
             combined with the alignment flags which override it in the
             corresponding direction if specified, e.g. @c wxEXPAND |
             wxALIGN_CENTRE_VERTICAL would expand the item only horizontally
             but center it vertically. Notice that this doesn't work for the
             default left/top alignment and @c wxEXPAND still applies in both
             directions if it is combined with @c wxALIGN_LEFT or @c wxALIGN_TOP.}
    @itemdef{wxSHAPED,
             The item will be expanded as much as possible while also
             maintaining its aspect ratio.}
    @itemdef{wxFIXED_MINSIZE,
             Normally sizers use the "best", i.e. most appropriate, size of the
             window to determine what the minimal size of window items should be.
             This allows layouts to adjust correctly when the item contents,
             and hence its best size, changes. If this behaviour is unwanted,
             @c wxFIXED_MINSIZE can be used to fix minimal size of the window
             to its initial value and not change it any more in the future.
             Note that the same thing can be accomplished by calling
             wxWindow::SetMinSize() explicitly as well.}
    @itemdef{wxRESERVE_SPACE_EVEN_IF_HIDDEN,
             Normally wxSizers don't allocate space for hidden windows or other
             items. This flag overrides this behaviour so that sufficient space
             is allocated for the window even if it isn't visible. This makes
             it possible to dynamically show and hide controls without resizing
             parent dialog, for example. (Available since 2.8.8.)}
    @itemdef{wxALIGN_CENTER<br>
             wxALIGN_CENTRE<br>
             wxALIGN_LEFT<br>
             wxALIGN_RIGHT<br>
             wxALIGN_TOP<br>
             wxALIGN_BOTTOM<br>
             wxALIGN_CENTER_VERTICAL<br>
             wxALIGN_CENTRE_VERTICAL<br>
             wxALIGN_CENTER_HORIZONTAL<br>
             wxALIGN_CENTRE_HORIZONTAL,
             The @c wxALIGN_* flags allow you to specify the alignment of the item
             within the space allotted to it by the sizer, adjusted for the
             border if any.}
    @endDefList

    @library{wxcore}
    @category{winlayout}

    @see @ref overview_sizer
*/
class wxSizer : public wxObject
{
public:
    /**
        The constructor.
        Note that wxSizer is an abstract base class and may not be instantiated.
    */
    wxSizer();

    /**
        The destructor.
    */
    virtual ~wxSizer();

    /**
        Appends a child to the sizer.

        wxSizer itself is an abstract class, but the parameters are equivalent
        in the derived classes that you will instantiate to use it so they are
        described here:

        @param window
            The window to be added to the sizer. Its initial size (either set
            explicitly by the user or calculated internally when using
            wxDefaultSize) is interpreted as the minimal and in many cases also
            the initial size.
        @param flags
            A wxSizerFlags object that enables you to specify most of the above
            parameters more conveniently.
    */
    wxSizerItem* Add(wxWindow* window, const wxSizerFlags& flags);

    /**
        Appends a child to the sizer.

        wxSizer itself is an abstract class, but the parameters are equivalent
        in the derived classes that you will instantiate to use it so they are
        described here:

        @param window
            The window to be added to the sizer. Its initial size (either set
            explicitly by the user or calculated internally when using
            wxDefaultSize) is interpreted as the minimal and in many cases also
            the initial size.
        @param proportion
            Although the meaning of this parameter is undefined in wxSizer, it
            is used in wxBoxSizer to indicate if a child of a sizer can change
            its size in the main orientation of the wxBoxSizer - where 0 stands
            for not changeable and a value of more than zero is interpreted
            relative to the value of other children of the same wxBoxSizer. For
            example, you might have a horizontal wxBoxSizer with three
            children, two of which are supposed to change their size with the
            sizer. Then the two stretchable windows would get a value of 1 each
            to make them grow and shrink equally with the sizer's horizontal
            dimension.
        @param flag
            OR-combination of flags affecting sizer's behaviour. See
            @ref wxsizer_flags "wxSizer flags list" for details.
        @param border
            Determines the border width, if the flag parameter is set to
            include any border flag.
        @param userData
            Allows an extra object to be attached to the sizer item, for use in
            derived classes when sizing information is more complex than the
            proportion and flag will allow for.
    */
    wxSizerItem* Add(wxWindow* window,
                     int proportion = 0,
                     int flag = 0,
                     int border = 0,
                     wxObject* userData = NULL);

    /**
        Appends a child to the sizer.

        wxSizer itself is an abstract class, but the parameters are equivalent
        in the derived classes that you will instantiate to use it so they are
        described here:

        @param sizer
            The (child-)sizer to be added to the sizer. This allows placing a
            child sizer in a sizer and thus to create hierarchies of sizers
            (typically a vertical box as the top sizer and several horizontal
            boxes on the level beneath).
        @param flags
            A wxSizerFlags object that enables you to specify most of the above
            parameters more conveniently.
    */
    wxSizerItem* Add(wxSizer* sizer, const wxSizerFlags& flags);

    /**
        Appends a child to the sizer.

        wxSizer itself is an abstract class, but the parameters are equivalent
        in the derived classes that you will instantiate to use it so they are
        described here:

        @param sizer
            The (child-)sizer to be added to the sizer. This allows placing a
            child sizer in a sizer and thus to create hierarchies of sizers
            (typically a vertical box as the top sizer and several horizontal
            boxes on the level beneath).
        @param proportion
            Although the meaning of this parameter is undefined in wxSizer, it
            is used in wxBoxSizer to indicate if a child of a sizer can change
            its size in the main orientation of the wxBoxSizer - where 0 stands
            for not changeable and a value of more than zero is interpreted
            relative to the value of other children of the same wxBoxSizer. For
            example, you might have a horizontal wxBoxSizer with three
            children, two of which are supposed to change their size with the
            sizer. Then the two stretchable windows would get a value of 1 each
            to make them grow and shrink equally with the sizer's horizontal
            dimension.
        @param flag
            OR-combination of flags affecting sizer's behaviour. See
            @ref wxsizer_flags "wxSizer flags list" for details.
        @param border
            Determines the border width, if the flag parameter is set to
            include any border flag.
        @param userData
            Allows an extra object to be attached to the sizer item, for use in
            derived classes when sizing information is more complex than the
            proportion and flag will allow for.
    */
    wxSizerItem* Add(wxSizer* sizer,
                     int proportion = 0,
                     int flag = 0,
                     int border = 0,
                     wxObject* userData = NULL);

    /**
        Appends a spacer child to the sizer.

        wxSizer itself is an abstract class, but the parameters are equivalent
        in the derived classes that you will instantiate to use it so they are
        described here.

        @a width and @a height specify the dimension of a spacer to be added to
        the sizer. Adding spacers to sizers gives more flexibility in the
        design of dialogs; imagine for example a horizontal box with two
        buttons at the bottom of a dialog: you might want to insert a space
        between the two buttons and make that space stretchable using the
        proportion flag and the result will be that the left button will be
        aligned with the left side of the dialog and the right button with the
        right side - the space in between will shrink and grow with the dialog.

        @param width
            Width of the spacer.
        @param height
            Height of the spacer.
        @param proportion
            Although the meaning of this parameter is undefined in wxSizer, it
            is used in wxBoxSizer to indicate if a child of a sizer can change
            its size in the main orientation of the wxBoxSizer - where 0 stands
            for not changeable and a value of more than zero is interpreted
            relative to the value of other children of the same wxBoxSizer. For
            example, you might have a horizontal wxBoxSizer with three
            children, two of which are supposed to change their size with the
            sizer. Then the two stretchable windows would get a value of 1 each
            to make them grow and shrink equally with the sizer's horizontal
            dimension.
        @param flag
            OR-combination of flags affecting sizer's behaviour. See
            @ref wxsizer_flags "wxSizer flags list" for details.
        @param border
            Determines the border width, if the flag parameter is set to
            include any border flag.
        @param userData
            Allows an extra object to be attached to the sizer item, for use in
            derived classes when sizing information is more complex than the
            proportion and flag will allow for.
    */
    wxSizerItem* Add(int width, int height,
                     int proportion = 0,
                     int flag = 0,
                     int border = 0,
                     wxObject* userData = NULL);

    /**
        Appends a spacer child to the sizer.

        @param width
            Width of the spacer.
        @param height
            Height of the spacer.
        @param flags
            A wxSizerFlags object that enables you to specify most of the other
            parameters more conveniently.
    */
    wxSizerItem* Add( int width, int height, const wxSizerFlags& flags);

    wxSizerItem* Add(wxSizerItem* item);

    /**
        This base function adds non-stretchable space to both the horizontal
        and vertical orientation of the sizer.
        More readable way of calling:
        @code
        wxSizer::Add(size, size, 0).
        @endcode
        @see wxBoxSizer::AddSpacer()
    */
    virtual wxSizerItem *AddSpacer(int size);

    /**
        Adds stretchable space to the sizer.
        More readable way of calling:
        @code
        wxSizer::Add(0, 0, prop).
        @endcode
    */
    wxSizerItem* AddStretchSpacer(int prop = 1);

    /**
        This method is abstract and has to be overwritten by any derived class.
        Here, the sizer will do the actual calculation of its children's minimal sizes.
    */
    virtual wxSize CalcMin() = 0;

    /**
        Detaches all children from the sizer.

        If @a delete_windows is @true then child windows will also be deleted.

        Notice that child sizers are always deleted, as a general consequence
        of the principle that sizers own their sizer children, but don't own
        their window children (because they are already owned by their parent
        windows).
    */
    virtual void Clear(bool delete_windows = false);

    /**
        Computes client area size for @a window so that it matches the sizer's
        minimal size. Unlike GetMinSize(), this method accounts for other
        constraints imposed on @e window, namely display's size (returned size
        will never be too large for the display) and maximum window size if
        previously set by wxWindow::SetMaxSize().

        The returned value is suitable for passing to wxWindow::SetClientSize() or
        wxWindow::SetMinClientSize().

        @since 2.8.8

        @see ComputeFittingWindowSize(), Fit()
    */
    wxSize ComputeFittingClientSize(wxWindow* window);

    /**
        Like ComputeFittingClientSize(), but converts the result into window
        size. The returned value is suitable for passing to wxWindow::SetSize()
        or wxWindow::SetMinSize().

        @since 2.8.8

        @see ComputeFittingClientSize(), Fit()
    */
    wxSize ComputeFittingWindowSize(wxWindow* window);

    /**
        Detach the child @a window from the sizer without destroying it.

        This method does not cause any layout or resizing to take place, call Layout()
        to update the layout "on screen" after detaching a child from the sizer.

        Returns @true if the child item was found and detached, @false otherwise.

        @see Remove()
    */
    virtual bool Detach(wxWindow* window);

    /**
        Detach the child @a sizer from the sizer without destroying it.

        This method does not cause any layout or resizing to take place, call Layout()
        to update the layout "on screen" after detaching a child from the sizer.

        Returns @true if the child item was found and detached, @false otherwise.

        @see Remove()
    */
    virtual bool Detach(wxSizer* sizer);

    /**
        Detach a item at position @a index from the sizer without destroying it.

        This method does not cause any layout or resizing to take place, call Layout()
        to update the layout "on screen" after detaching a child from the sizer.
        Returns @true if the child item was found and detached, @false otherwise.

        @see Remove()
    */
    virtual bool Detach(int index);

    /**
        Tell the sizer to resize the @a window so that its client area matches the
        sizer's minimal size (ComputeFittingClientSize() is called to determine it).
        This is commonly done in the constructor of the window itself, see sample
        in the description of wxBoxSizer.

        @return The new window size.

        @see ComputeFittingClientSize(), ComputeFittingWindowSize()
    */
    wxSize Fit(wxWindow* window);

    /**
        Tell the sizer to resize the virtual size of the @a window to match the sizer's
        minimal size. This will not alter the on screen size of the window, but may
        cause the addition/removal/alteration of scrollbars required to view the virtual
        area in windows which manage it.

        @see wxScrolled::SetScrollbars(), SetVirtualSizeHints()
    */
    void FitInside(wxWindow* window);

    /**
       Inform sizer about the first direction that has been decided (by
       parent item).  Returns true if it made use of the information (and
       recalculated min size).
    */
    virtual bool InformFirstDirection(int direction, int size, int availableOtherDir);


    //@{
    /**
        Returns the list of the items in this sizer.

        The elements of type-safe wxList @c wxSizerItemList are pointers to
        objects of type wxSizerItem.
    */
    wxSizerItemList& GetChildren();
    const wxSizerItemList& GetChildren() const;
    //@}

    /**
        Returns the window this sizer is used in or @NULL if none.
    */
    wxWindow* GetContainingWindow() const;

    /**
       Set the window this sizer is used in.
    */
    void SetContainingWindow(wxWindow *window);

    /**
       Returns the number of items in the sizer.

       If you just need to test whether the sizer is empty or not you can also
       use IsEmpty() function.
    */
    size_t GetItemCount() const;

    /**
        Finds wxSizerItem which holds the given @a window.
        Use parameter @a recursive to search in subsizers too.
        Returns pointer to item or @NULL.
    */
    wxSizerItem* GetItem(wxWindow* window, bool recursive = false);

    /**
        Finds wxSizerItem which holds the given @a sizer.
        Use parameter @a recursive to search in subsizers too.
        Returns pointer to item or @NULL.
    */

    wxSizerItem* GetItem(wxSizer* sizer, bool recursive = false);

    /**
        Finds wxSizerItem which is located in the sizer at position @a index.
        Use parameter @a recursive to search in subsizers too.
        Returns pointer to item or @NULL.
    */
    wxSizerItem* GetItem(size_t index);

    /**
        Finds item of the sizer which has the given @e id.
        This @a id is not the window id but the id of the wxSizerItem itself.
        This is mainly useful for retrieving the sizers created from XRC resources.
        Use parameter @a recursive to search in subsizers too.
        Returns pointer to item or @NULL.
    */
    wxSizerItem* GetItemById(int id, bool recursive = false);

    /**
        Returns the minimal size of the sizer.

        This is either the combined minimal size of all the children and their
        borders or the minimal size set by SetMinSize(), depending on which is bigger.
        Note that the returned value is client size, not window size.
        In particular, if you use the value to set toplevel window's minimal or
        actual size, use wxWindow::SetMinClientSize() or wxWindow::SetClientSize(),
        not wxWindow::SetMinSize() or wxWindow::SetSize().
    */
    wxSize GetMinSize();

    /**
        Returns the current position of the sizer.
    */
    wxPoint GetPosition() const;

    /**
        Returns the current size of the sizer.
    */
    wxSize GetSize() const;

    /**
        Hides the child @a window.

        To make a sizer item disappear, use Hide() followed by Layout().

        Use parameter @a recursive to hide elements found in subsizers.
        Returns @true if the child item was found, @false otherwise.

        @see IsShown(), Show()
    */
    bool Hide(wxWindow* window, bool recursive = false);

    /**
        Hides the child @a sizer.

        To make a sizer item disappear, use Hide() followed by Layout().

        Use parameter @a recursive to hide elements found in subsizers.
        Returns @true if the child item was found, @false otherwise.

        @see IsShown(), Show()
    */
    bool Hide(wxSizer* sizer, bool recursive = false);

    /**
        Hides the item at position @a index.

        To make a sizer item disappear, use Hide() followed by Layout().

        Use parameter @a recursive to hide elements found in subsizers.
        Returns @true if the child item was found, @false otherwise.

        @see IsShown(), Show()
    */
    bool Hide(size_t index);

    /**
        Insert a child into the sizer before any existing item at @a index.

        See Add() for the meaning of the other parameters.
    */
    wxSizerItem* Insert(size_t index, wxWindow* window,
                        const wxSizerFlags& flags);

    /**
        Insert a child into the sizer before any existing item at @a index.

        See Add() for the meaning of the other parameters.
    */
    wxSizerItem* Insert(size_t index, wxWindow* window,
                        int proportion = 0,
                        int flag = 0,
                        int border = 0,
                        wxObject* userData = NULL);

    /**
        Insert a child into the sizer before any existing item at @a index.

        See Add() for the meaning of the other parameters.
    */
    wxSizerItem* Insert(size_t index, wxSizer* sizer,
                        const wxSizerFlags& flags);

    /**
        Insert a child into the sizer before any existing item at @a index.

        See Add() for the meaning of the other parameters.
    */
    wxSizerItem* Insert(size_t index, wxSizer* sizer,
                        int proportion = 0,
                        int flag = 0,
                        int border = 0,
                        wxObject* userData = NULL);

    /**
        Insert a child into the sizer before any existing item at @a index.

        See Add() for the meaning of the other parameters.
    */
    wxSizerItem* Insert(size_t index, int width, int height,
                        int proportion = 0,
                        int flag = 0,
                        int border = 0,
                        wxObject* userData = NULL);
    /**
        Insert a child into the sizer before any existing item at @a index.

        See Add() for the meaning of the other parameters.
    */
    wxSizerItem* Insert(size_t index,
                        int width,
                        int height,
                        const wxSizerFlags& flags);

    wxSizerItem* Insert(size_t index, wxSizerItem* item);

    /**
        Inserts non-stretchable space to the sizer.
        More readable way of calling wxSizer::Insert(index, size, size).
    */
    wxSizerItem* InsertSpacer(size_t index, int size);

    /**
        Inserts stretchable space to the sizer.
        More readable way of calling wxSizer::Insert(0, 0, prop).
    */
    wxSizerItem* InsertStretchSpacer(size_t index, int prop = 1);

    /**
        Return @true if the sizer has no elements.

        @see GetItemCount()
     */
    bool IsEmpty() const;

    /**
        Returns @true if the @a window is shown.

        @see Hide(), Show(), wxSizerItem::IsShown()
    */
    bool IsShown(wxWindow* window) const;

    /**
        Returns @true if the @a sizer is shown.

        @see Hide(), Show(), wxSizerItem::IsShown()
    */
    bool IsShown(wxSizer* sizer) const;

    /**
        Returns @true if the item at @a index is shown.

        @see Hide(), Show(), wxSizerItem::IsShown()
    */
    bool IsShown(size_t index) const;

    /**
        Call this to force layout of the children anew, e.g.\ after having added a child
        to or removed a child (window, other sizer or space) from the sizer while
        keeping the current dimension.
    */
    virtual void Layout();

    /**
        Same as Add(), but prepends the items to the beginning of the
        list of items (windows, subsizers or spaces) owned by this sizer.
    */
    wxSizerItem* Prepend(wxWindow* window, const wxSizerFlags& flags);

    /**
        Same as Add(), but prepends the items to the beginning of the
        list of items (windows, subsizers or spaces) owned by this sizer.
    */
    wxSizerItem* Prepend(wxWindow* window, int proportion = 0,
                         int flag = 0,
                         int border = 0,
                         wxObject* userData = NULL);

    /**
        Same as Add(), but prepends the items to the beginning of the
        list of items (windows, subsizers or spaces) owned by this sizer.
    */
    wxSizerItem* Prepend(wxSizer* sizer,
                         const wxSizerFlags& flags);

    /**
        Same as Add(), but prepends the items to the beginning of the
        list of items (windows, subsizers or spaces) owned by this sizer.
    */
    wxSizerItem* Prepend(wxSizer* sizer, int proportion = 0,
                         int flag = 0,
                         int border = 0,
                         wxObject* userData = NULL);

    /**
        Same as Add(), but prepends the items to the beginning of the
        list of items (windows, subsizers or spaces) owned by this sizer.
    */
    wxSizerItem* Prepend(int width, int height,
                         int proportion = 0,
                         int flag = 0,
                         int border = 0,
                         wxObject* userData = NULL);

    /**
        Same as Add(), but prepends the items to the beginning of the
        list of items (windows, subsizers or spaces) owned by this sizer.
    */
    wxSizerItem* Prepend(int width, int height, const wxSizerFlags& flags);

    wxSizerItem* Prepend(wxSizerItem* item);

    /**
        Prepends non-stretchable space to the sizer.
        More readable way of calling wxSizer::Prepend(size, size, 0).
    */
    wxSizerItem* PrependSpacer(int size);

    /**
        Prepends stretchable space to the sizer.
        More readable way of calling wxSizer::Prepend(0, 0, prop).
    */
    wxSizerItem* PrependStretchSpacer(int prop = 1);

    /**
        Method which must be overridden in the derived sizer classes.

        The implementation should reposition the children using the current
        total size available to the sizer (@c m_size) and the size computed by
        the last call to CalcMin().

        Note that you should never call this method directly, call Layout()
        instead if you need to manually update the sizer elements positions.
        This method is only called by wxWidgets itself.

        @since 3.1.3, before this version RecalcSizes() method not taking any
            arguments had to be overridden in the derived classes instead.
    */
    virtual void RepositionChildren(const wxSize& minSize) = 0;

    /**
        Removes a child window from the sizer, but does @b not destroy it
        (because windows are owned by their parent window, not the sizer).

        @deprecated
        The overload of this method taking a wxWindow* parameter
        is deprecated as it does not destroy the window as would usually be
        expected from Remove(). You should use Detach() in new code instead.
        There is currently no wxSizer method that will both detach and destroy
        a wxWindow item.

        @note This method does not cause any layout or resizing to take
              place, call Layout() to update the layout "on screen" after
              removing a child from the sizer.

        @return @true if the child item was found and removed, @false otherwise.
    */
    virtual bool Remove(wxWindow* window);

    /**
        Removes a sizer child from the sizer and destroys it.

        @note This method does not cause any layout or resizing to take
              place, call Layout() to update the layout "on screen" after
              removing a child from the sizer.

        @param sizer The wxSizer to be removed.

        @return @true if the child item was found and removed, @false otherwise.
    */
    virtual bool Remove(wxSizer* sizer);

    /**
        Removes a child from the sizer and destroys it if it is a sizer or a
        spacer, but not if it is a window (because windows are owned by their
        parent window, not the sizer).

        @note This method does not cause any layout or resizing to take
              place, call Layout() to update the layout "on screen" after
              removing a child from the sizer.

        @param index
            The position of the child in the sizer, e.g. 0 for the first item.

        @return @true if the child item was found and removed, @false otherwise.
    */
    virtual bool Remove(int index);

    /**
        Detaches the given @a oldwin from the sizer and replaces it with the
        given @a newwin. The detached child window is @b not deleted (because
        windows are owned by their parent window, not the sizer).

        Use parameter @a recursive to search the given element recursively in subsizers.

        This method does not cause any layout or resizing to take place,
        call Layout() to update the layout "on screen" after replacing a
        child from the sizer.

        Returns @true if the child item was found and removed, @false otherwise.
    */
    virtual bool Replace(wxWindow* oldwin, wxWindow* newwin,
                         bool recursive = false);

    /**
        Detaches the given @a oldsz from the sizer and replaces it with the
        given @a newsz. The detached child sizer is deleted.

        Use parameter @a recursive to search the given element recursively in subsizers.

        This method does not cause any layout or resizing to take place,
        call Layout() to update the layout "on screen" after replacing a
        child from the sizer.

        Returns @true if the child item was found and removed, @false otherwise.
    */
    virtual bool Replace(wxSizer* oldsz, wxSizer* newsz,
                         bool recursive = false);

    /**
        Detaches the given item at position @a index from the sizer and
        replaces it with the given wxSizerItem @a newitem.

        The detached child is deleted @b only if it is a sizer or a spacer
        (but not if it is a wxWindow because windows are owned by their
        parent window, not the sizer).

        This method does not cause any layout or resizing to take place,
        call Layout() to update the layout "on screen" after replacing a
        child from the sizer.

        Returns @true if the child item was found and removed, @false otherwise.
    */
    virtual bool Replace(size_t index, wxSizerItem* newitem);

    /**
        Call this to force the sizer to take the given dimension and thus force
        the items owned by the sizer to resize themselves according to the
        rules defined by the parameter in the Add() and Prepend() methods.
    */
    void SetDimension(int x, int y, int width, int height);

    /**
        @overload
     */
    void SetDimension(const wxPoint& pos, const wxSize& size);

    /**
        Set an item's minimum size by window, sizer, or position.

        This function enables an application to set the size of an item after
        initial creation.

        The @a window or @a sizer will be found recursively in the sizer's
        descendants.

        @see wxSizerItem::SetMinSize()

        @return
            @true if the minimal size was successfully set or @false if the
            item was not found.
    */
    //@{
    bool SetItemMinSize(wxWindow* window, int width, int height);
    bool SetItemMinSize(wxWindow* window, const wxSize& size);

    bool SetItemMinSize(wxSizer* sizer, int width, int height);
    bool SetItemMinSize(wxSizer* sizer, const wxSize& size);

    bool SetItemMinSize(size_t index, int width, int height);
    bool SetItemMinSize(size_t index, const wxSize& size);
    //@}

    /**
        Call this to give the sizer a minimal size.

        Normally, the sizer will calculate its minimal size based purely on how
        much space its children need. After calling this method GetMinSize()
        will return either the minimal size as requested by its children or the
        minimal size set here, depending on which is bigger.
    */
    void SetMinSize(const wxSize& size);

    /**
        @overload
     */
    void SetMinSize(int width, int height);

    /**
        This method first calls Fit() and then wxTopLevelWindow::SetSizeHints()
        on the @a window passed to it.

        This only makes sense when @a window is actually a wxTopLevelWindow such
        as a wxFrame or a wxDialog, since SetSizeHints only has any effect in these classes.
        It does nothing in normal windows or controls.

        This method is implicitly used by wxWindow::SetSizerAndFit() which is
        commonly invoked in the constructor of a toplevel window itself (see
        the sample in the description of wxBoxSizer) if the toplevel window is
        resizable.
    */
    void SetSizeHints(wxWindow* window);

    /**
        Tell the sizer to set the minimal size of the @a window virtual area to match
        the sizer's minimal size. For windows with managed scrollbars this will set them
        appropriately.

        @deprecated This is exactly the same as FitInside() in wxWidgets 2.9
        and later, please replace calls to it with FitInside().

        @see wxScrolled::SetScrollbars()
    */
    void SetVirtualSizeHints(wxWindow* window);

    /**
        Shows or hides the @a window.
        To make a sizer item disappear or reappear, use Show() followed by Layout().

        Use parameter @a recursive to show or hide elements found in subsizers.

        Returns @true if the child item was found, @false otherwise.

        @see Hide(), IsShown()
    */
    bool Show(wxWindow* window, bool show = true,
              bool recursive = false);

    /**
        Shows or hides @a sizer.
        To make a sizer item disappear or reappear, use Show() followed by Layout().

        Use parameter @a recursive to show or hide elements found in subsizers.

        Returns @true if the child item was found, @false otherwise.

        @see Hide(), IsShown()
    */
    bool Show(wxSizer* sizer, bool show = true,
              bool recursive = false);

    /**
        Shows the item at @a index.
        To make a sizer item disappear or reappear, use Show() followed by Layout().

        Returns @true if the child item was found, @false otherwise.

        @see Hide(), IsShown()
    */
    bool Show(size_t index, bool show = true);


    /**
       Show or hide all items managed by the sizer.
    */
    virtual void ShowItems(bool show);

};


/**
    @class wxStdDialogButtonSizer

    This class creates button layouts which conform to the standard button spacing
    and ordering defined by the platform or toolkit's user interface guidelines
    (if such things exist). By using this class, you can ensure that all your
    standard dialogs look correct on all major platforms. Currently it conforms to
    the Windows, GTK+ and macOS human interface guidelines.

    When there aren't interface guidelines defined for a particular platform or
    toolkit, wxStdDialogButtonSizer reverts to the Windows implementation.

    To use this class, first add buttons to the sizer by calling
    wxStdDialogButtonSizer::AddButton (or wxStdDialogButtonSizer::SetAffirmativeButton,
    wxStdDialogButtonSizer::SetNegativeButton or wxStdDialogButtonSizer::SetCancelButton)
    and then call Realize in order to create the actual button layout used.
    Other than these special operations, this sizer works like any other sizer.

    If you add a button with wxID_SAVE, on macOS the button will be renamed to
    "Save" and the wxID_NO button will be renamed to "Don't Save" in accordance
    with the macOS Human Interface Guidelines.

    @library{wxcore}
    @category{winlayout}

    @see wxSizer, @ref overview_sizer, wxDialog::CreateButtonSizer
*/
class wxStdDialogButtonSizer : public wxBoxSizer
{
public:
    /**
        Constructor for a wxStdDialogButtonSizer.
    */
    wxStdDialogButtonSizer();

    /**
        Adds a button to the wxStdDialogButtonSizer. The @a button must have
        one of the following identifiers:
         - wxID_OK
         - wxID_YES
         - wxID_SAVE
         - wxID_APPLY
         - wxID_CLOSE
         - wxID_NO
         - wxID_CANCEL
         - wxID_HELP
         - wxID_CONTEXT_HELP
    */
    void AddButton(wxButton* button);

    /**
        Rearranges the buttons and applies proper spacing between buttons to make
        them match the platform or toolkit's interface guidelines.
    */
    void Realize();

    /**
        Sets the affirmative button for the sizer.

        This allows you to use identifiers other than the standard identifiers
        outlined above.
    */
    void SetAffirmativeButton(wxButton* button);

    /**
        Sets the cancel button for the sizer.

        This allows you to use identifiers other than the standard identifiers
        outlined above.
    */
    void SetCancelButton(wxButton* button);

    /**
        Sets the negative button for the sizer.

        This allows you to use identifiers other than the standard identifiers
        outlined above.
    */
    void SetNegativeButton(wxButton* button);

    virtual void RepositionChildren(const wxSize& minSize);
    virtual wxSize CalcMin();
};



/**
    @class wxSizerItem

    The wxSizerItem class is used to track the position, size and other
    attributes of each item managed by a wxSizer.

    It is not usually necessary to use this class because the sizer elements can
    also be identified by their positions or window or sizer pointers but sometimes
    it may be more convenient to use it directly.

    @library{wxcore}
    @category{winlayout}
*/
class wxSizerItem : public wxObject
{
public:
    /**
        Construct a sizer item for tracking a spacer.
    */
    wxSizerItem(int width, int height, int proportion=0, int flag=0,
                int border=0, wxObject* userData=NULL);

    //@{
    /**
        Construct a sizer item for tracking a window.
    */
    wxSizerItem(wxWindow* window, const wxSizerFlags& flags);
    wxSizerItem(wxWindow* window, int proportion=0, int flag=0,
                int border=0,
                wxObject* userData=NULL);
    //@}

    //@{
    /**
        Construct a sizer item for tracking a subsizer.
    */
    wxSizerItem(wxSizer* sizer, const wxSizerFlags& flags);
    wxSizerItem(wxSizer* sizer, int proportion=0, int flag=0,
                int border=0,
                wxObject* userData=NULL);
    //@}

    /**
        Deletes the user data and subsizer, if any.
    */
    virtual ~wxSizerItem();

    /**
        Set the window to be tracked by this item.

        @note This is a low-level method which is dangerous if used
            incorrectly, avoid using it if possible, i.e. if higher level
            methods such as wxSizer::Replace() can be used instead.

        If the sizer item previously contained a window, it is dissociated from
        the sizer containing this sizer item (if any), but this object doesn't
        have the pointer to the containing sizer and so it's the caller's
        responsibility to call wxWindow::SetContainingSizer() on @a window.
        Failure to do this can result in memory corruption when the window is
        destroyed later, so it is crucial to not forget to do it.

        Also note that the previously contained window is @e not deleted, so
        it's also the callers responsibility to do it, if necessary.
    */
    void AssignWindow(wxWindow *window);

    /**
        Set the sizer tracked by this item.

        Old sizer, if any, is deleted.
    */
    void AssignSizer(wxSizer *sizer);

    //@{
    /**
        Set the size of the spacer tracked by this item.

        Old spacer, if any, is deleted.
    */
    void AssignSpacer(const wxSize& size);
    void AssignSpacer(int w, int h);
    //@}

    /**
        Calculates the minimum desired size for the item, including any space
        needed by borders.
    */
    virtual wxSize CalcMin();

    /**
        Destroy the window or the windows in a subsizer, depending on the type
        of item.
    */
    virtual void DeleteWindows();

    /**
        Enable deleting the SizerItem without destroying the contained sizer.
    */
    void DetachSizer();

    /**
        Return the border attribute.
    */
    int GetBorder() const;

    /**
        Return the flags attribute.

        See @ref wxsizer_flags "wxSizer flags list" for details.
    */
    int GetFlag() const;

    /**
        Return the numeric id of wxSizerItem, or @c wxID_NONE if the id has
        not been set.
    */
    int GetId() const;

    /**
        Get the minimum size needed for the item.
    */
    wxSize GetMinSize() const;

    /**
        Sets the minimum size to be allocated for this item.

        If this item is a window, the @a size is also passed to
        wxWindow::SetMinSize().
     */
    void SetMinSize(const wxSize& size);

    /**
        @overload
     */
    void SetMinSize(int x, int y);

    /**
        What is the current position of the item, as set in the last Layout.
    */
    wxPoint GetPosition() const;

    /**
        Get the proportion item attribute.
    */
    int GetProportion() const;

    /**
        Get the ratio item attribute.
    */
    float GetRatio() const;

    /**
        Get the rectangle of the item on the parent window, excluding borders.
    */
    virtual wxRect GetRect();

    /**
        Get the current size of the item, as set in the last Layout.
    */
    virtual wxSize GetSize() const;

    /**
        If this item is tracking a sizer, return it.  @NULL otherwise.
    */
    wxSizer* GetSizer() const;

    /**
        If this item is tracking a spacer, return its size.
    */
    wxSize GetSpacer() const;

    /**
        Get the userData item attribute.
    */
    wxObject* GetUserData() const;

    /**
        If this item is tracking a window then return it. @NULL otherwise.
    */
    wxWindow* GetWindow() const;

    /**
        Returns @true if this item is a window or a spacer and it is shown or
        if this item is a sizer and not all of its elements are hidden.

        In other words, for sizer items, all of the child elements must be
        hidden for the sizer itself to be considered hidden.

        As an exception, if the @c wxRESERVE_SPACE_EVEN_IF_HIDDEN flag was
        used for this sizer item, then IsShown() always returns @true for it
        (see wxSizerFlags::ReserveSpaceEvenIfHidden()).
    */
    bool IsShown() const;

    /**
        Is this item a sizer?
    */
    bool IsSizer() const;

    /**
        Is this item a spacer?
    */
    bool IsSpacer() const;

    /**
        Is this item a window?
    */
    bool IsWindow() const;

    /**
        Set the border item attribute.
    */
    void SetBorder(int border);

    /**
        Set the position and size of the space allocated to the sizer, and
        adjust the position and size of the item to be within that space
        taking alignment and borders into account.
    */
    virtual void SetDimension(const wxPoint& pos, const wxSize& size);

    /**
        Set the flag item attribute.
    */
    void SetFlag(int flag);

    /**
        Sets the numeric id of the wxSizerItem to @e id.
    */
    void SetId(int id);

    /**
        Sets the minimum size to be allocated for this item.

        This is identical to SetMinSize(), prefer to use the other function, as
        its name is more clear.
    */
    void SetInitSize(int x, int y);

    /**
        Set the proportion item attribute.
    */
    void SetProportion(int proportion);

    //@{
    /**
        Set the ratio item attribute.
    */
    void SetRatio(int width, int height);
    void SetRatio(wxSize size);
    void SetRatio(float ratio);
    //@}

    /**
        Set the sizer tracked by this item.

        @deprecated This function does not free the old sizer which may result
        in memory leaks, use AssignSizer() which does free it instead.
    */
    void SetSizer(wxSizer* sizer);

    /**
        Set the size of the spacer tracked by this item.

        @deprecated This function does not free the old spacer which may result
        in memory leaks, use AssignSpacer() which does free it instead.
    */
    void SetSpacer(const wxSize& size);

    void SetUserData(wxObject* userData);

    /**
        Set the window to be tracked by this item.
        @deprecated @todo provide deprecation description
    */
    void SetWindow(wxWindow* window);

    /**
        Set the show item attribute, which sizers use to determine if the item
        is to be made part of the layout or not. If the item is tracking a
        window then it is shown or hidden as needed.
    */
    void Show(bool show);
};



/**
    @class wxSizerFlags

    Container for sizer items flags providing readable names for them.

    Normally, when you add an item to a sizer via wxSizer::Add, you have to
    specify a lot of flags and parameters which can be unwieldy. This is where
    wxSizerFlags comes in: it allows you to specify all parameters using the
    named methods instead. For example, instead of

    @code
    sizer->Add(ctrl, 0, wxEXPAND | wxALL, 10);
    @endcode

    you can now write

    @code
    sizer->Add(ctrl, wxSizerFlags().Expand().Border(wxALL, 10));
    @endcode

    This is more readable and also allows you to create wxSizerFlags objects which
    can be reused for several sizer items.

    @code
    wxSizerFlags flagsExpand(1);
        flagsExpand.Expand().Border(wxALL, 10);

        sizer->Add(ctrl1, flagsExpand);
        sizer->Add(ctrl2, flagsExpand);
    @endcode

    Note that by specification, all methods of wxSizerFlags return the wxSizerFlags
    object itself to allowing chaining multiple methods calls like in the examples
    above.

    @library{wxcore}
    @category{winlayout}

    @see wxSizer
*/
class wxSizerFlags
{
public:
    /**
        Creates the wxSizer with the proportion specified by @a proportion.
    */
    wxSizerFlags(int proportion = 0);

    /**
        Sets the alignment of this wxSizerFlags to @a align.

        This method replaces the previously set alignment with the specified one.

        @param alignment
            Combination of @c wxALIGN_XXX bit masks.

        @see Top(), Left(), Right(), Bottom(), Centre()
    */
    wxSizerFlags& Align(int alignment);

    /**
        Sets the wxSizerFlags to have a border of a number of pixels specified
        by @a borderinpixels with the directions specified by @a direction.

        Prefer to use the overload below or DoubleBorder() or TripleBorder()
        versions instead of hard-coding the border value in pixels to avoid too
        small borders on devices with high DPI displays.
    */
    wxSizerFlags& Border(int direction, int borderinpixels);

    /**
        Sets the wxSizerFlags to have a border with size as returned by
        GetDefaultBorder().

        @param direction
            Direction(s) to apply the border in.
    */
    wxSizerFlags& Border(int direction = wxALL);

    /**
        Aligns the object to the bottom, similar for @c Align(wxALIGN_BOTTOM).

        Unlike Align(), this method doesn't change the horizontal alignment of
        the item.
    */
    wxSizerFlags& Bottom();

    /**
        Sets the object of the wxSizerFlags to center itself in the area it is
        given.
    */
    wxSizerFlags& Center();

    /**
        Center() for people with the other dialect of English.
    */
    wxSizerFlags& Centre();

    /**
        Same as CentreHorizontal().

        @since 3.1.0
     */
    wxSizerFlags& CenterHorizontal();

    /**
        Same as CentreVertical().

        @since 3.1.0
     */
    wxSizerFlags& CenterVertical();

    /**
        Center an item only in horizontal direction.

        This is mostly useful for 2D sizers as for the 1D ones it is shorter to
        just use Centre() as the alignment is only used in one direction with
        them anyhow. For 2D sizers, centering an item in one direction is quite
        different from centering it in both directions however.

        @see CentreVertical()

        @since 3.1.0
     */
    wxSizerFlags& CentreHorizontal();

    /**
        Center an item only in vertical direction.

        The remarks in CentreHorizontal() documentation also apply to this
        function.

        @since 3.1.0
     */
    wxSizerFlags& CentreVertical();

    /**
        Sets the border in the given @a direction having twice the default
        border size.
    */
    wxSizerFlags& DoubleBorder(int direction = wxALL);

    /**
        Sets the border in left and right directions having twice the default
        border size.
    */
    wxSizerFlags& DoubleHorzBorder();

    /**
        Sets the object of the wxSizerFlags to expand to fill as much area as
        it can.
    */
    wxSizerFlags& Expand();

    /**
        Set the @c wxFIXED_MINSIZE flag which indicates that the initial size
        of the window should be also set as its minimal size.
    */
    wxSizerFlags& FixedMinSize();

    /**
        Set the @c wxRESERVE_SPACE_EVEN_IF_HIDDEN flag. Normally wxSizers
        don't allocate space for hidden windows or other items. This flag
        overrides this behaviour so that sufficient space is allocated for the
        window even if it isn't visible. This makes it possible to dynamically
        show and hide controls without resizing parent dialog, for example.

        @since 2.8.8
    */
    wxSizerFlags& ReserveSpaceEvenIfHidden();

    /**
        Returns the border used by default in Border() method.

        This value is scaled appropriately for the current DPI on the systems
        where physical pixel values are used for the control positions and
        sizes, i.e. not with wxGTK or wxOSX.

        @see GetDefaultBorderFractional()
    */
    static int GetDefaultBorder();

    /**
        Returns the border used by default, with fractional precision. For
        example when the border is scaled to a non-integer DPI.

        @see GetDefaultBorder()

        @since 3.1.4
    */
    static float GetDefaultBorderFractional();

    /**
        Aligns the object to the left, similar for @c Align(wxALIGN_LEFT).

        Unlike Align(), this method doesn't change the vertical alignment of
        the item.
    */
    wxSizerFlags& Left();

    /**
        Sets the proportion of this wxSizerFlags to @e proportion
    */
    wxSizerFlags& Proportion(int proportion);

    /**
        Aligns the object to the right, similar for @c Align(wxALIGN_RIGHT).

        Unlike Align(), this method doesn't change the vertical alignment of
        the item.
    */
    wxSizerFlags& Right();

    /**
        Set the @c wx_SHAPED flag which indicates that the elements should
        always keep the fixed width to height ratio equal to its original value.
    */
    wxSizerFlags& Shaped();

    /**
        Aligns the object to the top, similar for @c Align(wxALIGN_TOP).

        Unlike Align(), this method doesn't change the horizontal alignment of
        the item.
    */
    wxSizerFlags& Top();

    /**
        Sets the border in the given @a direction having thrice the default
        border size.
    */
    wxSizerFlags& TripleBorder(int direction = wxALL);
};


/**
    Values which define the behaviour for resizing wxFlexGridSizer cells in the
    "non-flexible" direction.
*/
enum wxFlexSizerGrowMode
{
    /// Don't resize the cells in non-flexible direction at all.
    wxFLEX_GROWMODE_NONE,

    /// Uniformly resize only the specified ones (default).
    wxFLEX_GROWMODE_SPECIFIED,

    /// Uniformly resize all cells.
    wxFLEX_GROWMODE_ALL
};

/**
    @class wxFlexGridSizer

    A flex grid sizer is a sizer which lays out its children in a two-dimensional
    table with all table fields in one row having the same height and all fields
    in one column having the same width, but all rows or all columns are not
    necessarily the same height or width as in the wxGridSizer.

    Since wxWidgets 2.5.0, wxFlexGridSizer can also size items equally in one
    direction but unequally ("flexibly") in the other. If the sizer is only
    flexible in one direction (this can be changed using wxFlexGridSizer::SetFlexibleDirection),
    it needs to be decided how the sizer should grow in the other ("non-flexible")
    direction in order to fill the available space.
    The wxFlexGridSizer::SetNonFlexibleGrowMode() method serves this purpose.

    @library{wxcore}
    @category{winlayout}

    @see wxSizer, @ref overview_sizer
*/
class wxFlexGridSizer : public wxGridSizer
{
public:
    //@{
    /**
        wxFlexGridSizer constructors.

        Please see wxGridSizer::wxGridSizer documentation.

        @since 2.9.1 (except for the four argument overload)
    */
    wxFlexGridSizer( int cols, int vgap, int hgap );
    wxFlexGridSizer( int cols, const wxSize& gap = wxSize(0, 0) );

    wxFlexGridSizer( int rows, int cols, int vgap, int hgap );
    wxFlexGridSizer( int rows, int cols, const wxSize& gap );
    //@}

    /**
        Specifies that column @a idx (starting from zero) should be grown if
        there is extra space available to the sizer.

        The @a proportion parameter has the same meaning as the stretch factor
        for the sizers (see wxBoxSizer) except that if all proportions are 0,
        then all columns are resized equally (instead of not being resized at all).

        Notice that the column must not be already growable, if you need to change
        the proportion you must call RemoveGrowableCol() first and then make it
        growable (with a different proportion) again. You can use IsColGrowable()
        to check whether a column is already growable.
    */
    void AddGrowableCol(size_t idx, int proportion = 0);

    /**
        Specifies that row idx (starting from zero) should be grown if there
        is extra space available to the sizer.

        This is identical to AddGrowableCol() except that it works with rows
        and not columns.
    */
    void AddGrowableRow(size_t idx, int proportion = 0);

    /**
        Returns a ::wxOrientation value that specifies whether the sizer flexibly
        resizes its columns, rows, or both (default).

        @return
            One of the following values:
            - wxVERTICAL: Rows are flexibly sized.
            - wxHORIZONTAL: Columns are flexibly sized.
            - wxBOTH: Both rows and columns are flexibly sized (this is the default value).

        @see SetFlexibleDirection()
    */
    int GetFlexibleDirection() const;

    /**
        Returns the value that specifies how the sizer grows in the "non-flexible"
        direction if there is one.

        The behaviour of the elements in the flexible direction (i.e. both rows
        and columns by default, or rows only if GetFlexibleDirection() is
        @c wxVERTICAL or columns only if it is @c wxHORIZONTAL) is always governed
        by their proportion as specified in the call to AddGrowableRow() or
        AddGrowableCol(). What happens in the other direction depends on the
        value of returned by this function as described below.

        @return
            One of the following values:
            - wxFLEX_GROWMODE_NONE: Sizer doesn't grow its elements at all in
              the non-flexible direction.
            - wxFLEX_GROWMODE_SPECIFIED: Sizer honors growable columns/rows set
              with AddGrowableCol() and AddGrowableRow() in the non-flexible
              direction as well. In this case equal sizing applies to minimum
              sizes of columns or rows (this is the default value).
            - wxFLEX_GROWMODE_ALL: Sizer equally stretches all columns or rows in
              the non-flexible direction, independently of the proportions
              applied in the flexible direction.

        @see SetFlexibleDirection(), SetNonFlexibleGrowMode()
    */
    wxFlexSizerGrowMode GetNonFlexibleGrowMode() const;

    /**
        Returns @true if column @a idx is growable.

        @since 2.9.0
    */
    bool IsColGrowable(size_t idx);

    /**
        Returns @true if row @a idx is growable.

        @since 2.9.0
    */
    bool IsRowGrowable(size_t idx);

    /**
        Specifies that the @a idx column index is no longer growable.
    */
    void RemoveGrowableCol(size_t idx);

    /**
        Specifies that the @a idx row index is no longer growable.
    */
    void RemoveGrowableRow(size_t idx);

    /**
        Specifies whether the sizer should flexibly resize its columns, rows, or both.

        Argument @a direction can be @c wxVERTICAL, @c wxHORIZONTAL or @c wxBOTH
        (which is the default value). Any other value is ignored.

        See GetFlexibleDirection() for the explanation of these values.
        Note that this method does not trigger relayout.
    */
    void SetFlexibleDirection(int direction);

    /**
        Specifies how the sizer should grow in the non-flexible direction if
        there is one (so SetFlexibleDirection() must have been called previously).

        Argument @a mode can be one of those documented in GetNonFlexibleGrowMode(),
        please see there for their explanation.
        Note that this method does not trigger relayout.
    */
    void SetNonFlexibleGrowMode(wxFlexSizerGrowMode mode);

    /**
       Returns a read-only array containing the heights of the rows in the sizer.
    */
    const wxArrayInt& GetRowHeights() const;

    /**
       Returns a read-only array containing the widths of the columns in the sizer.
    */
    const wxArrayInt& GetColWidths() const;

    virtual void RepositionChildren(const wxSize& minSize);
    virtual wxSize CalcMin();

};


/**
    @class wxGridSizer

    A grid sizer is a sizer which lays out its children in a two-dimensional
    table with all table fields having the same size, i.e. the width of each
    field is the width of the widest child, the height of each field is the
    height of the tallest child.

    @library{wxcore}
    @category{winlayout}

    @see wxSizer, @ref overview_sizer
*/
class wxGridSizer : public wxSizer
{
public:
    //@{
    /**
        wxGridSizer constructors.

        Usually only the number of columns in the flex grid sizer needs to be
        specified using @a cols argument. The number of rows will be deduced
        automatically depending on the number of the elements added to the
        sizer.

        If a constructor form with @a rows parameter is used (and the value of
        @a rows argument is not zero, meaning "unspecified") the sizer will
        check that no more than @c cols*rows elements are added to it, i.e.
        that no more than the given number of @a rows is used. Adding less than
        maximally allowed number of items is not an error however.

        Finally, it is also possible to specify the number of rows and use 0
        for @a cols. In this case, the sizer will use the given fixed number of
        rows and as many columns as necessary.

        The @a gap (or @a vgap and @a hgap, which correspond to the height and
        width of the wxSize object) argument defines the size of the padding
        between the rows (its vertical component, or @a vgap) and columns
        (its horizontal component, or @a hgap), in pixels.


        @since 2.9.1 (except for the four argument overload)
    */
    wxGridSizer( int cols, int vgap, int hgap );
    wxGridSizer( int cols, const wxSize& gap = wxSize(0, 0) );

    wxGridSizer( int rows, int cols, int vgap, int hgap );
    wxGridSizer( int rows, int cols, const wxSize& gap );
    //@}

    /**
        Returns the number of columns that has been specified for the
        sizer.

        Returns zero if the sizer is automatically adjusting the number of
        columns depending on number of its children. To get the effective
        number of columns or rows being currently used, see GetEffectiveColsCount()
    */
    int GetCols() const;

    /**
        Returns the number of rows that has been specified for the
        sizer.

        Returns zero if the sizer is automatically adjusting the number of
        rows depending on number of its children. To get the effective
        number of columns or rows being currently used, see GetEffectiveRowsCount().
    */
    int GetRows() const;

    /**
        Returns the number of columns currently used by the sizer.

        This will depend on the number of children the sizer has if
        the sizer is automatically adjusting the number of columns/rows.

        @since 2.9.1
    */
    int GetEffectiveColsCount() const;

    /**
        Returns the number of rows currently used by the sizer.

        This will depend on the number of children the sizer has if
        the sizer is automatically adjusting the number of columns/rows.

        @since 2.9.1
    */
    int GetEffectiveRowsCount() const;

    /**
        Returns the horizontal gap (in pixels) between cells in the sizer.
    */
    int GetHGap() const;

    /**
        Returns the vertical gap (in pixels) between the cells in the sizer.
    */
    int GetVGap() const;

    /**
        Sets the number of columns in the sizer.
    */
    void SetCols(int cols);

    /**
        Sets the horizontal gap (in pixels) between cells in the sizer.
    */
    void SetHGap(int gap);

    /**
        Sets the number of rows in the sizer.
    */
    void SetRows(int rows);

    /**
        Sets the vertical gap (in pixels) between the cells in the sizer.
    */
    void SetVGap(int gap);

    virtual wxSize CalcMin();
    virtual void RepositionChildren(const wxSize& minSize);
};



/**
    @class wxStaticBoxSizer

    wxStaticBoxSizer is a sizer derived from wxBoxSizer but adds a static box around
    the sizer.

    The static box may be either created independently or the sizer may create it
    itself as a convenience. In any case, the sizer owns the wxStaticBox control
    and will delete it in the wxStaticBoxSizer destructor.

    Note that since wxWidgets 2.9.1 you are encouraged to create the windows
    which are added to wxStaticBoxSizer as children of wxStaticBox itself, see
    this class documentation for more details.

    Example of use of this class:
    @code
        void MyFrame::CreateControls()
        {
            wxPanel *panel = new wxPanel(this);
            ...
            wxStaticBoxSizer *sz = new wxStaticBoxSizer(wxVERTICAL, panel, "Box");
            sz->Add(new wxStaticText(sz->GetStaticBox(), wxID_ANY,
                                     "This window is a child of the staticbox"));
            ...
        }
    @endcode

    @library{wxcore}
    @category{winlayout}

    @see wxSizer, wxStaticBox, wxBoxSizer, @ref overview_sizer
*/
class wxStaticBoxSizer : public wxBoxSizer
{
public:
    /**
        This constructor uses an already existing static box.

        @param box
            The static box to associate with the sizer (which will take its
            ownership).
        @param orient
            Can be either @c wxVERTICAL or @c wxHORIZONTAL.
    */
    wxStaticBoxSizer(wxStaticBox* box, int orient);

    /**
        This constructor creates a new static box with the given label and parent window.
    */
    wxStaticBoxSizer(int orient, wxWindow *parent,
                     const wxString& label = wxEmptyString);

    /**
        Returns the static box associated with the sizer.
    */
    wxStaticBox* GetStaticBox() const;

    virtual wxSize CalcMin();
    virtual void RepositionChildren(const wxSize& minSize);
};



/**
    @class wxBoxSizer

    The basic idea behind a box sizer is that windows will most often be laid out
    in rather simple basic geometry, typically in a row or a column or several
    hierarchies of either.

    For more information, please see @ref overview_sizer_box.

    @library{wxcore}
    @category{winlayout}

    @see wxSizer, @ref overview_sizer
*/
class wxBoxSizer : public wxSizer
{
public:
    /**
        Constructor for a wxBoxSizer. @a orient may be either of wxVERTICAL
        or wxHORIZONTAL for creating either a column sizer or a row sizer.
    */
    wxBoxSizer(int orient);

    /**
        Adds non-stretchable space to the main orientation of the sizer only.
        More readable way of calling:
        @code
        if ( wxBoxSizer::IsVertical() )
        {
            wxBoxSizer::Add(0, size, 0).
        }
        else
        {
            wxBoxSizer::Add(size, 0, 0).
        }
        @endcode
    */
    virtual wxSizerItem *AddSpacer(int size);

    /**
        Implements the calculation of a box sizer's minimal.

        It is used internally only and must not be called by the user.
        Documented for information.
    */
    virtual wxSize CalcMin();

    /**
        Returns the orientation of the box sizer, either wxVERTICAL
        or wxHORIZONTAL.
    */
    int GetOrientation() const;

    /**
        Sets the orientation of the box sizer, either wxVERTICAL
        or wxHORIZONTAL.
    */
    void SetOrientation(int orient);

    /**
        Method which must be overridden in the derived sizer classes.

        The implementation should reposition the children using the current
        total size available to the sizer (@c m_size) and the size computed by
        the last call to CalcMin().

        @since 3.1.3, before this version RecalcSizes() method not taking any
            arguments had to be overridden in the derived classes instead.
    */
    virtual void RepositionChildren(const wxSize& minSize);
};

