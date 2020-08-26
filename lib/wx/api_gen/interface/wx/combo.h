/////////////////////////////////////////////////////////////////////////////
// Name:        combo.h
// Purpose:     interface of wxComboCtrl and wxComboPopup
// Author:      wxWidgets team
// Licence:     wxWindows licence
/////////////////////////////////////////////////////////////////////////////

//
// New window styles for wxComboCtrlBase
//
enum
{
    // Double-clicking a read-only combo triggers call to popup's OnComboPopup.
    // In wxOwnerDrawnComboBox, for instance, it cycles item.
    wxCC_SPECIAL_DCLICK             = 0x0100,

    // Dropbutton acts like standard push button.
    wxCC_STD_BUTTON                 = 0x0200
};


/**
    @class wxComboPopup

    In order to use a custom popup with wxComboCtrl, an interface class must be
    derived from wxComboPopup.

    For more information on how to use it, see @ref comboctrl_custompopup.

    @library{wxcore}
    @category{ctrl}

    @see wxComboCtrl
*/
class wxComboPopup
{
public:
    /**
        Default constructor. It is recommended that internal variables are
        prepared in Init() instead (because m_combo is not valid in
        constructor).
    */
    wxComboPopup();

    /**
        The derived class must implement this to create the popup control.

        @return @true if the call succeeded, @false otherwise.
    */
    virtual bool Create(wxWindow* parent) = 0;

    /**
        You only need to implement this member function if you create
        your popup class in non-standard way. The default implementation can
        handle both multiple-inherited popup control (as seen in wxComboCtrl
        samples) and one allocated separately in heap.

        If you do completely re-implement this function, make sure it calls
        Destroy() for the popup control and also deletes @a this object
        (usually as the last thing).
    */
    virtual void DestroyPopup();

    /**
        Utility function that hides the popup.
    */
    void Dismiss();

    /**
        Implement to customize matching of value string to an item container
        entry.

        @param item
            String entered, usually by user or from SetValue() call.

        @param trueItem
            When item matches an entry, but the entry's string representation
            is not exactly the same (case mismatch, for example), then the
            true item string should be written back to here, if it is not
            a NULL pointer.

        @remarks
            Default implementation always return true and does not alter
            trueItem.
    */
    virtual bool FindItem(const wxString& item, wxString* trueItem=NULL);

    /**
        The derived class may implement this to return adjusted size for the
        popup control, according to the variables given.

        @param minWidth
            Preferred minimum width.
        @param prefHeight
            Preferred height. May be -1 to indicate no preference.
        @param maxHeight
            Max height for window, as limited by screen size.

        @remarks This function is called each time popup is about to be shown.
    */
    virtual wxSize GetAdjustedSize(int minWidth, int prefHeight, int maxHeight);

    /**
        Returns pointer to the associated parent wxComboCtrl.
    */
    wxComboCtrl* GetComboCtrl() const;

    /**
        The derived class must implement this to return pointer to the
        associated control created in Create().
    */
    virtual wxWindow* GetControl() = 0;

    /**
        The derived class must implement this to return string representation
        of the value.
    */
    virtual wxString GetStringValue() const = 0;

    /**
        The derived class must implement this to initialize its internal
        variables. This method is called immediately after construction
        finishes. m_combo member variable has been initialized before the call.
    */
    virtual void Init();

    /**
        Utility method that returns @true if Create has been called.

        Useful in conjunction with LazyCreate().
    */
    bool IsCreated() const;

    /**
        The derived class may implement this to return @true if it wants to
        delay call to Create() until the popup is shown for the first time. It
        is more efficient, but on the other hand it is often more convenient to
        have the control created immediately.

        @remarks Base implementation returns @false.
    */
    virtual bool LazyCreate();

    /**
        The derived class may implement this to do something when the parent
        wxComboCtrl gets double-clicked.
    */
    virtual void OnComboDoubleClick();

    /**
        The derived class may implement this to receive key events from the
        parent wxComboCtrl.

        Events not handled should be skipped, as usual.
    */
    virtual void OnComboKeyEvent(wxKeyEvent& event);

    /**
        The derived class may implement this to do special processing when
        popup is hidden.
    */
    virtual void OnDismiss();

    /**
        The derived class may implement this to do special processing when
        popup is shown.
    */
    virtual void OnPopup();

    /**
        The derived class may implement this to paint the parent wxComboCtrl.

        Default implementation draws value as string.
    */
    virtual void PaintComboControl(wxDC& dc, const wxRect& rect);

    /**
        The derived class must implement this to receive string value changes
        from wxComboCtrl.
    */
    virtual void SetStringValue(const wxString& value);

protected:
    /**
        Parent wxComboCtrl. This member variable is prepared automatically
        before Init() is called.
    */
    wxComboCtrl*    m_combo;
};



/**
    Features enabled for wxComboCtrl.

    @see wxComboCtrl::GetFeatures()
*/
struct wxComboCtrlFeatures
{
    enum
    {
        MovableButton   = 0x0001, ///< Button can be on either side of control.
        BitmapButton    = 0x0002, ///< Button may be replaced with bitmap.
        ButtonSpacing   = 0x0004, ///< Button can have spacing from the edge
                                  ///< of the control.
        TextIndent      = 0x0008, ///< wxComboCtrl::SetMargins() can be used.
        PaintControl    = 0x0010, ///< Combo control itself can be custom painted.
        PaintWritable   = 0x0020, ///< A variable-width area in front of writable
                                  ///< combo control's textctrl can be custom
                                  ///< painted.
        Borderless      = 0x0040, ///< wxNO_BORDER window style works.

        All             = MovableButton | BitmapButton | ButtonSpacing |
                          TextIndent | PaintControl | PaintWritable |
                          Borderless ///< All features.
    };
};


/**
    @class wxComboCtrl

    A combo control is a generic combobox that allows totally custom popup. In
    addition it has other customization features. For instance, position and
    size of the dropdown button can be changed.

    @section comboctrl_custompopup Setting Custom Popup for wxComboCtrl

    wxComboCtrl needs to be told somehow which control to use and this is done
    by SetPopupControl(). However, we need something more than just a wxControl
    in this method as, for example, we need to call
    SetStringValue("initial text value") and wxControl doesn't have such
    method. So we also need a wxComboPopup which is an interface which must be
    implemented by a control to be usable as a popup.

    We couldn't derive wxComboPopup from wxControl as this would make it
    impossible to have a class deriving from a wxWidgets control and from it,
    so instead it is just a mix-in.

    Here's a minimal sample of wxListView popup:

    @code
    #include <wx/combo.h>
    #include <wx/listctrl.h>

    class wxListViewComboPopup : public wxListView, public wxComboPopup
    {
    public:
        // Initialize member variables
        virtual void Init()
        {
            m_value = -1;
        }

        // Create popup control
        virtual bool Create(wxWindow* parent)
        {
            return wxListView::Create(parent,1,wxPoint(0,0),wxDefaultSize);
        }

        // Return pointer to the created control
        virtual wxWindow *GetControl() { return this; }

        // Translate string into a list selection
        virtual void SetStringValue(const wxString& s)
        {
            int n = wxListView::FindItem(-1,s);
            if ( n >= 0 && n < wxListView::GetItemCount() )
                wxListView::Select(n);
        }

        // Get list selection as a string
        virtual wxString GetStringValue() const
        {
            if ( m_value >= 0 )
            return wxListView::GetItemText(m_value);
            return wxEmptyString;
        }

        // Do mouse hot-tracking (which is typical in list popups)
        void OnMouseMove(wxMouseEvent& event)
        {
            // TODO: Move selection to cursor
        }

        // On mouse left up, set the value and close the popup
        void OnMouseClick(wxMouseEvent& WXUNUSED(event))
        {
            m_value = wxListView::GetFirstSelected();

            // TODO: Send event as well

            Dismiss();
        }

    protected:

        int m_value; // current item index

    private:
        wxDECLARE_EVENT_TABLE();
    };

    wxBEGIN_EVENT_TABLE(wxListViewComboPopup, wxListView)
        EVT_MOTION(wxListViewComboPopup::OnMouseMove)
        EVT_LEFT_UP(wxListViewComboPopup::OnMouseClick)
    wxEND_EVENT_TABLE()
    @endcode

    Here's how you would create and populate it in a dialog constructor:

    @code
    wxComboCtrl* comboCtrl = new wxComboCtrl(this, wxID_ANY, wxEmptyString);

    wxListViewComboPopup* popupCtrl = new wxListViewComboPopup();

    // It is important to call SetPopupControl() as soon as possible
    comboCtrl->SetPopupControl(popupCtrl);

    // Populate using wxListView methods
    popupCtrl->InsertItem(popupCtrl->GetItemCount(), "First Item");
    popupCtrl->InsertItem(popupCtrl->GetItemCount(), "Second Item");
    popupCtrl->InsertItem(popupCtrl->GetItemCount(), "Third Item");
    @endcode

    @beginStyleTable
    @style{wxCB_READONLY}
           Text will not be editable.
    @style{wxCB_SORT}
           Sorts the entries in the list alphabetically.
    @style{wxTE_PROCESS_ENTER}
           The control will generate the event @c wxEVT_TEXT_ENTER
           (otherwise pressing Enter key is either processed internally by the
           control or used for navigation between dialog controls). Windows
           only.
    @style{wxCC_SPECIAL_DCLICK}
           Double-clicking triggers a call to popup's OnComboDoubleClick.
           Actual behaviour is defined by a derived class. For instance,
           wxOwnerDrawnComboBox will cycle an item. This style only applies if
           wxCB_READONLY is used as well.
    @style{wxCC_STD_BUTTON}
           Drop button will behave more like a standard push button.
    @endStyleTable

    @beginEventEmissionTable{wxCommandEvent}
    @event{EVT_TEXT(id, func)}
           Process a @c wxEVT_TEXT event, when the text changes.
    @event{EVT_TEXT_ENTER(id, func)}
           Process a @c wxEVT_TEXT_ENTER event, when RETURN is pressed in
           the combo control.
    @event{EVT_COMBOBOX_DROPDOWN(id, func)}
           Process a @c wxEVT_COMBOBOX_DROPDOWN event, which is generated
           when the popup window is shown (drops down).
    @event{EVT_COMBOBOX_CLOSEUP(id, func)}
           Process a @c wxEVT_COMBOBOX_CLOSEUP event, which is generated
           when the popup window of the combo control disappears (closes up).
           You should avoid adding or deleting items in this event.
    @endEventTable

    @library{wxcore}
    @category{ctrl}
    @appearance{comboctrl}

    @see wxComboBox, wxChoice, wxOwnerDrawnComboBox, wxComboPopup,
         wxCommandEvent
*/
class wxComboCtrl : public wxControl,
                    public wxTextEntry
{
public:
    /**
        Default constructor.
    */
    wxComboCtrl();

    /**
        Constructor, creating and showing a combo control.

        @param parent
            Parent window. Must not be @NULL.
        @param id
            Window identifier. The value wxID_ANY indicates a default value.
        @param value
            Initial selection string. An empty string indicates no selection.
        @param pos
            Window position.
            If ::wxDefaultPosition is specified then a default position is chosen.
        @param size
            Window size.
            If ::wxDefaultSize is specified then the window is sized appropriately.
        @param style
            Window style. See wxComboCtrl.
        @param validator
            Window validator.
        @param name
            Window name.

        @see Create(), wxValidator
    */
    wxComboCtrl(wxWindow* parent, wxWindowID id = wxID_ANY,
                const wxString& value = wxEmptyString,
                const wxPoint& pos = wxDefaultPosition,
                const wxSize& size = wxDefaultSize,
                long style = 0,
                const wxValidator& validator = wxDefaultValidator,
                const wxString& name = wxComboBoxNameStr);

    /**
        Destructor, destroying the combo control.
    */
    virtual ~wxComboCtrl();

    /**
        Copies the selected text to the clipboard.
    */
    virtual void Copy();

    /**
        Creates the combo control for two-step construction. Derived classes
        should call or replace this function. See wxComboCtrl() for further
        details.
    */
    bool Create(wxWindow* parent, wxWindowID id = wxID_ANY,
                const wxString& value = wxEmptyString,
                const wxPoint& pos = wxDefaultPosition,
                const wxSize& size = wxDefaultSize,
                long style = 0,
                const wxValidator& validator = wxDefaultValidator,
                const wxString& name = wxComboBoxNameStr);

    /**
        Copies the selected text to the clipboard and removes the selection.
    */
    virtual void Cut();

    /**
        Dismisses the popup window.

        Notice that calling this function will generate a
        @c wxEVT_COMBOBOX_CLOSEUP event.

        @since 2.9.2
    */
    virtual void Dismiss();


    /**
        Enables or disables popup animation, if any, depending on the value of
        the argument.
    */
    void EnablePopupAnimation(bool enable = true);


    /**
       Returns true if given key combination should toggle the popup.
    */
    virtual bool IsKeyPopupToggle(const wxKeyEvent& event) const;


    /**
       Prepare background of combo control or an item in a dropdown list in a
       way typical on platform. This includes painting the focus/disabled
       background and setting the clipping region.

       Unless you plan to paint your own focus indicator, you should always
       call this in your wxComboPopup::PaintComboControl implementation.  In
       addition, it sets pen and text colour to what looks good and proper
       against the background.

       flags: wxRendererNative flags:
              wxCONTROL_ISSUBMENU: is drawing a list item instead of combo control
              wxCONTROL_SELECTED: list item is selected
              wxCONTROL_DISABLED: control/item is disabled
    */
    virtual void PrepareBackground( wxDC& dc, const wxRect& rect, int flags ) const;

    /**
       Returns true if focus indicator should be drawn in the control.
    */
    bool ShouldDrawFocus() const;

    /**
        Returns disabled button bitmap that has been set with
        SetButtonBitmaps().

        @return A reference to the disabled state bitmap.
    */
    const wxBitmap& GetBitmapDisabled() const;

    /**
        Returns button mouse hover bitmap that has been set with
        SetButtonBitmaps().

        @return A reference to the mouse hover state bitmap.
    */
    const wxBitmap& GetBitmapHover() const;

    /**
        Returns default button bitmap that has been set with
        SetButtonBitmaps().

        @return A reference to the normal state bitmap.
    */
    const wxBitmap& GetBitmapNormal() const;

    /**
        Returns depressed button bitmap that has been set with
        SetButtonBitmaps().

        @return A reference to the depressed state bitmap.
    */
    const wxBitmap& GetBitmapPressed() const;

    /**
        Returns current size of the dropdown button.
    */
    wxSize GetButtonSize();

    /**
        Returns custom painted area in control.

        @see SetCustomPaintWidth().
    */
    int GetCustomPaintWidth() const;

    /**
        Returns features supported by wxComboCtrl. If needed feature is
        missing, you need to instead use wxGenericComboCtrl, which however may
        lack a native look and feel (but otherwise sports identical API).

        @return Value returned is a combination of the flags defined in
                wxComboCtrlFeatures.
    */
    static int GetFeatures();

    /**
        Returns the current hint string.

        See SetHint() for more information about hints.

        @since 2.9.1
    */
    virtual wxString GetHint() const;

    /**
        Returns the insertion point for the combo control's text field.

        @note Under Windows, this function always returns 0 if the combo
              control doesn't have the focus.
    */
    virtual long GetInsertionPoint() const;

    /**
        Returns the last position in the combo control text field.
    */
    virtual long GetLastPosition() const;

    /**
        Returns the margins used by the control. The @c x field of the returned
        point is the horizontal margin and the @c y field is the vertical one.

        @remarks If given margin cannot be accurately determined, its value
                will be set to -1.

        @see SetMargins()

        @since 2.9.1
    */
    wxPoint GetMargins() const;

    /**
        Returns current popup interface that has been set with
        SetPopupControl().
    */
    wxComboPopup* GetPopupControl();

    /**
        Returns popup window containing the popup control.
    */
    wxWindow* GetPopupWindow() const;

    /**
        Get the text control which is part of the combo control.
    */
    wxTextCtrl* GetTextCtrl() const;

    /**
        Returns actual indentation in pixels.

        @deprecated Use GetMargins() instead.
    */
    wxCoord GetTextIndent() const;

    /**
        Returns area covered by the text field (includes everything except
        borders and the dropdown button).
    */
    const wxRect& GetTextRect() const;

    /**
        Returns text representation of the current value. For writable combo
        control it always returns the value in the text field.
    */
    virtual wxString GetValue() const;

    /**
        Dismisses the popup window.

        @param generateEvent
            Set this to @true in order to generate
            @c wxEVT_COMBOBOX_CLOSEUP event.

        @deprecated Use Dismiss() instead.
    */
    virtual void HidePopup(bool generateEvent=false);

    /**
        Returns @true if the popup is currently shown
    */
    bool IsPopupShown() const;

    /**
        Returns @true if the popup window is in the given state. Possible
        values are:

        @beginTable
        @row2col{wxComboCtrl::Hidden,    Popup window is hidden.}
        @row2col{wxComboCtrl::Animating, Popup window is being shown, but the
                                         popup animation has not yet finished.}
        @row2col{wxComboCtrl::Visible,   Popup window is fully visible.}
        @endTable
    */
    bool IsPopupWindowState(int state) const;

    /**
        Implement in a derived class to define what happens on dropdown button
        click. Default action is to show the popup.

        @note If you implement this to do something else than show the popup,
              you must then also implement DoSetPopupControl() to always return
              @NULL.
    */
    virtual void OnButtonClick();

    /**
        Pastes text from the clipboard to the text field.
    */
    virtual void Paste();

    /**
        Shows the popup portion of the combo control.

        Notice that calling this function will generate a
        @c wxEVT_COMBOBOX_DROPDOWN event.

        @since 2.9.2
    */
    virtual void Popup();

    /**
        Removes the text between the two positions in the combo control text
        field.

        @param from
            The first position.
        @param to
            The last position.
    */
    virtual void Remove(long from, long to);

    /**
        Replaces the text between two positions with the given text, in the
        combo control text field.

        @param from
            The first position.
        @param to
            The second position.
        @param text
            The text to insert.
    */
    virtual void Replace(long from, long to, const wxString& text);

    /**
        Sets custom dropdown button graphics.

        @param bmpNormal
            Default button image.
        @param pushButtonBg
            If @true, blank push button background is painted below the image.
        @param bmpPressed
            Depressed button image.
        @param bmpHover
            Button image when mouse hovers above it. This should be ignored on
            platforms and themes that do not generally draw different kind of
            button on mouse hover.
        @param bmpDisabled
            Disabled button image.
    */
    void SetButtonBitmaps(const wxBitmap& bmpNormal,
                          bool pushButtonBg = false,
                          const wxBitmap& bmpPressed = wxNullBitmap,
                          const wxBitmap& bmpHover = wxNullBitmap,
                          const wxBitmap& bmpDisabled = wxNullBitmap);

    /**
        Sets size and position of dropdown button.

        @param width
            Button width. Value = 0 specifies default.
        @param height
            Button height. Value = 0 specifies default.
        @param side
            Indicates which side the button will be placed. Value can be wxLEFT
            or wxRIGHT.
        @param spacingX
            Horizontal spacing around the button. Default is 0.
    */
    void SetButtonPosition(int width = -1, int height = -1,
                           int side = wxRIGHT, int spacingX = 0);

    /**
        Set width, in pixels, of custom painted area in control without
        @c wxCB_READONLY style. In read-only wxOwnerDrawnComboBox, this is used
        to indicate area that is not covered by the focus rectangle.
    */
    void SetCustomPaintWidth(int width);

    /**
        Sets a hint shown in an empty unfocused combo control.

        Notice that hints are known as <em>cue banners</em> under MSW or
        <em>placeholder strings</em> under macOS.

        @see wxTextEntry::SetHint()

        @since 2.9.1
    */
    virtual bool SetHint(const wxString& hint);

    /**
        Sets the insertion point in the text field.

        @param pos
            The new insertion point.
    */
    virtual void SetInsertionPoint(long pos);

    /**
        Sets the insertion point at the end of the combo control text field.
    */
    virtual void SetInsertionPointEnd();

    //@{
    /**
        Attempts to set the control margins. When margins are given as wxPoint,
        x indicates the left and y the top margin. Use -1 to indicate that
        an existing value should be used.

        @return
            @true if setting of all requested margins was successful.

        @since 2.9.1
    */
    bool SetMargins(const wxPoint& pt);
    bool SetMargins(wxCoord left, wxCoord top = -1);
    //@}

    /**
        Set side of the control to which the popup will align itself. Valid
        values are @c wxLEFT, @c wxRIGHT and 0. The default value 0 means that
        the most appropriate side is used (which, currently, is always
        @c wxLEFT).
    */
    void SetPopupAnchor(int anchorSide);

    /**
        Set popup interface class derived from wxComboPopup. This method should
        be called as soon as possible after the control has been created,
        unless OnButtonClick() has been overridden.
    */
    void SetPopupControl(wxComboPopup* popup);

    /**
        Extends popup size horizontally, relative to the edges of the combo
        control.

        @param extLeft
            How many pixel to extend beyond the left edge of the control.
            Default is 0.
        @param extRight
            How many pixel to extend beyond the right edge of the control.
            Default is 0.

        @remarks Popup minimum width may override arguments. It is up to the
                 popup to fully take this into account.
    */
    void SetPopupExtents(int extLeft, int extRight);

    /**
        Sets preferred maximum height of the popup.

        @remarks Value -1 indicates the default.
    */
    void SetPopupMaxHeight(int height);

    /**
        Sets minimum width of the popup. If wider than combo control, it will
        extend to the left.

        @remarks Value -1 indicates the default. Also, popup implementation may
                 choose to ignore this.
    */
    void SetPopupMinWidth(int width);

    /**
        Selects the text between the two positions, in the combo control text
        field.

        @param from
            The first position.
        @param to
            The second position.
    */
    virtual void SetSelection(long from, long to);

    /**
        Sets the text for the text field without affecting the popup. Thus,
        unlike SetValue(), it works equally well with combo control using
        @c wxCB_READONLY style.
    */
    void SetText(const wxString& value);

    /**
        Set a custom window style for the embedded wxTextCtrl. Usually you
        will need to use this during two-step creation, just before Create().
        For example:

        @code
            wxComboCtrl* comboCtrl = new wxComboCtrl();

            // Let's make the text right-aligned
            comboCtrl->SetTextCtrlStyle(wxTE_RIGHT);

            comboCtrl->Create(parent, wxID_ANY, wxEmptyString);
        @endcode
    */
    void SetTextCtrlStyle( int style );

    /**
        This will set the space in pixels between left edge of the control and
        the text, regardless whether control is read-only or not. Value -1 can
        be given to indicate platform default.

        @deprecated Use SetMargins() instead.
    */
    void SetTextIndent(int indent);

    /**
        Sets the text for the combo control text field.

        @note For a combo control with @c wxCB_READONLY style the string must
              be accepted by the popup (for instance, exist in the dropdown
              list), otherwise the call to SetValue() is ignored.
    */
    virtual void SetValue(const wxString& value);

    /**
       Changes value of the control as if user had done it by selecting an
       item from a combo box drop-down list.
     */
    void SetValueByUser(const wxString& value);

    /**
        Show the popup.

        @deprecated Use Popup() instead.
    */
    virtual void ShowPopup();

    /**
        Undoes the last edit in the text field. Windows only.
    */
    virtual void Undo();

    /**
        Enable or disable usage of an alternative popup window, which
        guarantees ability to focus the popup control, and allows common native
        controls to function normally. This alternative popup window is usually
        a wxDialog, and as such, when it is shown, its parent top-level window
        will appear as if the focus has been lost from it.
    */
    void UseAltPopupWindow(bool enable = true);

protected:

    /**
        This member function is not normally called in application code.
        Instead, it can be implemented in a derived class to create a custom
        popup animation.

        The parameters are the same as those for DoShowPopup().

        @return @true if animation finishes before the function returns,
                @false otherwise. In the latter case you need to manually call
                DoShowPopup() after the animation ends.
    */
    virtual bool AnimateShow(const wxRect& rect, int flags);

    /**
        This member function is not normally called in application code.
        Instead, it can be implemented in a derived class to return default
        wxComboPopup, in case @a popup is @NULL.

        @note If you have implemented OnButtonClick() to do something else than
              show the popup, then DoSetPopupControl() must always set @a popup
              to @NULL.
    */
    virtual void DoSetPopupControl(wxComboPopup* popup);

    /**
        Flags for DoShowPopup() and AnimateShow().
    */
    enum
    {
        ShowBelow       = 0x0000, //!< Show popup below the control.
        ShowAbove       = 0x0001, //!< Show popup above the control.
        CanDeferShow    = 0x0002  //!< Can only return true from AnimateShow() if this is set.
    };

    /**
        This member function is not normally called in application code.
        Instead, it must be called in a derived class to make sure popup is
        properly shown after a popup animation has finished (but only if
        AnimateShow() did not finish the animation within its function scope).

        @param rect
            Position to show the popup window at, in screen coordinates.
        @param flags
            Combination of any of the following: wxComboCtrl::ShowAbove,
            and wxComboCtrl::CanDeferShow.
    */
    virtual void DoShowPopup(const wxRect& rect, int flags);
};

