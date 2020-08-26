/////////////////////////////////////////////////////////////////////////////
// Name:        datectrl.h
// Purpose:     interface of wxDatePickerCtrl
// Author:      wxWidgets team
// Licence:     wxWindows licence
/////////////////////////////////////////////////////////////////////////////

/// wxDatePickerCtrl styles
enum
{
    /// default style on this platform, either wxDP_SPIN or wxDP_DROPDOWN
    wxDP_DEFAULT = 0,

    /// a spin control-like date picker (not supported in generic version)
    wxDP_SPIN = 1,

    /// a combobox-like date picker (not supported in mac version)
    wxDP_DROPDOWN = 2,

    /// always show century in the default date display (otherwise it depends on
    /// the system date format which may include the century or not)
    wxDP_SHOWCENTURY = 4,

    /// allow not having any valid date in the control (by default it always has
    /// some date, today initially if no valid date specified in ctor)
    wxDP_ALLOWNONE = 8
};


/**
    @class wxDatePickerCtrl

    This control allows the user to select a date. Unlike wxCalendarCtrl, which
    is a relatively big control, wxDatePickerCtrl is implemented as a small
    window showing the currently selected date. The control can be edited using
    the keyboard, and can also display a popup window for more user-friendly
    date selection, depending on the styles used and the platform.

    It is only available if @c wxUSE_DATEPICKCTRL is set to 1.

    @beginStyleTable
    @style{wxDP_SPIN}
           Creates a control without a month calendar drop down but with
           spin-control-like arrows to change individual date components. This
           style is not supported by the generic version.
    @style{wxDP_DROPDOWN}
           Creates a control with a month calendar drop-down part from which
           the user can select a date. This style is not supported in OSX/Cocoa
           native version.
    @style{wxDP_DEFAULT}
           Creates a control with the style that is best supported for the
           current platform (currently wxDP_SPIN under Windows and OSX/Cocoa
           and wxDP_DROPDOWN elsewhere).
    @style{wxDP_ALLOWNONE}
           With this style, the control allows the user to not enter any valid
           date at all. Without it - the default - the control always has some
           valid date. This style is not supported in OSX/Cocoa native version.
    @style{wxDP_SHOWCENTURY}
           Forces display of the century in the default date format. Without
           this style the century could be displayed, or not, depending on the
           default date representation in the system. This style is not
           supported in OSX/Cocoa native version currently.
    @endStyleTable

    As can be seen from the remarks above, most of the control style are only
    supported in the native MSW implementation. In portable code it's
    recommended to use @c wxDP_DEFAULT style only, possibly combined with @c
    wxDP_SHOWCENTURY (this is also the style used by default if none is
    specified).

    @beginEventEmissionTable{wxDateEvent}
    @event{EVT_DATE_CHANGED(id, func)}
           Process a wxEVT_DATE_CHANGED event, which fires when the user
           changes the current selection in the control.
    @endEventTable

    @library{wxcore}
    @category{pickers}
    @appearance{datepickerctrl}

    @see wxTimePickerCtrl, wxCalendarCtrl, wxDateEvent
*/
class wxDatePickerCtrl : public wxControl
{
public:
    /**
       Default constructor.
    */
    wxDatePickerCtrl();

    /**
        Initializes the object and calls Create() with all the parameters.
    */
    wxDatePickerCtrl(wxWindow* parent, wxWindowID id,
                     const wxDateTime& dt = wxDefaultDateTime,
                     const wxPoint& pos = wxDefaultPosition,
                     const wxSize& size = wxDefaultSize,
                     long style = wxDP_DEFAULT | wxDP_SHOWCENTURY,
                     const wxValidator& validator = wxDefaultValidator,
                     const wxString& name = "datectrl");

    /**
        Create the control window.

        This method should only be used for objects created using default
        constructor.

        @param parent
            Parent window, must not be non-@NULL.
        @param id
            The identifier for the control.
        @param dt
            The initial value of the control, if an invalid date (such as the
            default value) is used, the control is set to today.
        @param pos
            Initial position.
        @param size
            Initial size. If left at default value, the control chooses its own
            best size by using the height approximately equal to a text control
            and width large enough to show the date string fully.
        @param style
            The window style, see the description of the styles in the class
            documentation.
        @param validator
            Validator which can be used for additional date checks.
        @param name
            Control name.

        @return @true if the control was successfully created or @false if
                 creation failed.
    */
    bool Create(wxWindow* parent, wxWindowID id,
                const wxDateTime& dt = wxDefaultDateTime,
                const wxPoint& pos = wxDefaultPosition,
                const wxSize& size = wxDefaultSize,
                long style = wxDP_DEFAULT | wxDP_SHOWCENTURY,
                const wxValidator& validator = wxDefaultValidator,
                const wxString& name = "datectrl");

    /**
        If the control had been previously limited to a range of dates using
        SetRange(), returns the lower and upper bounds of this range. If no
        range is set (or only one of the bounds is set), @a dt1 and/or @a dt2
        are set to be invalid.

        Notice that when using a native MSW implementation of this control the
        lower range is always set, even if SetRange() hadn't been called
        explicitly, as the native control only supports dates later than year
        1601.

        @param dt1
            Pointer to the object which receives the lower range limit or
            becomes invalid if it is not set. May be @NULL if the caller is not
            interested in lower limit.
        @param dt2
            Same as above but for the upper limit.

        @return @false if no range limits are currently set, @true if at least
                 one bound is set.
    */
    virtual bool GetRange(wxDateTime* dt1, wxDateTime* dt2) const;

    /**
        Returns the currently entered date.

        For a control with @c wxDP_ALLOWNONE style the returned value may be
        invalid if no date is entered, otherwise it is always valid.
    */
    virtual wxDateTime GetValue() const;

    /**
        Sets the valid range for the date selection. If @a dt1 is valid, it
        becomes the earliest date (inclusive) accepted by the control. If
        @a dt2 is valid, it becomes the latest possible date.

        Notice that if the current value is not inside the new range, it will
        be adjusted to lie inside it, i.e. calling this method can change the
        control value, however no events are generated by it.

        @remarks If the current value of the control is outside of the newly
                 set range bounds, the behaviour is undefined.
    */
    virtual void SetRange(const wxDateTime& dt1, const wxDateTime& dt2);

    /**
        Changes the current value of the control.

        The date should be valid unless the control was created with @c
        wxDP_ALLOWNONE style and included in the currently selected range, if
        any.

        Calling this method does not result in a date change event.
    */
    virtual void SetValue(const wxDateTime& dt);
};

