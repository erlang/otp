/////////////////////////////////////////////////////////////////////////////
// Name:        wx/timectrl.h
// Purpose:     interface of wxTimePickerCtrl
// Author:      Vadim Zeitlin
// Created:     2011-09-22
// Copyright:   (c) 2011 Vadim Zeitlin <vadim@wxwidgets.org>
// Licence:     wxWindows licence
/////////////////////////////////////////////////////////////////////////////

/**
    Styles used with wxTimePickerCtrl.

    Currently no special styles are defined for this object.

    @library{wxcore}
    @category{pickers}

    @since 2.9.3
 */
enum
{
    wxTP_DEFAULT = 0
};

/**
    @class wxTimePickerCtrl

    This control allows the user to enter time.

    It is similar to wxDatePickerCtrl but is used for time, and not date,
    selection. While GetValue() and SetValue() still work with values of type
    wxDateTime (because wxWidgets doesn't provide a time-only class), their
    date part is ignored by this control.

    It is only available if @c wxUSE_TIMEPICKCTRL is set to 1.

    This control currently doesn't have any specific flags.

    @beginEventEmissionTable{wxDateEvent}
    @event{EVT_TIME_CHANGED(id, func)}
           Process a wxEVT_TIME_CHANGED event, which fires when the user
           changes the current selection in the control.
    @endEventTable

    @library{wxcore}
    @category{pickers}
    @appearance{timepickerctrl}

    @see wxDatePickerCtrl, wxCalendarCtrl, wxDateEvent

    @since 2.9.3
*/
class wxTimePickerCtrl : public wxControl
{
public:
    /**
       Default constructor.
    */
    wxTimePickerCtrl();

    /**
        Initializes the object and calls Create() with all the parameters.
    */
    wxTimePickerCtrl(wxWindow* parent, wxWindowID id,
                     const wxDateTime& dt = wxDefaultDateTime,
                     const wxPoint& pos = wxDefaultPosition,
                     const wxSize& size = wxDefaultSize,
                     long style = wxTP_DEFAULT,
                     const wxValidator& validator = wxDefaultValidator,
                     const wxString& name = wxTimePickerCtrlNameStr);

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
            default value) is used, the control is set to current time.
        @param pos
            Initial position.
        @param size
            Initial size. If left at default value, the control chooses its own
            best size by using the height approximately equal to a text control
            and width large enough to show the time fully.
        @param style
            The window style, should be left at 0 as there are no special
            styles for this control in this version.
        @param validator
            Validator which can be used for additional checks.
        @param name
            Control name.

        @return @true if the control was successfully created or @false if
                 creation failed.
    */
    bool Create(wxWindow* parent, wxWindowID id,
                const wxDateTime& dt = wxDefaultDateTime,
                const wxPoint& pos = wxDefaultPosition,
                const wxSize& size = wxDefaultSize,
                long style = wxTP_DEFAULT,
                const wxValidator& validator = wxDefaultValidator,
                const wxString& name = wxTimePickerCtrlNameStr);

    /**
        Returns the currently entered time as hours, minutes and seconds.

        All the arguments must be non-@NULL, @false is returned otherwise and
        none of them is modified.

        @see SetTime()

        @since 2.9.4
     */
    bool GetTime(int* hour, int* min, int* sec) const;

    /**
        Returns the currently entered time.

        The date part of the returned wxDateTime object is always set to today
        and should be ignored, only the time part is relevant.
    */
    virtual wxDateTime GetValue() const;

    /**
        Changes the current time of the control.

        Calling this method does not result in a time change event.

        @param hour The new hour value in 0..23 interval.
        @param min The new minute value in 0..59 interval.
        @param sec The new second value in 0..59 interval.
        @return @true if the time was changed or @false on failure, e.g. if the
            time components were invalid.

        @see GetTime()

        @since 2.9.4
     */
    bool SetTime(int hour, int min, int sec);

    /**
        Changes the current value of the control.

        The date part of @a dt is ignored, only the time part is displayed in
        the control. The @a dt object must however be valid.

        In particular, notice that it is a bad idea to use default wxDateTime
        constructor from hour, minute and second values as it uses the today
        date for the date part, which means that some values can be invalid if
        today happens to be the day of DST change. For example, when switching
        to summer time, the time 2:00 typically doesn't exist as the clocks jump
        directly to 3:00. To avoid this problem, use a fixed date on which DST
        is known not to change (e.g. Jan 1, 2012) for the date part of the
        argument or use SetTime().

        Calling this method does not result in a time change event.
    */
    virtual void SetValue(const wxDateTime& dt);
};
