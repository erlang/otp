/////////////////////////////////////////////////////////////////////////////
// Name:        calctrl.h
// Purpose:     interface of wxCalendarCtrl
// Author:      wxWidgets team
// Licence:     wxWindows licence
/////////////////////////////////////////////////////////////////////////////

enum
{
    // show Sunday as the first day of the week (default)
    wxCAL_SUNDAY_FIRST               = 0x0080,

    // show Monday as the first day of the week
    wxCAL_MONDAY_FIRST               = 0x0001,

    // highlight holidays
    wxCAL_SHOW_HOLIDAYS              = 0x0002,

    // disable the year change control, show only the month change one
    // deprecated
    wxCAL_NO_YEAR_CHANGE             = 0x0004,

    // don't allow changing neither month nor year (implies
    // wxCAL_NO_YEAR_CHANGE)
    wxCAL_NO_MONTH_CHANGE            = 0x000c,

    // use MS-style month-selection instead of combo-spin combination
    wxCAL_SEQUENTIAL_MONTH_SELECTION = 0x0010,

    // show the neighbouring weeks in the previous and next month
    wxCAL_SHOW_SURROUNDING_WEEKS     = 0x0020,

    // show week numbers on the left side of the calendar.
    wxCAL_SHOW_WEEK_NUMBERS          = 0x0040
};


/**
    @class wxCalendarEvent

    The wxCalendarEvent class is used together with wxCalendarCtrl.

    @library{wxcore}
    @category{events}

    @see wxCalendarCtrl
*/
class wxCalendarEvent : public wxDateEvent
{
public:
    wxCalendarEvent();
    wxCalendarEvent(wxWindow *win, const wxDateTime& dt, wxEventType type);

    /**
        Returns the week day on which the user clicked in
        @c EVT_CALENDAR_WEEKDAY_CLICKED handler. It doesn't make sense to call
        this function in other handlers.
    */
    wxDateTime::WeekDay GetWeekDay() const;

    /**
        Sets the week day carried by the event, normally only used by the
        library internally.
    */
    void SetWeekDay(wxDateTime::WeekDay day);
};

wxEventType wxEVT_CALENDAR_SEL_CHANGED;
wxEventType wxEVT_CALENDAR_PAGE_CHANGED;
wxEventType wxEVT_CALENDAR_DOUBLECLICKED;
wxEventType wxEVT_CALENDAR_WEEKDAY_CLICKED;
wxEventType wxEVT_CALENDAR_WEEK_CLICKED;



/**
    Possible kinds of borders which may be used to decorate a date using
    wxCalendarDateAttr.
*/
enum wxCalendarDateBorder
{
    wxCAL_BORDER_NONE,      ///< No Border (Default)
    wxCAL_BORDER_SQUARE,    ///< Rectangular Border
    wxCAL_BORDER_ROUND      ///< Round Border
};

/**
    @class wxCalendarDateAttr

    wxCalendarDateAttr is a custom attributes for a calendar date. The objects
    of this class are used with wxCalendarCtrl.

    @library{wxcore}
    @category{data}

    @see wxCalendarCtrl
*/
class wxCalendarDateAttr
{
public:
    /**
        Constructor for specifying all wxCalendarDateAttr properties.
    */
    wxCalendarDateAttr(const wxColour& colText = wxNullColour,
                       const wxColour& colBack = wxNullColour,
                       const wxColour& colBorder = wxNullColour,
                       const wxFont& font = wxNullFont,
                       wxCalendarDateBorder border = wxCAL_BORDER_NONE);

    /**
        Constructor using default properties except the given border.
    */
    wxCalendarDateAttr(wxCalendarDateBorder border,
                       const wxColour& colBorder = wxNullColour);

    /**
        Returns the background colour set for the calendar date.
    */
    const wxColour& GetBackgroundColour() const;

    /**
        Returns the border set for the calendar date.
    */
    wxCalendarDateBorder GetBorder() const;

    /**
        Returns the border colour set for the calendar date.
    */
    const wxColour& GetBorderColour() const;

    /**
        Returns the font set for the calendar date.
    */
    const wxFont& GetFont() const;

    /**
        Returns the text colour set for the calendar date.
    */
    const wxColour& GetTextColour() const;

    /**
        Returns @true if a non-default text background colour is set.
    */
    bool HasBackgroundColour() const;

    /**
        Returns @true if a non-default (i.e.\ any) border is set.
    */
    bool HasBorder() const;

    /**
        Returns @true if a non-default border colour is set.
    */
    bool HasBorderColour() const;

    /**
        Returns @true if a non-default font is set.
    */
    bool HasFont() const;

    /**
        Returns @true if a non-default text foreground colour is set.
    */
    bool HasTextColour() const;

    /**
        Returns @true if this calendar day is displayed as a holiday.
    */
    bool IsHoliday() const;

    /**
        Sets the text background colour to use.
    */
    void SetBackgroundColour(const wxColour& colBack);

    /**
        Sets the border to use.
    */
    void SetBorder(wxCalendarDateBorder border);

    /**
        Sets the border colour to use.
    */
    void SetBorderColour(const wxColour& col);

    /**
        Sets the font to use.
    */
    void SetFont(const wxFont& font);

    /**
        If @a holiday is @true, this calendar day will be displayed as a
        holiday.
    */
    void SetHoliday(bool holiday);

    /**
        Sets the text (foreground) colour to use.
    */
    void SetTextColour(const wxColour& colText);

    /**
        Used (internally) by the generic wxCalendarCtrl::Mark().
    */
    static const wxCalendarDateAttr& GetMark();

    /**
        Set the attributes that will be used to Mark() days on the generic
        wxCalendarCtrl.
    */
    static void SetMark(const wxCalendarDateAttr& m);
};



/**
    Possible return values from wxCalendarCtrl::HitTest().
*/
enum wxCalendarHitTestResult
{
    wxCAL_HITTEST_NOWHERE,  ///< Hit outside of anything.
    wxCAL_HITTEST_HEADER,   ///< Hit on the header (weekdays).
    wxCAL_HITTEST_DAY,      ///< Hit on a day in the calendar.
    wxCAL_HITTEST_INCMONTH, ///< Hit on next month arrow (in alternate month selector mode).
    wxCAL_HITTEST_DECMONTH, ///< Hit on previous month arrow (in alternate month selector mode).
    wxCAL_HITTEST_SURROUNDING_WEEK, ///< Hit on surrounding week of previous/next month (if shown).
    wxCAL_HITTEST_WEEK      ///< Hit on week of the year number (if shown).
};

/**
    @class wxCalendarCtrl

    The calendar control allows the user to pick a date.  The user can move the
    current selection using the keyboard and select the date (generating
    @c EVT_CALENDAR event) by pressing @c @<Return@> or double clicking it.

    Generic calendar has advanced possibilities for the customization of its
    display, described below. If you want to use these possibilities on every
    platform, use wxGenericCalendarCtrl instead of wxCalendarCtrl.

    All global settings (such as colours and fonts used) can, of course, be
    changed. But also, the display style for each day in the month can be set
    independently using wxCalendarDateAttr class.

    An item without custom attributes is drawn with the default colours and
    font and without border, but setting custom attributes with SetAttr()
    allows modifying its appearance. Just create a custom attribute object and
    set it for the day you want to be displayed specially (note that the
    control will take ownership of the pointer, i.e. it will delete it itself).
    A day may be marked as being a holiday, even if it is not recognized as
    one by wxDateTime using the wxCalendarDateAttr::SetHoliday() method.

    As the attributes are specified for each day, they may change when the
    month is changed, so you will often want to update them in
    @c EVT_CALENDAR_PAGE_CHANGED event handler.

    If neither the @c wxCAL_SUNDAY_FIRST or @c wxCAL_MONDAY_FIRST style is given,
    the first day of the week is determined from operating system's settings,
    if possible. The native wxGTK calendar chooses the first weekday based on
    locale, and these styles have no effect on it.

    @beginStyleTable
    @style{wxCAL_SUNDAY_FIRST}
           Show Sunday as the first day in the week (not in wxGTK)
    @style{wxCAL_MONDAY_FIRST}
           Show Monday as the first day in the week (not in wxGTK)
    @style{wxCAL_SHOW_HOLIDAYS}
           Highlight holidays in the calendar (only generic)
    @style{wxCAL_NO_YEAR_CHANGE}
           Disable the year changing (deprecated, only generic)
    @style{wxCAL_NO_MONTH_CHANGE}
           Disable the month (and, implicitly, the year) changing
    @style{wxCAL_SHOW_SURROUNDING_WEEKS}
           Show the neighbouring weeks in the previous and next months
           (only generic, always on for the native controls)
    @style{wxCAL_SEQUENTIAL_MONTH_SELECTION}
           Use alternative, more compact, style for the month and year
           selection controls. (only generic)
    @style{wxCAL_SHOW_WEEK_NUMBERS}
           Show week numbers on the left side of the calendar. (not in generic)
    @endStyleTable

    @beginEventEmissionTable{wxCalendarEvent}
    @event{EVT_CALENDAR(id, func)}
           A day was double clicked in the calendar.
    @event{EVT_CALENDAR_SEL_CHANGED(id, func)}
           The selected date changed.
    @event{EVT_CALENDAR_PAGE_CHANGED(id, func)}
           The selected month (and/or year) changed.
    @event{EVT_CALENDAR_WEEKDAY_CLICKED(id, func)}
           User clicked on the week day header (only generic).
    @event{EVT_CALENDAR_WEEK_CLICKED(id, func)}
           User clicked on the week of the year number (only generic).
    @endEventTable

    @note Changing the selected date will trigger an EVT_CALENDAR_DAY, MONTH or
          YEAR event as well as an EVT_CALENDAR_SEL_CHANGED event.

    @library{wxcore}
    @category{ctrl}
    @appearance{calendarctrl}

    @nativeimpl{wxgtk,wxmsw}

    @see @ref page_samples_calendar, wxCalendarDateAttr, wxCalendarEvent,
         wxDatePickerCtrl
*/
class wxCalendarCtrl : public wxControl
{
public:
    /**
        Default constructor.
    */
    wxCalendarCtrl();

    /**
        Does the same as Create() method.
    */
    wxCalendarCtrl(wxWindow* parent, wxWindowID id,
                   const wxDateTime& date = wxDefaultDateTime,
                   const wxPoint& pos = wxDefaultPosition,
                   const wxSize& size = wxDefaultSize,
                   long style = wxCAL_SHOW_HOLIDAYS,
                   const wxString& name = wxCalendarNameStr);

    /**
        Destroys the control.
    */
    ~wxCalendarCtrl();

    /**
        Creates the control. See wxWindow::wxWindow() for the meaning of the
        parameters and the control overview for the possible styles.
    */
    bool Create(wxWindow* parent, wxWindowID id,
                const wxDateTime& date = wxDefaultDateTime,
                const wxPoint& pos = wxDefaultPosition,
                const wxSize& size = wxDefaultSize,
                long style = wxCAL_SHOW_HOLIDAYS,
                const wxString& name = wxCalendarNameStr);

    /**
        This function should be used instead of changing @c wxCAL_SHOW_HOLIDAYS
        style bit directly. It enables or disables the special highlighting of
        the holidays.
    */
    virtual void EnableHolidayDisplay(bool display = true);

    /**
        This function should be used instead of changing
        @c wxCAL_NO_MONTH_CHANGE style bit. It allows or disallows the user to
        change the month interactively. Note that if the month cannot be
        changed, the year cannot be changed neither.

        @return @true if the value of this option really changed or @false if
                it was already set to the requested value.
    */
    virtual bool EnableMonthChange(bool enable = true);

    /**
        @deprecated

        This function should be used instead of changing
        @c wxCAL_NO_YEAR_CHANGE style bit directly. It allows or disallows the
        user to change the year interactively. Only in generic wxCalendarCtrl.
    */
    virtual void EnableYearChange(bool enable = true);

    /**
        Returns the attribute for the given date (should be in the range
        1...31). The returned pointer may be @NULL. Only in generic
        wxCalendarCtrl.
    */
    virtual wxCalendarDateAttr* GetAttr(size_t day) const;

    /**
        Gets the currently selected date.
    */
    virtual wxDateTime GetDate() const;

    /**
        Gets the background colour of the header part of the calendar window.

        This method is currently only implemented in generic wxCalendarCtrl and
        always returns @c wxNullColour in the native versions.

        @see SetHeaderColours()
    */
    virtual const wxColour& GetHeaderColourBg() const;

    /**
        Gets the foreground colour of the header part of the calendar window.

        This method is currently only implemented in generic wxCalendarCtrl and
        always returns @c wxNullColour in the native versions.

        @see SetHeaderColours()
    */
    virtual const wxColour& GetHeaderColourFg() const;

    /**
        Gets the background highlight colour. Only in generic wxCalendarCtrl.

        This method is currently only implemented in generic wxCalendarCtrl and
        always returns @c wxNullColour in the native versions.

        @see SetHighlightColours()
    */
    virtual const wxColour& GetHighlightColourBg() const;

    /**
        Gets the foreground highlight colour. Only in generic wxCalendarCtrl.

        This method is currently only implemented in generic wxCalendarCtrl and
        always returns @c wxNullColour in the native versions.

        @see SetHighlightColours()
    */
    virtual const wxColour& GetHighlightColourFg() const;

    /**
        Return the background colour currently used for holiday highlighting.

        Only useful with generic wxCalendarCtrl as native versions currently
        don't support holidays display at all and always return
        @c wxNullColour.

        @see SetHolidayColours()
    */
    virtual const wxColour& GetHolidayColourBg() const;

    /**
        Return the foreground colour currently used for holiday highlighting.

        Only useful with generic wxCalendarCtrl as native versions currently
        don't support holidays display at all and always return
        @c wxNullColour.

        @see SetHolidayColours()
    */
    virtual const wxColour& GetHolidayColourFg() const;

    /**
        Returns one of wxCalendarHitTestResult constants and fills either
        @a date or @a wd pointer with the corresponding value depending on the
        hit test code.

        Not implemented in wxGTK currently.
    */
    virtual wxCalendarHitTestResult HitTest(const wxPoint& pos,
                                            wxDateTime* date = NULL,
                                            wxDateTime::WeekDay* wd = NULL);

    /**
        Clears any attributes associated with the given day (in the range
        1...31). Only in generic wxCalendarCtrl.
    */
    virtual void ResetAttr(size_t day);

    /**
        Associates the attribute with the specified date (in the range 1...31).
        If the pointer is @NULL, the items attribute is cleared. Only in
        generic wxCalendarCtrl.
    */
    virtual void SetAttr(size_t day, wxCalendarDateAttr* attr);

    /**
        Sets the current date.

        The @a date parameter must be valid and in the currently valid range as
        set by SetDateRange(), otherwise the current date is not changed and
        the function returns @false and, additionally, triggers an assertion
        failure if the date is invalid.
    */
    virtual bool SetDate(const wxDateTime& date);

    /**
        Set the colours used for painting the weekdays at the top of the
        control.

        This method is currently only implemented in generic wxCalendarCtrl and
        does nothing in the native versions.
    */
    virtual void SetHeaderColours(const wxColour& colFg,
                                  const wxColour& colBg);

    /**
        Set the colours to be used for highlighting the currently selected
        date.

        This method is currently only implemented in generic wxCalendarCtrl and
        does nothing in the native versions.
    */
    virtual void SetHighlightColours(const wxColour& colFg,
                                     const wxColour& colBg);

    /**
        Marks the specified day as being a holiday in the current month.

        This method is only implemented in the generic version of the control
        and does nothing in the native ones.
    */
    virtual void SetHoliday(size_t day);

    /**
        Sets the colours to be used for the holidays highlighting.

        This method is only implemented in the generic version of the control
        and does nothing in the native ones. It should also only be called if
        the window style includes @c wxCAL_SHOW_HOLIDAYS flag or
        EnableHolidayDisplay() had been called.

    */
    virtual void SetHolidayColours(const wxColour& colFg,
                                   const wxColour& colBg);

    /**
        Mark or unmark the day. This day of month will be marked in every
        month. In generic wxCalendarCtrl,
    */
    virtual void Mark(size_t day, bool mark);

    /**
        @name Date Range Functions
     */
    //@{

    /**
        Restrict the dates that can be selected in the control to the specified
        range.

        If either date is set, the corresponding limit will be enforced and
        @true returned. If none are set, the existing restrictions are removed
        and @false is returned.

        @see GetDateRange()

        @param lowerdate
            The low limit for the dates shown by the control or
            ::wxDefaultDateTime.
        @param upperdate
            The high limit for the dates shown by the control or
            ::wxDefaultDateTime.
        @return
            @true if either limit is valid, @false otherwise
     */
    virtual bool SetDateRange(const wxDateTime& lowerdate = wxDefaultDateTime,
                                const wxDateTime& upperdate = wxDefaultDateTime);

    /**
        Returns the limits currently being used.

        @see SetDateRange()

        @param lowerdate
            If non-@NULL, the value of the low limit for the dates shown by the
            control is returned (which may be ::wxDefaultDateTime if no limit
            is set).
        @param upperdate
            If non-@NULL, the value of the upper limit for the dates shown by
            the control is returned (which may be ::wxDefaultDateTime if no
            limit is set).
        @return
            @true if either limit is set, @false otherwise
     */
    virtual bool GetDateRange(wxDateTime *lowerdate,
                                wxDateTime *upperdate) const;

    //@}
};

