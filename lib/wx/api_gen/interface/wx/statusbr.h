/////////////////////////////////////////////////////////////////////////////
// Name:        statusbr.h
// Purpose:     interface of wxStatusBar
// Author:      wxWidgets team
// Licence:     wxWindows licence
/////////////////////////////////////////////////////////////////////////////

// wxStatusBar styles
#define wxSTB_SIZEGRIP         0x0010
#define wxSTB_SHOW_TIPS        0x0020

#define wxSTB_ELLIPSIZE_START   0x0040
#define wxSTB_ELLIPSIZE_MIDDLE  0x0080
#define wxSTB_ELLIPSIZE_END     0x0100

#define wxSTB_DEFAULT_STYLE    (wxSTB_SIZEGRIP|wxSTB_ELLIPSIZE_END|wxSTB_SHOW_TIPS|wxFULL_REPAINT_ON_RESIZE)

// style flags for wxStatusBar fields
#define wxSB_NORMAL    0x0000
#define wxSB_FLAT      0x0001
#define wxSB_RAISED    0x0002
#define wxSB_SUNKEN    0x0003


/**
    @class wxStatusBarPane

    A status bar pane data container used by wxStatusBar.

    @library{wxcore}
    @category{data}

    @see wxStatusBar
*/
class wxStatusBarPane
{
public:
    /**
        Constructs the pane with the given @a style and @a width.
    */
    wxStatusBarPane(int style = wxSB_NORMAL, int width = 0);

    /**
        Returns the pane width; it maybe negative, indicating a variable-width field.
    */
    int GetWidth() const;

    /**
        Returns the pane style.
    */
    int GetStyle() const;

    /**
        Returns the text currently shown in this pane.
     */
    wxString GetText() const;
};

/**
    @class wxStatusBar

    A status bar is a narrow window that can be placed along the bottom of a frame
    to give small amounts of status information. It can contain one or more fields,
    one or more of which can be variable length according to the size of the window.

    wxStatusBar also maintains an independent stack of status texts for each field
    (see PushStatusText() and PopStatusText()).

    Note that in wxStatusBar context, the terms @e pane and @e field are synonyms.

    @beginStyleTable
    @style{wxSTB_SIZEGRIP}
        Displays a gripper at the right-hand side of the status bar which can be used
        to resize the parent window.
    @style{wxSTB_SHOW_TIPS}
        Displays tooltips for those panes whose status text has been ellipsized/truncated
        because the status text doesn't fit the pane width.
        Note that this style has effect only on wxGTK (with GTK+ >= 2.12) currently.
    @style{wxSTB_ELLIPSIZE_START}
        Replace the beginning of the status texts with an ellipsis when the status text
        widths exceed the status bar pane's widths (uses wxControl::Ellipsize).
    @style{wxSTB_ELLIPSIZE_MIDDLE}
        Replace the middle of the status texts with an ellipsis when the status text
        widths exceed the status bar pane's widths (uses wxControl::Ellipsize).
    @style{wxSTB_ELLIPSIZE_END}
        Replace the end of the status texts with an ellipsis when the status text
        widths exceed the status bar pane's widths (uses wxControl::Ellipsize).
    @style{wxSTB_DEFAULT_STYLE}
        The default style: includes
        @c wxSTB_SIZEGRIP|wxSTB_SHOW_TIPS|wxSTB_ELLIPSIZE_END|wxFULL_REPAINT_ON_RESIZE.
    @endStyleTable

    @remarks
    It is possible to create controls and other windows on the status bar.
    Position these windows from an OnSize() event handler.

    @remarks
    Notice that only the first 127 characters of a string will be shown in
    status bar fields under Windows if a proper manifest indicating that the
    program uses version 6 of common controls library is not used. This is a
    limitation of the native control on these platforms.

    @library{wxcore}
    @category{miscwnd}

    @see wxStatusBarPane, wxFrame, @ref page_samples_statbar
*/
class wxStatusBar : public wxControl
{
public:
    /**
        Default ctor.
    */
    wxStatusBar();

    /**
        Constructor, creating the window.

        @param parent
            The window parent, usually a frame.
        @param id
            The window identifier.
            It may take a value of -1 to indicate a default value.
        @param style
            The window style. See wxStatusBar.
        @param name
            The name of the window. This parameter is used to associate a name with the
            item, allowing the application user to set Motif resource values for
            individual windows.

        @see Create()
    */
    wxStatusBar(wxWindow* parent, wxWindowID id = wxID_ANY,
                long style = wxSTB_DEFAULT_STYLE,
                const wxString& name = wxStatusBarNameStr);

    /**
        Destructor.
    */
    virtual ~wxStatusBar();

    /**
        Creates the window, for two-step construction.
        See wxStatusBar() for details.
    */
    bool Create(wxWindow* parent, wxWindowID id = wxID_ANY,
                long style = wxSTB_DEFAULT_STYLE,
                const wxString& name = wxStatusBarNameStr);

    /**
        Returns the size and position of a field's internal bounding rectangle.

        @param i
            The field in question.
        @param rect
            The rectangle values are placed in this variable.

        @return @true if the field index is valid, @false otherwise.

        @beginWxPerlOnly
        In wxPerl this function returns a @c Wx::Rect if the field
        index is valid, @c undef otherwise.
        @endWxPerlOnly

        @see wxRect
    */
    virtual bool GetFieldRect(int i, wxRect& rect) const;

    /**
        Returns the number of fields in the status bar.
    */
    int GetFieldsCount() const;

    /**
        Returns the wxStatusBarPane representing the @a n-th field.
    */
    const wxStatusBarPane& GetField(int n) const;

    /**
        Returns the horizontal and vertical borders used when rendering the field
        text inside the field area.

        Note that the rect returned by GetFieldRect() already accounts for the
        presence of horizontal and vertical border returned by this function.
    */
    wxSize GetBorders() const;

    /**
        Returns the string associated with a status bar field.

        @param i
            The number of the status field to retrieve, starting from zero.

        @return The status field string if the field is valid, otherwise the
                empty string.

        @see SetStatusText()
    */
    virtual wxString GetStatusText(int i = 0) const;

    /**
        Returns the width of the @a n-th field.

        See wxStatusBarPane::GetWidth() for more info.
    */
    int GetStatusWidth(int n) const;

    /**
        Returns the style of the @a n-th field.

        See wxStatusBarPane::GetStyle() for more info.
    */
    int GetStatusStyle(int n) const;

    /**
        Restores the text to the value it had before the last call to
        PushStatusText().

        Notice that if SetStatusText() had been called in the meanwhile,
        PopStatusText() will not change the text, i.e. it does not override
        explicit changes to status text but only restores the saved text if it
        hadn't been changed since.

        @see PushStatusText()
    */
    void PopStatusText(int field = 0);

    /**
        Saves the current field text in a per-field stack, and sets the field
        text to the string passed as argument.

        @see PopStatusText()
    */
    void PushStatusText(const wxString& string, int field = 0);

    /**
        Sets the number of fields, and optionally the field widths.

        @param number
            The number of fields. If this is greater than the previous number,
            then new fields with empty strings will be added to the status bar.
        @param widths
            An array of n integers interpreted in the same way as
            in SetStatusWidths().

        @beginWxPerlOnly
        In wxPerl this function accepts only the @a number parameter.
        Use SetStatusWidths to set the field widths.
        @endWxPerlOnly
    */
    virtual void SetFieldsCount(int number = 1, const int* widths = NULL);

    /**
        Sets the minimal possible height for the status bar.

        The real height may be bigger than the height specified here depending
        on the size of the font used by the status bar.
    */
    virtual void SetMinHeight(int height);

    /**
        Sets the styles of the fields in the status line which can make fields appear
        flat or raised instead of the standard sunken 3D border.

        @param n
            The number of fields in the status bar. Must be equal to the
            number passed to SetFieldsCount() the last time it was called.
        @param styles
            Contains an array of @a n integers with the styles for each field.
            There are four possible styles:
            - @c wxSB_NORMAL (default): The field appears with the default native border.
            - @c wxSB_FLAT: No border is painted around the field so that it appears flat.
            - @c wxSB_RAISED: A raised 3D border is painted around the field.
            - @c wxSB_SUNKEN: A sunken 3D border is painted around the field
              (this style is new since wxWidgets 2.9.5).
    */
    virtual void SetStatusStyles(int n, const int* styles);

    /**
        Sets the status text for the @a i-th field.

        The given text will replace the current text. The display of the status
        bar is updated immediately, so there is no need to call
        wxWindow::Update() after calling this function.

        Note that if PushStatusText() had been called before the new text will
        also replace the last saved value to make sure that the next call to
        PopStatusText() doesn't restore the old value, which was overwritten by
        the call to this function.

        @param text
            The text to be set. Use an empty string ("") to clear the field.
        @param i
            The field to set, starting from zero.

        @see GetStatusText(), wxFrame::SetStatusText
    */
    virtual void SetStatusText(const wxString& text, int i = 0);

    /**
        Sets the widths of the fields in the status line. There are two types of
        fields: @b fixed widths and @b variable width fields. For the fixed width fields
        you should specify their (constant) width in pixels. For the variable width
        fields, specify a negative number which indicates how the field should expand:
        the space left for all variable width fields is divided between them according
        to the absolute value of this number. A variable width field with width of -2
        gets twice as much of it as a field with width -1 and so on.

        For example, to create one fixed width field of width 100 in the right part of
        the status bar and two more fields which get 66% and 33% of the remaining
        space correspondingly, you should use an array containing -2, -1 and 100.

        @param n
            The number of fields in the status bar. Must be equal to the
            number passed to SetFieldsCount() the last time it was called.
        @param widths_field
            Contains an array of n integers, each of which is either an
            absolute status field width in pixels if positive or indicates a
            variable width field if negative.
            The special value @NULL means that all fields should get the same width.

        @remarks The widths of the variable fields are calculated from the total
                 width of all fields, minus the sum of widths of the
                 non-variable fields, divided by the number of variable fields.

        @beginWxPerlOnly
        In wxPerl this method takes as parameters the field widths.
        @endWxPerlOnly

        @see SetFieldsCount(), wxFrame::SetStatusWidths()
    */
    virtual void SetStatusWidths(int n, const int* widths_field);
};

