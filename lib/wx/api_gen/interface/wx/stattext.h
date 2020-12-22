/////////////////////////////////////////////////////////////////////////////
// Name:        stattext.h
// Purpose:     interface of wxStaticText
// Author:      wxWidgets team
// Licence:     wxWindows licence
/////////////////////////////////////////////////////////////////////////////

#define wxST_NO_AUTORESIZE         0x0001
#define wxST_ELLIPSIZE_START       0x0004
#define wxST_ELLIPSIZE_MIDDLE      0x0008
#define wxST_ELLIPSIZE_END         0x0010

/**
    @class wxStaticText

    A static text control displays one or more lines of read-only text.
    wxStaticText supports the three classic text alignments, label
    ellipsization i.e. replacing parts of the text with the ellipsis ("...") if
    the label doesn't fit into the provided space and also formatting markup
    with wxControl::SetLabelMarkup().

    @beginStyleTable
    @style{wxALIGN_LEFT}
           Align the text to the left.
    @style{wxALIGN_RIGHT}
           Align the text to the right.
    @style{wxALIGN_CENTRE_HORIZONTAL}
           Center the text (horizontally).
    @style{wxST_NO_AUTORESIZE}
           By default, the control will adjust its size to exactly fit to the
           size of the text when SetLabel() is called. If this style flag is
           given, the control will not change its size (this style is
           especially useful with controls which also have the @c wxALIGN_RIGHT or
           the @c wxALIGN_CENTRE_HORIZONTAL style because otherwise they won't make sense any
           longer after a call to SetLabel()).
    @style{wxST_ELLIPSIZE_START}
           If the labeltext width exceeds the control width, replace the beginning
           of the label with an ellipsis; uses wxControl::Ellipsize.
    @style{wxST_ELLIPSIZE_MIDDLE}
           If the label text width exceeds the control width, replace the middle
           of the label with an ellipsis; uses wxControl::Ellipsize.
    @style{wxST_ELLIPSIZE_END}
           If the label text width exceeds the control width, replace the end
           of the label with an ellipsis; uses wxControl::Ellipsize.
    @endStyleTable

    @library{wxcore}
    @category{ctrl}
    @appearance{statictext}

    @see wxStaticBitmap, wxStaticBox
*/
class wxStaticText : public wxControl
{
public:
    /**
        Default constructor.
    */
    wxStaticText();

    /**
        Constructor, creating and showing a text control.

        @param parent
            Parent window. Should not be @NULL.
        @param id
            Control identifier. A value of -1 denotes a default value.
        @param label
            Text label.
        @param pos
            Window position.
        @param size
            Window size.
        @param style
            Window style. See wxStaticText.
        @param name
            Window name.

        @see Create()
    */
    wxStaticText(wxWindow* parent, wxWindowID id,
                 const wxString& label,
                 const wxPoint& pos = wxDefaultPosition,
                 const wxSize& size = wxDefaultSize,
                 long style = 0,
                 const wxString& name = wxStaticTextNameStr);

    /**
        Creation function, for two-step construction. For details see wxStaticText().
    */
    bool Create(wxWindow* parent, wxWindowID id, const wxString& label,
                const wxPoint& pos = wxDefaultPosition,
                const wxSize& size = wxDefaultSize, long style = 0,
                const wxString& name = wxStaticTextNameStr);

    /**
        Returns @true if the window styles for this control contains one of the
        @c wxST_ELLIPSIZE_START, @c wxST_ELLIPSIZE_MIDDLE or @c wxST_ELLIPSIZE_END styles.
    */
    bool IsEllipsized() const;

    /**
        Change the label shown in the control.

        Notice that since wxWidgets 3.1.1 this function is guaranteed not to do
        anything if the label didn't really change, so there is no benefit to
        checking if the new label is different from the current one in the
        application code.

        @see wxControl::SetLabel()
     */
    virtual void SetLabel(const wxString& label);

    /**
        This functions wraps the controls label so that each of its lines becomes at
        most @a width pixels wide if possible (the lines are broken at words
        boundaries so it might not be the case if words are too long).

        If @a width is negative, no wrapping is done. Note that this width is not
        necessarily the total width of the control, since a few pixels for the
        border (depending on the controls border style) may be added.

        @since 2.6.2
    */
    void Wrap(int width);
};

