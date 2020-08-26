/////////////////////////////////////////////////////////////////////////////
// Name:        wx/commandlinkbutton.h
// Purpose:     interface of wxCommandLinkButton
// Author:      wxWidgets team
// Licence:     wxWindows licence
/////////////////////////////////////////////////////////////////////////////

/**
    @class wxCommandLinkButton

    Objects of this class are similar in appearance to the normal wxButtons but
    are similar to the links in a web page in functionality.

    Pressing such button usually results in switching to another window of the
    program and so they can be used as a replacement for the "Next" button in a
    multi-page dialog (such as wxWizard), for example.

    Their advantage compared to the ordinary wxButtons is that they emphasize
    the action of switching the window and also that they allow to give more
    detailed explanation to the user because, in addition to the short button
    label, they also show a longer description string.

    The short, title-like, part of the label is called the <em>main label</em>
    and the longer description is the <em>note</em>. Both of them can be set
    and queried independently using wxCommandLinkButton-specific methods such
    as SetMainLabel() or GetNote() or also via SetLabel() and GetLabel()
    methods inherited from wxButton. When using the latter, the main label and
    the note are concatenated into a single string using a new line character
    between them (notice that the note part can have more new lines in it).

    wxCommandLinkButton generates the same event as wxButton but doesn't
    support any of wxButton-specific styles nor adds any new styles of its own.

    Currently this class uses native implementation under Windows Vista and
    later versions and a generic implementation for the other platforms and
    earlier Windows versions.

    @since 2.9.2

    @library{wxcore}
    @category{ctrl}
    @appearance{commandlinkbutton}

    @see wxButton, wxBitmapButton
*/
class wxCommandLinkButton : public wxButton
{
public:
    /**
        Default constructor.

        Use Create() to really create the control.
    */
    wxCommandLinkButton();

    /**
        Constructor really creating a command Link button.

        The button will be decorated with stock icons under GTK+ 2.

        @param parent
            Parent window. Must not be @NULL.
        @param id
            Button identifier. A value of wxID_ANY indicates a default value.
        @param mainLabel
            First line of text on the button, typically the label of an action
            that will be made when the button is pressed.
        @param note
            Second line of text describing the action performed when the button
            is pressed.
        @param pos
            Button position.
        @param size
            Button size. If the default size is specified then the button is sized
            appropriately for the text.
        @param style
            Window style. See wxButton class description.
        @param validator
            Window validator.
        @param name
            Window name.

        @see Create(), wxValidator
    */
    wxCommandLinkButton(wxWindow* parent, wxWindowID id,
                        const wxString& mainLabel = wxEmptyString,
                        const wxString& note = wxEmptyString,
                        const wxPoint& pos = wxDefaultPosition,
                        const wxSize& size = wxDefaultSize,
                        long style = 0,
                        const wxValidator& validator = wxDefaultValidator,
                        const wxString& name = wxButtonNameStr);

    /**
        Button creation function for two-step creation.
        For more details, see wxCommandLinkButton().
    */
    bool Create(wxWindow* parent, wxWindowID id,
                const wxString& mainLabel = wxEmptyString,
                const wxString& note = wxEmptyString,
                const wxPoint& pos = wxDefaultPosition,
                const wxSize& size = wxDefaultSize,
                long style = 0,
                const wxValidator& validator = wxDefaultValidator,
                const wxString& name = wxButtonNameStr);

    /**
        Sets a new main label and note for the button.

        Neither of the arguments can be empty, if you need to change just the
        label or just the note, use SetMainLabel() or SetNote() instead of this
        function.

        @param mainLabel
            New main label to use.
        @param note
            New note to use.
    */
    void SetMainLabelAndNote(const wxString& mainLabel, const wxString& note);

    /**
        Sets the string label and note for the button.

        @param label
            The label and note to set, with the two separated
            by the first newline or none to set a blank note.
    */
    virtual void SetLabel(const wxString& label);

    /**
        Returns the string label for the button.

        @see SetLabel()

        @return
            A string with the main label and note concatenated
            together with a newline separating them.
    */
    wxString GetLabel() const;

    /**
        Changes the main label.

        @param mainLabel
            New main label to use.
    */
    void SetMainLabel(const wxString& mainLabel);

    /**
        Changes the note.

        @param note
            New note to use.
    */
    void SetNote(const wxString& note);

    /**
        Returns the current main label.

        @return
            Main label currently displayed.
    */
    wxString GetMainLabel() const;

    /**
        Returns the currently used note.

        @return
            Note currently displayed.
    */
    wxString GetNote() const;
};
