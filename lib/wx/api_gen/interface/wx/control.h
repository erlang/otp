/////////////////////////////////////////////////////////////////////////////
// Name:        control.h
// Purpose:     interface of wxControl
// Author:      wxWidgets team
// Licence:     wxWindows licence
/////////////////////////////////////////////////////////////////////////////

/**
    @class wxControl

    This is the base class for a control or "widget".

    A control is generally a small window which processes user input and/or
    displays one or more item of data.

    @beginEventEmissionTable{wxClipboardTextEvent}
    @event{EVT_TEXT_COPY(id, func)}
           Some or all of the controls content was copied to the clipboard.
    @event{EVT_TEXT_CUT(id, func)}
           Some or all of the controls content was cut (i.e. copied and
           deleted).
    @event{EVT_TEXT_PASTE(id, func)}
           Clipboard content was pasted into the control.
    @endEventTable

    @library{wxcore}
    @category{ctrl}

    @see wxValidator
*/
class wxControl : public wxWindow
{
public:

    /**
        Constructs a control.

        @param parent
            Pointer to a parent window.
        @param id
            Control identifier. If wxID_ANY, will automatically create an identifier.
        @param pos
            Control position. wxDefaultPosition indicates that wxWidgets
            should generate a default position for the control.
        @param size
            Control size. wxDefaultSize indicates that wxWidgets should generate
            a default size for the window. If no suitable size can  be found, the
            window will be sized to 20x20 pixels so that the window is visible but
            obviously not correctly sized.
        @param style
            Control style. For generic window styles, please see wxWindow.
        @param validator
            Control validator.
        @param name
            Control name.
    */
   wxControl(wxWindow *parent, wxWindowID id,
             const wxPoint& pos = wxDefaultPosition,
             const wxSize& size = wxDefaultSize, long style = 0,
             const wxValidator& validator = wxDefaultValidator,
             const wxString& name = wxControlNameStr);

    /**
       Default constructor to allow 2-phase creation.
    */
    wxControl();

    bool Create(wxWindow *parent, wxWindowID id,
            const wxPoint& pos = wxDefaultPosition,
            const wxSize& size = wxDefaultSize, long style = 0,
            const wxValidator& validator = wxDefaultValidator,
            const wxString& name = wxControlNameStr);

    /**
        Simulates the effect of the user issuing a command to the item.

        @see wxCommandEvent
    */
    virtual void Command(wxCommandEvent& event);

    /**
        Returns the control's label, as it was passed to SetLabel().

        Note that the returned string may contains mnemonics ("&" characters) if they were
        passed to the SetLabel() function; use GetLabelText() if they are undesired.

        Also note that the returned string is always the string which was passed to
        SetLabel() but may be different from the string passed to SetLabelText()
        (since this last one escapes mnemonic characters).
    */
    wxString GetLabel() const;

    /**
        Returns the control's label without mnemonics.

        Note that because of the stripping of the mnemonics the returned string may differ
        from the string which was passed to SetLabel() but should always be the same which
        was passed to SetLabelText().
    */
    wxString GetLabelText() const;

    /**
        Determine the size needed by the control to leave the given area for
        its text.

        This function is mostly useful with control displaying short amounts of
        text that can be edited by the user, e.g. wxTextCtrl, wxComboBox,
        wxSearchCtrl etc. Typically it is used to size these controls for the
        maximal amount of input they are supposed to contain, for example:
        @code
            // Create a control for post code entry.
            wxTextCtrl* postcode = new wxTextCtrl(this, ...);

            // And set its initial and minimal size to be big enough for
            // entering 5 digits.
            postcode->SetInitialSize(
                postcode->GetSizeFromTextSize(
                    postcode->GetTextExtent("99999")));
        @endcode

        Currently this method is only implemented for wxTextCtrl, wxComboBox
        and wxChoice in wxMSW and wxGTK.

        @param xlen The horizontal extent of the area to leave for text, in
            pixels.
        @param ylen The vertical extent of the area to leave for text, in
            pixels. By default -1 meaning that the vertical component of the
            returned size should be the default height of this control.
        @return The size that the control should have to leave the area of the
            specified size for its text. May return wxDefaultSize if this
            method is not implemented for this particular control under the
            current platform.

        @since 2.9.5
     */
    wxSize GetSizeFromTextSize(int xlen, int ylen = -1) const;

    /**
        @overload
    */
    wxSize GetSizeFromTextSize(const wxSize& tsize) const;

    /**
        Determine the minimum size needed by the control to display the given text.

        The helper function that uses combination of GetSizeFromTextSize() and
        GetTextExtent() which used together pretty often:
        @code
            wxSize GetSizeFromText(const wxString& text) const
            {
                return GetSizeFromTextSize(GetTextExtent(text).GetWidth());
            }
        @endcode

        @param text The given text.
        @return The size that the control should have to leave the area of the
            specified text. May return wxDefaultSize if this method is not
            implemented for this particular control under the current platform.

        @since 3.1.3
     */
    wxSize GetSizeFromText(const wxString& text) const;

    /**
        Sets the control's label.

        All "&" characters in the @a label are special and indicate that the
        following character is a @e mnemonic for this control and can be used to
        activate it from the keyboard (typically by using @e Alt key in
        combination with it). To insert a literal ampersand character, you need
        to double it, i.e. use "&&". If this behaviour is undesirable, use
        SetLabelText() instead.
    */
    void SetLabel(const wxString& label);

    /**
        Sets the control's label to exactly the given string.

        Unlike SetLabel(), this function shows exactly the @a text passed to it
        in the control, without interpreting ampersands in it in any way.
        Notice that it means that the control can't have any mnemonic defined
        for it using this function.

        @see EscapeMnemonics()
     */
    void SetLabelText(const wxString& text);

    // NB: when writing docs for the following function remember that Doxygen
    //     will always expand HTML entities (e.g. &quot;) and thus we need to
    //     write e.g. "&amp;lt;" to have in the output the "&lt;" string.

    /**
        Sets the controls label to a string using markup.

        Simple markup supported by this function can be used to apply different
        fonts or colours to different parts of the control label when supported.
        If markup is not supported by the control or platform, it is simply
        stripped and SetLabel() is used with the resulting string.

        For example,
        @code
            wxStaticText *text;
            ...
            text->SetLabelMarkup("<b>&amp;Bed</b> &amp;mp; "
                                 "<span foreground='red'>breakfast</span> "
                                 "available <big>HERE</big>");
        @endcode
        would show the string using bold, red and big for the corresponding
        words under wxGTK but will simply show the string "Bed &amp; breakfast
        available HERE" on the other platforms. In any case, the "B" of "Bed"
        will be underlined to indicate that it can be used as a mnemonic for
        this control.

        The supported tags are:
        <TABLE>
            <TR>
                <TD><b>Tag</b></TD>
                <TD><b>Description</b></TD>
            </TR>
            <TR>
                <TD>&lt;b&gt;</TD>
                <TD>bold text</TD>
            </TR>
            <TR>
                <TD>&lt;big&gt;</TD>
                <TD>bigger text</TD>
            </TR>
            <TR>
                <TD>&lt;i&gt;</TD>
                <TD>italic text</TD>
            </TR>
            <TR>
                <TD>&lt;s&gt;</TD>
                <TD>strike-through text</TD>
            </TR>
            <TR>
                <TD>&lt;small&gt;</TD>
                <TD>smaller text</TD>
            </TR>
            <TR>
                <TD>&lt;tt&gt;</TD>
                <TD>monospaced text</TD>
            </TR>
            <TR>
                <TD>&lt;u&gt;</TD>
                <TD>underlined text</TD>
            </TR>
            <TR>
                <TD>&lt;span&gt;</TD>
                <TD>generic formatter tag, see the table below for supported
                attributes.
                </TD>
            </TR>
        </TABLE>

        Supported @c &lt;span&gt; attributes:
        <TABLE>
            <TR>
                <TD><b>Name</b></TD>
                <TD><b>Description</b></TD>
            </TR>
            <TR>
                <TD>foreground, fgcolor, color</TD>
                <TD>Foreground text colour, can be a name or RGB value.</TD>
            </TR>
            <TR>
                <TD>background, bgcolor</TD>
                <TD>Background text colour, can be a name or RGB value.</TD>
            </TR>
            <TR>
                <TD>font_family, face</TD>
                <TD>Font face name.</TD>
            </TR>
            <TR>
                <TD>font_weight, weight</TD>
                <TD>Numeric value in 0..900 range or one of "ultralight",
                "light", "normal" (all meaning non-bold), "bold", "ultrabold"
                and "heavy" (all meaning bold).</TD>
            </TR>
            <TR>
                <TD>font_style, style</TD>
                <TD>Either "oblique" or "italic" (both with the same meaning)
                or "normal".</TD>
            </TR>
            <TR>
                <TD>size</TD>
                <TD>The font size can be specified either as "smaller" or
                "larger" relatively to the current font, as a CSS font size
                name ("xx-small", "x-small", "small", "medium", "large",
                "x-large" or "xx-large") or as a number giving font size in
                1024th parts of a point, i.e. 10240 for a 10pt font.</TD>
            </TR>
        </TABLE>

        This markup language is a strict subset of Pango markup (described at
        http://library.gnome.org/devel/pango/unstable/PangoMarkupFormat.html)
        and any tags and span attributes not documented above can't be used
        under non-GTK platforms.

        Also note that you need to escape the following special characters:
        <TABLE>
            <TR>
                <TD><b>Special character</b></TD>
                <TD><b>Escape as</b></TD>
            </TR>
            <TR>
                <TD>@c &amp;</TD>
                <TD>@c &amp;amp; or as @c &amp;&amp;</TD>
            </TR>
            <TR>
                <TD>@c &apos;</TD>
                <TD>@c &amp;apos;</TD>
            </TR>
            <TR>
                <TD>@c &quot;</TD>
                <TD>@c &amp;quot;</TD>
            </TR>
            <TR>
                <TD>@c &lt;</TD>
                <TD>@c &amp;lt;</TD>
            </TR>
            <TR>
                <TD>@c &gt;</TD>
                <TD>@c &amp;gt;</TD>
            </TR>
        </TABLE>

        The non-escaped ampersand @c &amp; characters are interpreted as
        mnemonics as with wxControl::SetLabel.


        @param markup
            String containing markup for the label. It may contain markup tags
            described above and newline characters but currently only wxGTK and
            wxOSX support multiline labels with markup, the generic
            implementation (also used in wxMSW) only handles single line markup
            labels. Notice that the string must be well-formed (e.g. all tags
            must be correctly closed) and won't be shown at all otherwise.
        @return
            @true if the new label was set (even if markup in it was ignored)
            or @false if we failed to parse the markup. In this case the label
            remains unchanged.


        Currently wxButton supports markup in all major ports (wxMSW, wxGTK and
        wxOSX/Cocoa) while wxStaticText supports it in wxGTK and wxOSX and its
        generic version (which can be used under MSW if markup support is
        required). Extending support to more controls is planned in the future.

        @since 2.9.2
    */
    bool SetLabelMarkup(const wxString& markup);


public:     // static functions

    /**
        Returns the given @a label string without mnemonics ("&" characters).
    */
    static wxString GetLabelText(const wxString& label);

    /**
        Returns the given @a str string without mnemonics ("&" characters).

        @note This function is identical to GetLabelText() and is provided
              mostly for symmetry with EscapeMnemonics().
    */
    static wxString RemoveMnemonics(const wxString& str);

    /**
        Escapes the special mnemonics characters ("&") in the given string.

        This function can be helpful if you need to set the controls label to a
        user-provided string. If the string contains ampersands, they wouldn't
        appear on the display but be used instead to indicate that the
        character following the first of them can be used as a control mnemonic.
        While this can sometimes be desirable (e.g. to allow the user to
        configure mnemonics of the controls), more often you will want to use
        this function before passing a user-defined string to SetLabel().
        Alternatively, if the label is entirely user-defined, you can just call
        SetLabelText() directly -- but this function must be used if the label
        is a combination of a part defined by program containing the control
        mnemonics and a user-defined part.

        @param text
            The string such as it should appear on the display.
        @return
            The same string with the ampersands in it doubled.
     */
    static wxString EscapeMnemonics(const wxString& text);

    /**
        Replaces parts of the @a label string with ellipsis, if needed, so
        that it fits into @a maxWidth pixels if possible.

        Note that this function does @em not guarantee that the returned string
        will always be shorter than @a maxWidth; if @a maxWidth is extremely
        small, ellipsized text may be larger.

        @param label
            The string to ellipsize
        @param dc
            The DC used to retrieve the character widths through the
            wxDC::GetPartialTextExtents() function.
        @param mode
            The ellipsization mode. This is the setting which determines
            which part of the string should be replaced by the ellipsis
            (unless it is ::wxELLIPSIZE_NONE in which case nothing is done).
            See ::wxEllipsizeMode enumeration values for more info.
        @param maxWidth
            The maximum width of the returned string in pixels.
            This argument determines how much characters of the string need to
            be removed (and replaced by ellipsis).
        @param flags
            One or more of the ::wxEllipsizeFlags enumeration values combined.
    */
    static wxString Ellipsize(const wxString& label, const wxDC& dc,
                              wxEllipsizeMode mode, int maxWidth,
                              int flags = wxELLIPSIZE_FLAGS_DEFAULT);
};

