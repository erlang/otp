/////////////////////////////////////////////////////////////////////////////
// Name:        wx/textentry.h
// Purpose:     interface of wxTextEntry
// Author:      Vadim Zeitlin
// Created:     2009-03-01 (extracted from wx/textctrl.h)
// Copyright:   (c) 2009 Vadim Zeitlin <vadim@wxwidgets.org>
// Licence:     wxWindows licence
/////////////////////////////////////////////////////////////////////////////


/**
    wxTextPos is a position in the text
*/
typedef long wxTextPos;


/**
    @class wxTextEntry

    Common base class for single line text entry fields.

    This class is not a control itself, as it doesn't derive from wxWindow.
    Instead it is used as a base class by other controls, notably wxTextCtrl
    and wxComboBox and gathers the methods common to both of them.

    @library{wxcore}
    @category{ctrl}

    @see wxTextCtrl, wxComboBox

    @since 2.9.0
*/
class wxTextEntry
{
public:
    /**
        Appends the text to the end of the text control.

        @param text
            Text to write to the text control.

        @remarks
            After the text is appended, the insertion point will be at the
            end of the text control. If this behaviour is not desired, the
            programmer should use GetInsertionPoint() and SetInsertionPoint().

        @see WriteText()
    */
    virtual void AppendText(const wxString& text);

    /**
        Call this function to enable auto-completion of the text typed in a
        single-line text control using the given @a choices.

        @since 2.9.0

        @return
            @true if the auto-completion was enabled or @false if the operation
            failed, typically because auto-completion is not supported by the
            current platform.

        @see AutoCompleteFileNames()
    */
    bool AutoComplete(const wxArrayString& choices);

    /**
        Enable auto-completion using the provided completer object.

        This method should be used instead of AutoComplete() overload taking
        the array of possible completions if the total number of strings is too
        big as it allows returning the completions dynamically, depending on
        the text already entered by user and so is more efficient.

        The specified @a completer object will be used to retrieve the list of
        possible completions for the already entered text and will be deleted
        by wxTextEntry itself when it's not needed any longer.

        Notice that you need to include @c wx/textcompleter.h in order to
        define your class inheriting from wxTextCompleter.

        @since 2.9.2

        @param completer
            The object to be used for generating completions if non-@NULL. If
            it is @NULL, auto-completion is disabled. The wxTextEntry object
            takes ownership of this pointer and will delete it in any case
            (i.e. even if this method returns @false).

        @return
            @true if the auto-completion was enabled or @false if the operation
            failed, typically because auto-completion is not supported by the
            current platform.

        @see wxTextCompleter
     */
    bool AutoComplete(wxTextCompleter *completer);

    /**
        Call this function to enable auto-completion of the text typed in a
        single-line text control using all valid file system paths.

        Notice that currently this function is only implemented in wxMSW port
        and does nothing under the other platforms.

        @since 2.9.0

        @return
            @true if the auto-completion was enabled or @false if the operation
            failed, typically because auto-completion is not supported by the
            current platform.

        @see AutoComplete()
    */
    bool AutoCompleteFileNames();

    /**
        Call this function to enable auto-completion of the text using the file
        system directories.

        Unlike AutoCompleteFileNames() which completes both file names and
        directories, this function only completes the directory names.

        Notice that currently this function is only implemented in wxMSW port
        and does nothing under the other platforms.

        @since 2.9.3

        @return
            @true if the auto-completion was enabled or @false if the operation
            failed, typically because auto-completion is not supported by the
            current platform.

        @see AutoComplete()
     */
    bool AutoCompleteDirectories();

    /**
        Returns @true if the selection can be copied to the clipboard.
    */
    virtual bool CanCopy() const;

    /**
        Returns @true if the selection can be cut to the clipboard.
    */
    virtual bool CanCut() const;

    /**
        Returns @true if the contents of the clipboard can be pasted into the
        text control.

        On some platforms (Motif, GTK) this is an approximation and returns
        @true if the control is editable, @false otherwise.
    */
    virtual bool CanPaste() const;

    /**
        Returns @true if there is a redo facility available and the last
        operation can be redone.
    */
    virtual bool CanRedo() const;

    /**
        Returns @true if there is an undo facility available and the last
        operation can be undone.
    */
    virtual bool CanUndo() const;

    /**
        Sets the new text control value.

        It also marks the control as not-modified which means that IsModified()
        would return @false immediately after the call to ChangeValue().

        The insertion point is set to the start of the control (i.e. position
        0) by this function.

        This functions does not generate the @c wxEVT_TEXT
        event but otherwise is identical to SetValue().

        See @ref overview_events_prog for more information.

        @since 2.7.1

        @param value
            The new value to set. It may contain newline characters if the text
            control is multi-line.
    */
    virtual void ChangeValue(const wxString& value);

    /**
        Clears the text in the control.

        Note that this function will generate a @c wxEVT_TEXT
        event, i.e. its effect is identical to calling @c SetValue("").
    */
    virtual void Clear();

    /**
        Copies the selected text to the clipboard.
    */
    virtual void Copy();

    /**
        Copies the selected text to the clipboard and removes it from the control.
    */
    virtual void Cut();

    /**
        Convert all text entered into the control to upper case.

        Call this method to ensure that all text entered into the control is
        converted on the fly to upper case. If the control is not empty, its
        existing contents is also converted to upper case.

        @since 3.1.0
     */
    void ForceUpper();

    /**
        Returns the insertion point, or cursor, position.

        This is defined as the zero based index of the character position to
        the right of the insertion point. For example, if the insertion point
        is at the end of the single-line text control, it is equal to
        GetLastPosition().

        Notice that insertion position is, in general, different from the index
        of the character the cursor position at in the string returned by
        GetValue(). While this is always the case for the single line controls,
        multi-line controls can use two characters @c "\\r\\n" as line
        separator (this is notably the case under MSW) meaning that indices in
        the control and its string value are offset by 1 for every line.

        Hence to correctly get the character at the current cursor position,
        taking into account that there can be none if the cursor is at the end
        of the string, you could do the following:

        @code
        wxString GetCurrentChar(wxTextCtrl *tc)
        {
            long pos = tc->GetInsertionPoint();
            if ( pos == tc->GetLastPosition() )
                return wxString();

            return tc->GetRange(pos, pos + 1);
        }
        @endcode
    */
    virtual long GetInsertionPoint() const;

    /**
        Returns the zero based index of the last position in the text control,
        which is equal to the number of characters in the control.
    */
    virtual wxTextPos GetLastPosition() const;

    /**
        Returns the string containing the text starting in the positions
        @a from and up to @a to in the control.

        The positions must have been returned by another wxTextCtrl method.
        Please note that the positions in a multiline wxTextCtrl do @b not
        correspond to the indices in the string returned by GetValue() because
        of the different new line representations (@c CR or @c CR LF) and so
        this method should be used to obtain the correct results instead of
        extracting parts of the entire value. It may also be more efficient,
        especially if the control contains a lot of data.
    */
    virtual wxString GetRange(long from, long to) const;

    /**
        Gets the current selection span.

        If the returned values are equal, there was no selection. Please note
        that the indices returned may be used with the other wxTextCtrl methods
        but don't necessarily represent the correct indices into the string
        returned by GetValue() for multiline controls under Windows (at least,)
        you should use GetStringSelection() to get the selected text.

        @param from
            The returned first position.
        @param to
            The returned last position.

        @beginWxPerlOnly
        In wxPerl this method takes no parameters and returns a
        2-element list (from, to).
        @endWxPerlOnly
    */
    virtual void GetSelection(long* from, long* to) const;

    /**
        Gets the text currently selected in the control.

        If there is no selection, the returned string is empty.
    */
    virtual wxString GetStringSelection() const;

    /**
        Gets the contents of the control.

        Notice that for a multiline text control, the lines will be separated
        by (Unix-style) @c \\n characters, even under Windows where they are
        separated by a @c \\r\\n sequence in the native control.
    */
    virtual wxString GetValue() const;

    /**
        Returns @true if the controls contents may be edited by user (note that
        it always can be changed by the program).

        In other words, this functions returns @true if the control hasn't been
        put in read-only mode by a previous call to SetEditable().
    */
    virtual bool IsEditable() const;

    /**
        Returns @true if the control is currently empty.

        This is the same as @c GetValue().empty() but can be much more
        efficient for the multiline controls containing big amounts of text.

        @since 2.7.1
    */
    virtual bool IsEmpty() const;

    /**
        Pastes text from the clipboard to the text item.
    */
    virtual void Paste();

    /**
        If there is a redo facility and the last operation can be redone,
        redoes the last operation.

        Does nothing if there is no redo facility.
    */
    virtual void Redo();

    /**
        Removes the text starting at the first given position up to
        (but not including) the character at the last position.

        This function puts the current insertion point position at @a to as a
        side effect.

        @param from
            The first position.
        @param to
            The last position.
    */
    virtual void Remove(long from, long to);

    /**
        Replaces the text starting at the first position up to
        (but not including) the character at the last position with the given text.

        This function puts the current insertion point position at @a to as a
        side effect.

        @param from
            The first position.
        @param to
            The last position.
        @param value
            The value to replace the existing text with.
    */
    virtual void Replace(long from, long to, const wxString& value);

    /**
        Makes the text item editable or read-only, overriding the
        @b wxTE_READONLY flag.

        @param editable
            If @true, the control is editable. If @false, the control is
            read-only.

        @see IsEditable()
    */
    virtual void SetEditable(bool editable);

    /**
        Sets the insertion point at the given position.

        @param pos
            Position to set, in the range from 0 to GetLastPosition() inclusive.
    */
    virtual void SetInsertionPoint(long pos);

    /**
        Sets the insertion point at the end of the text control.

        This is equivalent to calling wxTextCtrl::SetInsertionPoint() with
        wxTextCtrl::GetLastPosition() argument.
    */
    virtual void SetInsertionPointEnd();

    /**
        This function sets the maximum number of characters the user can enter
        into the control.

        In other words, it allows limiting the text value length to @a len not
        counting the terminating @c NUL character.

        If @a len is 0, the previously set max length limit, if any, is discarded
        and the user may enter as much text as the underlying native text control widget
        supports (typically at least 32Kb).
        If the user tries to enter more characters into the text control when it
        already is filled up to the maximal length, a @c wxEVT_TEXT_MAXLEN
        event is sent to notify the program about it (giving it the possibility
        to show an explanatory message, for example) and the extra input is discarded.

        Note that in wxGTK this function may only be used with single line text controls.
    */
    virtual void SetMaxLength(unsigned long len);

    /**
        Selects the text starting at the first position up to (but not
        including) the character at the last position.

        If both parameters are equal to -1 all text in the control is selected.

        Notice that the insertion point will be moved to @a from by this
        function.

        @param from
            The first position.
        @param to
            The last position.

        @see SelectAll()
    */
    virtual void SetSelection(long from, long to);

    /**
        Selects all text in the control.

        @see SetSelection()
    */
    virtual void SelectAll();

    /**
        Deselects selected text in the control.

        @since 2.9.5
    */
    virtual void SelectNone();

    /**
        Sets a hint shown in an empty unfocused text control.

        The hints are usually used to indicate to the user what is supposed to
        be entered into the given entry field, e.g. a common use of them is to
        show an explanation of what can be entered in a wxSearchCtrl.

        The hint is shown (usually greyed out) for an empty control until it
        gets focus and is shown again if the control loses it and remains
        empty. It won't be shown once the control has a non-empty value,
        although it will be shown again if the control contents is cleared.
        Because of this, it generally only makes sense to use hints with the
        controls which are initially empty.

        Notice that hints are known as <em>cue banners</em> under MSW or
        <em>placeholder strings</em> under macOS.

        @remarks Currently implemented natively on Windows (Vista and later
            only), macOS and GTK+ (3.2 and later).

            For the platforms without native hints support, the implementation
            has several known limitations. Notably, the hint display will not
            be properly updated if you change wxTextEntry contents
            programmatically when the hint is displayed using methods other
            than SetValue() or ChangeValue() or others which use them
            internally (e.g. Clear()). In other words, currently you should
            avoid calling methods such as WriteText() or Replace() when using
            hints and the text control is empty. If you bind to the control's
            focus and wxEVT_TEXT events, you must call wxEvent::Skip() on them
            so that the generic implementation works correctly.

            Another limitation is that hints are ignored for the controls with
            @c wxTE_PASSWORD style.

        @remarks Hints can be used for single line text controls under all
            platforms, but only MSW and GTK+ 2 support them for multi-line text
            controls, they are ignored for them under the other platforms.

        @since 2.9.0
     */
    virtual bool SetHint(const wxString& hint);

    /**
        Returns the current hint string.

        See SetHint() for more information about hints.

        @since 2.9.0
     */
    virtual wxString GetHint() const;

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
        Returns the margins used by the control. The @c x field of the returned
        point is the horizontal margin and the @c y field is the vertical one.

        @remarks If given margin cannot be accurately determined, its value
                will be set to -1. On some platforms you cannot obtain valid
                margin values until you have called SetMargins().

        @see SetMargins()

        @since 2.9.1
    */
    wxPoint GetMargins() const;

    /**
        Sets the new text control value.

        It also marks the control as not-modified which means that IsModified()
        would return @false immediately after the call to SetValue().

        The insertion point is set to the start of the control (i.e. position
        0) by this function unless the control value doesn't change at all, in
        which case the insertion point is left at its original position.

        Note that, unlike most other functions changing the controls values,
        this function generates a @c wxEVT_TEXT event. To avoid
        this you can use ChangeValue() instead.

        @param value
            The new value to set. It may contain newline characters if the text
            control is multi-line.
    */
    virtual void SetValue(const wxString& value);

    /**
        If there is an undo facility and the last operation can be undone,
        undoes the last operation.

        Does nothing if there is no undo facility.
    */
    virtual void Undo();

    /**
        Writes the text into the text control at the current insertion position.

        @param text
            Text to write to the text control.

        @remarks
            Newlines in the text string are the only control characters
            allowed, and they will cause appropriate line breaks.
            See operator<<() and AppendText() for more convenient ways of
            writing to the window.
            After the write operation, the insertion point will be at the end
            of the inserted text, so subsequent write operations will be appended.
            To append text after the user may have interacted with the control,
            call wxTextCtrl::SetInsertionPointEnd() before writing.
    */
    virtual void WriteText(const wxString& text);
};
