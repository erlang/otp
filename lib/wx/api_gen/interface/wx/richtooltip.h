///////////////////////////////////////////////////////////////////////////////
// Name:        interface/wx/richtooltip.h
// Purpose:     wxRichToolTip class documentation
// Author:      Vadim Zeitlin
// Created:     2011-10-18
// Copyright:   (c) 2011 Vadim Zeitlin <vadim@wxwidgets.org>
// Licence:     wxWindows licence
///////////////////////////////////////////////////////////////////////////////

/**
    Support tip kinds for wxRichToolTip.

    This enum describes the kind of the tip shown which combines both the tip
    position and appearance because the two are related (when the tip is
    positioned asymmetrically, a right handed triangle is used but an
    equilateral one when it's in the middle of a side).

    Automatic selects the tip appearance best suited for the current platform
    and the position best suited for the window the tooltip is shown for, i.e.
    chosen in such a way that the tooltip is always fully on screen.

    Other values describe the position of the tooltip itself, not the window it
    relates to. E.g. wxTipKind_Top places the tip on the top of the tooltip and
    so the tooltip itself is located beneath its associated window.
 */
enum wxTipKind
{
    /// Don't show any tip, the tooltip will be (roughly) rectangular.
    wxTipKind_None,
    /// Show a right triangle tip in the top left corner of the tooltip.
    wxTipKind_TopLeft,
    /// Show an equilateral triangle tip in the middle of the tooltip top side.
    wxTipKind_Top,
    /// Show a right triangle tip in the top right corner of the tooltip.
    wxTipKind_TopRight,
    /// Show a right triangle tip in the bottom left corner of the tooltip.
    wxTipKind_BottomLeft,
    /// Show an equilateral triangle tip in the middle of the tooltip bottom side.
    wxTipKind_Bottom,
    /// Show a right triangle tip in the bottom right corner of the tooltip.
    wxTipKind_BottomRight,
    /**
        Choose the appropriate tip shape and position automatically.

        This is the default and shouldn't normally need to be changed.

        Notice that currently wxTipKind_Top or wxTipKind_Bottom are used under
        Mac while one of the other four values is selected for the other
        platforms.
     */
    wxTipKind_Auto
};
/**
    Allows showing a tool tip with more customizations than wxToolTip.

    Using this class is very simple, to give a standard warning for a password
    text control if the password was entered correctly you could simply do:
    @code
    wxTextCtrl* password = new wxTextCtrl(..., wxTE_PASSWORD);
    ...
    wxRichToolTip tip("Caps Lock is on",
                      "You might have made an error in your password\n"
                      "entry because Caps Lock is turned on.\n"
                      "\n"
                      "Press Caps Lock key to turn it off.");
    tip.SetIcon(wxICON_WARNING);
    tip.ShowFor(password);
    @endcode

    Currently this class has generic implementation that can be used with any
    window and implements all the functionality but doesn't exactly match the
    appearance of the native tooltips (even though it makes some efforts to
    use the style most appropriate for the current platform) and a native MSW
    version which can be only used with text controls and doesn't provide as
    much in the way of customization. Because of this, it's inadvisable to
    customize the tooltips unnecessarily as doing this turns off auto-detection
    of the native style in the generic version and may prevent the native MSW
    version from being used at all.

    Notice that this class is not derived from wxWindow and hence doesn't
    represent a window, even if its ShowFor() method does create one internally
    to show the tooltip.

    The images below show some examples of rich tooltips on different
    platforms, with various customizations applied.

    @library{wxcore}
    @category{miscwnd}
    @appearance{richtooltip}

    @since 2.9.3
 */
class wxRichToolTip
{
public:
    /**
        Constructor must specify the tooltip title and main message.

        The main message can contain embedded new lines. Both the title and
        message must be non-empty.

        Additional attributes can be set later.
     */
    wxRichToolTip(const wxString& title, const wxString& message);

    /**
        Set the background colour.

        If two colours are specified, the background is drawn using a gradient
        from top to bottom, otherwise a single solid colour is used.

        By default the colour or colours most appropriate for the current
        platform are used. If a colour is explicitly set, native MSW version
        won't be used as it doesn't support setting the colour.
     */
    void SetBackgroundColour(const wxColour& col,
                             const wxColour& colEnd = wxColour());

    /**
        Set the small icon to show.

        The icon can be either one of the standard information/warning/error
        ones, i.e. wxICON_INFORMATION, wxICON_WARNING or wxICON_ERROR
        respectively (the question icon doesn't make sense for a tooltip so
        wxICON_QUESTION can't be used here) or a custom icon. The latter is
        unsupported by the native MSW implementation of this class so the use
        of a standard icon is preferred.
     */
    //@{
    void SetIcon(int icon = wxICON_INFORMATION);
    void SetIcon(const wxIcon& icon);
    //@}

    /**
        Set timeout after which the tooltip should disappear and
        optionally set a delay before the tooltip is shown, in milliseconds.

        By default the tooltip is shown immediately and hidden after a
        system-dependent interval of time elapses. This method can be used to
        change this or also disable hiding the tooltip automatically entirely
        by passing 0 in this parameter (but doing this will prevent the native
        MSW version from being used).

        Notice that the tooltip will always be hidden if the user presses a key
        or clicks a mouse button.

        Parameter @a millisecondsDelay is new since wxWidgets 2.9.5.
     */
    void SetTimeout(unsigned millisecondsTimeout, unsigned millisecondsDelay = 0);

    /**
        Choose the tip kind, possibly none.

        See wxTipKind documentation for the possible choices here.

        By default the tip is positioned automatically, as if wxTipKind_Auto
        was used. Native MSW implementation doesn't support setting the tip
        kind explicitly and won't be used if this method is called with any
        value other than wxTipKind_Auto.

        Notice that using non automatic tooltip kind may result in the tooltip
        being positioned partially off screen and it's the callers
        responsibility to ensure that this doesn't happen in this case.
     */
    void SetTipKind(wxTipKind tipKind);

    /**
        Set the title text font.

        By default it's emphasized using the font style or colour appropriate
        for the current platform. Calling this method prevents the native MSW
        implementation from being used as it doesn't support changing the font.
     */
    void SetTitleFont(const wxFont& font);

    /**
        Show the tooltip for the given window and optionally specify where to
        show the tooltip.

        By default the tooltip tip points to the (middle of the) specified
        window which must be non-@NULL or, if @a rect is non-@NULL, the middle
        of the specified wxRect.

        The coordinates of the @a rect parameter are relative to the given window.

        Currently the native MSW implementation is used only if @a win is a
        wxTextCtrl and @a rect is @NULL. This limitation may be removed in the
        future.

        Parameter @a rect is new since wxWidgets 2.9.5.
     */
    void ShowFor(wxWindow* win, const wxRect* rect = NULL);

    /**
        Destructor.

        Notice that destroying this object does not hide the tooltip if it's
        currently shown, it will be hidden and destroyed when the user
        dismisses it or the timeout expires.

        The destructor is non-virtual as this class is not supposed to be
        derived from.
     */
    ~wxRichToolTip();
};
