/////////////////////////////////////////////////////////////////////////////
// Name:        uiaction.h
// Purpose:     interface of wxUIActionSimulator
// Author:      wxWidgets team
// Licence:     wxWindows licence
/////////////////////////////////////////////////////////////////////////////

/**
    @class wxUIActionSimulator

    wxUIActionSimulator is a class used to simulate user interface actions
    such as a mouse click or a key press.

    Common usage for this class would be to provide playback and record (aka
    macro recording) functionality for users, or to drive unit tests by
    simulating user sessions.

    See the @ref page_samples_uiaction for an example of using this class.

    @since 2.9.2

    @library{wxcore}
*/

class wxUIActionSimulator
{
public:
    /**
        Default constructor.
    */
    wxUIActionSimulator();

    /**
        Move the mouse to the specified coordinates.

        @param x
            x coordinate to move to, in screen coordinates.

        @param y
            y coordinate to move to, in screen coordinates.
    */
    bool MouseMove(long x, long y);

    /**
        Move the mouse to the specified coordinates.

        @param point
            Point to move to, in screen coordinates.
    */
    bool MouseMove(const wxPoint& point);

    /**
        Press a mouse button.

        @param button
            Button to press. Valid constants are @c wxMOUSE_BTN_LEFT,
            @c wxMOUSE_BTN_MIDDLE, and @c wxMOUSE_BTN_RIGHT.
    */
    bool MouseDown(int button = wxMOUSE_BTN_LEFT);

    /**
        Release a mouse button.

        @param button
            Button to press. See wxUIActionSimulator::MouseDown for a list of
            valid constants.
    */
    bool MouseUp(int button = wxMOUSE_BTN_LEFT);
    /**
        Click a mouse button.

        @param button
            Button to press. See wxUIActionSimulator::MouseDown for a list of
            valid constants.
    */
    bool MouseClick(int button = wxMOUSE_BTN_LEFT);
    /**
        Double-click a mouse button.

        @param button
            Button to press. See wxUIActionSimulator::MouseDown for a list of
            valid constants.
    */
    bool MouseDblClick(int button = wxMOUSE_BTN_LEFT);

    /**
        Perform a drag and drop operation.

        @param x1
            x start coordinate, in screen coordinates.

        @param y1
            y start coordinate, in screen coordinates.

        @param x2
            x destination coordinate, in screen coordinates.

        @param y2
            y destination coordinate, in screen coordinates.

        @param button
            Button to press. See wxUIActionSimulator::MouseDown for a list of
            valid constants.
    */
    bool MouseDragDrop(long x1, long y1, long x2, long y2,
                       int button = wxMOUSE_BTN_LEFT);

    /**
        Press a key.

        If you are using modifiers then it needs to be paired with an identical
        KeyUp or the modifiers will not be released (MSW and macOS).

        @param keycode
            Key to operate on, as an integer. It is interpreted as a wxKeyCode.

        @param modifiers
            A combination of ::wxKeyModifier flags to be pressed with the given
            keycode.
    */
    bool KeyDown(int keycode, int modifiers = wxMOD_NONE);

    /**
        Release a key.

        @param keycode
            Key to operate on, as an integer. It is interpreted as a wxKeyCode.

        @param modifiers
            A combination of ::wxKeyModifier flags to be pressed with the given
            keycode.
    */
    bool KeyUp(int keycode, int modifiers = wxMOD_NONE);

    /**
        Press and release a key.

        @param keycode
            Key to operate on, as an integer. It is interpreted as a wxKeyCode.

        @param modifiers
            A combination of ::wxKeyModifier flags to be pressed with the given
            keycode.
    */
    bool Char(int keycode, int modifiers = wxMOD_NONE);

    /**
        Simulate selection of an item with the given text.

        This method selects an item in the currently focused wxChoice,
        wxComboBox, wxListBox and similar controls. It does it by simulating
        keyboard events, so the behaviour should be the same as if the item
        was really selected by the user.

        Notice that the implementation of this method uses wxYield() and so
        events can be dispatched from it.

        @param text
            The text of the item to select.

        @return
            @true if the item @a text was successfully selected or @false if
            the currently focused window is not one of the controls allowing
            item selection or if the item with the given text was not found in
            it.

        @since 3.1.0
     */
    bool Select(const wxString& text);

    /**
        Emulate typing in the keys representing the given string.

        Currently only the ASCII letters are universally supported. Digits and
        punctuation characters can be used with the standard QWERTY (US)
        keyboard layout but may not work with other layouts.

        @param text
            The string, containing only US ASCII characters, to type.
    */
    bool Text(const char* text);
};

