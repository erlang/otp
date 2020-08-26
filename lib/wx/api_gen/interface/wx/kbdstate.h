/////////////////////////////////////////////////////////////////////////////
// Name:        wx/kbdstate.h
// Purpose:     documentation of wxKeyboardState
// Author:      wxWidgets team
// Created:     2008-09-19
// Licence:     wxWindows licence
/////////////////////////////////////////////////////////////////////////////

/**
    Provides methods for testing the state of the keyboard modifier keys.

    This class is used as a base class of wxKeyEvent and wxMouseState and,
    hence, indirectly, of wxMouseEvent, so its methods may be used to get
    information about the modifier keys which were pressed when the event
    occurred.

    This class is implemented entirely inline in @<wx/kbdstate.h@> and thus has
    no linking requirements.

    @nolibrary
    @category{events}

    @see wxKeyEvent, wxMouseState
 */
class wxKeyboardState
{
public:
    /**
        Constructor initializes the modifier key settings.

        By default, no modifiers are active.
     */
    wxKeyboardState(bool controlDown = false,
                    bool shiftDown = false,
                    bool altDown = false,
                    bool metaDown = false);

    /**
        Return the bit mask of all pressed modifier keys.

        The return value is a combination of @c wxMOD_ALT, @c wxMOD_CONTROL,
        @c wxMOD_SHIFT and @c wxMOD_META bit masks. Additionally, @c wxMOD_NONE
        is defined as 0, i.e. corresponds to no modifiers (see HasAnyModifiers())
        and @c wxMOD_CMD is either @c wxMOD_CONTROL (MSW and Unix) or
        @c wxMOD_META (Mac), see CmdDown().
        See ::wxKeyModifier for the full list of modifiers.

        Notice that this function is easier to use correctly than, for example,
        ControlDown() because when using the latter you also have to remember to
        test that none of the other modifiers is pressed:

        @code
        if ( ControlDown() && !AltDown() && !ShiftDown() && !MetaDown() )
            ... handle Ctrl-XXX ...
        @endcode

        and forgetting to do it can result in serious program bugs (e.g. program
        not working with European keyboard layout where @c AltGr key which is
        seen by the program as combination of CTRL and ALT is used). On the
        other hand, you can simply write:

        @code
        if ( GetModifiers() == wxMOD_CONTROL )
            ... handle Ctrl-XXX ...
        @endcode

        with this function.
    */
    int GetModifiers() const;

    /**
        Returns true if any modifiers at all are pressed.

        This is equivalent to @c GetModifiers() @c != @c wxMOD_NONE.

        Notice that this is different from HasModifiers() method which doesn't
        take e.g. Shift modifier into account. This method is most suitable for
        mouse events when any modifier, including Shift, can change the
        interpretation of the event.

        @since 2.9.5
     */
    bool HasAnyModifiers() const;

    /**
        Returns true if Control or Alt are pressed.

        Checks if Control, Alt or, under macOS only, Command key are pressed
        (notice that the real Control key is still taken into account under OS
        X too).

        This method returns @false if only Shift is pressed for compatibility
        reasons and also because pressing Shift usually doesn't change the
        interpretation of key events, see HasAnyModifiers() if you want to
        take Shift into account as well.
     */
    bool HasModifiers() const;

    /**
        Returns true if the Control key or Apple/Command key under macOS is pressed.

        This function doesn't distinguish between right and left control keys.

        Notice that GetModifiers() should usually be used instead of this one.
     */
    bool ControlDown() const;

    /**
        Returns true if the Control key (also under macOS).

        This function doesn't distinguish between right and left control keys.

        Notice that GetModifiers() should usually be used instead of this one.
     */
    bool RawControlDown() const;

    /**
        Returns true if the Shift key is pressed.

        This function doesn't distinguish between right and left shift keys.

        Notice that GetModifiers() should usually be used instead of this one.
     */
    bool ShiftDown() const;

    /**
        Returns true if the Meta/Windows/Apple key is pressed.

        This function tests the state of the key traditionally called Meta
        under Unix systems, Windows keys under MSW
        Notice that GetModifiers() should usually be used instead of this one.

        @see CmdDown()
     */
    bool MetaDown() const;

    /**
        Returns true if the Alt key is pressed.

        Notice that GetModifiers() should usually be used instead of this one.
     */
    bool AltDown() const;

    /**
        Returns true if the key used for command accelerators is pressed. Same as
        ControlDown(). Deprecated.

        Notice that GetModifiers() should usually be used instead of this one.
     */
    bool CmdDown() const;


    void SetControlDown(bool down);
    void SetRawControlDown(bool down);
    void SetShiftDown(bool down);
    void SetAltDown(bool down);
    void SetMetaDown(bool down);

};

