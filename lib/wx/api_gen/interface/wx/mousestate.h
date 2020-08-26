/////////////////////////////////////////////////////////////////////////////
// Name:        wx/mousestate.h
// Purpose:     documentation of wxMouseState
// Author:      wxWidgets team
// Created:     2008-09-19
// Licence:     wxWindows licence
/////////////////////////////////////////////////////////////////////////////

/// Symbolic names for the mouse buttons.
enum wxMouseButton
{
    /// Any mouse button, means to check for any button being pressed for
    /// example.
    wxMOUSE_BTN_ANY     = -1,

    /// None of the mouse buttons.
    wxMOUSE_BTN_NONE    = 0,

    /// Left mouse button.
    wxMOUSE_BTN_LEFT    = 1,

    /// Middle mouse button.
    wxMOUSE_BTN_MIDDLE  = 2,

    /// Right mouse button.
    wxMOUSE_BTN_RIGHT   = 3,

    /// First additional mouse button.
    wxMOUSE_BTN_AUX1    = 4,

    /// Second additional mouse button.
    wxMOUSE_BTN_AUX2    = 5,

    wxMOUSE_BTN_MAX
};


/**
    @class wxMouseState

    Represents the mouse state.

    This class is used as a base class by wxMouseEvent and so its methods may
    be used to obtain information about the mouse state for the mouse events.
    It also inherits from wxKeyboardState and so carries information about the
    keyboard state and not only the mouse one.

    This class is implemented entirely inline in @<wx/mousestate.h@> and thus
    has no linking requirements.

    @nolibrary
    @category{events}

    @see wxGetMouseState(), wxMouseEvent
 */
class wxMouseState : public wxKeyboardState
{
public:
    /**
        Default constructor.
    */
    wxMouseState();

    /**
        Returns X coordinate of the physical mouse event position.
    */
    wxCoord GetX() const;

    /**
        Returns Y coordinate of the physical mouse event position.
    */
    wxCoord GetY() const;

    /**
        Returns the physical mouse position.
    */
    //@{
    wxPoint GetPosition() const;
    void GetPosition(int *x, int *y) const;
    //@}

    /**
        Returns @true if the left mouse button is currently down.
    */
    bool LeftIsDown() const;

    /**
        Returns @true if the middle mouse button is currently down.
    */
    bool MiddleIsDown() const;

    /**
        Returns @true if the right mouse button is currently down.
    */
    bool RightIsDown() const;

    /**
        Returns @true if the first extra button mouse button is currently down.
    */
    bool Aux1IsDown() const;

    /**
        Returns @true if the second extra button mouse button is currently down.
    */
    bool Aux2IsDown() const;


    void SetX(wxCoord x);
    void SetY(wxCoord y);
    void SetPosition(const wxPoint& pos);

    void SetLeftDown(bool down);
    void SetMiddleDown(bool down);
    void SetRightDown(bool down);
    void SetAux1Down(bool down);
    void SetAux2Down(bool down);

    void SetState(const wxMouseState& state);

};


