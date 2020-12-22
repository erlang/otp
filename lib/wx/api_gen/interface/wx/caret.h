/////////////////////////////////////////////////////////////////////////////
// Name:        caret.h
// Purpose:     interface of wxCaret
// Author:      wxWidgets team
// Licence:     wxWindows licence
/////////////////////////////////////////////////////////////////////////////

/**
    @class wxCaret

    A caret is a blinking cursor showing the position where the typed text will
    appear. Text controls usually have their own caret but wxCaret provides a
    way to use a caret in other windows.

    Currently, the caret appears as a rectangle of the given size. In the
    future, it will be possible to specify a bitmap to be used for the caret
    shape.

    A caret is always associated with a window and the current caret can be
    retrieved using wxWindow::GetCaret(). The same caret can't be reused in two
    different windows.

    @library{wxcore}
    @category{misc}
*/
class wxCaret
{
public:
    /**
        Default constructor.
    */
    wxCaret();

    //@{
    /**
        Creates a caret with the given size (in pixels) and associates it with
        the @a window.
    */
    wxCaret(wxWindow* window, int width, int height);
    wxCaret(wxWindow* window, const wxSize& size);
    //@}

    //@{
    /**
        Creates a caret with the given size (in pixels) and associates it with
        the @a window (same as the equivalent constructors).
    */
    bool Create(wxWindow* window, int width, int height);
    bool Create(wxWindow* window, const wxSize& size);
    //@}

    /**
        Returns the blink time which is measured in milliseconds and is the
        time elapsed between 2 inversions of the caret (blink time of the caret
        is the same for all carets, so this functions is static).
    */
    static int GetBlinkTime();

    //@{
    /**
        Get the caret position (in pixels).

        @beginWxPerlOnly
        In wxPerl there are two methods instead of a single overloaded
        method:
        - GetPosition(): returns a Wx::Point object.
        - GetPositionXY(): returns a 2-element list (x, y).
        @endWxPerlOnly
    */
    void GetPosition(int* x, int* y) const;
    wxPoint GetPosition() const;
    //@}

    //@{
    /**
        Get the caret size.

        @beginWxPerlOnly
        In wxPerl there are two methods instead of a single overloaded
        method:
        - GetSize(): returns a Wx::Size object.
        - GetSizeWH(): returns a 2-element list (width, height).
        @endWxPerlOnly
    */
    void GetSize(int* width, int* height) const;
    wxSize GetSize() const;
    //@}

    /**
        Get the window the caret is associated with.
    */
    wxWindow* GetWindow() const;

    /**
        Hides the caret, same as Show(@false).
    */
    virtual void Hide();

    /**
        Returns @true if the caret was created successfully.
    */
    bool IsOk() const;

    /**
        Returns @true if the caret is visible and @false if it is permanently
        hidden (if it is blinking and not shown currently but will be after
        the next blink, this method still returns @true).
    */
    bool IsVisible() const;

    //@{
    /**
        Move the caret to given position (in logical coordinates).
    */
    void Move(int x, int y);
    void Move(const wxPoint& pt);
    //@}

    /**
        Sets the blink time for all the carets.

        @warning Under Windows, this function will change the blink time for
                 all carets permanently (until the next time it is called),
                 even for carets in other applications.

        @see GetBlinkTime()
    */
    static void SetBlinkTime(int milliseconds);

    //@{
    /**
        Changes the size of the caret.
    */
    void SetSize(int width, int height);
    void SetSize(const wxSize& size);
    //@}

    /**
        Shows or hides the caret. Notice that if the caret was hidden N times,
        it must be shown N times as well to reappear on the screen.
    */
    virtual void Show(bool show = true);
};

