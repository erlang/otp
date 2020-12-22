/////////////////////////////////////////////////////////////////////////////
// Name:        anybutton.h
// Purpose:     interface of wxAnyButton
// Author:      wxWidgets team
// Licence:     wxWindows licence
/////////////////////////////////////////////////////////////////////////////

#define wxBU_LEFT            0x0040
#define wxBU_TOP             0x0080
#define wxBU_RIGHT           0x0100
#define wxBU_BOTTOM          0x0200
#define wxBU_ALIGN_MASK      ( wxBU_LEFT | wxBU_TOP | wxBU_RIGHT | wxBU_BOTTOM )

#define wxBU_EXACTFIT        0x0001
#define wxBU_NOTEXT          0x0002
#define wxBU_AUTODRAW        0x0004 ///< Obsolete, has no effect.


/**
   @class wxAnyButton

   A class for common button functionality used as the base for the
   various button classes.
*/
class wxAnyButton : public wxControl
{
public:
    wxAnyButton();
    ~wxAnyButton();

    /**
        Return the bitmap shown by the button.

        The returned bitmap may be invalid only if the button doesn't show any
        images.

        @see SetBitmap()

        @since 2.9.1
     */
    wxBitmap GetBitmap() const;

    /**
        Returns the bitmap used when the mouse is over the button, which may be
        invalid.

        @see SetBitmapCurrent()

        @since 2.9.1 (available as wxBitmapButton::GetBitmapHover() in previous
            versions)
    */
    wxBitmap GetBitmapCurrent() const;

    /**
        Returns the bitmap for the disabled state, which may be invalid.

        @see SetBitmapDisabled()

        @since 2.9.1 (available in wxBitmapButton only in previous versions)
    */
    wxBitmap GetBitmapDisabled() const;

    /**
        Returns the bitmap for the focused state, which may be invalid.

        @see SetBitmapFocus()

        @since 2.9.1 (available in wxBitmapButton only in previous versions)
    */
    wxBitmap GetBitmapFocus() const;

    /**
        Returns the bitmap for the normal state.

        This is exactly the same as GetBitmap() but uses a name
        backwards-compatible with wxBitmapButton.

        @see SetBitmap(), SetBitmapLabel()

        @since 2.9.1 (available in wxBitmapButton only in previous versions)
    */
    wxBitmap GetBitmapLabel() const;

    /**
        Returns the bitmap for the pressed state, which may be invalid.

        @see SetBitmapPressed()

        @since 2.9.1 (available as wxBitmapButton::GetBitmapSelected() in
            previous versions)
    */
    wxBitmap GetBitmapPressed() const;


    /**
        Sets the bitmap to display in the button.

        The bitmap is displayed together with the button label. This method
        sets up a single bitmap which is used in all button states, use
        SetBitmapDisabled(), SetBitmapPressed(), SetBitmapCurrent() or
        SetBitmapFocus() to change the individual images used in different
        states.

        @param bitmap
            The bitmap to display in the button. If the bitmap is invalid, any
            currently shown bitmaps are removed from the button.
        @param dir
            The position of the bitmap inside the button. By default it is
            positioned to the left of the text, near to the left button border.
            Other possible values include wxRIGHT, wxTOP and wxBOTTOM.

        @see SetBitmapPosition(), SetBitmapMargins()

        @since 2.9.1
     */
    void SetBitmap(const wxBitmap& bitmap, wxDirection dir = wxLEFT);

    /**
        Sets the bitmap to be shown when the mouse is over the button.

        If @a bitmap is invalid, the normal bitmap will be used in the current
        state.

        @see GetBitmapCurrent()

        @since 2.9.1 (available as wxBitmapButton::SetBitmapHover() in previous
            versions)
    */
    void SetBitmapCurrent(const wxBitmap& bitmap);

    /**
        Sets the bitmap for the disabled button appearance.

        If @a bitmap is invalid, the disabled bitmap is set to the
        automatically generated greyed out version of the normal bitmap, i.e.
        the same bitmap as is used by default if this method is not called at
        all. Use SetBitmap() with an invalid bitmap to remove the bitmap
        completely (for all states).

        @see GetBitmapDisabled(), SetBitmapLabel(),
             SetBitmapPressed(), SetBitmapFocus()

        @since 2.9.1 (available in wxBitmapButton only in previous versions)
    */
    void SetBitmapDisabled(const wxBitmap& bitmap);

    /**
        Sets the bitmap for the button appearance when it has the keyboard
        focus.

        If @a bitmap is invalid, the normal bitmap will be used in the focused
        state.

        @see GetBitmapFocus(), SetBitmapLabel(),
             SetBitmapPressed(), SetBitmapDisabled()

        @since 2.9.1 (available in wxBitmapButton only in previous versions)
    */
    void SetBitmapFocus(const wxBitmap& bitmap);

    /**
        Sets the bitmap label for the button.

        @remarks This is the bitmap used for the unselected state, and for all
                 other states if no other bitmaps are provided.

        @see SetBitmap(), GetBitmapLabel()

        @since 2.9.1 (available in wxBitmapButton only in previous versions)
    */
    void SetBitmapLabel(const wxBitmap& bitmap);

    /**
        Sets the bitmap for the selected (depressed) button appearance.

        @since 2.9.1 (available as wxBitmapButton::SetBitmapSelected() in
            previous versions)
    */
    void SetBitmapPressed(const wxBitmap& bitmap);


    /**
        Get the margins between the bitmap and the text of the button.

        @see SetBitmapMargins()

        @since 2.9.1
     */
    wxSize GetBitmapMargins();

    /**
        Set the margins between the bitmap and the text of the button.

        This method is currently only implemented under MSW. If it is not
        called, default margin is used around the bitmap.

        @see SetBitmap(), SetBitmapPosition()

        @since 2.9.1
     */
    //@{
    void SetBitmapMargins(wxCoord x, wxCoord y);
    void SetBitmapMargins(const wxSize& sz);
    //@}

    /**
        Set the position at which the bitmap is displayed.

        This method should only be called if the button does have an associated
        bitmap.

        @since 2.9.1

        @param dir
            Direction in which the bitmap should be positioned, one of wxLEFT,
            wxRIGHT, wxTOP or wxBOTTOM.
     */
    void SetBitmapPosition(wxDirection dir);
};

