/////////////////////////////////////////////////////////////////////////////
// Name:        popupwin.h
// Purpose:     interface of wxPopupWindow
// Author:      wxWidgets team
// Licence:     wxWindows licence
/////////////////////////////////////////////////////////////////////////////

#define wxPU_CONTAINS_CONTROLS     0x0001

/**
    @class wxPopupWindow

    A special kind of top level window used for popup menus,
    combobox popups and such.

    @beginStyleTable
    @style{wxPU_CONTAINS_CONTROLS}
        By default in wxMSW, a popup window will not take focus from its parent
        window. However many standard controls, including common ones such as
        wxTextCtrl, need focus to function correctly and will not work when
        placed on a default popup. This flag can be used to make the popup take
        focus and let all controls work but at the price of not allowing the
        parent window to keep focus while the popup is shown, which can also be
        sometimes desirable. This style is currently only implemented in MSW
        and simply does nothing under the other platforms (it's new since
        wxWidgets 3.1.3).
    @endStyleTable

    @library{wxcore}
    @category{managedwnd}

    @see wxDialog, wxFrame
*/

class wxPopupWindow: public wxNonOwnedWindow
{
public:

    /**
      Default constructor
    */
    wxPopupWindow();

    /**
      Constructor
    */
    wxPopupWindow(wxWindow *parent, int flags = wxBORDER_NONE);

    /**
      Create method for two-step creation
    */
    bool Create(wxWindow *parent, int flags = wxBORDER_NONE);

    /**
        Move the popup window to the right position, i.e.\ such that it is
        entirely visible.

        The popup is positioned at ptOrigin + size if it opens below and to the
        right (default), at ptOrigin - sizePopup if it opens above and to the
        left etc.

        @param ptOrigin
            Must be given in screen coordinates!
        @param sizePopup
            The size of the popup window
    */
    virtual void Position(const wxPoint& ptOrigin,
                          const wxSize& sizePopup);
};

/**
    @class wxPopupTransientWindow

    A wxPopupWindow which disappears automatically when the user clicks mouse
    outside it or if it loses focus in any other way.

    This window can be useful for implementing custom combobox-like controls
    for example.

    @library{wxcore}
    @category{managedwnd}

    @see wxPopupWindow
*/

class wxPopupTransientWindow : public wxPopupWindow
{
public:
    /**
        Default constructor.
    */
    wxPopupTransientWindow();

    /**
        Constructor.
    */
    wxPopupTransientWindow(wxWindow *parent, int flags = wxBORDER_NONE);

    /**
        Popup the window (this will show it too).

        If @a focus is non-@NULL, it will be kept focused while this window
        is shown if supported by the current platform, otherwise the popup
        itself will receive focus. In any case, the popup will disappear
        automatically if it loses focus because of a user action.

        @see Dismiss()
    */
    virtual void Popup(wxWindow *focus = NULL);

    /**
        Hide the window.
    */
    virtual void Dismiss();

    /**
        Called when a mouse is pressed while the popup is shown.

        Return @true from here to prevent its normal processing by the popup
        (which consists in dismissing it if the mouse is clicked outside it).
    */
    virtual bool ProcessLeftDown(wxMouseEvent& event);

protected:
    /**
       This is called when the popup is disappeared because of anything
       else but direct call to Dismiss().
    */
    virtual void OnDismiss();

};
