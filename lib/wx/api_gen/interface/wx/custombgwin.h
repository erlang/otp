/////////////////////////////////////////////////////////////////////////////
// Name:        wx/custombgwin.h
// Purpose:     Documentation of wxCustomBackgroundWindow.
// Author:      Vadim Zeitlin
// Created:     2011-10-10
// Copyright:   (c) 2011 Vadim Zeitlin <vadim@wxwidgets.org>
// Licence:     wxWindows licence
/////////////////////////////////////////////////////////////////////////////

/**
    A helper class making it possible to use custom background for any window.

    wxWindow itself only provides SetBackgroundColour() method taking a (solid)
    wxColour. This class extends it by allowing to use custom bitmap
    backgrounds with any window, provided that you inherit from it. Notice that
    the usual rule of not interfering with event handling or painting of native
    controls still applies, so you shouldn't try to use custom backgrounds with
    classes such as wxButton (even if this might work on some platforms, it's
    not guaranteed to work in general). But you can use this class in
    conjunction with wxWindow, wxPanel, wxFrame and other similar classes, e.g.
    the erase sample shows how to use it with wxScrolledWindow:

    @code
        #include "wx/custombgwin.h"

        class MyCanvas : public wxCustomBackgroundWindow<wxScrolledWindow>
        {
        public:
            MyCanvas(wxWindow* parent)
            {
                // Notice that we must explicitly call base class Create()
                // instead of using its ctor as wxCustomBackgroundWindow
                // doesn't define any non-default ctors.
                Create(parent, wxID_ANY);

                ...

                SetBackgroundBitmap(bitmap);
            }
        };
    @endcode

    @category{miscwnd}

    @since 2.9.3
 */
template <class W>
class wxCustomBackgroundWindow : public W
{
public:
    /// Trivial default constructor.
    wxCustomBackgroundWindow();

    /**
        Set the background bitmap for this window.

        If @a bmp is a valid bitmap, this bitmap will be tiled over the panel
        background and show through any of its transparent children. Passing an
        invalid bitmap reverts to the default background appearance.

        Notice that you must not prevent the base class EVT_ERASE_BACKGROUND
        handler from running (i.e. not to handle this event yourself) for this
        to work.
    */
    void SetBackgroundBitmap(const wxBitmap& bmp);
};
