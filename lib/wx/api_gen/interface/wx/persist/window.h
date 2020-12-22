/////////////////////////////////////////////////////////////////////////////
// Name:        wx/persist/window.h
// Purpose:     interface of wxPersistentWindow<>
// Author:      Vadim Zeitlin
// Copyright:   (c) 2009 Vadim Zeitlin <vadim@wxwidgets.org>
// Licence:     wxWindows licence
/////////////////////////////////////////////////////////////////////////////

/**
    Base class for persistent windows.

    Compared to wxPersistentObject this class does three things:
        - Most importantly, wxPersistentWindow catches wxWindowDestroyEvent
        generated when the window is destroyed and saves its properties
        automatically when it happens.
        - It implements GetName() using wxWindow::GetName() so that the derived
        classes don't need to do it.
        - It adds a convenient wxPersistentWindow::Get() accessor returning the
        window object of the correct type.
 */
template <class T>
class wxPersistentWindow : public wxPersistentObject
{
public:
    /// The type of the associated window.
    typedef T WindowType;

    /**
        Constructor for a persistent window object.

        The constructor uses wxEvtHandler::Connect() to catch
        wxWindowDestroyEvent generated when the window is destroyed and call
        wxPersistenceManager::SaveAndUnregister() when this happens. This
        ensures that the window properties are saved and that this object
        itself is deleted when the window is.
     */
    wxPersistentWindow(WindowType *win);

    WindowType *Get() const { return static_cast<WindowType *>(GetWindow()); }
    /**
        Implements the base class pure virtual method using wxWindow::GetName().

        Notice that window names are usually not unique while this function
        must return a unique (at least among the objects of this type) string.
        Because of this you need to specify a non-default window name in its
        constructor when creating it or explicitly call wxWindow::SetName()
        before saving or restoring persistent properties.
     */
    virtual wxString GetName() const;
};
