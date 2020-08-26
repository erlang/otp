/////////////////////////////////////////////////////////////////////////////
// Name:        wx/persist/bookctrl.h
// Purpose:     interface of wxPersistentBookCtrl
// Author:      Vadim Zeitlin
// Copyright:   (c) 2009 Vadim Zeitlin <vadim@wxwidgets.org>
// Licence:     wxWindows licence
/////////////////////////////////////////////////////////////////////////////

/**
    Persistence adapter for wxBookCtrlBase.

    This adapter handles the selected page of wxBookCtrlBase, i.e. it saves its
    value when the associated book control is destroyed and restores it when it
    is recreated.

    @see wxPersistentTreeBookCtrl
 */
class wxPersistentBookCtrl : public wxPersistentWindow<wxBookCtrlBase>
{
public:
    /**
        Constructor.

        @param book
            The associated book control.
     */
    wxPersistentBookCtrl(wxBookCtrlBase *book);

    /**
        Save the currently selected page index.
     */
    virtual void Save() const;

    /**
        Restore the selected page index.

        The book control must be initialized before calling this function, i.e.
        all of its pages should be already added to it -- otherwise restoring
        the selection has no effect.
     */
    virtual bool Restore();
};

/// Overload allowing persistence adapter creation for wxBookCtrlBase-derived
/// objects.
wxPersistentObject *wxCreatePersistentObject(wxBookCtrlBase *book);
