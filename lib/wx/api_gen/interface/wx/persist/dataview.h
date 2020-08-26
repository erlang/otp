/////////////////////////////////////////////////////////////////////////////
// Name:        wx/persist/dataview.h
// Purpose:     interface of wxPersistentDataViewCtrl
// Author:      Vadim Zeitlin
// Copyright:   (c) 2009 Vadim Zeitlin <vadim@wxwidgets.org>
// Licence:     wxWindows licence
/////////////////////////////////////////////////////////////////////////////

/**
    Persistence adapter for wxDataViewCtrl.

    This adapter handles wxDataViewCtrl column widths and sort order.

    @since 3.1.1
 */
class wxPersistentDataViewCtrl : public wxPersistentWindow<wxDataViewCtrl>
{
public:
    /**
        Constructor.

        @param control The associated control.
     */
    wxPersistentDataViewCtrl(wxDataViewCtrl* control);

    /**
        Save the current column widths and sort order.
     */
    void Save() const override;

    /**
        Restore the column widths and sort order.

        The wxDataViewCtrl must be initialized before calling this function, i.e.
        all of its columns should be already added to it -- otherwise restoring
        their width would have no effect.
     */
    bool Restore() override;
};

/// Overload allowing persistence adapter creation for wxDataViewCtrl objects.
wxPersistentObject *wxCreatePersistentObject(wxDataViewCtrl *book);
