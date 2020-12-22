/////////////////////////////////////////////////////////////////////////////
// Name:        iconloc.h
// Purpose:     interface of wxIconLocation
// Author:      wxWidgets team
// Licence:     wxWindows licence
/////////////////////////////////////////////////////////////////////////////

/**
    @class wxIconLocation

    wxIconLocation is a tiny class describing the location of an (external, i.e.
    not embedded into the application resources) icon. For most platforms it simply
    contains the file name but under some others (notably Windows) the same file
    may contain multiple icons and so this class also stores the index of the icon
    inside the file.

    In any case, its details should be of no interest to the application code and
    most of them are not even documented here (on purpose) as it is only meant to
    be used as an opaque class: the application may get the object of this class
    from somewhere and the only reasonable thing to do with it later is to create
    a wxIcon from it.

    @library{wxbase}
    @category{gdi}

    @see wxIcon, wxFileType::GetIcon
*/
class wxIconLocation
{
public:
    /**
        Returns @true if the object is valid, i.e.\ was properly initialized, and
        @false otherwise.
    */
    bool IsOk() const;

    void SetFileName(const wxString& filename);
    const wxString& GetFileName() const;

};

