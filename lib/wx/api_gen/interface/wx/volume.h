/////////////////////////////////////////////////////////////////////////////
// Name:        volume.h
// Purpose:     interface of wxFSVolume
// Author:      wxWidgets team
// Licence:     wxWindows licence
/////////////////////////////////////////////////////////////////////////////

/**
    The volume flags.
*/
enum wxFSVolumeFlags
{
    /// Is the volume mounted?
    wxFS_VOL_MOUNTED = 0x0001,

    /// Is the volume removable (floppy, CD, ...)?
    wxFS_VOL_REMOVABLE = 0x0002,

    /// Read only? (otherwise read write).
    wxFS_VOL_READONLY = 0x0004,

    /// Network resources.
    wxFS_VOL_REMOTE = 0x0008
};

/**
    The volume types.
*/
enum wxFSVolumeKind
{
    wxFS_VOL_FLOPPY,
    wxFS_VOL_DISK,
    wxFS_VOL_CDROM,
    wxFS_VOL_DVDROM,
    wxFS_VOL_NETWORK,
    wxFS_VOL_OTHER,
    wxFS_VOL_MAX
};

/**
    Icon types used by wxFSVolume.
*/
enum wxFSIconType
{
    wxFS_VOL_ICO_SMALL = 0,
    wxFS_VOL_ICO_LARGE,
    wxFS_VOL_ICO_SEL_SMALL,
    wxFS_VOL_ICO_SEL_LARGE,
    wxFS_VOL_ICO_MAX
};

/**
    @class wxFSVolume

    wxFSVolume represents a volume (also known as 'drive') in a file system
    under wxMSW.

    Unix ports of wxWidgets do not have the concept of volumes and thus do
    not implement wxFSVolume.

    @onlyfor{wxmsw}

    @library{wxbase}
    @category{misc}
*/
class wxFSVolume
{
public:
    /**
        Default ctor. Use Create() later.
    */
    wxFSVolume();

    /**
        Create the volume object with the given @a name (which should be one of
        those returned by GetVolumes()).
    */
    wxFSVolume(const wxString& name);

    /**
        Create the volume object with the given @a name (which should be one of
        those returned by GetVolumes()).
    */
    bool Create(const wxString& name);

    /**
        Returns an array containing the names of the volumes of this system.

        Only the volumes with @e flags such that the expression
        @code (flags & flagsSet) == flagsSet && !(flags & flagsUnset) @endcode
        is @true, are returned. By default, all mounted ones are returned.
        See ::wxFSVolumeFlags enumeration values for a list of valid flags.

        This operation may take a while and, even if this function is
        synchronous, it can be stopped using CancelSearch().
    */
    static wxArrayString GetVolumes(int flagsSet = wxFS_VOL_MOUNTED,
                                    int flagsUnset = 0);

    /**
        Stops execution of GetVolumes() called previously (should be called from
        another thread, of course).
    */
    static void CancelSearch();

    /**
        Is this a valid volume?
    */
    bool IsOk() const;

    /**
        Returns the kind of this volume.
    */
    wxFSVolumeKind GetKind() const;

    /**
        Returns the flags of this volume. See ::wxFSVolumeFlags enumeration values.
    */
    int GetFlags() const;

    /**
        Returns @true if this volume is writable.
    */
    bool IsWritable() const;

    /**
        Returns the name of the volume; this is the internal name
        for the volume used by the operating system.
    */
    wxString GetName() const;

    /**
        Returns the name of the volume meant to be shown to the user.
    */
    wxString GetDisplayName() const;

    /**
        This function is available only when @c wxUSE_GUI is @c 1.

        Returns the icon used by the native toolkit for the given file system type.
    */
    wxIcon GetIcon(wxFSIconType type) const;
};
