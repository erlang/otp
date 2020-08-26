/////////////////////////////////////////////////////////////////////////////
// Name:        versioninfo.h
// Purpose:     interface of wxVersionInfo
// Author:      Troels K
// Licence:     wxWindows licence
/////////////////////////////////////////////////////////////////////////////

/**
    @class wxVersionInfo

    wxVersionInfo contains version information.

    This class is used by wxWidgets to provide version information about the
    libraries it uses and itself, but you can also apply it in user space, to
    provide version information about your own libraries, or other libraries
    that you use.

    @library{wxbase}

    @category{data}

    @since 2.9.2
*/
class wxVersionInfo
{
public:
    /**
        Constructor.

        The version information objects need to be initialized with this
        constructor and are immutable once they are created.

        @param name The name of the library or other entity that this object
            pertains to.
        @param major The major version component.
        @param minor The minor version component.
        @param micro The micro version component, 0 by default.
        @param description Free form description of this version, none by
            default.
        @param copyright Copyright string, none by default.
    */
    wxVersionInfo(const wxString& name = wxString(),
                  int major = 0,
                  int minor = 0,
                  int micro = 0,
                  const wxString& description = wxString(),
                  const wxString& copyright = wxString());

    /**
        Get the name of the object (library).

        @return Name string.
    */
    const wxString& GetName() const;

    /**
        Get the major version number.

        @return Major version number.
    */
    int GetMajor() const;

    /**
        Get the minor version number.

        @return Minor version number.
    */
    int GetMinor() const;

    /**
        Get the micro version, or release number.

        @return Micro version, or release number.
    */
    int GetMicro() const;

    /**
        Get the string representation of this version object.

        This function returns the description if it is non-empty or
        GetVersionString() if there is no description.

        @see GetDescription(), GetVersionString()
    */
    wxString ToString() const;

    /**
        Get the string representation.

        The micro component of the version is ignored/not used if it is 0.

        @return The version string in the form "name major.minor[.micro]".
    */
    wxString GetVersionString() const;

    /**
        Return @true if a description string has been specified.

        @see GetDescription()
    */
    bool HasDescription() const;

    /**
        Get the description string.

        The description may be empty.

        @return The description string, free-form.
    */
    const wxString& GetDescription();

    /**
        Returns @true if a copyright string has been specified.

        @see GetCopyright()
    */
    bool HasCopyright() const;

    /**
        Get the copyright string.

        The copyright string may be empty.

        @return The copyright string.
    */
    const wxString& GetCopyright() const;
};

//@}
