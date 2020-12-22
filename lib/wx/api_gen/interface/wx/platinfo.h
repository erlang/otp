/////////////////////////////////////////////////////////////////////////////
// Name:        platinfo.h
// Purpose:     interface of wxPlatformInfo
// Author:      wxWidgets team
// Licence:     wxWindows licence
/////////////////////////////////////////////////////////////////////////////


/**
    The following are the operating systems which are recognized by wxWidgets and
    whose version can be detected at run-time.

    The values of the constants are chosen so that they can be combined as flags;
    this allows checking for operating system families like e.g. @c wxOS_MAC and @c wxOS_UNIX.

    Note that you can obtain more detailed information about the current OS
    version in use by checking the major, minor, and micro version numbers
    returned by ::wxGetOsVersion() or by wxPlatformInfo::GetOSMajorVersion(),
    wxPlatformInfo::GetOSMinorVersion(), and wxPlatformInfo::GetOSMicroVersion().
*/
enum wxOperatingSystemId
{
    wxOS_UNKNOWN = 0,                 //!< returned on error

    wxOS_MAC_OS         = 1 << 0,     //!< Apple Mac OS 8/9/X with Mac paths
    wxOS_MAC_OSX_DARWIN = 1 << 1,     //!< Apple macOS with Unix paths

    //! A combination of all @c wxOS_MAC_* values previously listed.
    wxOS_MAC = wxOS_MAC_OS|wxOS_MAC_OSX_DARWIN,

    wxOS_WINDOWS_NT     = 1 << 3,     //!< Windows NT family (XP/Vista/7/8/10)

    //! Any Windows system, currently can be only wxOS_WINDOWS_NT.
    wxOS_WINDOWS = wxOS_WINDOWS_NT,

    wxOS_UNIX_LINUX     = 1 << 6,       //!< Linux
    wxOS_UNIX_FREEBSD   = 1 << 7,       //!< FreeBSD
    wxOS_UNIX_OPENBSD   = 1 << 8,       //!< OpenBSD
    wxOS_UNIX_NETBSD    = 1 << 9,       //!< NetBSD
    wxOS_UNIX_SOLARIS   = 1 << 10,      //!< SunOS
    wxOS_UNIX_AIX       = 1 << 11,      //!< AIX
    wxOS_UNIX_HPUX      = 1 << 12,      //!< HP/UX

    //! A combination of all @c wxOS_UNIX_* values previously listed.
    wxOS_UNIX = wxOS_UNIX_LINUX     |
                wxOS_UNIX_FREEBSD   |
                wxOS_UNIX_OPENBSD   |
                wxOS_UNIX_NETBSD    |
                wxOS_UNIX_SOLARIS   |
                wxOS_UNIX_AIX       |
                wxOS_UNIX_HPUX
};

/**
    The list of wxWidgets ports.

    Some of them can be used with more than a single (native) toolkit.
*/
enum wxPortId
{
    wxPORT_UNKNOWN  = 0,            //!< returned on error

    wxPORT_BASE     = 1 << 0,       //!< wxBase, no native toolkit used

    wxPORT_MSW      = 1 << 1,       //!< wxMSW, native toolkit is Windows API
    wxPORT_MOTIF    = 1 << 2,       //!< wxMotif, using [Open]Motif or Lesstif
    wxPORT_GTK      = 1 << 3,       //!< wxGTK, using GTK+ 1.x, 2.x, 3.x, GPE
    wxPORT_DFB      = 1 << 4,       //!< wxDFB, using wxUniversal
    wxPORT_X11      = 1 << 5,       //!< wxX11, using wxUniversal
    wxPORT_MAC      = 1 << 7,       //!< wxMac, using Carbon or Classic Mac API
    wxPORT_COCOA    = 1 << 8,       //!< wxCocoa, using Cocoa NextStep/Mac API
    wxPORT_QT       = 1 << 10       //!< wxQT, using Qt 5+
};


/**
    The architecture of the operating system
    (regardless of the build environment of wxWidgets library - see ::wxIsPlatform64Bit()
    documentation for more info).
*/
enum wxArchitecture
{
    wxARCH_INVALID = -1,        //!< returned on error

    wxARCH_32,                  //!< 32 bit
    wxARCH_64,                  //!< 64 bit

    wxARCH_MAX
};


/**
    The endian-ness of the machine.
*/
enum wxEndianness
{
    wxENDIAN_INVALID = -1,      //!< returned on error

    wxENDIAN_BIG,               //!< 4321
    wxENDIAN_LITTLE,            //!< 1234
    wxENDIAN_PDP,               //!< 3412

    wxENDIAN_MAX
};

/**
    A structure containing information about a Linux distribution as returned
    by the @c lsb_release utility.

    See wxGetLinuxDistributionInfo() or wxPlatformInfo::GetLinuxDistributionInfo()
    for more info.
*/
struct wxLinuxDistributionInfo
{
    wxString Id;                //!< The id of the distribution; e.g. "Ubuntu"
    wxString Release;           //!< The version of the distribution; e.g. "9.04"
    wxString CodeName;          //!< The code name of the distribution; e.g. "jaunty"
    wxString Description;       //!< The description of the distribution; e.g. "Ubuntu 9.04"

    bool operator==(const wxLinuxDistributionInfo& ldi) const;
    bool operator!=(const wxLinuxDistributionInfo& ldi) const;
};


/**
    @class wxPlatformInfo

    This class holds information about the operating system, the toolkit and the
    basic architecture of the machine where the application is currently running.

    This class does not only have @e getters for the information above, it also has
    @e setters. This allows you to e.g. save the current platform information in a
    data file (maybe in string form) so that when you later load it, you can easily
    retrieve (see the static getters for string->enum conversion functions) and store
    inside a wxPlatformInfo instance (using its setters) the signature of the system
    which generated it.

    In general however you only need to use the static Get() function and then
    access the various information for the current platform:
    @code
        wxLogMessage("This application is running under %s.",
                     wxPlatformInfo::Get().GetOperatingSystemIdName());
    @endcode

    @library{wxbase}
    @category{cfg}

    @see ::wxGetOsVersion(), wxIsPlatformLittleEndian(), wxIsPlatform64Bit(),
         wxAppTraits, @ref group_funcmacro_networkuseros
*/
class wxPlatformInfo
{
public:

    /**
        Initializes the instance with the values corresponding to the currently
        running platform.

        This is a fast operation because it only requires to copy the values
        internally cached for the currently running platform.

        @see Get()
    */
    wxPlatformInfo();

    /**
        Initializes the object using given values.
    */
    wxPlatformInfo(wxPortId pid,
                   int tkMajor = -1,
                   int tkMinor = -1,
                   wxOperatingSystemId id = wxOS_UNKNOWN,
                   int osMajor = -1,
                   int osMinor = -1,
                   wxArchitecture arch = wxARCH_INVALID,
                   wxEndianness endian = wxENDIAN_INVALID);


    /**
        Returns @true if the OS version is at least @c major.minor.micro.

        @see GetOSMajorVersion(), GetOSMinorVersion(), GetOSMicroVersion(),
             CheckToolkitVersion()
    */
    bool CheckOSVersion(int major, int minor, int micro = 0) const;

    /**
        Returns @true if the toolkit version is at least @c major.minor.micro.

        @see GetToolkitMajorVersion(), GetToolkitMinorVersion(),
             GetToolkitMicroVersion(), CheckOSVersion()
    */
    bool CheckToolkitVersion(int major, int minor, int micro = 0) const;

    /**
        Returns @true if this instance is fully initialized with valid values.
    */
    bool IsOk() const;

    /**
        Returns @true if this wxPlatformInfo describes wxUniversal build.
    */
    bool IsUsingUniversalWidgets() const;

    /**
        Inequality operator. Tests all class' internal variables.
    */
    bool operator!=(const wxPlatformInfo& t) const;

    /**
        Equality operator. Tests all class' internal variables.
    */
    bool operator==(const wxPlatformInfo& t) const;

    /**
        Returns the global wxPlatformInfo object, initialized with the values
        for the currently running platform.
    */
    static const wxPlatformInfo& Get();

    /**
        @name Static enum getters

        These getters allow for easy string-to-enumeration-value conversion.
    */
    //@{

    /**
        Converts the given string to a wxArchitecture enum value or to
        @c wxARCH_INVALID if the given string is not a valid architecture string
        (i.e. does not contain nor @c 32 nor @c 64 strings).
    */
    static wxArchitecture GetArch(const wxString& arch);

    /**
        Converts the given string to a wxEndianness enum value or to
        @c wxENDIAN_INVALID if the given string is not a valid endianness
        string (i.e. does not contain nor little nor big strings).
    */
    static wxEndianness GetEndianness(const wxString& end);

    /**
        Converts the given string to a wxOperatingSystemId enum value or to @c
        wxOS_UNKNOWN if the given string is not a valid operating system name.
    */
    static wxOperatingSystemId GetOperatingSystemId(const wxString& name);

    /**
        Converts the given string to a wxWidgets port ID value or to @c wxPORT_UNKNOWN
        if the given string does not match any of the wxWidgets canonical name ports
        ("wxGTK", "wxMSW", etc) nor any of the short wxWidgets name ports ("gtk", "msw", etc).
    */
    static wxPortId GetPortId(const wxString& portname);

    //@}


    /**
        @name Static string-form getters

        These getters allow for easy enumeration-value-to-string conversion.
    */
    //@{

    /**
        Returns the name for the given wxArchitecture enumeration value.
    */
    static wxString GetArchName(wxArchitecture arch);

    /**
        Returns name for the given wxEndianness enumeration value.
    */
    static wxString GetEndiannessName(wxEndianness end);

    /**
        Returns the operating system family name for the given wxOperatingSystemId
        enumeration value: @c Unix for @c wxOS_UNIX, @c OSX for @c wxOS_MAC_OS,
        @c Windows for @c wxOS_WINDOWS.
    */
    static wxString GetOperatingSystemFamilyName(wxOperatingSystemId os);

    /**
        Returns the name for the given operating system ID value.

        This can be a long name (e.g. <tt>Microsoft Windows NT</tt>);
        use GetOperatingSystemFamilyName() to retrieve a short, generic name.
    */
    static wxString GetOperatingSystemIdName(wxOperatingSystemId os);

    /**
        Returns the name of the given wxWidgets port ID value.
        The @a usingUniversal argument specifies whether the port is in its native
        or wxUniversal variant.

        The returned string always starts with the "wx" prefix and is a mixed-case string.
    */
    static wxString GetPortIdName(wxPortId port, bool usingUniversal);

    /**
        Returns the short name of the given wxWidgets port ID value.
        The @a usingUniversal argument specifies whether the port is in its native
        or wxUniversal variant.

        The returned string does not start with the "wx" prefix and is always lower case.
    */
    static wxString GetPortIdShortName(wxPortId port,
                                       bool usingUniversal);

    /**
        Returns the operating system directory.

        See wxGetOSDirectory() for more info.
    */
    static wxString GetOperatingSystemDirectory();

    //@}


    /**
        @name Getters
    */
    //@{

    /**
        Returns the architecture ID of this wxPlatformInfo instance.
    */
    wxArchitecture GetArchitecture() const;

    /**
        Returns the endianness ID of this wxPlatformInfo instance.
    */
    wxEndianness GetEndianness() const;

    /**
        Returns the run-time major version of the OS associated with this
        wxPlatformInfo instance.

        @see ::wxGetOsVersion(), CheckOSVersion()
    */
    int GetOSMajorVersion() const;

    /**
        Returns the run-time minor version of the OS associated with this
        wxPlatformInfo instance.

        @see ::wxGetOsVersion(), CheckOSVersion()
    */
    int GetOSMinorVersion() const;

    /**
        Returns the run-time micro version of the OS associated with this
        wxPlatformInfo instance.

        @see ::wxGetOsVersion(), CheckOSVersion()

        @since 3.1.1
    */
    int GetOSMicroVersion() const;

    /**
        Returns the operating system ID of this wxPlatformInfo instance.

        See wxGetOsVersion() for more info.
    */
    wxOperatingSystemId GetOperatingSystemId() const;

    /**
        Returns the description of the operating system of this wxPlatformInfo instance.

        See wxGetOsDescription() for more info.
    */
    wxString GetOperatingSystemDescription() const;

    /**
        Returns the wxWidgets port ID associated with this wxPlatformInfo instance.
    */
    wxPortId GetPortId() const;

    /**
        Returns the Linux distribution info associated with this wxPlatformInfo instance.

        See wxGetLinuxDistributionInfo() for more info.
    */
    wxLinuxDistributionInfo GetLinuxDistributionInfo() const;

    /**
        Returns the desktop environment associated with this wxPlatformInfo instance.

        See wxAppTraits::GetDesktopEnvironment() for more info.
    */
    wxString GetDesktopEnvironment() const;

    /**
        Returns the run-time major version of the toolkit associated with this
        wxPlatformInfo instance.

        Note that if GetPortId() returns @c wxPORT_BASE, then this value is zero
        (unless externally modified with SetToolkitVersion()); that is, no native
        toolkit is in use.
        See wxAppTraits::GetToolkitVersion() for more info.

        @see CheckToolkitVersion()
    */
    int GetToolkitMajorVersion() const;

    /**
        Returns the run-time minor version of the toolkit associated with this
        wxPlatformInfo instance.

        Note that if GetPortId() returns @c wxPORT_BASE, then this value is zero
        (unless externally modified with SetToolkitVersion()); that is, no native
        toolkit is in use.
        See wxAppTraits::GetToolkitVersion() for more info.

        @see CheckToolkitVersion()
    */
    int GetToolkitMinorVersion() const;

    /**
        Returns the run-time micro version of the toolkit associated with this
        wxPlatformInfo instance.

        Note that if GetPortId() returns @c wxPORT_BASE, then this value is zero
        (unless externally modified with SetToolkitVersion()); that is, no native
        toolkit is in use.
        See wxAppTraits::GetToolkitVersion() for more info.

        @see CheckToolkitVersion()

        @since 3.1.1
    */
    int GetToolkitMicroVersion() const;

    //@}


    /**
        @name String-form getters
    */
    //@{

    /**
        Returns the name for the architecture of this wxPlatformInfo instance.
    */
    wxString GetArchName() const;

    /**
        Returns the name for the endianness of this wxPlatformInfo instance.
    */
    wxString GetEndiannessName() const;

    /**
        Returns the operating system family name of the OS associated with this
        wxPlatformInfo instance.
    */
    wxString GetOperatingSystemFamilyName() const;

    /**
        Returns the operating system name of the OS associated with this wxPlatformInfo
        instance.
    */
    wxString GetOperatingSystemIdName() const;

    /**
        Returns the name of the wxWidgets port ID associated with this wxPlatformInfo
        instance.
    */
    wxString GetPortIdName() const;

    /**
        Returns the short name of the wxWidgets port ID associated with this
        wxPlatformInfo instance.
    */
    wxString GetPortIdShortName() const;

    //@}



    /**
        @name Setters
    */
    //@{

    /**
        Sets the architecture enum value associated with this wxPlatformInfo instance.
    */
    void SetArchitecture(wxArchitecture n);

    /**
        Sets the endianness enum value associated with this wxPlatformInfo instance.
    */
    void SetEndianness(wxEndianness n);

    /**
        Sets the version of the operating system associated with this wxPlatformInfo
        instance.
    */
    void SetOSVersion(int major, int minor, int micro = 0);

    /**
        Sets the operating system associated with this wxPlatformInfo instance.
    */
    void SetOperatingSystemId(wxOperatingSystemId n);

    /**
        Sets the wxWidgets port ID associated with this wxPlatformInfo instance.
    */
    void SetPortId(wxPortId n);

    /**
        Sets the version of the toolkit associated with this wxPlatformInfo instance.
    */
    void SetToolkitVersion(int major, int minor, int micro = 0);

    /**
        Sets the operating system description associated with this wxPlatformInfo instance.
    */
    void SetOperatingSystemDescription(const wxString& desc);

    /**
        Sets the desktop environment associated with this wxPlatformInfo instance.
    */
    void SetDesktopEnvironment(const wxString& de);

    /**
        Sets the linux distribution info associated with this wxPlatformInfo instance.
    */
    void SetLinuxDistributionInfo(const wxLinuxDistributionInfo& di);

    //@}
};

