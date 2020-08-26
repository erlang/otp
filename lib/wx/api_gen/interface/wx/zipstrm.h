/////////////////////////////////////////////////////////////////////////////
// Name:        zipstrm.h
// Purpose:     interface of wxZipNotifier
// Author:      wxWidgets team
// Licence:     wxWindows licence
/////////////////////////////////////////////////////////////////////////////



/// Compression Method, only 0 (store) and 8 (deflate) are supported here
enum wxZipMethod
{
    wxZIP_METHOD_STORE,
    wxZIP_METHOD_SHRINK,
    wxZIP_METHOD_REDUCE1,
    wxZIP_METHOD_REDUCE2,
    wxZIP_METHOD_REDUCE3,
    wxZIP_METHOD_REDUCE4,
    wxZIP_METHOD_IMPLODE,
    wxZIP_METHOD_TOKENIZE,
    wxZIP_METHOD_DEFLATE,
    wxZIP_METHOD_DEFLATE64,
    wxZIP_METHOD_BZIP2 = 12,
    wxZIP_METHOD_DEFAULT = 0xffff
};

/// Originating File-System.
///
/// These are Pkware's values. Note that Info-zip disagree on some of them,
/// most notably NTFS.
enum wxZipSystem
{
    wxZIP_SYSTEM_MSDOS,
    wxZIP_SYSTEM_AMIGA,
    wxZIP_SYSTEM_OPENVMS,
    wxZIP_SYSTEM_UNIX,
    wxZIP_SYSTEM_VM_CMS,
    wxZIP_SYSTEM_ATARI_ST,
    wxZIP_SYSTEM_OS2_HPFS,
    wxZIP_SYSTEM_MACINTOSH,
    wxZIP_SYSTEM_Z_SYSTEM,
    wxZIP_SYSTEM_CPM,
    wxZIP_SYSTEM_WINDOWS_NTFS,
    wxZIP_SYSTEM_MVS,
    wxZIP_SYSTEM_VSE,
    wxZIP_SYSTEM_ACORN_RISC,
    wxZIP_SYSTEM_VFAT,
    wxZIP_SYSTEM_ALTERNATE_MVS,
    wxZIP_SYSTEM_BEOS,
    wxZIP_SYSTEM_TANDEM,
    wxZIP_SYSTEM_OS_400
};

/// Dos/Win file attributes
enum wxZipAttributes
{
    wxZIP_A_RDONLY = 0x01,
    wxZIP_A_HIDDEN = 0x02,
    wxZIP_A_SYSTEM = 0x04,
    wxZIP_A_SUBDIR = 0x10,
    wxZIP_A_ARCH   = 0x20,

    wxZIP_A_MASK   = 0x37
};

/// Values for the flags field in the zip headers
enum wxZipFlags
{
    wxZIP_ENCRYPTED         = 0x0001,
    wxZIP_DEFLATE_NORMAL    = 0x0000,   // normal compression
    wxZIP_DEFLATE_EXTRA     = 0x0002,   // extra compression
    wxZIP_DEFLATE_FAST      = 0x0004,   // fast compression
    wxZIP_DEFLATE_SUPERFAST = 0x0006,   // superfast compression
    wxZIP_DEFLATE_MASK      = 0x0006,
    wxZIP_SUMS_FOLLOW       = 0x0008,   // crc and sizes come after the data
    wxZIP_ENHANCED          = 0x0010,
    wxZIP_PATCH             = 0x0020,
    wxZIP_STRONG_ENC        = 0x0040,
    wxZIP_UNUSED            = 0x0F80,
    wxZIP_RESERVED          = 0xF000
};

/**
    Zip archive format

    @since 3.1.1
*/
enum wxZipArchiveFormat
{
    /// Default zip format: use ZIP64 if it is determined to be necessary.
    wxZIP_FORMAT_DEFAULT,
    /// ZIP64 format: force the use of ZIP64 format.
    wxZIP_FORMAT_ZIP64
};


/**
    @class wxZipNotifier

    If you need to know when a wxZipInputStream updates a wxZipEntry,
    you can create a notifier by deriving from this abstract base class,
    overriding wxZipNotifier::OnEntryUpdated().

    An instance of your notifier class can then be assigned to wxZipEntry
    objects, using wxZipEntry::SetNotifier().

    Setting a notifier is not usually necessary. It is used to handle
    certain cases when modifying an zip in a pipeline (i.e. between
    non-seekable streams).
    See @ref overview_archive_noseek.

    @library{wxbase}
    @category{archive,streams}

    @see @ref overview_archive_noseek, wxZipEntry, wxZipInputStream, wxZipOutputStream
*/
class wxZipNotifier
{
public:
    /**
        Override this to receive notifications when an wxZipEntry object changes.
    */
    virtual void OnEntryUpdated(wxZipEntry& entry) = 0;
};



/**
    @class wxZipEntry

    Holds the meta-data for an entry in a zip.

    @section zipentry_avail Field availability

    When reading a zip from a stream that is seekable, wxZipInputStream::GetNextEntry()
    returns a fully populated wxZipEntry object except for wxZipEntry::GetLocalExtra().
    wxZipEntry::GetLocalExtra() becomes available when the entry is opened, either by
    calling wxZipInputStream::OpenEntry() or by making an attempt to read the entry's data.

    For zips on non-seekable streams, the following fields are always available
    when wxZipInputStream::GetNextEntry() returns:
    - wxZipEntry::GetDateTime
    - wxZipEntry::GetInternalFormat
    - wxZipEntry::GetInternalName
    - wxZipEntry::GetFlags
    - wxZipEntry::GetLocalExtra
    - wxZipEntry::GetMethod
    - wxZipEntry::GetName
    - wxZipEntry::GetOffset
    - wxZipEntry::IsDir

    The following fields are also usually available when GetNextEntry() returns,
    however, if the zip was also written to a non-seekable stream the zipper is
    permitted to store them after the entry's data. In that case they become
    available when the entry's data has been read to Eof(), or CloseEntry()
    has been called. (GetFlags() & wxZIP_SUMS_FOLLOW) != 0 indicates that
    one or more of these come after the data:
    - wxZipEntry::GetCompressedSize
    - wxZipEntry::GetCrc
    - wxZipEntry::GetSize

    The following are stored at the end of the zip, and become available when the
    end of the zip has been reached, i.e. after GetNextEntry() returns @NULL
    and Eof() is true:
    - wxZipEntry::GetComment
    - wxZipEntry::GetExternalAttributes
    - wxZipEntry::GetExtra
    - wxZipEntry::GetMode
    - wxZipEntry::GetSystemMadeBy
    - wxZipEntry::IsReadOnly
    - wxZipEntry::IsMadeByUnix
    - wxZipEntry::IsText

    @library{wxbase}
    @category{archive,streams}

    @see @ref overview_archive, wxZipInputStream, wxZipOutputStream, wxZipNotifier
*/
class wxZipEntry : public wxArchiveEntry
{
public:
    wxZipEntry(const wxString& name = wxEmptyString,
               const wxDateTime& dt = Now(),
               wxFileOffset size = wxInvalidOffset);

    /**
        Copy constructor.
    */
    wxZipEntry(const wxZipEntry& entry);

    /**
        Make a copy of this entry.
    */
    wxZipEntry* Clone() const;

    //@{
    /**
        Gets and sets the short comment for this entry.
    */
    wxString GetComment() const;
    void SetComment(const wxString& comment);
    //@}

    //@{
    /**
        The low 8 bits are always the DOS/Windows file attributes for this entry.
        The values of these attributes are given in the enumeration ::wxZipAttributes.

        The remaining bits can store platform specific permission bits or
        attributes, and their meaning depends on the value of SetSystemMadeBy().
        If IsMadeByUnix() is @true then the high 16 bits are unix mode bits.

        The following other accessors access these bits:
        - IsReadOnly() / SetIsReadOnly()
        - IsDir() / SetIsDir()
        - GetMode() / SetMode()
    */
    wxUint32 GetExternalAttributes() const;
    void SetExternalAttributes(wxUint32 attr);
    //@}

    //@{
    /**
        The extra field from the entry's central directory record.

        The extra field is used to store platform or application specific
        data. See Pkware's document 'appnote.txt' for information on its format.
    */
    const char* GetExtra() const;
    size_t GetExtraLen() const;
    void SetExtra(const char* extra, size_t len);
    //@}

    //@{
    /**
        The extra field from the entry's local record.

        The extra field is used to store platform or application specific
        data. See Pkware's document 'appnote.txt' for information on its format.
    */
    const char* GetLocalExtra() const;
    size_t GetLocalExtraLen() const;
    void SetLocalExtra(const char* extra, size_t len);
    //@}

    //@{
    /**
        The compression method.
        The enumeration ::wxZipMethod lists the possible values.

        The default constructor sets this to @c wxZIP_METHOD_DEFAULT,
        which allows wxZipOutputStream to choose the method when writing the entry.
    */
    int GetMethod() const;
    void SetMethod(int method);
    //@}

    //@{
    /**
        If IsMadeByUnix() is true then returns the unix permission bits stored
        in GetExternalAttributes(). Otherwise synthesises them from the DOS attributes.
    */
    int GetMode() const;

    /**
        Sets the DOS attributes in GetExternalAttributes() to be consistent with
        the @a mode given.

        If IsMadeByUnix() is @true then also stores @a mode in GetExternalAttributes().
        Note that the default constructor sets GetSystemMadeBy() to
        @c wxZIP_SYSTEM_MSDOS by default. So to be able to store unix
        permissions when creating zips, call SetSystemMadeBy(wxZIP_SYSTEM_UNIX).
    */
    void SetMode(int mode);
    //@}

    //@{
    /**
        The originating file-system.

        The default constructor sets this to @c wxZIP_SYSTEM_MSDOS.
        Set it to @c wxZIP_SYSTEM_UNIX in order to be able to store unix
        permissions using SetMode().
    */
    int GetSystemMadeBy() const;
    void SetSystemMadeBy(int system);
    //@}

    /**
        The compressed size of this entry in bytes.
    */
    wxFileOffset GetCompressedSize() const;

    /**
        CRC32 for this entry's data.
    */
    wxUint32 GetCrc() const;

    /**
        Returns a combination of the bits flags in the enumeration @c wxZipFlags.
    */
    int GetFlags() const;

    /**
        A static member that translates a filename into the internal format used
        within the archive. If the third parameter is provided, the bool pointed
        to is set to indicate whether the name looks like a directory name
        (i.e. has a trailing path separator).

        @see @ref overview_archive_byname
    */
    static wxString GetInternalName(const wxString& name,
                                    wxPathFormat format = wxPATH_NATIVE,
                                    bool* pIsDir = NULL);
    /**
        Returns the entry's filename in the internal format used within the archive.
        The name can include directory components, i.e. it can be a full path.

        The names of directory entries are returned without any trailing path separator.
        This gives a canonical name that can be used in comparisons.
    */
    wxString GetInternalName() const;

    /**
        Returns @true if GetSystemMadeBy() is a flavour of unix.
    */
    bool IsMadeByUnix() const;

    //@{
    /**
        Indicates that this entry's data is text in an 8-bit encoding.
    */
    bool IsText() const;
    void SetIsText(bool isText = true);
    //@}

    //@{
    /**
        Sets the notifier (see wxZipNotifier) for this entry.
        Whenever the wxZipInputStream updates this entry, it will then invoke
        the associated notifier's wxZipNotifier::OnEntryUpdated() method.

        Setting a notifier is not usually necessary. It is used to handle
        certain cases when modifying an zip in a pipeline (i.e. between
        non-seekable streams).

        @see @ref overview_archive_noseek, wxZipNotifier
    */
    void SetNotifier(wxZipNotifier& notifier);
    void UnsetNotifier();
    //@}

    /**
        Assignment operator.
    */
    wxZipEntry& operator=(const wxZipEntry& entry);
};


/**
    @class wxZipInputStream

    Input stream for reading zip files.

    wxZipInputStream::GetNextEntry() returns a wxZipEntry object containing the
    meta-data for the next entry in the zip (and gives away ownership).
    Reading from the wxZipInputStream then returns the entry's data.
    Eof() becomes @true after an attempt has been made to read past the end of
    the entry's data.
    When there are no more entries, GetNextEntry() returns @NULL and sets Eof().

    Note that in general zip entries are not seekable, and
    wxZipInputStream::SeekI() always returns ::wxInvalidOffset.

    @library{wxbase}
    @category{archive,streams}

    @see @ref overview_archive, wxZipEntry, wxZipOutputStream
*/
class wxZipInputStream : public wxArchiveInputStream
{
public:

    //@{
    /**
        Constructor. In a Unicode build the second parameter @a conv is used to
        translate the filename and comment fields into Unicode.
        It has no effect on the stream's data.
        If the parent stream is passed as a pointer then the new filter stream
        takes ownership of it. If it is passed by reference then it does not.
    */
    wxZipInputStream(wxInputStream& stream,
                     wxMBConv& conv = wxConvLocal);
    wxZipInputStream(wxInputStream* stream,
                     wxMBConv& conv = wxConvLocal);
    //@}

    /**
        Closes the current entry.
        On a non-seekable stream reads to the end of the current entry first.
    */
    bool CloseEntry();

    /**
        Returns the zip comment.

        This is stored at the end of the zip, therefore when reading a zip
        from a non-seekable stream, it returns the empty string until the end
        of the zip has been reached, i.e. when GetNextEntry() returns @NULL.
    */
    wxString GetComment();

    /**
        Closes the current entry if one is open, then reads the meta-data for
        the next entry and returns it in a wxZipEntry object, giving away ownership.
        The stream is then open and can be read.
    */
    wxZipEntry* GetNextEntry();

    /**
        For a zip on a seekable stream returns the total number of entries in
        the zip. For zips on non-seekable streams returns the number of entries
        returned so far by GetNextEntry().
    */
    int GetTotalEntries();

    /**
        Closes the current entry if one is open, then opens the entry specified
        by the @a entry object.

        @a entry should be from the same zip file, and the zip should
        be on a seekable stream.

        @see overview_archive_byname
    */
    bool OpenEntry(wxZipEntry& entry);
};



/**
    @class wxZipClassFactory

    Class factory for the zip archive format.
    See the base class for details.

    @library{wxbase}
    @category{archive,streams}

    @see @ref overview_archive,
         @ref overview_archive_generic,
         wxZipEntry, wxZipInputStream, wxZipOutputStream
*/
class wxZipClassFactory : public wxArchiveClassFactory
{
public:

};



/**
    @class wxZipOutputStream

    Output stream for writing zip files.

    wxZipOutputStream::PutNextEntry() is used to create a new entry in the
    output zip, then the entry's data is written to the wxZipOutputStream.
    Another call to wxZipOutputStream::PutNextEntry() closes the current
    entry and begins the next.

    @library{wxbase}
    @category{archive,streams}

    @see @ref overview_archive, wxZipEntry, wxZipInputStream
*/
class wxZipOutputStream : public wxArchiveOutputStream
{
public:
    //@{
    /**
        Constructor.

        @a level is the compression level to use.
        It can be a value between 0 and 9 or -1 to use the default value
        which currently is equivalent to 6.

        If the parent stream is passed as a pointer then the new filter stream
        takes ownership of it. If it is passed by reference then it does not.
        In a Unicode build the third parameter @a conv is used to translate
        the filename and comment fields to an 8-bit encoding.
        It has no effect on the stream's data.

        Since version 3.1.1, filenames in the generated archive will be encoded
        using UTF-8 and marked according to ZIP specification. To get the
        previous behaviour wxConvLocal may be provided as the conv object.
        Please note that not all unzip applications are fully ZIP spec
        compatible and may not correctly decode UTF-8 characters. For the best
        interoperability using only ASCII characters is the safest option.
    */
    wxZipOutputStream(wxOutputStream& stream, int level = -1,
                      wxMBConv& conv = wxConvUTF8);
    wxZipOutputStream(wxOutputStream* stream, int level = -1,
                      wxMBConv& conv = wxConvUTF8);
    //@}

    /**
        The destructor calls Close() to finish writing the zip if it has
        not been called already.
    */
    virtual ~wxZipOutputStream();

    /**
        Finishes writing the zip, returning @true if successful.
        Called by the destructor if not called explicitly.
    */
    bool Close();

    /**
        Close the current entry.
        It is called implicitly whenever another new entry is created with CopyEntry()
        or PutNextEntry(), or when the zip is closed.
    */
    bool CloseEntry();

    /**
        Transfers the zip comment from the wxZipInputStream
        to this output stream.
    */
    bool CopyArchiveMetaData(wxZipInputStream& inputStream);

    /**
        Takes ownership of @a entry and uses it to create a new entry
        in the zip. @a entry is then opened in @a inputStream and its contents
        copied to this stream.

        CopyEntry() is much more efficient than transferring the data using
        Read() and Write() since it will copy them without decompressing and
        recompressing them.

        For zips on seekable streams, @a entry must be from the same zip file
        as @a inputStream. For non-seekable streams, @a entry must also be the
        last thing read from @a inputStream.
    */
    bool CopyEntry(wxZipEntry* entry, wxZipInputStream& inputStream);

    //@{
    /**
        Set the compression level that will be used the next time an entry is
        created.

        It can be a value between 0 and 9 or -1 to use the default value
        which currently is equivalent to 6.
    */
    int GetLevel() const;
    void SetLevel(int level);
    //@}

    /**
        Create a new directory entry (see wxArchiveEntry::IsDir) with the given
        name and timestamp.

        PutNextEntry() can also be used to create directory entries, by supplying
        a name with a trailing path separator.
    */
    bool PutNextDirEntry(const wxString& name,
                         const wxDateTime& dt = wxDateTime::Now());

    //@{
    /**
        Takes ownership of @a entry and uses it to create a new entry in the zip.

        If you do not specify a size and plan to put more than 4GB data into the
        entry see SetFormat()
    */
    bool PutNextEntry(wxZipEntry* entry);

    /**
        Create a new entry with the given name, timestamp and size.

        If you do not specify a size and plan to put more than 4GB data into the
        entry see SetFormat()
    */
    bool PutNextEntry(const wxString& name,
                      const wxDateTime& dt = wxDateTime::Now(),
                      wxFileOffset size = wxInvalidOffset);
    //@}

    /**
        Sets a comment for the zip as a whole.
        It is written at the end of the zip.
    */
    void SetComment(const wxString& comment);

    /**
        Set the format of the archive.

        The normal zip format is limited to single files and the complete
        archive smaller than 4GB with less than 65k files. If any of these
        limits are exceeded, this class will automatically create a ZIP64 file,
        so in most situations calling SetFormat() is not necessary.

        However, to support single entries with more than 4GB of data
        (compressed or original) whose sizes are unknown when adding the
        entry with PutNextEntry(), the format has to be set to
        wxZIP_FORMAT_ZIP64 before adding such entries.

        @since 3.1.1
    */
    void SetFormat(wxZipArchiveFormat format);

    /**
        Get the format of the archive.

        This returns the value passed to SetFormat() and not necessarily the
        actual archive format (e.g. this method could return
        wxZIP_FORMAT_DEFAULT even if a ZIP64 will end up being created).

        @since 3.1.1
    */
    wxZipArchiveFormat GetFormat() const;
};

