/////////////////////////////////////////////////////////////////////////////
// Name:        tarstrm.h
// Purpose:     interface of wxTar* classes
// Author:      wxWidgets team
// Licence:     wxWindows licence
/////////////////////////////////////////////////////////////////////////////


/** wxTarEntry::GetTypeFlag() values */
enum wxTarType
{
    wxTAR_REGTYPE   = '0',      //!< regular file
    wxTAR_LNKTYPE   = '1',      //!< hard link
    wxTAR_SYMTYPE   = '2',      //!< symbolic link
    wxTAR_CHRTYPE   = '3',      //!< character special
    wxTAR_BLKTYPE   = '4',      //!< block special
    wxTAR_DIRTYPE   = '5',      //!< directory
    wxTAR_FIFOTYPE  = '6',      //!< named pipe
    wxTAR_CONTTYPE  = '7'       //!< contiguous file
};

/** Archive Formats (use wxTAR_PAX, it's backward compatible) used by wxTarEntry */
enum wxTarFormat
{
    wxTAR_USTAR,                //!< POSIX.1-1990 tar format
    wxTAR_PAX                   //!< POSIX.1-2001 tar format
};


/**
    @class wxTarInputStream

    Input stream for reading tar files.

    wxTarInputStream::GetNextEntry() returns a wxTarEntry object containing the
    meta-data for the next entry in the tar (and gives away ownership).
    Reading from the wxTarInputStream then returns the entry's data.
    wxTarInputStream::Eof() becomes @true after an attempt has been made to read
    past the end of the entry's data.

    When there are no more entries, wxTarInputStream::GetNextEntry() returns @NULL
    and sets wxTarInputStream::Eof().

    Tar entries are seekable if the parent stream is seekable. In practice this
    usually means they are only seekable if the tar is stored as a local file and
    is not compressed.

    @library{wxbase}
    @category{archive,streams}

    @see @ref overview_archive_byname
*/
class wxTarInputStream : public wxArchiveInputStream
{
public:
    //@{
    /**
        Constructor. In a Unicode build the second parameter @a conv is
        used to translate fields from the standard tar header into Unicode.

        It has no effect on the stream's data. @a conv is only used for the standard
        tar headers, any pax extended headers are always UTF-8 encoded.

        If the parent stream is passed as a pointer then the new filter stream
        takes ownership of it. If it is passed by reference then it does not.
    */
    wxTarInputStream(wxInputStream& stream,
                     wxMBConv& conv = wxConvLocal);
    wxTarInputStream(wxInputStream* stream,
                     wxMBConv& conv = wxConvLocal);
    //@}

    /**
        Closes the current entry.
        On a non-seekable stream reads to the end of the current entry first.
    */
    bool CloseEntry();

    /**
        Closes the current entry if one is open, then reads the meta-data for
        the next entry and returns it in a wxTarEntry object, giving away ownership.
        The stream is then open and can be read.
    */
    wxTarEntry* GetNextEntry();

    /**
        Closes the current entry if one is open, then opens the entry specified
        by the @a entry object.

        @a entry should be from the same tar file, and the tar should be on a
        seekable stream.
    */
    bool OpenEntry(wxTarEntry& entry);
};



/**
    @class wxTarClassFactory

    Class factory for the tar archive format.
    See the base class for details.

    @library{wxbase}
    @category{archive,streams}

    @see @ref overview_archive, @ref overview_archive_generic,
         wxTarEntry, wxTarInputStream, wxTarOutputStream
*/
class wxTarClassFactory : public wxArchiveClassFactory
{
public:

};



/**
    @class wxTarOutputStream

    Output stream for writing tar files.

    wxTarOutputStream::PutNextEntry() is used to create a new entry in the output tar,
    then the entry's data is written to the wxTarOutputStream.
    Another call to wxTarOutputStream::PutNextEntry() closes the current entry
    and begins the next.

    @library{wxbase}
    @category{streams}

    @see @ref overview_archive, wxTarEntry, wxTarInputStream
*/
class wxTarOutputStream : public wxArchiveOutputStream
{
public:
    //@{
    /**
        If the parent stream is passed as a pointer then the new filter stream
        takes ownership of it. If it is passed by reference then it does not.

        In a Unicode build the third parameter @a conv is used to translate the
        headers fields into an 8-bit encoding. It has no effect on the stream's data.

        When the @a format is @e wxTAR_PAX, pax extended headers are generated
        when any header field will not fit the standard tar header block or if it
        uses any non-ascii characters.

        Extended headers are stored as extra 'files' within the tar, and will be
        extracted as such by any other tar program that does not understand them.
        The @a conv parameter only affect the standard tar headers, the extended
        headers are always UTF-8 encoded.

        When the @a format is @e wxTAR_USTAR, no extended headers are generated,
        and instead a warning message is logged if any header field overflows.
    */
    wxTarOutputStream(wxOutputStream& stream,
                      wxTarFormat format = wxTAR_PAX,
                      wxMBConv& conv = wxConvLocal);
    wxTarOutputStream(wxOutputStream* stream,
                      wxTarFormat format = wxTAR_PAX,
                      wxMBConv& conv = wxConvLocal);
    //@}

    /**
        The destructor calls Close() to finish writing the tar if it has
        not been called already.
    */
    virtual ~wxTarOutputStream();

    /**
        Finishes writing the tar, returning @true if successful.
        Called by the destructor if not called explicitly.
    */
    bool Close();

    /**
        Close the current entry.

        It is called implicitly whenever another new entry is created with
        CopyEntry() or PutNextEntry(), or when the tar is closed.
    */
    bool CloseEntry();

    /**
        See wxArchiveOutputStream::CopyArchiveMetaData().
        For the tar format this function does nothing.
    */
    bool CopyArchiveMetaData(wxTarInputStream& s);

    /**
        Takes ownership of @a entry and uses it to create a new entry in the tar.
        @a entry is then opened in @a inputStream and its contents copied to this stream.

        For some other archive formats CopyEntry() is much more efficient than
        transferring the data using Read() and Write() since it will copy them
        without decompressing and recompressing them.
        For tar however it makes no difference.

        For tars on seekable streams, @a entry must be from the same tar file
        as @a inputStream. For non-seekable streams, @a entry must also be the
        last thing read from @a inputStream.
    */
    bool CopyEntry(wxTarEntry* entry, wxTarInputStream& inputStream);

    //@{
    /**
        The tar is zero padded to round its size up to @e BlockingFactor * 512 bytes.

        The blocking factor defaults to 10 for @e wxTAR_PAX and 20 for @e wxTAR_USTAR
        (see wxTarOutputStream()), as specified in the POSIX standards.
    */
    int GetBlockingFactor() const;
    void SetBlockingFactor(int factor);
    //@}

    /**
        Create a new directory entry (see wxArchiveEntry::IsDir()) with the given
        name and timestamp.

        PutNextEntry() can also be used to create directory entries, by supplying
        a name with a trailing path separator.
    */
    bool PutNextDirEntry(const wxString& name, const wxDateTime& dt = wxDateTime::Now());

    /**
        Takes ownership of entry and uses it to create a new entry in the tar.
    */
    bool PutNextEntry(wxTarEntry* entry);

    /**
        Create a new entry with the given name, timestamp and size.
    */
    bool PutNextEntry(const wxString& name, const wxDateTime& dt = wxDateTime::Now(),
                      wxFileOffset size = wxInvalidOffset);
};



/**
    @class wxTarEntry

    Holds the meta-data for an entry in a tar.

    @section tarentry_fields Field availability

    The tar format stores all the meta-data for an entry ahead of its data,
    therefore GetNextEntry() always returns a fully populated wxTarEntry object,
    both when reading from seekable and non-seekable streams.

    @library{wxbase}
    @category{archive,streams}

    @see @ref overview_archive, wxTarInputStream, wxTarOutputStream
*/
class wxTarEntry : public wxArchiveEntry
{
public:
    /**
        Constructor.

        The tar archive format stores the entry's size ahead of the entry's data.
        Therefore when creating an archive on a non-seekable stream it is necessary
        to supply the correct size when each entry is created.
    */
    wxTarEntry(const wxString& name = wxEmptyString,
               const wxDateTime& dt = wxDateTime::Now(),
               wxFileOffset size = wxInvalidOffset);

    /**
        Copy constructor.
    */
    wxTarEntry(const wxTarEntry& entry);

    //@{
    /**
        Gets/sets the entry's access time stamp.
        See also wxArchiveEntry::GetDateTime() and wxArchiveEntry::SetDateTime().
    */
    wxDateTime GetAccessTime() const;
    void SetAccessTime(const wxDateTime& dt);
    //@}

    //@{
    /**
        The entry's creation time stamp.
        See also wxArchiveEntry::GetDateTime() and wxArchiveEntry::SetDateTime().
    */
    wxDateTime GetCreateTime() const;
    void SetCreateTime(const wxDateTime& dt);
    //@}

    //@{
    /**
        OS specific IDs defining a device; these are only meaningful when
        wxTarEntry::GetTypeFlag() is @e wxTAR_CHRTYPE or @e wxTAR_BLKTYPE.
    */
    int GetDevMajor() const;
    int GetDevMinor() const;
    void SetDevMajor(int dev);
    void SetDevMinor(int dev);
    //@}

    //@{
    /**
        The user ID and group ID that has permissions (see wxTarEntry::GetMode())
        over this entry.

        These values aren't usually useful unless the file will only be
        restored to the same system it originated from.
        wxTarEntry::GetGroupName() and wxTarEntry::GetUserName() can be used instead.
    */
    int GetGroupId() const;
    int GetUserId() const;
    void SetGroupId(int id);
    void SetUserId(int id);
    //@}

    //@{
    /**
        The names of the user and group that has permissions (see wxTarEntry::GetMode())
        over this entry. These are not present in very old tars.
    */
    wxString GetGroupName() const;
    wxString GetUserName() const;
    void SetGroupName(const wxString& group);
    void SetUserName(const wxString& user);
    //@}

    //@{
    /**
        The filename of a previous entry in the tar that this entry is a link to.
        Only meaningful when wxTarEntry::GetTypeFlag() is set to @e wxTAR_LNKTYPE
        or @e wxTAR_SYMTYPE.
    */
    wxString GetLinkName() const;
    void SetLinkName(const wxString& link);
    //@}

    //@{
    /**
        UNIX permission bits for this entry.
        Giving read, write and execute permissions to the file's user and group
        (see GetGroupName() and GetUserName()) and to others.

        The integer is one or more ::wxPosixPermissions flags OR-combined.
    */
    int GetMode() const;
    void SetMode(int mode);
    //@}

    //@{
    /**
        The size of the entry's data in bytes.

        The tar archive format stores the entry's size ahead of the entry's data.
        Therefore when creating an archive on a non-seekable stream it is necessary to
        supply the correct size when each entry is created.

        For seekable streams this is not necessary as wxTarOutputStream will attempt
        to seek back and fix the entry's header when the entry is closed, though it is
        still more efficient if the size is given beforehand.
    */
    void SetSize(wxFileOffset size);
    wxFileOffset GetSize() const;
    //@}

    //@{
    /**
        Returns/Sets the type of the entry as a ::wxTarType value.

        When creating archives use only one of ::wxTarType values.
        When reading archives, GetTypeFlag() may return a value which does not
        match any value of ::wxTarType; in this case the returned value should be
        treated as @e wxTAR_REGTYPE.
    */
    int GetTypeFlag() const;
    void SetTypeFlag(int type);
    //@}

    /**
        Returns the entry's filename in the internal format used within the archive.

        The name can include directory components, i.e. it can be a full path.
        The names of directory entries are returned without any trailing path separator.
        This gives a canonical name that can be used in comparisons.
    */
    wxString GetInternalName() const;

    /**
        A static member that translates a filename into the internal format used
        within the archive.

        If the third parameter is provided, the bool pointed to is set to indicate
        whether the name looks like a directory name (i.e. has a trailing path separator).
    */
    static wxString GetInternalName(const wxString& name,
                                    wxPathFormat format = wxPATH_NATIVE,
                                    bool* pIsDir = NULL);

    /**
        Assignment operator.
    */
    wxTarEntry& operator operator=(const wxTarEntry& entry);
};

