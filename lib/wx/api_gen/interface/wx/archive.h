/////////////////////////////////////////////////////////////////////////////
// Name:        archive.h
// Purpose:     interface of wxArchive* classes
// Author:      wxWidgets team
// Licence:     wxWindows licence
/////////////////////////////////////////////////////////////////////////////

/**
    @class wxArchiveInputStream

    This is an abstract base class which serves as a common interface to
    archive input streams such as wxZipInputStream.

    wxArchiveInputStream::GetNextEntry returns an wxArchiveEntry object containing
    the meta-data for the next entry in the archive (and gives away ownership).

    Reading from the wxArchiveInputStream then returns the entry's data. Eof()
    becomes @true after an attempt has been made to read past the end of the
    entry's data.

    When there are no more entries, GetNextEntry() returns @NULL and sets Eof().

    @library{wxbase}
    @category{archive,streams}

    @see @ref overview_archive, wxArchiveEntry, wxArchiveOutputStream
*/
class wxArchiveInputStream : public wxFilterInputStream
{
public:
    /**
        Closes the current entry. On a non-seekable stream reads to the end of
        the current entry first.
    */
    virtual bool CloseEntry() = 0;

    /**
        Closes the current entry if one is open, then reads the meta-data for
        the next entry and returns it in a wxArchiveEntry object, giving away
        ownership. Reading this wxArchiveInputStream then returns the entry's data.
    */
    wxArchiveEntry* GetNextEntry();

    /**
        Closes the current entry if one is open, then opens the entry specified
        by the wxArchiveEntry object.

        @a entry must be from the same archive file that this wxArchiveInputStream
        is reading, and it must be reading it from a seekable stream.
    */
    virtual bool OpenEntry(wxArchiveEntry& entry) = 0;
};



/**
    @class wxArchiveOutputStream

    This is an abstract base class which serves as a common interface to
    archive output streams such as wxZipOutputStream.

    wxArchiveOutputStream::PutNextEntry is used to create a new entry in the
    output archive, then the entry's data is written to the wxArchiveOutputStream.
    Another call to PutNextEntry() closes the current entry and begins the next.

    @library{wxbase}
    @category{archive,streams}

    @see @ref overview_archive, wxArchiveEntry, wxArchiveInputStream
*/
class wxArchiveOutputStream : public wxFilterOutputStream
{
public:
    /**
        Calls Close() if it has not already been called.
    */
    virtual ~wxArchiveOutputStream();

    /**
        Closes the archive, returning @true if it was successfully written.
        Called by the destructor if not called explicitly.

        @see wxOutputStream::Close()
    */
    virtual bool Close();

    /**
        Close the current entry.
        It is called implicitly whenever another new entry is created with CopyEntry()
        or PutNextEntry(), or when the archive is closed.
    */
    virtual bool CloseEntry() = 0;

    /**
        Some archive formats have additional meta-data that applies to the archive
        as a whole.
        For example in the case of zip there is a comment, which is stored at the end
        of the zip file.  CopyArchiveMetaData() can be used to transfer such information
        when writing a modified copy of an archive.

        Since the position of the meta-data can vary between the various archive
        formats, it is best to call CopyArchiveMetaData() before transferring
        the entries.  The wxArchiveOutputStream will then hold on to the meta-data
        and write it at the correct point in the output file.

        When the input archive is being read from a non-seekable stream, the
        meta-data may not be available when CopyArchiveMetaData() is called,
        in which case the two streams set up a link and transfer the data
        when it becomes available.
    */
    virtual bool CopyArchiveMetaData(wxArchiveInputStream& stream) = 0;

    /**
        Takes ownership of @a entry and uses it to create a new entry in the
        archive. @a entry is then opened in the input stream @a stream
        and its contents copied to this stream.

        For archive types which compress entry data, CopyEntry() is likely to be
        much more efficient than transferring the data using Read() and Write()
        since it will copy them without decompressing and recompressing them.

        @a entry must be from the same archive file that @a stream is
        accessing. For non-seekable streams, @a entry must also be the last
        thing read from @a stream.
    */
    virtual bool CopyEntry(wxArchiveEntry* entry,
                           wxArchiveInputStream& stream) = 0;

    /**
        Create a new directory entry (see wxArchiveEntry::IsDir) with the given
        name and timestamp.

        PutNextEntry() can also be used to create directory entries, by supplying
        a name with a trailing path separator.
    */
    virtual bool PutNextDirEntry(const wxString& name,
                                 const wxDateTime& dt = wxDateTime::Now()) = 0;

    /**
        Takes ownership of entry and uses it to create a new entry in the archive.
        The entry's data can then be written by writing to this wxArchiveOutputStream.
    */
    virtual bool PutNextEntry(wxArchiveEntry* entry) = 0;

    /**
        Create a new entry with the given name, timestamp and size. The entry's
        data can then be written by writing to this wxArchiveOutputStream.
    */
    virtual bool PutNextEntry(const wxString& name,
                              const wxDateTime& dt = wxDateTime::Now(),
                              wxFileOffset size = wxInvalidOffset) = 0;
};



/**
    @class wxArchiveEntry

    This is an abstract base class which serves as a common interface to
    archive entry classes such as wxZipEntry.
    These hold the meta-data (filename, timestamp, etc.), for entries
    in archive files such as zips and tars.

    @section archiveentry_nonseekable About non-seekable streams

    This information applies only when reading archives from non-seekable streams.
    When the stream is seekable GetNextEntry() returns a fully populated wxArchiveEntry.
    See @ref overview_archive_noseek for more information.

    For generic programming, when the worst case must be assumed, you can rely on
    all the fields of wxArchiveEntry being fully populated when
    wxArchiveInputStream::GetNextEntry() returns, with the following exceptions:

    @li GetSize(): guaranteed to be available after the entry has been read to Eof(),
        or CloseEntry() has been called;
    @li IsReadOnly(): guaranteed to be available after the end of the archive has
        been reached, i.e. after GetNextEntry() returns NULL and Eof() is true.

    @library{wxbase}
    @category{archive,streams}

    @see @ref overview_archive, @ref overview_archive_generic,
         wxArchiveInputStream, wxArchiveOutputStream, wxArchiveNotifier
*/
class wxArchiveEntry : public wxObject
{
public:
    /**
        Returns a copy of this entry object.
    */
    wxArchiveEntry* Clone() const;

    /**
        Gets the entry's timestamp.
    */
    virtual wxDateTime GetDateTime() const = 0;

    /**
        Sets the entry's timestamp.
    */
    virtual void SetDateTime(const wxDateTime& dt) = 0;

    /**
        Returns the entry's name, by default in the native format.
        The name can include directory components, i.e. it can be a full path.

        If this is a directory entry, (i.e. if IsDir() is @true) then the
        returned string is the name with a trailing path separator.
    */
    virtual wxString GetName(wxPathFormat format = wxPATH_NATIVE) const = 0;

    /**
        Sets the entry's name.
        Setting a name with a trailing path separator sets IsDir().

        @see GetName()
    */
    virtual void SetName(const wxString& name,
                         wxPathFormat format = wxPATH_NATIVE) = 0;

    /**
        Returns the size of the entry's data in bytes.
    */
    virtual wxFileOffset GetSize() const = 0;

    /**
        Sets the size of the entry's data in bytes.
    */
    virtual void SetSize(wxFileOffset size) = 0;

    /**
        Returns the path format used internally within the archive to store
        filenames.
    */
    virtual wxPathFormat GetInternalFormat() const = 0;

    /**
        Returns the entry's filename in the internal format used within the
        archive. The name can include directory components, i.e. it can be a
        full path.

        The names of directory entries are returned without any trailing path
        separator. This gives a canonical name that can be used in comparisons.

        @see @ref overview_archive_byname
    */
    virtual wxString GetInternalName() const = 0;

    /**
        Returns a numeric value unique to the entry within the archive.
    */
    virtual wxFileOffset GetOffset() const = 0;

    /**
        Returns @true if this is a directory entry.

        Directory entries are entries with no data, which are used to store
        the meta-data of directories. They also make it possible for completely
        empty directories to be stored.

        @note
        The names of entries within an archive can be complete paths, and
        unarchivers typically create whatever directories are necessary as they
        restore files, even if the archive contains no explicit directory entries.
    */
    virtual bool IsDir() const = 0;

    /**
        Marks this entry as a directory if @a isDir is @true. See IsDir() for more info.
    */
    virtual void SetIsDir(bool isDir = true) = 0;

    /**
        Returns @true if the entry is a read-only file.
    */
    virtual bool IsReadOnly() const = 0;

    /**
        Sets this entry as a read-only file.
    */
    virtual void SetIsReadOnly(bool isReadOnly = true) = 0;

    /**
        Sets the notifier (see wxArchiveNotifier) for this entry.

        Whenever the wxArchiveInputStream updates this entry, it will then invoke
        the associated notifier's wxArchiveNotifier::OnEntryUpdated method.

        Setting a notifier is not usually necessary. It is used to handle
        certain cases when modifying an archive in a pipeline (i.e. between
        non-seekable streams).
    */
    void SetNotifier(wxArchiveNotifier& notifier);

    /**
        Unsets the notifier eventually attached to this entry.
    */
    virtual void UnsetNotifier();
};



/**
    @class wxArchiveClassFactory

    Allows the creation of streams to handle archive formats such as zip and tar.

    For example, given a filename you can search for a factory that will
    handle it and create a stream to read it:

    @code
        factory = wxArchiveClassFactory::Find(filename, wxSTREAM_FILEEXT);
        if (factory)
            stream = factory->NewStream(new wxFFileInputStream(filename));
    @endcode

    wxArchiveClassFactory::Find can also search for a factory by MIME type
    or wxFileSystem protocol.

    The available factories can be enumerated using
    wxArchiveClassFactory::GetFirst() and wxArchiveClassFactory::GetNext().

    @library{wxbase}
    @category{archive,streams}

    @see @ref overview_archive, @ref overview_archive_generic, wxArchiveEntry,
         wxArchiveInputStream, wxArchiveOutputStream, wxFilterClassFactory
*/
class wxArchiveClassFactory : public wxObject
{
public:
    /**
        Returns @true if this factory can handle the given protocol, MIME type
        or file extension.

        When using wxSTREAM_FILEEXT for the second parameter, the first parameter
        can be a complete filename rather than just an extension.
    */
    bool CanHandle(const wxString& protocol,
                   wxStreamProtocolType type = wxSTREAM_PROTOCOL) const;

    /**
        A static member that finds a factory that can handle a given protocol, MIME
        type or file extension.  Returns a pointer to the class factory if found, or
        @NULL otherwise. It does not give away ownership of the factory.

        When using wxSTREAM_FILEEXT for the second parameter, the first parameter
        can be a complete filename rather than just an extension.
    */
    static const wxArchiveClassFactory* Find(const wxString& protocol,
            wxStreamProtocolType type = wxSTREAM_PROTOCOL);

    /**
        Returns the wxMBConv object that the created streams will use when
        translating meta-data. The initial default, set by the constructor,
        is wxConvLocal.
    */
    wxMBConv& GetConv() const;

    /**
        Sets the wxMBConv object that the created streams will use when
        translating meta-data.
    */
    void SetConv(wxMBConv& conv);

    //@{
    /**
        GetFirst and GetNext can be used to enumerate the available factories.
        For example, to list them:

        @code
        wxString list;
        const wxArchiveClassFactory *factory = wxArchiveClassFactory::GetFirst();

        while (factory) {
            list << factory->GetProtocol() << wxT("\n");
            factory = factory->GetNext();
        }
        @endcode

        GetFirst() and GetNext() return a pointer to a factory or @NULL if no more
        are available. They do not give away ownership of the factory.
    */
    static const wxArchiveClassFactory* GetFirst();
    const wxArchiveClassFactory* GetNext() const;
    //@}

    /**
        Calls the static GetInternalName() function for the archive entry type,
        for example wxZipEntry::GetInternalName.
    */
    virtual wxString GetInternalName(const wxString& name,
                                     wxPathFormat format = wxPATH_NATIVE) const = 0;

    /**
        Returns the wxFileSystem protocol supported by this factory.
        Equivalent to @code wxString(*GetProtocols()) @endcode.
    */
    wxString GetProtocol() const;

    /**
        Returns the protocols, MIME types or file extensions supported by this
        factory, as an array of null terminated strings.

        It does not give away ownership of the array or strings.
        For example, to list the file extensions a factory supports:

        @code
        wxString list;
        const wxChar *const *p;

        for (p = factory->GetProtocols(wxSTREAM_FILEEXT); *p; p++)
            list << *p << wxT("\n");
        @endcode
    */
    virtual const wxChar** GetProtocols(wxStreamProtocolType type = wxSTREAM_PROTOCOL) const = 0;

    /**
        Create a new wxArchiveEntry object of the appropriate type.
    */
    wxArchiveEntry* NewEntry() const;

    //@{
    /**
        Create a new input or output stream to read or write an archive.

        If the parent stream is passed as a pointer then the new archive stream
        takes ownership of it. If it is passed by reference then it does not.
    */
    wxArchiveInputStream* NewStream(wxInputStream& stream) const;
    wxArchiveOutputStream* NewStream(wxOutputStream& stream) const;
    wxArchiveInputStream* NewStream(wxInputStream* stream) const;
    wxArchiveOutputStream* NewStream(wxOutputStream* stream) const;
    //@}

    /**
        Adds this class factory to the list returned by GetFirst() or GetNext().

        It is not necessary to do this to use the archive streams. It is usually
        used when implementing streams, typically the implementation will
        add a static instance of its factory class.

        It can also be used to change the order of a factory already in the list,
        bringing it to the front. This isn't a thread safe operation
        so can't be done when other threads are running that will be using the list.
        The list does not take ownership of the factory.
    */
    void PushFront();

    /**
        Removes this class factory from the list returned by GetFirst() and GetNext().

        Removing from the list isn't a thread safe operation so can't be done when
        other threads are running that will be using the list.
        The list does not own the factories, so removing a factory does not delete it.
    */
    void Remove();
};



/**
    @class wxArchiveNotifier

    If you need to know when a wxArchiveInputStream updates a wxArchiveEntry
    object, you can create a notifier by deriving from this abstract base class,
    overriding wxArchiveNotifier::OnEntryUpdated.

    An instance of your notifier class can then be assigned to the wxArchiveEntry
    object using wxArchiveEntry::SetNotifier.
    Your OnEntryUpdated() method will then be invoked whenever the input
    stream updates the entry.

    Setting a notifier is not usually necessary. It is used to handle
    certain cases when modifying an archive in a pipeline (i.e. between
    non-seekable streams).
    See @ref overview_archive_noseek.

    @library{wxbase}
    @category{archive,streams}

    @see @ref overview_archive_noseek, wxArchiveEntry, wxArchiveInputStream,
         wxArchiveOutputStream
*/
class wxArchiveNotifier
{
public:
    /**
        This method must be overridden in your derived class.
    */
    virtual void OnEntryUpdated(wxArchiveEntry& entry) = 0;
};



/**
    @class wxArchiveIterator

    An input iterator template class that can be used to transfer an archive's
    catalogue to a container. It is only available if wxUSE_STL is set to 1
    in setup.h, and the uses for it outlined below require a compiler which
    supports member templates.

    @code
    template<class Arc, class T = typename Arc::entry_type*>
    class wxArchiveIterator
    {
        // this constructor creates an 'end of sequence' object
        wxArchiveIterator();

        // template parameter 'Arc' should be the type of an archive input stream
        wxArchiveIterator(Arc& arc) {
            // ...
        }
    };
    @endcode

    The first template parameter should be the type of archive input stream
    (e.g. wxArchiveInputStream) and the second can either be a pointer to an entry
    (e.g. wxArchiveEntry*), or a string/pointer pair
    (e.g. std::pair<wxString,wxArchiveEntry*>).

    The @c wx/archive.h header defines the following typedefs:

    @code
    typedef wxArchiveIterator<wxArchiveInputStream> wxArchiveIter;

    typedef wxArchiveIterator<wxArchiveInputStream,
            std::pair<wxString, wxArchiveEntry*> > wxArchivePairIter;
    @endcode

    The header for any implementation of this interface should define similar
    typedefs for its types, for example in @c wx/zipstrm.h there is:

    @code
    typedef wxArchiveIterator<wxZipInputStream> wxZipIter;

    typedef wxArchiveIterator<wxZipInputStream,
             std::pair<wxString, wxZipEntry*> > wxZipPairIter;
    @endcode

    Transferring the catalogue of an archive @e arc to a vector @e cat,
    can then be done something like this:

    @code
        std::vector<wxArchiveEntry*> cat((wxArchiveIter)arc, wxArchiveIter());
    @endcode

    When the iterator is dereferenced, it gives away ownership of an entry
    object. So in the above example, when you have finished with @e cat
    you must delete the pointers it contains.

    If you have smart pointers with normal copy semantics (i.e. not auto_ptr
    or wxScopedPtr), then you can create an iterator  which uses them instead.

    For example, with a smart pointer class for zip entries @e ZipEntryPtr:

    @code
    typedef std::vector<ZipEntryPtr> ZipCatalog;
    typedef wxArchiveIterator<wxZipInputStream, ZipEntryPtr> ZipIter;
    ZipCatalog cat((ZipIter)zip, ZipIter());
    @endcode

    Iterators that return std::pair objects can be used to populate a std::multimap,
    to allow entries to be looked up by name.
    The string is initialised using the wxArchiveEntry object's
    wxArchiveEntry::GetInternalName function.

    @code
    typedef std::multimap<wxString, wxZipEntry*> ZipCatalog;
    ZipCatalog cat((wxZipPairIter)zip, wxZipPairIter());
    @endcode

    Note that this iterator also gives away ownership of an entry
    object each time it is dereferenced. So in the above example, when
    you have finished with @e cat you must delete the pointers it contains.

    Or if you have them, a pair containing a smart pointer can be used
    (again @e ZipEntryPtr), no worries about ownership:

    @code
    typedef std::multimap<wxString, ZipEntryPtr> ZipCatalog;
    typedef wxArchiveIterator<wxZipInputStream,
                std::pair<wxString, ZipEntryPtr> > ZipPairIter;
    ZipCatalog cat((ZipPairIter)zip, ZipPairIter());
    @endcode

    @library{wxbase}
    @category{archive,streams}

    @see wxArchiveEntry, wxArchiveInputStream, wxArchiveOutputStream
*/
class wxArchiveIterator
{
public:
    /**
        Default constructor.
    */
    wxArchiveIterator();

    /**
        Construct the iterator that returns all the entries in the archive input
        stream @a arc.
    */
    wxArchiveIterator(Arc& arc);

    /**
        Returns an entry object from the archive input stream, giving away
        ownership.
    */
    const T operator*() const;

    //@{
    /**
        Position the input iterator at the next entry in the archive input stream.
    */
    wxArchiveIterator operator++();
    wxArchiveIterator operator++(int);
    //@}
};

