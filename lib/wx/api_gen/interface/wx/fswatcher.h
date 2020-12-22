/////////////////////////////////////////////////////////////////////////////
// Name:        wx/fswatcher.h
// Purpose:     wxFileSystemWatcher
// Author:      Bartosz Bekier
// Created:     2009-05-23
// Copyright:   (c) 2009 Bartosz Bekier <bartosz.bekier@gmail.com>
// Licence:     wxWindows licence
/////////////////////////////////////////////////////////////////////////////

/**
    @class wxFileSystemWatcher

    The wxFileSystemWatcher class allows receiving notifications of file
    system changes.

    @note Implementation limitations: this class is currently implemented for
          MSW, macOS and GTK ports but doesn't detect all changes correctly
          everywhere: under MSW accessing the file is not detected (only
          modifying it is) and under macOS neither accessing nor modifying is
          detected (only creating and deleting files is). Moreover, macOS
          version doesn't currently collapse pairs of create/delete events in a
          rename event, unlike the other ones.

    @note The application's event loop needs to be running before a
          wxFileSystemWatcher can be properly created, and that is why one
          should not be created too early during application startup.
          If you intend to create a wxFileSystemWatcher at startup, you can
          override wxAppConsole::OnEventLoopEnter() to ensure it is not done
          too early.

    For the full list of change types that are reported see wxFSWFlags.

    This class notifies the application about the file system changes by
    sending events of wxFileSystemWatcherEvent class. By default these events
    are sent to the wxFileSystemWatcher object itself so you can derive from it
    and use the event table @c EVT_FSWATCHER macro to handle these events in a
    derived class method. Alternatively, you can use
    wxFileSystemWatcher::SetOwner() to send the events to another object. Or
    you could use wxEvtHandler::Connect() with @c wxEVT_FSWATCHER to handle
    these events in any other object. See the fswatcher sample for an example
    of the latter approach.

    @library{wxbase}
    @category{file}

    @since 2.9.1
 */
class wxFileSystemWatcher: public wxEvtHandler
{
public:
    /**
        Default constructor.
     */
    wxFileSystemWatcher();

    /**
        Destructor. Stops all paths from being watched and frees any system
        resources used by this file system watcher object.
     */
    virtual ~wxFileSystemWatcher();

    /**
        Adds @a path to currently watched files.

        The @a path argument can currently only be a directory and any changes
        to this directory itself or its immediate children will generate the
        events. Use AddTree() to monitor the directory recursively.

        Note that on platforms that use symbolic links, you should consider the
        possibility that @a path is a symlink. To watch the symlink itself and
        not its target you may call wxFileName::DontFollowLink() on @a path.

        @param path
            The name of the path to watch.
        @param events
            An optional filter to receive only events of particular types.
            This is currently implemented only for GTK.
     */
    virtual bool Add(const wxFileName& path, int events = wxFSW_EVENT_ALL);

    /**
        This is the same as Add(), but also recursively adds every
        file/directory in the tree rooted at @a path.

        Additionally a file mask can be specified to include only files
        matching that particular mask.

        This method is implemented efficiently on MSW and macOS, but
        should be used with care on other platforms for directories with lots
        of children (e.g. the root directory) as it calls Add() for each
        subdirectory, potentially creating a lot of watches and taking a long
        time to execute.

        Note that on platforms that use symbolic links, you will probably want
        to have called wxFileName::DontFollowLink on @a path. This is especially
        important if the symlink targets may themselves be watched.
     */
    virtual bool AddTree(const wxFileName& path, int events = wxFSW_EVENT_ALL,
                         const wxString& filter = wxEmptyString);

    /**
        Removes @a path from the list of watched paths.

        See the comment in Add() about symbolic links. @a path should treat
        symbolic links in the same way as in the original Add() call.
     */
    virtual bool Remove(const wxFileName& path);

    /**
        This is the same as Remove(), but also removes every file/directory
        belonging to the tree rooted at @a path.

        See the comment in AddTree() about symbolic links. @a path should treat
        symbolic links in the same way as in the original AddTree() call.
     */
    virtual bool RemoveTree(const wxFileName& path);

    /**
        Clears the list of currently watched paths.
     */
    virtual bool RemoveAll();

    /**
        Returns the number of currently watched paths.

        @see GetWatchedPaths()
     */
    int GetWatchedPathsCount() const;

    /**
        Retrieves all watched paths and places them in @a paths. Returns
        the number of watched paths, which is also the number of entries added
        to @a paths.
     */
    int GetWatchedPaths(wxArrayString* paths) const;

    /**
        Associates the file system watcher with the given @a handler object.

        All the events generated by this object will be passed to the specified
        owner.
     */
    void SetOwner(wxEvtHandler* handler);
};



/**
    @class wxFileSystemWatcherEvent

    A class of events sent when a file system event occurs. Types of events
    reported may vary depending on a platform, however all platforms report
    at least creation of new file/directory and access, modification, move
    (rename) or deletion of an existing one.

    @library{wxbase}
    @category{events}

    @see wxFileSystemWatcher
    @see @ref overview_events

    @since 2.9.1
*/
class wxFileSystemWatcherEvent : public wxEvent
{
public:
    wxFileSystemWatcherEvent(int changeType = 0,
                             int watchid = wxID_ANY);
    wxFileSystemWatcherEvent(int changeType,
                             wxFSWWarningType warningType,
                             const wxString& errorMsg,
                             int watchid = wxID_ANY);
    wxFileSystemWatcherEvent(int changeType,
                             const wxFileName& path,
                             const wxFileName& newPath,
                             int watchid = wxID_ANY);

    /**
        Returns the path at which the event occurred.
     */
    const wxFileName& GetPath() const;

    /**
        Returns the new path of the renamed file/directory if this is a rename
        event.

        Otherwise it returns the same path as GetPath().
     */
    const wxFileName& GetNewPath() const;

    /**
        Returns the type of file system change that occurred. See wxFSWFlags for
        the list of possible file system change types.
     */
    int GetChangeType() const;

    /**
        Returns @c true if this error is an error event

        Error event is an event generated when a warning or error condition
        arises.
     */
    bool IsError() const;

    /**
        Return a description of the warning or error if this is an error event.

        This string may be empty if the exact reason for the error or the
        warning is not known.
     */
    wxString GetErrorDescription() const;

    /**
        Return the type of the warning if this event is a warning one.

        If this is not a warning event, i.e. if GetChangeType() doesn't include
        ::wxFSW_EVENT_WARNING, returns ::wxFSW_WARNING_NONE.

        @since 3.0
     */
    wxFSWWarningType GetWarningType() const;

    /**
        Returns a wxString describing an event, useful for logging, debugging
        or testing.
     */
    wxString ToString() const;
};

wxEventType wxEVT_FSWATCHER;

/**
    These are the possible types of file system change events.

    Not all of these events are reported on all platforms currently.

    @since 2.9.1
 */
enum wxFSWFlags
{
    /// File or directory was created.
    wxFSW_EVENT_CREATE = 0x01,

    /// File or directory was deleted.
    wxFSW_EVENT_DELETE = 0x02,

    /**
        File or directory was renamed.

        Notice that under MSW this event is sometimes -- although not always --
        followed by a ::wxFSW_EVENT_MODIFY for the new file.

        Under macOS this event is only detected when watching entire trees. When
        watching directories, separate ::wxFSW_EVENT_CREATE and
        ::wxFSW_EVENT_DELETE events are detected instead.
     */
    wxFSW_EVENT_RENAME = 0x04,

    /**
        File or directory was modified.

        Depending on the program doing the file modification, multiple such
        events can be reported for a single logical file update.

        Under macOS this event is only detected when watching entire trees.
     */
    wxFSW_EVENT_MODIFY = 0x08,

    /**
        File or directory was accessed.

        This event is currently only detected under Linux.
     */
    wxFSW_EVENT_ACCESS = 0x10,

    /**
        The item's metadata was changed, e.g.\ its permissions or timestamps.

        This event is currently only detected under Linux and macOS.
        Under macOS this event is only detected when watching entire trees.

        @since 2.9.5
     */
    wxFSW_EVENT_ATTRIB = 0x20,

    /**
        The file system containing a watched item was unmounted.

        wxFSW_EVENT_UNMOUNT cannot be set; unmount events are produced automatically. This flag
        is therefore not included in wxFSW_EVENT_ALL.

        This event is currently only detected under Linux and macOS.
        Under macOS this event is only detected when watching entire trees.

        @since 2.9.5
    */
    wxFSW_EVENT_UNMOUNT = 0x2000,

    /**
        A warning condition arose.

        This is something that probably needs to be shown to the user in an
        interactive program as it can indicate a relatively serious problem,
        e.g. some events could have been missed because of an overflow. But
        more events will still be coming in the future, unlike for the error
        condition below.
     */
    wxFSW_EVENT_WARNING = 0x40,

    /**
        An error condition arose.

        Errors are fatal, i.e. no more events will be reported after an error
        and the program can stop watching the directories currently being
        monitored.
    */
    wxFSW_EVENT_ERROR = 0x80,

    wxFSW_EVENT_ALL = wxFSW_EVENT_CREATE | wxFSW_EVENT_DELETE |
                         wxFSW_EVENT_RENAME | wxFSW_EVENT_MODIFY |
                         wxFSW_EVENT_ACCESS | wxFSW_EVENT_ATTRIB |
                         wxFSW_EVENT_WARNING | wxFSW_EVENT_ERROR
};

/**
    Possible warning types for the warning events generated by
    wxFileSystemWatcher.

    @since 3.0
 */
enum wxFSWWarningType
{
    /**
        This is not a warning at all.
     */
    wxFSW_WARNING_NONE,

    /**
        A generic warning.

        Further information may be provided in the user-readable message
        available from wxFileSystemWatcherEvent::GetErrorDescription()
     */
    wxFSW_WARNING_GENERAL,

    /**
        An overflow event.

        This warning indicates that some file system changes were not signaled
        by any events, usually because there were too many of them and the
        internally used queue has overflown. If such event is received it is
        recommended to completely rescan the files or directories being
        monitored.
     */
    wxFSW_WARNING_OVERFLOW
};
