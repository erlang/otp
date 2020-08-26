/////////////////////////////////////////////////////////////////////////////
// Name:        sound.h
// Purpose:     interface of wxSound
// Author:      wxWidgets team
// Licence:     wxWindows licence
/////////////////////////////////////////////////////////////////////////////


#define wxSOUND_SYNC  0
#define wxSOUND_ASYNC 1
#define wxSOUND_LOOP  2


/**
    @class wxSound

    This class represents a short sound (loaded from Windows WAV file), that
    can be stored in memory and played.

    Currently this class is implemented on Windows and Unix and can use either
    Open Sound System (OSS) or Simple DirectMedia Layer (SDL) under the latter.
    Notice that OSS is not provided any more by most, and maybe even all,
    Linux systems in 2017 and osspd (OSS Proxy Daemon) package typically needs
    to be installed to make it work.

    @library{wxcore}
    @category{media}
*/
class wxSound : public wxObject
{
public:
    /**
        Default ctor.
    */
    wxSound();

    /**
        Constructs a wave object from a file or, under Windows, from a Windows
        resource. Call IsOk() to determine whether this succeeded.

        @param fileName
            The filename or Windows resource.
        @param isResource
            @true if fileName is a resource, @false if it is a filename.
    */
    wxSound(const wxString& fileName, bool isResource = false);

    /**
        Constructs a wave object from in-memory data.

        @param size
            Size of the buffer pointer to by @a data.
        @param data
            The buffer containing the sound data in WAV format.
     */
    wxSound(size_t size, const void* data);

    /**
        Destroys the wxSound object.
    */
    virtual ~wxSound();

    /**
        Constructs a wave object from a file or resource.

        @param fileName
            The filename or Windows resource.
        @param isResource
            @true if fileName is a resource, @false if it is a filename.

        @return @true if the call was successful, @false otherwise.
    */
    bool Create(const wxString& fileName, bool isResource = false);

    /**
        Constructs a wave object from in-memory data.

        @param size
            Size of the buffer pointer to by @a data.
        @param data
            The buffer containing the sound data in WAV format.
     */
    bool Create(size_t size, const void* data);

    /**
        Returns @true if the object contains a successfully loaded file or resource,
        @false otherwise.
    */
    bool IsOk() const;

    /**
        Returns @true if a sound is played at the moment.

        This method is currently not available under Windows and may not be
        always implemented in Unix ports depending on the compilation options
        used (in this case it just always returns @false).

        @onlyfor{wxgtk,wxosx}
    */
    static bool IsPlaying();

    //@{
    /**
        Plays the sound file. If another sound is playing, it will be interrupted.

        Returns @true on success, @false otherwise. Note that in general it is
        possible to delete the object which is being asynchronously played any time
        after calling this function and the sound would continue playing, however this
        currently doesn't work under Windows for sound objects loaded from memory data.

        The possible values for @a flags are:
        - wxSOUND_SYNC: @c Play will block and wait until the sound is replayed.
        - wxSOUND_ASYNC: Sound is played asynchronously, @c Play returns immediately.
        - wxSOUND_ASYNC|wxSOUND_LOOP: Sound is played asynchronously and loops
                                      until another sound is played, Stop() is
                                      called or the program terminates.

        The static form is shorthand for this code:
        @code
        wxSound(filename).Play(flags);
        @endcode
    */
    bool Play(unsigned flags = wxSOUND_ASYNC) const;
    static bool Play(const wxString& filename,
                     unsigned flags = wxSOUND_ASYNC);
    //@}

    /**
        If a sound is played, this function stops it.
    */
    static void Stop();
};

