/////////////////////////////////////////////////////////////////////////////
// Name:        stackwalk.h
// Purpose:     interface of wxStackWalker
// Author:      wxWidgets team
// Licence:     wxWindows licence
/////////////////////////////////////////////////////////////////////////////

/**
    This is the default value of the wxStackWalker::Walk function.
*/
#define wxSTACKWALKER_MAX_DEPTH   (200)

/**
    @class wxStackWalker

    wxStackWalker allows an application to enumerate, or walk, the stack frames
    (the function callstack).

    It is mostly useful in only two situations: inside wxApp::OnFatalException
    function to programmatically get the location of the crash and, in debug builds,
    in wxApp::OnAssertFailure to report the caller of the failed assert.

    wxStackWalker works by repeatedly calling the wxStackWalker::OnStackFrame
    method for each frame in the stack, so to use it you must derive your own
    class from it and override this method.

    This class will not return anything except raw stack frame addresses if the
    debug information is not available. Under Microsoft Windows this means that
    the PDB file matching the program being executed should be present.
    Note that if you use Microsoft Visual C++ compiler, you can create PDB files
    even for the programs built in release mode and it doesn't affect the program
    size (at least if you don't forget to add @c /opt:ref option which is suppressed
    by using @c /debug linker option by default but should be always enabled for
    release builds). If your compiler doesn't provide a direct way of
    generating PDB files but does produce debug information in the older "Code
    View" format or compatible, which is the case for MinGW, you can use @c
    cv2pdb tool available at https://github.com/rainers/cv2pdb to create PDB
    for your binaries which will work well when using this class.

    Under Unix, you need to compile your program with debugging information
    (usually using @c -g compiler and linker options) to get the file and line
    numbers information, however function names should be available even without it.
    Of course, all this is only @true if you build using a recent enough version
    of GNU libc which provides the @c backtrace() function needed to walk the stack.

    See @ref overview_debugging for how to make it available.

    @library{wxbase}
    @category{debugging}

    @see wxStackFrame
*/
class wxStackWalker
{
public:
    /**
        Constructor does nothing, use Walk() to walk the stack.
    */
    wxStackWalker(const char* argv0 = NULL);

    /**
        Destructor does nothing neither but should be virtual as this class is used as
        a base one.
    */
    virtual ~wxStackWalker();

    /**
        Enumerate stack frames from the current location, skipping the initial
        number of them (this can be useful when Walk() is called from some known
        location and you don't want to see the first few frames anyhow; also
        notice that Walk() frame itself is not included if skip = 1).

        Up to @a maxDepth frames are walked from the innermost to the outermost one.
        It defaults to ::wxSTACKWALKER_MAX_DEPTH.
    */
    virtual void Walk(size_t skip = 1, size_t maxDepth = wxSTACKWALKER_MAX_DEPTH);

    /**
        Enumerate stack frames from the location of uncaught exception.
        This method can only be called from wxApp::OnFatalException().

        Up to @a maxDepth frames are walked from the innermost to the outermost one.
        It defaults to ::wxSTACKWALKER_MAX_DEPTH.
    */
    virtual void WalkFromException(size_t maxDepth = wxSTACKWALKER_MAX_DEPTH);

protected:
    /**
        This function must be overridden to process the given frame.
    */
    virtual void OnStackFrame(const wxStackFrame& frame) = 0;
};



/**
    @class wxStackFrame

    wxStackFrame represents a single stack frame, or a single function in the call
    stack, and is used exclusively together with wxStackWalker, see there for a more
    detailed discussion.

    @library{wxbase}
    @category{debugging}

    @see wxStackWalker
*/
class wxStackFrame
{
public:
    /**
        Return the address of this frame.
    */
    void* GetAddress() const;

    /**
        Return the name of the file containing this frame, empty if unavailable
        (typically because debug info is missing).

        Use HasSourceLocation() to check whether the file name is available.
    */
    wxString GetFileName() const;

    /**
        Get the level of this frame (deepest/innermost one is 0).
    */
    size_t GetLevel() const;

    /**
        Return the line number of this frame, 0 if unavailable.

        @see GetFileName()
    */
    size_t GetLine() const;

    /**
        Get the module this function belongs to (empty if not available).
    */
    wxString GetModule() const;

    /**
        Return the unmangled (if possible) name of the function containing this frame.
    */
    wxString GetName() const;

    /**
        Return the return address of this frame.
    */
    size_t GetOffset() const;

    /**
        Get the name, type and value (in text form) of the given parameter.
        Any pointer may be @NULL if you're not interested in the corresponding value.

        Return @true if at least some values could be retrieved.
        This function currently is only implemented under Win32 and requires a PDB file.
    */
    virtual bool GetParam(size_t n, wxString* type, wxString* name,
                          wxString* value) const;

    /**
        Return the number of parameters of this function (may return 0 if we
        can't retrieve the parameters info even although the function does have
        parameters).
    */
    virtual size_t GetParamCount() const;

    /**
        Return @true if we have the file name and line number for this frame.
    */
    bool HasSourceLocation() const;
};

