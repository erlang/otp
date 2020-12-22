/////////////////////////////////////////////////////////////////////////////
// Name:        memory.h
// Purpose:     interface of wxDebugContext
// Author:      wxWidgets team
// Licence:     wxWindows licence
/////////////////////////////////////////////////////////////////////////////


/**
    @class wxDebugContext

    A class for performing various debugging and memory tracing operations.

    Full functionality (such as printing out objects currently allocated) is
    only present in a debugging build of wxWidgets, i.e. if the __WXDEBUG__
    symbol is defined. wxDebugContext and related functions and macros can be
    compiled out by setting wxUSE_DEBUG_CONTEXT to 0 is setup.h

    @library{wxbase}
    @category{debugging}

    @see @ref overview_debugging
*/
class wxDebugContext
{
public:
    /**
        Checks the memory blocks for errors, starting from the currently set
        checkpoint.

        @return Returns the number of errors, so a value of zero represents
                success. Returns -1 if an error was detected that prevents
                further checking.
    */
    static int Check(bool checkAll = false);

    /**
        Performs a memory dump from the currently set checkpoint, writing to the
        current debug stream. Calls the @b Dump member function for each wxObject
        derived instance.

        @return @true if the function succeeded, @false otherwise.
    */
    static bool Dump();

    /**
        Returns @true if the memory allocator checks all previous memory blocks for
        errors.

        By default, this is @false since it slows down execution considerably.

        @see SetCheckPrevious()
    */
    static bool GetCheckPrevious();

    /**
        Returns @true if debug mode is on.

        If debug mode is on, the wxObject new and delete operators store or use
        information about memory allocation. Otherwise, a straight malloc and
        free will be performed by these operators.

        @see SetDebugMode()
    */
    static bool GetDebugMode();

    /**
        Gets the debug level (default 1).

        The debug level is used by the wxTraceLevel function and the WXTRACELEVEL
        macro to specify how detailed the trace information is; setting a
        different level will only have an effect if trace statements in the
        application specify a value other than one.

        @deprecated
        This is obsolete, replaced by wxLog functionality.

        @see SetLevel()
    */
    static int GetLevel();

    /**
        Prints a list of the classes declared in this application, giving derivation
        and whether instances of this class can be dynamically created.

        @see PrintStatistics()
    */
    static bool PrintClasses();

    /**
        Performs a statistics analysis from the currently set checkpoint, writing
        to the current debug stream. The number of object and non-object
        allocations is printed, together with the total size.

        @param detailed
            If @true, the function will also print how many objects of each class
            have been allocated, and the space taken by these class instances.

        @see PrintStatistics()
    */
    static bool PrintStatistics(bool detailed = true);

    /**
        Tells the memory allocator to check all previous memory blocks for errors.
        By default, this is @false since it slows down execution considerably.

        @see GetCheckPrevious()
    */
    static void SetCheckPrevious(bool check);

    /**
        Sets the current checkpoint: Dump and PrintStatistics operations will
        be performed from this point on. This allows you to ignore allocations
        that have been performed up to this point.

        @param all
            If @true, the checkpoint is reset to include all memory allocations
            since the program started.
    */
    static void SetCheckpoint(bool all = false);

    /**
        Sets the debug mode on or off.

        If debug mode is on, the wxObject new and delete operators store or use
        information about memory allocation. Otherwise, a straight malloc and free
        will be performed by these operators.

        By default, debug mode is on if __WXDEBUG__ is defined. If the application
        uses this function, it should make sure that all object memory allocated
        is deallocated with the same value of debug mode. Otherwise, the delete
        operator might try to look for memory information that does not exist.

        @see GetDebugMode()
    */
    static void SetDebugMode(bool debug);

    /**
        Sets the debug level (default 1).

        The debug level is used by the wxTraceLevel function and the WXTRACELEVEL
        macro to specify how detailed the trace information is; setting
        a different level will only have an effect if trace statements in the
        application specify a value other than one.

        @deprecated
        This is obsolete, replaced by wxLog functionality.

        @see GetLevel()
    */
    static void SetLevel(int level);

    /**
        Installs a function to be called at the end of wxWidgets shutdown.
        It will be called after all files with global instances of
        wxDebugContextDumpDelayCounter have run their destructors.

        The shutdown function must be take no parameters and return nothing.
    */
    static void SetShutdownNotifyFunction(wxShutdownNotifyFunction func);
};



// ============================================================================
// Global functions/macros
// ============================================================================

/** @addtogroup group_funcmacro_log */
//@{

/**
    @deprecated Use one of the wxLogTrace() functions or one of the
                wxVLogTrace() functions instead.

    Calls wxTrace() with printf-style variable argument syntax. Output is
    directed to the current output stream (see wxDebugContext).

    @header{wx/memory.h}
*/
#define WXTRACE(format, ...)

/**
    @deprecated Use one of the wxLogTrace() functions or one of the
                wxVLogTrace() functions instead.

    Calls wxTraceLevel with printf-style variable argument syntax. Output is
    directed to the current output stream (see wxDebugContext). The first
    argument should be the level at which this information is appropriate. It
    will only be output if the level returned by wxDebugContext::GetLevel is
    equal to or greater than this value.

    @header{wx/memory.h}
*/
#define WXTRACELEVEL(level, format, ...)

/**
    @deprecated Use one of the wxLogTrace() functions or one of the
                wxVLogTrace() functions instead.

    Takes printf-style variable argument syntax. Output is directed to the
    current output stream (see wxDebugContext).

    @header{wx/memory.h}
*/
void wxTrace(const wxString& format, ...);

/**
    @deprecated Use one of the wxLogTrace() functions or one of the
                wxVLogTrace() functions instead.

    Takes @e printf() style variable argument syntax. Output is directed to the
    current output stream (see wxDebugContext). The first argument should be
    the level at which this information is appropriate. It will only be output
    if the level returned by wxDebugContext::GetLevel() is equal to or greater
    than this value.

    @header{wx/memory.h}
*/
void wxTraceLevel(int level, const wxString& format, ...);

//@}

