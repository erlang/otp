/////////////////////////////////////////////////////////////////////////////
// Name:        wx/debug.h
// Purpose:     interface of global functions
// Author:      wxWidgets team
// Licence:     wxWindows licence
/////////////////////////////////////////////////////////////////////////////

/** @addtogroup group_funcmacro_debug */
//@{

/**
    Exits the program immediately.

    This is a simple wrapper for the standard abort() function.

    @since 2.9.4
 */
void wxAbort();

/**
    @def wxDEBUG_LEVEL

    Preprocessor symbol defining the level of debug support available.

    This symbol is defined to 1 by default meaning that asserts are compiled in
    (although they may be disabled by a call to wxDisableAsserts()). You may
    predefine it as 0 prior to including any wxWidgets headers to omit the
    calls to wxASSERT() and related macros entirely in your own code and you
    may also predefine it as 0 when building wxWidgets to also avoid including
    any asserts in wxWidgets itself.

    Alternatively, you may predefine it as 2 to include wxASSERT_LEVEL_2() and
    similar macros which are used for asserts which have non-trivial run-time
    costs and so are disabled by default.

    @since 2.9.1

    @header{wx/debug.h}
 */
#define wxDEBUG_LEVEL

/**
    @def __WXDEBUG__

    Compatibility macro indicating presence of debug support.

    This symbol is defined if wxDEBUG_LEVEL is greater than 0 and undefined
    otherwise.

    @header{wx/debug.h}
 */
#define __WXDEBUG__

/**
    Type for the function called in case of assert failure.

    @see wxSetAssertHandler()
 */
typedef void (*wxAssertHandler_t)(const wxString& file,
                                  int line,
                                  const wxString& func,
                                  const wxString& cond,
                                  const wxString& msg);

/**
    Assert macro. An error message will be generated if the condition is @false in
    debug mode, but nothing will be done in the release build.

    Please note that the condition in wxASSERT() should have no side effects
    because it will not be executed in release mode at all.

    This macro should be used to catch (in debug builds) logical errors done
    by the programmer.

    @see wxASSERT_MSG(), wxCOMPILE_TIME_ASSERT()

    @header{wx/debug.h}
*/
#define wxASSERT( condition )

/**
    Assert macro for expensive run-time checks.

    This macro does nothing unless wxDEBUG_LEVEL is 2 or more and is meant to
    be used for the assertions with noticeable performance impact and which,
    hence, should be disabled during run-time.

    If wxDEBUG_LEVEL is 2 or more, it becomes the same as wxASSERT().

    @header{wx/debug.h}
 */
#define wxASSERT_LEVEL_2( condition )

/**
    Assert macro with a custom message for expensive run-time checks.

    If wxDEBUG_LEVEL is 2 or more, this is the same as wxASSERT_MSG(),
    otherwise it doesn't do anything at all.

    @see wxASSERT_LEVEL_2()

    @header{wx/debug.h}
 */
#define wxASSERT_LEVEL_2_MSG( condition, msg)


/**
    This macro results in a @ref wxCOMPILE_TIME_ASSERT "compile time assertion failure"
    if the size of the given @c type is less than @c size bits.

    This macro should be used to catch (in debug builds) logical errors done
    by the programmer.

    You may use it like this, for example:

    @code
    // we rely on the int being able to hold values up to 2^32
    wxASSERT_MIN_BITSIZE(int, 32);

    // can't work with the platforms using UTF-8 for wchar_t
    wxASSERT_MIN_BITSIZE(wchar_t, 16);
    @endcode

    @header{wx/debug.h}
*/
#define wxASSERT_MIN_BITSIZE( type, size )

/**
    Assert macro with message.
    An error message will be generated if the condition is @false.

    This macro should be used to catch (in debug builds) logical errors done
    by the programmer.

    @see wxASSERT(), wxCOMPILE_TIME_ASSERT()

    @header{wx/debug.h}
*/
#define wxASSERT_MSG( condition, message )

/**
    Assert macro pretending to assert at the specified location.

    This macro is the same as wxASSERT_MSG(), but the assert message will use
    the specified source file, line number and function name instead of the
    values corresponding to the current location as done by wxASSERT_MSG() by
    default.

    It is mostly useful for asserting inside functions called from macros, as
    by passing the usual @c __FILE__, @c __LINE__ and @c __FUNCTION__ values to
    a function, it's possible to pretend that the assert happens at the
    location of the macro in the source code (which can be useful) instead of
    inside the function itself (which is never useful as these values are
    always the same for the given assertion).

    @since 3.1.0

    @header{wx/debug.h}
 */
#define wxASSERT_MSG_AT( condition, message, file, line, func )

/**
    Checks that the condition is @true, returns with the given return value if
    not (stops execution in debug mode). This check is done even in release mode.

    This macro should be used to catch (both in debug and release builds) logical
    errors done by the programmer.

    @header{wx/debug.h}
*/
#define wxCHECK( condition, retValue )

/**
    Checks that the condition is @true, returns with the given return value if
    not (stops execution in debug mode). This check is done even in release mode.

    This macro may be only used in non-void functions, see also wxCHECK_RET().

    This macro should be used to catch (both in debug and release builds) logical
    errors done by the programmer.

    @header{wx/debug.h}
*/
#define wxCHECK_MSG( condition, retValue, message )

/**
    Checks that the condition is @true, and returns if not (stops execution
    with the given error message in debug mode). This check is done even in
    release mode.

    This macro should be used in void functions instead of wxCHECK_MSG().

    This macro should be used to catch (both in debug and release builds) logical
    errors done by the programmer.

    @header{wx/debug.h}
*/
#define wxCHECK_RET( condition, message )

/**
    Checks that the condition is @true, and if not, it will wxFAIL() and
    execute the given @c operation if it is not. This is a generalisation of
    wxCHECK() and may be used when something else than just returning from the
    function must be done when the @c condition is @false. This check is done
    even in release mode.

    This macro should be used to catch (both in debug and release builds) logical
    errors done by the programmer.

    @header{wx/debug.h}
*/
#define wxCHECK2(condition, operation)

/**
    This is the same as wxCHECK2(), but wxFAIL_MSG() with the specified
    @c message is called instead of wxFAIL() if the @c condition is @false.

    This macro should be used to catch (both in debug and release builds) logical
    errors done by the programmer.

    @header{wx/debug.h}
*/
#define wxCHECK2_MSG( condition, operation, message )

/**
    Using wxCOMPILE_TIME_ASSERT() results in a compilation error if the
    specified @c condition is @false. The compiler error message should include
    the @c message identifier - please note that it must be a valid C++
    identifier and not a string unlike in the other cases.

    This macro is mostly useful for testing the expressions involving the
    @c sizeof operator as they can't be tested by the preprocessor but it is
    sometimes desirable to test them at the compile time.

    Note that this macro internally declares a struct whose name it tries to
    make unique by using the @c __LINE__ in it but it may still not work if you
    use it on the same line in two different source files. In this case you may
    either change the line in which either of them appears on or use the
    wxCOMPILE_TIME_ASSERT2() macro.

    Also note that Microsoft Visual C++ has a bug which results in compiler
    errors if you use this macro with 'Program Database For Edit And Continue'
    (@c /ZI) option, so you shouldn't use it ('Program Database' (@c /Zi) is ok
    though) for the code making use of this macro.

    This macro should be used to catch misconfigurations at compile-time.

    @see wxASSERT_MSG(), wxASSERT_MIN_BITSIZE()

    @header{wx/debug.h}
*/
#define wxCOMPILE_TIME_ASSERT( condition, message )

/**
    This macro is identical to wxCOMPILE_TIME_ASSERT() except that it allows
    you to specify a unique @c name for the struct internally defined by this
    macro to avoid getting the compilation errors described for
    wxCOMPILE_TIME_ASSERT().

    This macro should be used to catch misconfigurations at compile-time.

    @header{wx/debug.h}
*/
#define wxCOMPILE_TIME_ASSERT2(condition, message, name)

/**
    Disable the condition checks in the assertions.

    This is the same as calling wxSetAssertHandler() with @NULL handler.

    @since 2.9.0

    @header{wx/debug.h}
 */
void wxDisableAsserts();

/**
    @def wxDISABLE_ASSERTS_IN_RELEASE_BUILD

    Use this macro to disable asserts in release build when not using
    wxIMPLEMENT_APP().

    By default, assert message boxes are suppressed in release build by
    wxIMPLEMENT_APP() which uses this macro. If you don't use wxIMPLEMENT_APP()
    because your application initializes wxWidgets directly (e.g. calls
    wxEntry() or wxEntryStart() itself) but still want to suppress assert
    notifications in release build you need to use this macro directly.

    @see wxDISABLE_DEBUG_SUPPORT()

    @since 2.9.1

    @header{wx/debug.h}
 */
#define wxDISABLE_ASSERTS_IN_RELEASE_BUILD() wxDisableAsserts()

/**
    Will always generate an assert error if this code is reached (in debug mode).
    Note that you don't have to (and cannot) use brackets when invoking this
    macro:

    @code
        if (...some condition...) {
            wxFAIL;
        }
    @endcode

    This macro should be used to catch (in debug builds) logical errors done
    by the programmer.

    @see wxFAIL_MSG()

    @header{wx/debug.h}
*/
#define wxFAIL

/**
    Will always generate an assert error with specified message if this code is
    reached (in debug mode).

    This macro is useful for marking "unreachable" code areas, for example it
    may be used in the "default:" branch of a switch statement if all possible
    cases are processed above.

    This macro should be used to catch (in debug builds) logical errors done
    by the programmer.

    @see wxFAIL()

    @header{wx/debug.h}
*/
#define wxFAIL_MSG( message )

/**
    Assert failure macro pretending to assert at the specified location.

    This is a cross between wxASSERT_MSG_AT() and wxFAIL_MSG(), see their
    documentation for more details.

    @since 3.1.0

    @header{wx/debug.h}
 */
#define wxFAIL_MSG_AT( message, file, line, func )

/**
    Returns @true if the program is running under debugger, @false otherwise.

    Please note that this function is currently only implemented for Win32 and
    always returns @false elsewhere.

    @header{wx/debug.h}
*/
bool wxIsDebuggerRunning();

/**
    Sets the function to be called in case of assertion failure.

    The default assert handler forwards to wxApp::OnAssertFailure() whose
    default behaviour is, in turn, to show the standard assertion failure
    dialog if a wxApp object exists or shows the same dialog itself directly
    otherwise.

    While usually it is enough -- and more convenient -- to just override
    OnAssertFailure(), to handle all assertion failures, including those
    occurring even before wxApp object creation of after its destruction you
    need to provide your assertion handler function.

    This function also provides a simple way to disable all asserts: simply
    pass @NULL pointer to it. Doing this will result in not even evaluating
    assert conditions at all, avoiding almost all run-time cost of asserts.

    Notice that this function is not MT-safe, so you should call it before
    starting any other threads.

    The return value of this function is the previous assertion handler. It can
    be called after any pre-processing by your handler and can also be restored
    later if you uninstall your handler.

    @param handler
        The function to call in case of assertion failure or @NULL.
    @return
        The previous assert handler which is not @NULL by default but could be
        @NULL if it had been previously set to this value using this function.

    @since 2.9.0

    @header{wx/debug.h}
 */
wxAssertHandler_t wxSetAssertHandler(wxAssertHandler_t handler);

/**
    Reset the assert handler to default function which shows a message box when
    an assert happens.

    This can be useful for the applications compiled in release build (with @c
    NDEBUG defined) for which the asserts are by default disabled: if you wish
    to enable them even in this case you need to call this function.

    @since 2.9.1

    @header{wx/debug.h}
 */
void wxSetDefaultAssertHandler();

/**
    Generate a debugger exception meaning that the control is passed to the
    debugger if one is attached to the process.

    Otherwise the program just terminates abnormally.

    If @c wxDEBUG_LEVEL is 0 (which is not the default) this function does
    nothing.

    @header{wx/debug.h}
*/
void wxTrap();

//@}

