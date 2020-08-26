/////////////////////////////////////////////////////////////////////////////
// Name:        recguard.h
// Purpose:     interface of wxRecursionGuardFlag
// Author:      wxWidgets team
// Licence:     wxWindows licence
/////////////////////////////////////////////////////////////////////////////

/**
    @class wxRecursionGuardFlag

    This is a completely opaque class which exists only to be used with
    wxRecursionGuard, please see the example in that class' documentation.

    @remarks

    wxRecursionGuardFlag object must be declared @c static or the recursion
    would never be detected.

    @library{wxbase}
    @category{misc}
*/
class wxRecursionGuardFlag
{
public:

};



/**
    @class wxRecursionGuard

    wxRecursionGuard is a very simple class which can be used to prevent reentrancy
    problems in a function. It is not thread-safe and so should be used only in
    single-threaded programs or in combination with some thread synchronization
    mechanisms.

    wxRecursionGuard is always used together with the
    wxRecursionGuardFlag like in this example:

    @code
    void Foo()
    {
        static wxRecursionGuardFlag s_flag;
        wxRecursionGuard guard(s_flag);
        if ( guard.IsInside() )
        {
            // don't allow reentrancy
            return;
        }

        ...
    }
    @endcode

    As you can see, wxRecursionGuard simply tests the flag value and sets it to
    @true if it hadn't been already set.
    IsInside() allows testing the old flag
    value. The advantage of using this class compared to directly manipulating the
    flag is that the flag is always reset in the wxRecursionGuard destructor and so
    you don't risk to forget to do it even if the function returns in an unexpected
    way (for example because an exception has been thrown).

    @library{wxbase}
    @category{misc}
*/
class wxRecursionGuard
{
public:
    /**
        A wxRecursionGuard object must always be initialized with a @c static
        wxRecursionGuardFlag. The constructor saves the
        value of the flag to be able to return the correct value from
        IsInside().
    */
    wxRecursionGuard(wxRecursionGuardFlag& flag);

    /**
        The destructor resets the flag value so that the function can be entered again
        the next time.

        @note This is not virtual, so this class is not meant to be derived
              from (besides, there is absolutely no reason to do it anyhow).
    */
    ~wxRecursionGuard();

    /**
        Returns @true if we're already inside the code block "protected" by this
        wxRecursionGuard (i.e. between this line and the end of current scope).
        Usually the function using wxRecursionGuard takes some specific actions
        in such case (may be simply returning) to prevent reentrant calls to itself.

        If this method returns @false, it is safe to continue.
    */
    bool IsInside() const;
};

