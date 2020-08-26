/////////////////////////////////////////////////////////////////////////////
// Name:        scopeguard.h
// Purpose:     interface of global functions
// Author:      wxWidgets team
// Licence:     wxWindows licence
/////////////////////////////////////////////////////////////////////////////

/**
    @class wxScopeGuard

    Scope guard is an object which allows executing an action on scope exit.

    The objects of this class must be constructed using wxMakeGuard() function.

    @nolibrary
    @category{misc}
 */
class wxScopeGuard
{
public:
    /**
        Call this method to dismiss the execution of the action on scope exit.

        A typical example:
        @code
            Update1();

            // ensure that changes done so far are rolled back if the next
            // operation throws
            wxScopeGuard guard = wxMakeGuard(RollBack);
            Update2();

            // it didn't throw so commit the changes, i.e. avoid rolling back
            guard.Dismiss();
        @endcode
     */
    void Dismiss();
};

/** @addtogroup group_funcmacro_misc */
//@{
/**
    Returns a scope guard object which will call the specified function with
    the given parameters on scope exit.

    This function is overloaded to take several parameters up to some
    implementation-defined (but relatively low) limit.

    The @a func should be a functor taking parameters of the types P1, ..., PN,
    i.e. the expression @c func(p1, ..., pN) should be valid.
 */
template <typename F, typename P1, ..., typename PN>
wxScopeGuard wxMakeGuard(F func, P1 p1, ..., PN pN);

//@}

/** @addtogroup group_funcmacro_misc */
//@{
/**
    Ensure that the global @a function with a few (up to some
    implementation-defined limit) is executed on scope exit, whether due to a
    normal function return or because an exception has been thrown.

    A typical example of its usage:

    @code
    void *buf = malloc(size);
    wxON_BLOCK_EXIT1(free, buf);
    @endcode

    Please see the original article by Andrei Alexandrescu and Petru Marginean
    published in December 2000 issue of C/C++ Users Journal for more details.

    @see wxON_BLOCK_EXIT_OBJ0()

    @header{wx/scopeguard.h}
*/
#define wxON_BLOCK_EXIT(function, ...)
#define wxON_BLOCK_EXIT0(function)
#define wxON_BLOCK_EXIT1(function, p1)
#define wxON_BLOCK_EXIT2(function, p1, p2)
#define wxON_BLOCK_EXIT3(function, p1, p2, p3)
//@}

/** @addtogroup group_funcmacro_misc */
//@{
/**
    This family of macros is similar to wxON_BLOCK_EXIT(), but calls a method
    of the given object instead of a free function.

    @header{wx/scopeguard.h}
*/
#define wxON_BLOCK_EXIT_OBJ(object, method, ...)
#define wxON_BLOCK_EXIT_OBJ0(object, method)
#define wxON_BLOCK_EXIT_OBJ1(object, method, p1)
#define wxON_BLOCK_EXIT_OBJ2(object, method, p1, p2)
#define wxON_BLOCK_EXIT_OBJ3(object, method, p1, p2, p3)
//@}

/** @addtogroup group_funcmacro_misc */
//@{
/**
    This family of macros is similar to wxON_BLOCK_OBJ(), but calls a method
    of @c this object instead of a method of the specified object.

    @header{wx/scopeguard.h}
*/
#define wxON_BLOCK_EXIT_THIS(method, ...)
#define wxON_BLOCK_EXIT_THIS0(method)
#define wxON_BLOCK_EXIT_THIS1(method, p1)
#define wxON_BLOCK_EXIT_THIS2(method, p1, p2)
#define wxON_BLOCK_EXIT_THIS3(method, p1, p2, p3)
//@}

/** @addtogroup group_funcmacro_misc */
//@{
/**
    This macro sets a variable to the specified value on scope exit.

    Example of usage:
    @code
    void foo()
    {
        bool isDoingSomething = true;
        {
            wxON_BLOCK_EXIT_SET(isDoingSomething, false);
            ... do something ...
        }
        ... isDoingSomething is false now ...
    }
    @endcode

    Notice that @a value is copied, i.e. stored by value, so it can be a
    temporary object returned by a function call, for example.

    @see wxON_BLOCK_EXIT_OBJ0(), wxON_BLOCK_EXIT_NULL()

    @header{wx/scopeguard.h}
*/
#define wxON_BLOCK_EXIT_SET(var, value)

/**
    This macro sets the pointer passed to it as argument to NULL on scope exit.

    It must be used instead of wxON_BLOCK_EXIT_SET() when the value being set
    is @c NULL.

    @header{wx/scopeguard.h}
 */
#define wxON_BLOCK_EXIT_NULL(ptr)

//@}

