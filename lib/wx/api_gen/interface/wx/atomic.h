/////////////////////////////////////////////////////////////////////////////
// Name:        atomic.h
// Purpose:     interface of global functions
// Author:      wxWidgets team
// Licence:     wxWindows licence
/////////////////////////////////////////////////////////////////////////////


// ============================================================================
// Global functions/macros
// ============================================================================

/** @addtogroup group_funcmacro_atomic */
//@{

/**
    This function increments @a value in an atomic manner.

    Whenever possible wxWidgets provides an efficient, CPU-specific,
    implementation of this function. If such implementation is available, the
    symbol wxHAS_ATOMIC_OPS is defined. Otherwise this function still exists
    but is implemented in a generic way using a critical section which can be
    prohibitively expensive for use in performance-sensitive code.

    @header{wx/atomic.h}
*/
void wxAtomicInc(wxAtomicInt& value);

/**
    This function decrements value in an atomic manner.

    Returns 0 if value is 0 after decrement or any non-zero value (not
    necessarily equal to the value of the variable) otherwise.

    @see wxAtomicInc

    @header{wx/atomic.h}
*/
wxInt32 wxAtomicDec(wxAtomicInt& value);

//@}

