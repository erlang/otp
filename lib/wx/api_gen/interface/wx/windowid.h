/////////////////////////////////////////////////////////////////////////////
// Name:        windowid.h
// Purpose:     interface of wxIdManager
// Author:      wxWidgets team
// Licence:     wxWindows licence
/////////////////////////////////////////////////////////////////////////////

/**
    The type of unique identifiers (ID) used for wxWindow-derived classes.
*/
typedef int wxWindowID;

/**
    @class wxIdManager

    wxIdManager is responsible for allocating and releasing window IDs.
    It is used by wxWindow::NewControlId() and wxWindow::UnreserveControlId(),
    and can also be used be used directly.

    @library{wxcore}
    @category{cfg}

    @see wxWindow::NewControlId(), wxWindow::UnreserveControlId(),
         @ref overview_windowids
*/
class wxIdManager
{
public:
    /**
        Called directly by wxWindow::NewControlId(), this function will create
        a new ID or range of IDs.
        The IDs will be reserved until assigned to a wxWindowIDRef() or unreserved
        with UnreserveControlId().
        Only ID values that are not assigned to a wxWindowIDRef() need to be unreserved.

        @param count
            The number of sequential IDs to reserve.

        @return The value of the first ID in the sequence, or wxID_NONE.
    */
    static wxWindowID ReserveId(int count = 1);

    /**
        Called directly by wxWindow::UnreserveControlId(), this function will
        unreserve an ID or range of IDs that is currently reserved.
        This should only be called for IDs returned by ReserveControlId() that
        have NOT been assigned to a wxWindowIDRef (see @ref overview_windowids).

        @param id
            The first of the range of IDs to unreserve.
        @param count
            The number of sequential IDs to unreserve.

        @return The value of the first ID in the sequence, or wxID_NONE.
    */
    static void UnreserveId(wxWindowID id, int count = 1);
};

