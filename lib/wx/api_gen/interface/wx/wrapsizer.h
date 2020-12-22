/////////////////////////////////////////////////////////////////////////////
// Name:        wrapsizer.h
// Purpose:     interface of wxWrapSizer
// Author:      wxWidgets team
// Licence:     wxWindows licence
/////////////////////////////////////////////////////////////////////////////

// flags for wxWrapSizer
enum
{
    wxEXTEND_LAST_ON_EACH_LINE,
    wxREMOVE_LEADING_SPACES,
    wxWRAPSIZER_DEFAULT_FLAGS
};

/**
    @class wxWrapSizer

    A wrap sizer lays out its items in a single line, like a box sizer -- as long
    as there is space available in that direction.
    Once all available space in the primary direction has been used, a new line
    is added and items are added there.

    So a wrap sizer has a primary orientation for adding items, and adds lines
    as needed in the secondary direction.

    @library{wxcore}
    @category{winlayout}

    @see wxBoxSizer, wxSizer, @ref overview_sizer
*/
class wxWrapSizer : public wxBoxSizer
{
public:
    /**
        Constructor for a wxWrapSizer.

        @a orient determines the primary direction of the sizer (the most common
        case being @c wxHORIZONTAL). The flags parameter can be a combination of
        the values @c wxEXTEND_LAST_ON_EACH_LINE which will cause the last item
        on each line to use any remaining space on that line and @c wxREMOVE_LEADING_SPACES
        which removes any spacer elements from the beginning of a row.

        Both of these flags are on by default.
    */
    wxWrapSizer(int orient = wxHORIZONTAL,
                int flags = wxWRAPSIZER_DEFAULT_FLAGS);

    /**
        Not used by an application.

        This is the mechanism by which sizers can inform sub-items of the first
        determined size component.
        The sub-item can then better determine its size requirements.

        Returns @true if the information was used (and the sub-item min size was
        updated).
    */
    virtual bool InformFirstDirection(int direction, int size,
                                      int availableOtherDir);

    virtual void RepositionChildren(const wxSize& minSize);
    virtual wxSize CalcMin();

protected:
    /**
        Can be overridden in the derived classes to treat some normal items as
        spacers.

        This method is used to determine whether the given @a item should be
        considered to be a spacer for the purposes of @c wxREMOVE_LEADING_SPACES
        implementation. By default only returns @true for the real spacers.
     */
    virtual bool IsSpaceItem(wxSizerItem *item) const;
};

