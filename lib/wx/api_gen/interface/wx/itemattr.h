/////////////////////////////////////////////////////////////////////////////
// Name:        wx/itemattr.h
// Purpose:     wxItemAttr documentation
// Author:      Vadim Zeitlin
// Copyright:   (c) 2016 Vadim Zeitlin <vadim@wxwidgets.org>
// Licence:     wxWindows licence
/////////////////////////////////////////////////////////////////////////////

/**
    @class wxItemAttr

    Represents the attributes (colour, font, ...) of an item of a control with
    multiple items such as e.g. wxListCtrl.

    @library{wxcore}
    @category{data}

    @see @ref overview_listctrl

    @since 3.1.1 (previous versions had a similar wxListItemAttr class)
*/
class wxItemAttr
{
public:
    /**
        Default Constructor.
    */
    wxItemAttr();

    /**
        Construct a wxItemAttr with the specified foreground and
        background colours and font.
    */
    wxItemAttr(const wxColour& colText,
               const wxColour& colBack,
               const wxFont& font);

    /**
        Compare two item attributes for equality.
    */
    bool operator==(const wxItemAttr& other) const;

    /**
        Compare two item attributes for inequality.
    */
    bool operator!=(const wxItemAttr& other) const;

    /**
        Returns the currently set background colour.
    */
    const wxColour& GetBackgroundColour() const;

    /**
        Returns the currently set font.
    */
    const wxFont& GetFont() const;

    /**
        Returns the currently set text colour.
    */
    const wxColour& GetTextColour() const;

    /**
        Returns @true if the currently set background colour is valid.
    */
    bool HasBackgroundColour() const;

    /**
        Returns @true if either text or background colour is set.

        @see HasBackgroundColour(), HasTextColour()
    */
    bool HasColours() const;

    /**
        Returns @true if the currently set font is valid.
    */
    bool HasFont() const;

    /**
        Returns @true if the currently set text colour is valid.
    */
    bool HasTextColour() const;

    /**
        Returns @true if this object has no custom attributes set.
     */
    bool IsDefault() const;

    /**
        Sets a new background colour.
    */
    void SetBackgroundColour(const wxColour& colour);

    /**
        Sets a new font.
    */
    void SetFont(const wxFont& font);

    /**
        Sets a new text colour.
    */
    void SetTextColour(const wxColour& colour);
};
