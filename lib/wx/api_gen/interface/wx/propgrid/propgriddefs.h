/////////////////////////////////////////////////////////////////////////////
// Name:        propgriddefs.h
// Purpose:     various constants, etc. used in documented propgrid API
// Author:      wxWidgets team
// Licence:     wxWindows licence
/////////////////////////////////////////////////////////////////////////////


// -----------------------------------------------------------------------

/** Used to tell wxPGProperty to use label as name as well
*/
#define wxPG_LABEL              (*wxPGProperty::sm_wxPG_LABEL)

/** This is the value placed in wxPGProperty::sm_wxPG_LABEL
*/
#define wxPG_LABEL_STRING       wxS("@!")
#define wxPG_NULL_BITMAP        wxNullBitmap
#define wxPG_COLOUR_BLACK       (*wxBLACK)

/** Convert Red, Green and Blue to a single 32-bit value.
*/
#define wxPG_COLOUR(R,G,B) ((wxUint32)(R+(G<<8)+(B<<16)))


/** If property is supposed to have custom-painted image, then returning
    this in wxPGProperty::OnMeasureImage() will usually be enough.
*/
#define wxPG_DEFAULT_IMAGE_SIZE             wxDefaultSize


/** This callback function is used for sorting properties.

    Call wxPropertyGrid::SetSortFunction() to set it.

    Sort function should return a value greater than 0 if position of @a p1 is
    after @a p2. So, for instance, when comparing property names, you can use
    following implementation:

        @code
            int MyPropertySortFunction(wxPropertyGrid* propGrid,
                                       wxPGProperty* p1,
                                       wxPGProperty* p2)
            {
                return p1->GetBaseName().compare( p2->GetBaseName() );
            }
        @endcode
*/
typedef int (*wxPGSortCallback)(wxPropertyGrid* propGrid,
                                wxPGProperty* p1,
                                wxPGProperty* p2);

// -----------------------------------------------------------------------

/** Used to indicate wxPGChoices::Add() etc that the value is actually not given
    by the caller.
*/
#define wxPG_INVALID_VALUE      INT_MAX

// -----------------------------------------------------------------------

enum wxPG_GETPROPERTYVALUES_FLAGS
{
/** Flag for wxPropertyGridInterface::SetProperty* functions,
    wxPropertyGridInterface::HideProperty(), etc.
    Apply changes only for the property in question.
    @hideinitializer
*/
wxPG_DONT_RECURSE                 = 0x00000000,

/** Flag for wxPropertyGridInterface::GetPropertyValues().
    Use this flag to retain category structure; each sub-category
    will be its own wxVariantList of wxVariant.
    @hideinitializer
*/
wxPG_KEEP_STRUCTURE               = 0x00000010,

/** Flag for wxPropertyGridInterface::SetProperty* functions,
    wxPropertyGridInterface::HideProperty(), etc.
    Apply changes recursively for the property and all its children.
    @hideinitializer
*/
wxPG_RECURSE                      = 0x00000020,

/** Flag for wxPropertyGridInterface::GetPropertyValues().
    Use this flag to include property attributes as well.
    @hideinitializer
*/
wxPG_INC_ATTRIBUTES               = 0x00000040,

/** Used when first starting recursion.
    @hideinitializer
*/
wxPG_RECURSE_STARTS               = 0x00000080,

/** Force value change.
    @hideinitializer
*/
wxPG_FORCE                        = 0x00000100,

/** Only sort categories and their immediate children.
    Sorting done by ::wxPG_AUTO_SORT option uses this.
    @hideinitializer
*/
wxPG_SORT_TOP_LEVEL_ONLY          = 0x00000200
};

// -----------------------------------------------------------------------

/** Misc argument flags.
*/
enum wxPG_MISC_ARG_FLAGS
{
    /** Get/Store full value instead of displayed value.
        @hideinitializer
    */
    wxPG_FULL_VALUE                     = 0x00000001,

    /** Perform special action in case of unsuccessful conversion.
        @hideinitializer
    */
    wxPG_REPORT_ERROR                   = 0x00000002,

    /**
        @hideinitializer
    */
    wxPG_PROPERTY_SPECIFIC              = 0x00000004,

    /** Get/Store editable value instead of displayed one (should only be
        different in the case of common values).
        @hideinitializer
    */
    wxPG_EDITABLE_VALUE                 = 0x00000008,

    /** Used when dealing with fragments of composite string value
        @hideinitializer
    */
    wxPG_COMPOSITE_FRAGMENT             = 0x00000010,

    /** Means property for which final string value is for cannot really be
        edited.
        @hideinitializer
    */
    wxPG_UNEDITABLE_COMPOSITE_FRAGMENT  = 0x00000020,

    /** wxPGProperty::ValueToString() called from wxPGProperty::GetValueAsString()
        (guarantees that input wxVariant value is current own value)
        @hideinitializer
    */
    wxPG_VALUE_IS_CURRENT               = 0x00000040,

    /** Value is being set programmatically (i.e. not by user)
        @hideinitializer
    */
    wxPG_PROGRAMMATIC_VALUE             = 0x00000080
};

// -----------------------------------------------------------------------

/** wxPGProperty::SetValue() flags
*/
enum wxPG_SETVALUE_FLAGS
{
    /**
        @hideinitializer
    */
    wxPG_SETVAL_REFRESH_EDITOR      = 0x0001,
    /**
        @hideinitializer
    */
    wxPG_SETVAL_AGGREGATED          = 0x0002,
    /**
        @hideinitializer
    */
    wxPG_SETVAL_FROM_PARENT         = 0x0004,
    /** Set if value changed by user
        @hideinitializer
    */
    wxPG_SETVAL_BY_USER             = 0x0008
};

// -----------------------------------------------------------------------

//
/** Valid constants for ::wxPG_UINT_BASE attribute
    (@c long because of wxVariant constructor)
*/
#define wxPG_BASE_OCT                       8L
#define wxPG_BASE_DEC                       10L
#define wxPG_BASE_HEX                       16L
#define wxPG_BASE_HEXL                      32L

/** Valid constants for ::wxPG_UINT_PREFIX attribute
*/
#define wxPG_PREFIX_NONE                    0L
#define wxPG_PREFIX_0x                      1L
#define wxPG_PREFIX_DOLLAR_SIGN             2L

// -----------------------------------------------------------------------
