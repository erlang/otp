/////////////////////////////////////////////////////////////////////////////
// Name:        wx/containr.h
// Purpose:     documentation of wxNavigationEnabled<>
// Author:      Vadim Zeitlin
// Created:     2011-07-23
// Copyright:   (c) 2011 Vadim Zeitlin <vadim@wxwidgets.org>
// Licence:     wxWindows licence
/////////////////////////////////////////////////////////////////////////////

/**
    A helper class implementing TAB navigation among the window children.

    This class contains the functionality needed to correctly implement TAB
    navigation among the children of the window. Its exact contents is not
    important and is intentionally not documented as the only way to use this
    class is to inherit from it instead of inheriting from the usual base class
    directly. For example, if some class needs to inherit from wxControl but
    contains multiple sub-windows and needs to support keyboard navigation, it
    is enough to declare it in the following way:
    @code
        class MyControlWithSubChildren :
            public wxNavigationEnabled<wxControl>
        {
        public:
            // Default constructor is implemented in the same way as always.
            MyControlWithSubChildren() { }

            // Non-default constructor can't use wxControl ctor any more as
            // wxControl is not its direct base class, but it can use Create().
            MyControlWithSubChildren(wxWindow *parent, wxWindowID winid)
            {
                wxControl::Create(parent, winid);

                // More creation code...
            }

            // Everything else as usual ...
        };
    @endcode

    @library{wxcore}

    @since 2.9.3
 */
template <class W>
class wxNavigationEnabled : public W
{
public:
    /// The name of the real base window class that this class derives from.
    typedef W BaseWindowClass;

    /**
        Default constructor.

        This class provides only the default constructor as it's not possible,
        in general, to provide all the constructors of the real base class
        BaseWindowClass.

        This is however not usually a problem for wxWindow-derived classes as,
        by convention, they always define a Create() method such that calling
        it on an object initialized using the default constructor is equivalent
        to using a non-default constructor directly. So the classes inheriting
        from wxNavigationEnabled<W> should simply call W::Create() in their
        constructors.
     */
    wxNavigationEnabled();
};
