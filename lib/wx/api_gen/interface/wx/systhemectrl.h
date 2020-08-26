/////////////////////////////////////////////////////////////////////////////
// Name:        wx/systhemectrl.h
// Purpose:     Documentation for wxSystemThemedControl
// Author:      Tobias Taschner
// Created:     2014-08-15
// Copyright:   (c) 2014 wxWidgets development team
// Licence:     wxWindows licence
/////////////////////////////////////////////////////////////////////////////

/**
   A helper class making it possible to use system theme for any control.

   Under MSW, there an alternative theme available for the list and list-like
   controls since Windows Vista. This theme us used by Windows Explorer list
   and tree view and so is arguably more familiar to the users than the standard
   appearance of these controls.

   This class is used in wxWidgets to enable this system theme in wxTreeCtrl,
   wxListCtrl and wxDataViewCtrl and thus give them the same, familiar look.
   It can also be used as a helper for implementing custom controls with the same
   appearance. Notice that when using this class it is especially important
   to use wxRendererNative::DrawItemSelectionRect() and
   wxRendererNative::DrawItemText() to draw the control items to ensure that
   they appear correctly under all platforms and Windows versions.

   The following example shows implementation of a system theme enabled wxVListBox:
   @code
        #include <wx/systhemectrl.h>

        class MyListCtrl : public wxSystemThemedControl<wxVListBox>
        {
        public:
            MyListCtrl(wxWindow* parent)
            {

                ...

                EnableSystemTheme();
            }

            void OnDrawItem(wxDC& dc, const wxRect& rect, size_t n) const wxOVERRIDE
            {
                wxRendererNative::GetDefault().DrawItemText(this, dc, "Item #x", rect);
            }
        };
   @endcode

   Please also note that if you want to disable the system theme use in the
   control that use it by default, it's best to do it before actually creating
   the control as enabling the system theme can't always be completely undone
   later. I.e. instead of
   @code
        // THIS CODE IS WRONG, DO NOT DO IT LIKE THIS
        wxTreeCtrl* tree = new wxTreeCtrl(parent, wxID_ANY);
        tree->EnableSystemTheme(false);
   @endcode
   prefer the following version:
   @code
        // Use default ctor to create the object, avoiding creating the window.
        wxTreeCtrl* tree = new wxTreeCtrl();

        // Then disable the system theme used by default.
        tree->EnableSystemTheme(false);

        // And only then actually create the window.
        tree->Create(parent, wxID_ANY);
   @endcode

   On non-MSW platforms this class currently does nothing but is still
   available, so that it can be used in portable code without any conditional
   compilation directives.

   @category{miscwnd}

   @see wxTreeCtrl, wxListCtrl, wxDataViewCtrl, wxRendererNative

   @since 3.1.0
*/
template <class C>
class wxSystemThemedControl : public C
{
public:
    /// Trivial default constructor.
    wxSystemThemedControl();

    /**
       This method may be called to disable the system theme of controls
       using it by default.
    */
    void EnableSystemTheme(bool enable = true);
};
