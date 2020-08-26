/////////////////////////////////////////////////////////////////////////////
// Name:        wupdlock.h
// Purpose:     interface of wxWindowUpdateLocker
// Author:      wxWidgets team
// Licence:     wxWindows licence
/////////////////////////////////////////////////////////////////////////////

/**
    @class wxWindowUpdateLocker

    This tiny class prevents redrawing of a wxWindow during its lifetime by using
    wxWindow::Freeze() and wxWindow::Thaw() methods.

    It is typically used for creating automatic objects to temporarily suppress
    window updates before a batch of operations is performed:

    @code
    void MyFrame::Foo()
        {
            m_text = new wxTextCtrl(this, ...);

            wxWindowUpdateLocker noUpdates(m_text);
            m_text-AppendText();
            ... many other operations with m_text...
            m_text-WriteText();
        }
    @endcode

    Using this class is easier and safer than calling wxWindow::Freeze() and
    wxWindow::Thaw() because you don't risk to forget calling the latter.

    @library{wxbase}
    @category{misc}
*/
class wxWindowUpdateLocker
{
public:
    /**
        Default constructor doesn't do anything.

        Prefer using the non-default constructor if possible, this constructor
        is only useful if Lock() must be called conditionally, i.e. if it may
        or not be called depending on some run-time condition.

        @since 3.1.4
     */
    wxWindowUpdateLocker();

    /**
        Creates an object preventing the updates of the specified @e win.
        The parameter must be non-@NULL and the window must exist for longer than
        wxWindowUpdateLocker object itself.
    */
    explicit wxWindowUpdateLocker(wxWindow* win);

    /**
        Really lock window updates.

        This method can only be called on an object initialized using the
        default constructor.

        @param win Non-@NULL window which must exist for longer than this object.

        @since 3.1.4
     */
    void Lock(wxWindow *win);

    /**
        Destructor reenables updates for the window this object is associated with.
    */
    ~wxWindowUpdateLocker();
};

