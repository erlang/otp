/////////////////////////////////////////////////////////////////////////////
// Name:        wx/textwrapper.h
// Purpose:     documentation of wxTextWrapper interface
// Author:      Vadim Zeitlin
// Copyright:   (c) 2009 Vadim Zeitlin <vadim@wxwidgets.org>
// Licence:     wxWindows licence
/////////////////////////////////////////////////////////////////////////////

/**
    @class wxTextWrapper

    Helps wrap lines of text to given width.

    This is a generic purpose class which can be used to wrap lines of text to
    the specified width. It doesn't do anything by itself but simply calls its
    virtual OnOutputLine() and OnNewLine() methods for each wrapped line of
    text, you need to implement them in your derived class to actually do
    something useful.

    Here is an example function using this class which inserts hard line breaks
    into a string of text at the positions where it would be wrapped:

    @code
    wxString WrapText(wxWindow *win, const wxString& text, int widthMax)
    {
        class HardBreakWrapper : public wxTextWrapper
        {
        public:
            HardBreakWrapper(wxWindow *win, const wxString& text, int widthMax)
            {
                Wrap(win, text, widthMax);
            }

            wxString const& GetWrapped() const { return m_wrapped; }

        protected:
            virtual void OnOutputLine(const wxString& line)
            {
                m_wrapped += line;
            }

            virtual void OnNewLine()
            {
                m_wrapped += '\n';
            }

        private:
            wxString m_wrapped;
        };

        HardBreakWrapper wrapper(win, text, widthMax);
        return wrapper.GetWrapped();
    }
    @endcode

    @nolibrary
    @category{gdi}
 */
class wxTextWrapper
{
public:
    /**
        Trivial default constructor.
     */
    wxTextWrapper();

    /**
        Wrap the given text.

        This method will call OnOutputLine() for every line of wrapped text and
        OnNewLine() before the beginning of every new line after the first one
        (so it might be never called at all if the width of entire @a text is
        less than @a widthMax).

        @param win
            A non-@NULL window used for measuring the text extents.
        @param text
            The text to wrap.
        @param widthMax
            Maximal width of each line of text or @c -1 to disable wrapping.
     */
    void Wrap(wxWindow *win, const wxString& text, int widthMax);

protected:
    /**
        Called by Wrap() for each wrapped line of text.

        This method will always be called at least once by Wrap(). Notice that
        @a line may be empty if the text passed to Wrap() was empty itself.
     */
    virtual void OnOutputLine(const wxString& line) = 0;

    /**
        Called at the start of each subsequent line of text by Wrap().

        This method may not be called at all if the entire text passed to
        Wrap() fits into the specified width.
     */
    virtual void OnNewLine();
};
