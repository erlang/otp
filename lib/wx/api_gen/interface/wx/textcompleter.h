/////////////////////////////////////////////////////////////////////////////
// Name:        wx/textcompleter.h
// Purpose:     interface of wxTextCompleter
// Author:      Vadim Zeitlin
// Created:     2011-04-13
// Copyright:   (c) 2011 Vadim Zeitlin <vadim@wxwidgets.org>
// Licence:     wxWindows licence
/////////////////////////////////////////////////////////////////////////////

/**
    @class wxTextCompleter

    Base class for custom text completer objects.

    Custom completer objects used with wxTextEntry::AutoComplete() must derive
    from this class and implement its pure virtual method returning the
    completions. You would typically use a custom completer when the total
    number of completions is too big for performance to be acceptable if all of
    them need to be returned at once but if they can be generated
    hierarchically, i.e. only the first component initially, then the second
    one after the user finished entering the first one and so on.

    When inheriting from this class you need to implement its two pure virtual
    methods. This allows returning the results incrementally and may or not be
    convenient depending on where do they come from. If you prefer to return
    all the completions at once, you should inherit from wxTextCompleterSimple
    instead.

    @since 2.9.2
 */
class wxTextCompleter
{
public:
    /**
        Function called to start iteration over the completions for the given
        prefix.

        This function could start a database query, for example, if the results
        are read from a database.

        Notice that under some platforms (currently MSW only) it is called from
        another thread context and so the appropriate synchronization mechanism
        should be used to access any data also used by the main UI thread.

        @param prefix
            The prefix for which completions are to be generated.
        @return
            @true to continue with calling GetNext() or @false to indicate that
            there are no matches and GetNext() shouldn't be called at all.
     */
    virtual bool Start(const wxString& prefix) = 0;

    /**
        Called to retrieve the next completion.

        All completions returned by this function should start with the prefix
        passed to the last call to Start().

        Notice that, as Start(), this method is called from a worker thread
        context under MSW.

        @return
            The next completion or an empty string to indicate that there are
            no more of them.
     */
    virtual wxString GetNext() = 0;
};

/**
    A simpler base class for custom completer objects.

    This class may be simpler to use than the base wxTextCompleter as it allows
    to implement only a single virtual method instead of two of them (at the
    price of storing all completions in a temporary array).

    Here is a simple example of a custom completer that completes the names of
    some chess pieces. Of course, as the total list here has only four items it
    would have been much simpler to just specify the array containing all the
    completions in this example but the same approach could be used when the
    total number of completions is much higher provided the number of
    possibilities for each word is still relatively small:
    @code
    class MyTextCompleter : public wxTextCompleterSimple
    {
    public:
        virtual void GetCompletions(const wxString& prefix, wxArrayString& res)
        {
            const wxString firstWord = prefix.BeforeFirst(' ');
            if ( firstWord == "white" )
            {
                res.push_back("white pawn");
                res.push_back("white rook");
            }
            else if ( firstWord == "black" )
            {
                res.push_back("black king");
                res.push_back("black queen");
            }
            else
            {
                res.push_back("white");
                res.push_back("black");
            }
        }
    };
    ...
    wxTextCtrl *text = ...;
    text->AutoComplete(new MyTextCompleter);
    @endcode

    @library{wxcore}

    @since 2.9.2
*/
class wxTextCompleterSimple : public wxTextCompleter
{
public:
    /**
        Pure virtual method returning all possible completions for the given
        prefix.

        The custom completer should examine the provided prefix and return all
        the possible completions for it in the output array @a res.

        Please notice that the returned values should start with the prefix,
        otherwise they will be simply ignored, making adding them to the array
        in the first place useless.

        Notice that this function may be called from thread other than main one
        (this is currently always the case under MSW) so the appropriate
        synchronization mechanism should be used to protect the shared data.

        @param prefix
            The possibly empty prefix that the user had already entered.
        @param res
            Initially empty array that should be filled with all possible
            completions (possibly none if there are no valid possibilities
            starting with the given prefix).
     */
    virtual void GetCompletions(const wxString& prefix, wxArrayString& res) = 0;
};
