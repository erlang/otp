/////////////////////////////////////////////////////////////////////////////
// Name:        tracker.h
// Purpose:     interface of wxTrackable
// Author:      wxWidgets team
// Licence:     wxWindows licence
/////////////////////////////////////////////////////////////////////////////

/**
    @class wxTrackable

    Add-on base class for a trackable object. This class maintains an internal
    linked list of classes of type wxTrackerNode and calls OnObjectDestroy() on
    them if this object is destroyed. The most common usage is by using the
    wxWeakRef<T> class template which automates this. This class has no public
    API. Its only use is by deriving another class from it to make it trackable.

    @code
    class MyClass: public Foo, public wxTrackable
    {
        // whatever
    }

    typedef wxWeakRef<MyClass> MyClassRef;
    @endcode

    @library{wxbase}
    @category{smartpointers}
*/
class wxTrackable
{
public:

};

