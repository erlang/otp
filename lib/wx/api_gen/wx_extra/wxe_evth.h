
class wxeEvtHandler : public wxObject
{

  /**
    @class wxeEvtHandler

    A class that can handle events from the windowing system.
    wxWindow is (and therefore all window classes are) derived from this class.

    To get events from wxwidgets objects you subscribe to them by
    calling Connect().

    If the @c callback option is not supplied events are sent as messages.

    These messages will be @c #wx{} where @c EventRecord is a record that
    depends on the @c wxEventType. The records are defined in: @b wx/include/wx.hrl.

    If a callback was supplied to connect, the callback will be invoked
    (in another process) to handle the event. The callback should be of arity 2.

    @verbatim fun Callback (EventRecord::wx(), EventObject::wxObject()). @endverbatim

    @note The callback will be in executed in new process each time.

    @see @ref overview_events_processing
  */


 public:

  /**
   This function subscribes to events.

   Subscribes to events of type @c EventType, in the range @c id, @c lastId.

   The events will be received as messages if no callback is supplied.

   Options

   id:  @verbatim {id, integer()} @endverbatim
    The identifier (or first of the identifier range) to be
   associated with this event handler.
   Default is @c ?wxID_ANY

   lastid:  @verbatim {lastId,integer()} @endverbatim
    The second part of the identifier range.
   If used 'id' must be set as the starting identifier range.
   Default is @c ?wxID_ANY

   skip:  @verbatim {skip,boolean()} @endverbatim
    If skip is true further event_handlers will be called.
   This is not used if the 'callback' option is used.
   Default is @c false.

   callback: @verbatim {callback,function()} @endverbatim
    Use a callback @verbatim fun(EventRecord::wx(),EventObject::wxObject()) @endverbatim
    to process the event. Default not specified i.e. a message will
   be delivered to the process calling this function.

   userData: @verbatim {userData,term()} @endverbatim
    An erlang term that will be sent with the event. Default: @c [].

  */
   void Connect(int eventType, int options = -1);

   /**
       This function unsubscribes the process or callback fun from the event handler.
       EventType may be the atom 'null' to match any eventtype.
       Notice that the options skip and userdata is not used to match the eventhandler.
   */

   void Disconnect(int eventType, int winid = -1);

   void Disconnect(int eventType);

   void Disconnect();
}
