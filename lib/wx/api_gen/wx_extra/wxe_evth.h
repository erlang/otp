
// This add some extra functions, special code is in the compiler
// this file is currently not used at all except to get the inherited
// functions in the erlang modules.


class wxeEvtHandler : public wxObject
{
 public:
   // Dynamic association of a member function handler with the event handler,
   // winid and event type
   void Connect(int eventType,
		int options = -1);

   // Disconnect

   // Convenience function: take just one id
   void Disconnect(int eventType, int winid = -1);

   // Even more convenient: without id (same as using id of wxID_ANY)
   void Disconnect(int eventType);

   void Disconnect();
   
}
