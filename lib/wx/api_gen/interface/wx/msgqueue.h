/////////////////////////////////////////////////////////////////////////////
// Name:        wx/msgqueue.h
// Purpose:     interface of wxMessageQueue<T>
// Author:      wxWidgets team
// Licence:     wxWindows licence
/////////////////////////////////////////////////////////////////////////////

/**
    Error codes for wxMessageQueue<> operations.

    This enum contains the possible return value of wxMessageQueue<> methods.

    @since 2.9.0
    @category{threading}
 */
enum wxMessageQueueError
{
    /// Indicates that the operation completed successfully.
    wxMSGQUEUE_NO_ERROR = 0,

    /**
        Indicates that no messages were received before timeout expired.

        This return value is only used by wxMessageQueue<>::ReceiveTimeout().
     */
    wxMSGQUEUE_TIMEOUT,

    /// Some unexpected (and fatal) error has occurred.
    wxMSGQUEUE_MISC_ERROR
};

/**
    wxMessageQueue allows passing messages between threads.

    This class should be typically used to communicate between the main and worker
    threads. The main thread calls wxMessageQueue::Post and the worker thread
    calls wxMessageQueue::Receive.

    @tparam T
        For this class a message is an object of arbitrary type T.

    Notice that often there is a some special message indicating that the thread
    should terminate as there is no other way to gracefully shutdown a thread
    waiting on the message queue.

    @since 2.9.0

    @nolibrary
    @category{threading}

    @see wxThread
*/
template <typename T>
class wxMessageQueue<T>
{
public:
    /**
        Default and only constructor.
        Use wxMessageQueue::IsOk to check if the object was successfully initialized.
    */
    wxMessageQueue();

    /**
        Remove all messages from the queue.

        This method is meant to be called from the same thread(s) that call
        Post() to discard any still pending requests if they became
        unnecessary.

        @since 2.9.1
     */
    wxMessageQueueError Clear();

    /**
        Returns @true if the object had been initialized successfully, @false
        if an error occurred.
    */
    bool IsOk() const;

    /**
        Add a message to this queue and signal the threads waiting for messages
        (i.e. the threads which called wxMessageQueue::Receive or
        wxMessageQueue::ReceiveTimeout).

        This method is safe to call from multiple threads in parallel.
    */
    wxMessageQueueError Post(T const& msg);

    /**
        Block until a message becomes available in the queue.
        Waits indefinitely long or until an error occurs.

        The message is returned in @a msg.
    */
    wxMessageQueueError Receive(T& msg);

    /**
        Block until a message becomes available in the queue, but no more than
        @a timeout milliseconds has elapsed.

        If no message is available after @a timeout milliseconds then returns
        @b wxMSGQUEUE_TIMEOUT.

        If @a timeout is 0 then checks for any messages present in the queue
        and returns immediately without waiting.

        The message is returned in @a msg.
    */
    wxMessageQueueError ReceiveTimeout(long timeout, T& msg);
};

