DTrace and Erlang/OTP
=====================

History
-------

The first implementation of DTrace probes for the Erlang virtual
machine was presented at the [2008 Erlang User Conference] [1].  That
work, based on the Erlang/OTP R12 release, was discontinued due to
what appears to be miscommunication with the original developers.

Several users have created Erlang port drivers, linked-in drivers, or
NIFs that allow Erlang code to try to activate a probe,
e.g. `foo_module:dtrace_probe("message goes here!")`.

Goals
-----

* Annotate as much of the Erlang VM as is practical.
   * The initial goal is to trace file I/O operations.
* Support all platforms that implement DTrace: OS X, Solaris,
  and (I hope) FreeBSD and NetBSD.
* To the extent that it's practical, support SystemTap on Linux
  via DTrace provider compatibility.
* Allow Erlang code to supply annotations.

Supported platforms
-------------------

* OS X 10.6.x / Snow Leopard.  It should also work for 10.7 / Lion,
  but I haven't personally tested it.
* Solaris 10.  I have done limited testing on Solaris 11 and
  OpenIndiana release 151a, and both appear to work.
* FreeBSD 9.0, though please see the "FreeBSD 9.0 Release Notes"
  section below!
* Linux via SystemTap compatibility.  Please see
  [$ERL_TOP/HOWTO/SYSTEMTAP.md][] for more details.

Just add the `--with-dynamic-trace=dtrace` option to your command when you 
run the `configure` script. If you are using systemtap, the configure option 
is `--with-dynamic-trace=systemtap`

Status
------

As of R15B01, the dynamic trace code is included in the OTP source distribution,
although it's considered experimental. The main development of the dtrace code 
still happens outside of Ericsson, but there is no need to fetch a patched 
version of the OTP source to get the basic funtionality.

Implementation summary
----------------------

So far, most effort has been focused on the `efile_drv.c` code,
which implements most file I/O on behalf of the Erlang virtual
machine.  This driver also presents a big challenge: its use of an I/O
worker pool (enabled by using the `erl +A 8` flag, for example) makes
it much more difficult to trace I/O activity because each of the
following may be executed in a different Pthread:

* I/O initiation (Erlang code)
* I/O proxy process handling, e.g. read/write when file is not opened
  in `raw` mode, operations executed by the code & file server processes.
  (Erlang code)
* `efile_drv` command setup (C code)
* `efile_drv` command execution (C code)
* `efile_drv` status return (C code)

Example output from `lib/runtime_tools/examples/efile_drv.d` while executing
`file:rename("old-name", "new-name")`:

    efile_drv enter tag={3,84} user tag some-user-tag | RENAME (12) | args: old-name new-name ,\
          0 0 (port #Port<0.59>)
    async I/O worker tag={3,83} | RENAME (12) | efile_drv-int_entry
    async I/O worker tag={3,83} | RENAME (12) | efile_drv-int_return
    efile_drv return tag={3,83} user tag  | RENAME (12) | errno 2

... where the following key can help decipher the output:

* `{3,83}` is the Erlang scheduler thread number (3) and operation
  counter number (83) assigned to this I/O operation.  Together,
  these two numbers form a unique ID for the I/O operation.
* `12` is the command number for the rename operation.  See the
  definition for `FILE_RENAME` in the source code file `efile_drv.c`
  or the `BEGIN` section of the D script `lib/runtime_tools/examples/efile_drv.d`.
* `old-name` and `new-name` are the two string arguments for the
  source and destination of the `rename(2)` system call.
  The two integer arguments are unused; the simple formatting code
  prints the arguments anyway, 0 and 0.
* The worker pool code was called on behalf of Erlang port `#Port<0.59>`.
* The system call failed with a POSIX errno value of 2: `ENOENT`,
  because the path `old-name` does not exist.
* The `efile_drv-int_entry` and `efile_drv_int_return` probes are
  provided in case the user is
  interested in measuring only the latency of code executed by
  `efile_drv` asynchronous functions by I/O worker pool threads
  and the OS system call that they encapsulate.

So, where does the `some-user-tag` string come from?

At the moment, the user tag comes from code like the following:

    dyntrace:put_tag("some-user-tag"),
    file:rename("old-name", "new-name"),

This method of tagging I/O at the Erlang level is subject to change.

Example DTrace probe specification
----------------------------------

    /**
     * Fired when a message is sent from one local process to another.
     *
     * NOTE: The 'size' parameter is in machine-dependent words and
     *       that the actual size of any binary terms in the message
     *       are not included.
     *
     * @param sender the PID (string form) of the sender
     * @param receiver the PID (string form) of the receiver
     * @param size the size of the message being delivered (words)
     * @param token_label for the sender's sequential trace token
     * @param token_previous count for the sender's sequential trace token
     * @param token_current count for the sender's sequential trace token
     */
    probe message__send(char *sender, char *receiver, uint32_t size,
                        int token_label, int token_previous, int token_current);

    /**
     * Fired when a message is sent from a local process to a remote process.
     *
     * NOTE: The 'size' parameter is in machine-dependent words and
     *       that the actual size of any binary terms in the message
     *       are not included.
     *
     * @param sender the PID (string form) of the sender
     * @param node_name the Erlang node name (string form) of the receiver
     * @param receiver the PID/name (string form) of the receiver
     * @param size the size of the message being delivered (words)
     * @param token_label for the sender's sequential trace token
     * @param token_previous count for the sender's sequential trace token
     * @param token_current count for the sender's sequential trace token
     */
    probe message__send__remote(char *sender, char *node_name, char *receiver,
                                uint32_t size,
                        int token_label, int token_previous, int token_current);

    /**
     * Fired when a message is queued to a local process.  This probe
     * will not fire if the sender's pid == receiver's pid.
     *
     * NOTE: The 'size' parameter is in machine-dependent words and
     *       that the actual size of any binary terms in the message
     *       are not included.
     *
     * @param receiver the PID (string form) of the receiver
     * @param size the size of the message being delivered (words)
     * @param queue_len length of the queue of the receiving process
     * @param token_label for the sender's sequential trace token
     * @param token_previous count for the sender's sequential trace token
     * @param token_current count for the sender's sequential trace token
     */
    probe message__queued(char *receiver, uint32_t size, uint32_t queue_len,
                        int token_label, int token_previous, int token_current);

    /**
     * Fired when a message is 'receive'd by a local process and removed
     * from its mailbox.
     *
     * NOTE: The 'size' parameter is in machine-dependent words and
     *       that the actual size of any binary terms in the message
     *       are not included.
     *
     * @param receiver the PID (string form) of the receiver
     * @param size the size of the message being delivered (words)
     * @param queue_len length of the queue of the receiving process
     * @param token_label for the sender's sequential trace token
     * @param token_previous count for the sender's sequential trace token
     * @param token_current count for the sender's sequential trace token
     */
    probe message__receive(char *receiver, uint32_t size, uint32_t queue_len,
                        int token_label, int token_previous, int token_current);

    /* ... */

    /* Async driver pool */

    /**
     * Show the post-add length of the async driver thread pool member's queue.
     *
     * NOTE: The port name is not available: additional lock(s) must
     *       be acquired in order to get the port name safely in an SMP
     *       environment.  The same is true for the aio__pool_get probe.
     *
     * @param port the Port (string form)
     * @param new queue length
     */
    probe aio_pool__add(char *, int);

    /**
     * Show the post-get length of the async driver thread pool member's queue.
     *
     * @param port the Port (string form)
     * @param new queue length
     */
    probe aio_pool__get(char *, int);

    /* Probes for efile_drv.c */

    /**
     * Entry into the efile_drv.c file I/O driver
     *
     * For a list of command numbers used by this driver, see the section
     * "Guide to probe arguments" in ../../../README.md.  That section
     * also contains explanation of the various integer and string
     * arguments that may be present when any particular probe fires.
     *
     * TODO: Adding the port string, args[10], is a pain.  Making that
     *       port string available to all the other efile_drv.c probes
     *       will be more pain.  Is the pain worth it?  If yes, then
     *       add them everywhere else and grit our teeth.  If no, then
     *       rip it out.
     *
     * @param thread-id number of the scheduler Pthread                   arg0
     * @param tag number: {thread-id, tag} uniquely names a driver operation
     * @param user-tag string                                             arg2
     * @param command number                                              arg3
     * @param string argument 1                                           arg4
     * @param string argument 2                                           arg5
     * @param integer argument 1                                          arg6
     * @param integer argument 2                                          arg7
     * @param integer argument 3                                          arg8
     * @param integer argument 4                                          arg9
     * @param port the port ID of the busy port                       args[10]
     */
    probe efile_drv__entry(int, int, char *, int, char *, char *,
                           int64_t, int64_t, int64_t, int64_t, char *);

    /**
     * Entry into the driver's internal work function.  Computation here
     * is performed by a async worker pool Pthread.
     *
     * @param thread-id number
     * @param tag number
     * @param command number
     */
    probe efile_drv__int_entry(int, int, int);

    /**
     * Return from the driver's internal work function.
     *
     * @param thread-id number
     * @param tag number
     * @param command number
     */
    probe efile_drv__int_return(int, int, int);

    /**
     * Return from the efile_drv.c file I/O driver
     *
     * @param thread-id number                                            arg0
     * @param tag number                                                  arg1
     * @param user-tag string                                             arg2
     * @param command number                                              arg3
     * @param Success? 1 is success, 0 is failure                         arg4
     * @param If failure, the errno of the error.                         arg5
     */
    probe efile_drv__return(int, int, char *, int, int, int);

Guide to efile_drv.c probe arguments
------------------------------------

    /* Driver op code: used by efile_drv-entry      arg3 */
    /*                 used by efile_drv-int_entry  arg3 */
    /*                 used by efile_drv-int_return arg3 */
    /*                 used by efile_drv-return     arg3 */

    #define FILE_OPEN            1                 (probe arg3)
            probe arg6 = C driver dt_i1 = flags;
            probe arg4 = C driver dt_s1 = path;

    #define FILE_READ            2                 (probe arg3)
            probe arg6 = C driver dt_i1 = fd;
            probe arg7 = C driver dt_i2 = flags;
            probe arg8 = C driver dt_i3 = size;

    #define FILE_LSEEK           3                 (probe arg3)
            probe arg6 = C driver dt_i1 = fd;
            probe arg7 = C driver dt_i2 = offset;
            probe arg8 = C driver dt_i3 = origin;

    #define FILE_WRITE           4                 (probe arg3)
            probe arg6 = C driver dt_i1 = fd;
            probe arg7 = C driver dt_i2 = flags;
            probe arg8 = C driver dt_i3 = size;

    #define FILE_FSTAT           5                 (probe arg3)
            probe arg6 = C driver dt_i1 = fd;

    #define FILE_PWD             6                 (probe arg3)
            none

    #define FILE_READDIR         7                 (probe arg3)
            probe arg4 = C driver dt_s1 = path;

    #define FILE_CHDIR           8                 (probe arg3)
            probe arg4 = C driver dt_s1 = path;

    #define FILE_FSYNC           9                 (probe arg3)
                probe arg6 = C driver dt_i1 = fd;

    #define FILE_MKDIR          10                 (probe arg3)
            probe arg4 = C driver dt_s1 = path;

    #define FILE_DELETE         11                 (probe arg3)
            probe arg4 = C driver dt_s1 = path;

    #define FILE_RENAME         12                 (probe arg3)
            probe arg4 = C driver dt_s1 = old_name;
            probe arg5 = C driver dt_s2 = new_name;

    #define FILE_RMDIR          13                 (probe arg3)
            probe arg4 = C driver dt_s1 = path;

    #define FILE_TRUNCATE       14                 (probe arg3)
            probe arg6 = C driver dt_i1 = fd;
            probe arg7 = C driver dt_i2 = flags;

    #define FILE_READ_FILE      15                 (probe arg3)
            probe arg4 = C driver dt_s1 = path;

    #define FILE_WRITE_INFO     16                 (probe arg3)
            probe arg6 = C driver dt_i1 = mode;
            probe arg7 = C driver dt_i2 = uid;
            probe arg8 = C driver dt_i3 = gid;

    #define FILE_LSTAT          19                 (probe arg3)
            probe arg4 = C driver dt_s1 = path;

    #define FILE_READLINK       20                 (probe arg3)
            probe arg4 = C driver dt_s1 = path;

    #define FILE_LINK           21                 (probe arg3)
            probe arg4 = C driver dt_s1 = existing_path;
            probe arg5 = C driver dt_s2 = new_path;

    #define FILE_SYMLINK        22                 (probe arg3)
            probe arg4 = C driver dt_s1 = existing_path;
            probe arg5 = C driver dt_s2 = new_path;

    #define FILE_CLOSE          23                 (probe arg3)
            probe arg6 = C driver dt_i1 = fd;
            probe arg7 = C driver dt_i2 = flags;

    #define FILE_PWRITEV        24                 (probe arg3)
            probe arg6 = C driver dt_i1 = fd;
            probe arg7 = C driver dt_i2 = flags;
            probe arg8 = C driver dt_i3 = size;

    #define FILE_PREADV         25                 (probe arg3)
            probe arg6 = C driver dt_i1 = fd;
            probe arg7 = C driver dt_i2 = flags;
            probe arg8 = C driver dt_i3 = size;

    #define FILE_SETOPT         26                 (probe arg3)
            probe arg6 = C driver dt_i1 = opt_name;
            probe arg7 = C driver dt_i2 = opt_specific_value;

    #define FILE_IPREAD         27                 (probe arg3)
            probe arg6 = C driver dt_i1 = fd;
            probe arg7 = C driver dt_i2 = flags;
            probe arg8 = C driver dt_i3 = offsets[0];
            probe arg9 = C driver dt_i4 = size;

    #define FILE_ALTNAME        28                 (probe arg3)
            probe arg4 = C driver dt_s1 = path;

    #define FILE_READ_LINE      29                 (probe arg3)
            probe arg6 = C driver dt_i1 = fd;
            probe arg7 = C driver dt_i2 = flags;
            probe arg8 = C driver dt_i3 = read_offset;
            probe arg9 = C driver dt_i4 = read_ahead;

    #define FILE_FDATASYNC      30                 (probe arg3)
            probe arg6 = C driver dt_i1 = fd;

    #define FILE_FADVISE        31                 (probe arg3)
            probe arg6 = C driver dt_i1 = fd;
            probe arg7 = C driver dt_i2 = offset;
            probe arg8 = C driver dt_i3 = length;
            probe arg9 = C driver dt_i4 = advise_type;

   [1]: http://www.erlang.org/euc/08/
   [$ERL_TOP/HOWTO/SYSTEMTAP.md]: SYSTEMTAP.md
