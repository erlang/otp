    <section>
      <title>VxWorks Support</title>
      <p>Some parts of OTP has had limited VxWorks support, such as for
      example <seealso marker="erl_interface:index"><c>erl_interface</c></seealso>.
      This support is now deprecated and has also been
      <seealso marker="scheduled_for_removal#OTP-23">scheduled for removal</seealso>.</p>
    </section>
    <section>
      <title>Legacy parts of erl_interface</title>
      <p>The old legacy <seealso marker="erl_interface:index"><c>erl_interface</c></seealso>
      library (functions with prefix <c>erl_</c>) is deprecated as of OTP 22. These
      parts of <c>erl_interface</c> has been informally deprecated
      for a very long time. You typically want to replace the usage of
      the <c>erl_interface</c> library with the use of the <c>ei</c> library
      which also is part of the <c>erl_interface</c> application. The old legacy
      <seealso marker="erl_interface:index"><c>erl_interface</c></seealso>
      library has also been <seealso marker="scheduled_for_removal#OTP-23">scheduled
      for removal</seealso>.</p>
    </section>
    <section>
      <title>System Events</title>
      <p>
	The format of "System Events" as defined in the man page for
	<seealso marker="stdlib:sys">sys</seealso>
	has been clarified and cleaned up.
	Due to this, code that relied on the internal badly
	documented previous (before this change) format
	of OTP's "System Events", needs to be changed.
      </p>
      <p>
	In the wake of this the function
	<seealso marker="stdlib:sys#get_debug/3">sys:get_debug/3</seealso>
	that returns data with undocumented and internal format
	(and therefore is practically useless) has been deprecated,
	and a new function
	<seealso marker="stdlib:sys#get_log/1">sys:get_log/1</seealso>
	has been added,
	that hopefully does what the deprecated function was intended for.
      </p>
    </section>
