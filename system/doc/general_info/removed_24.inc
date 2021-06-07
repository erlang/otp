    <section>
      <title>erl_interface registry</title>
      <p>
	The <c>registry</c> functionality part of <c>erl_interface</c> was as of
	<seeguide marker="deprecations#otp-23">OTP 23 deprecated</seeguide>
	and was removed in OTP 24.
      </p>
    </section>
    <section>
       <title>Compilation of Latin-1 Encoded Erlang Files</title>
       <p>The Erlang compiler now refuses to compile source files
	encoded in Latin-1 without a <c>%% coding: latin-1</c> comment at the
	beginning of the file.</p>
    </section>
    <section>
       <title>igor and erl_tidy modules in syntax_tools</title>
       <p>The <c>igor</c> and <c>erl_tidy</c> modules have been removed from
       OTP and is now maintained by their original author Richard Carlsson.
       They can be found at
       <url href="https://github.com/richcarl/igor">github.com/richcarl/igor</url>
       and
       <url href="https://github.com/richcarl/erl_tidy">github.com/richcarl/erl_tidy</url>, respectively.</p>
    </section>
    <section>
      <title>Distributed Disk Logs</title>
      <p>
	The distributed
	<seeerl marker="kernel:disk_log"><c>disk_log</c></seeerl>
	feature was as of
	<seeguide marker="deprecations#otp-23">OTP 23 deprecated</seeguide>
	and was removed in OTP 24.
      </p>
    </section>
    <section>
      <title>Old Crypto API</title>
      <p>The <seeguide marker="crypto:new_api#the-old-api">Old API</seeguide> was
      removed in OTP 24. The support was formally deprecated as of OTP 23.</p>
      <p>For replacement functions see the
      <seeguide marker="crypto:new_api#the-new-api">New API</seeguide>.</p>
    </section>
    <section>
      <title>Megaco version 3 encoding config</title>
      <p>The pre-release version 3 encoding configs;
      <c>prev3a</c>, <c>prev3b</c> and <c>prev3c</c> was removed in OTP 24.
      Use the full version instead. </p>
      <p>The (encoding) config option for the full version, <c>{version3, 3}</c>,
      will still be supported, even though its no longer necessary to specify
      it this way. </p>
    </section>
