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
