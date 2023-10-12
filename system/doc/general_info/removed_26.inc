    <section>
      <title>Erlang Distribution Without Large Node Container Support</title>
      <p>
	Communication over the Erlang distribution without support for large
	<seeguide marker="erts:erl_dist_protocol#DFLAG_V4_NC"> node container
	data types (version 4)</seeguide> was as of
	<seeguide marker="deprecations#otp-24">OTP 24 deprecated</seeguide>
	and support for it was scheduled for removal in OTP 26. That is, as
	of OTP 26, support for large node container data types will become
	mandatory. This also includes external term format produced by
        <c>term_to_binary()</c>/<c>term_to_iovec()</c>.
      </p>
    </section>

    <section>
      <title>Old Link Protocol</title>
      <p>
	The old link protocol used when communicating over the Erlang
	distribution was as of <seeguide marker="deprecations#otp-24">
	OTP 24 deprecated</seeguide> and support for it was scheduled for
	removal in OTP 26. As of OTP 26 the
	<seeguide marker="erts:erl_dist_protocol#new_link_protocol">new
	link protocol</seeguide> became mandatory. That is, Erlang nodes
	will refuse to connect to nodes not implementing the new
	link protocol.
      </p>
    </section>
