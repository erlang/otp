    <section>
      <title>Erlang Distribution Without Large Node Container Support</title>
      <p>
	Communication over the Erlang distribution without support for large
	<seeguide marker="erts:erl_dist_protocol#DFLAG_V4_NC">node container
	data types (version 4)</seeguide> is as of OTP 24 deprecated and is
	<seeguide marker="scheduled_for_removal#otp-26">scheduled for removal
	in OTP 26</seeguide>. That is, as of OTP 26, support for large
	node container data types will become mandatory.
      </p>
    </section>

    <section>
      <title>Old Link Protocol</title>
      <p>
	The <seeguide marker="erts:erl_dist_protocol#old_link_protocol">old
	link protocol</seeguide> used when communicating over the Erlang
	distribution is as of OTP 24 deprecated and support for it is
	<seeguide marker="scheduled_for_removal#otp-26">scheduled for removal
	in OTP 26</seeguide>. As of OTP 26, the
	<seeguide marker="erts:erl_dist_protocol#new_link_protocol">new
	link protocol</seeguide> will become mandatory. That is, Erlang nodes
	will then refuse to connect to nodes not implementing the new
	link protocol. If you implement the Erlang distribution yourself, you
	are, however, encouraged to implement the new link protocol as soon as
	possible since the old protocol can cause links to enter an
	inconsistent state.
      </p>
    </section>
 
    <section>
      <title>?NO_APP macro</title>
      <p>
        The ?NO_APP macro in the edoc include file <c>edoc_doclet.hrl</c> has
        been deprecated.
      </p>
    </section>
