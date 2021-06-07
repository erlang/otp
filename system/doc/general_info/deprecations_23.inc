    <section>
      <title>Crypto Old API</title>
      <p>The <seeguide marker="crypto:new_api#the-old-api">Old API</seeguide> is deprecated
      as of OTP 23 and has been <seeguide marker="removed#otp-24">removed in OTP 24</seeguide>.</p>
      <p>For replacement functions see the
      <seeguide marker="crypto:new_api#the-new-api">New API</seeguide>.</p>
    </section>

    <section>
      <title>http_uri</title>
      <p>Since OTP 21 the recommended module to handle URIs is
      <seeerl marker="stdlib:uri_string">uri_string</seeerl>. The module
      http_uri does not provide a implementation that satisfies the
      RFC.
      </p>
    </section>

    <section>
	<title>ssh</title>
	<p>The public key algorithm <c>'ssh-rsa</c> is regarded as insecure due
	to its usage of SHA1, and is therfore deprecated.
	It will not be available by default from OTP-24.
	</p>

	<p>The public key algorithm <c>'ssh-dss</c> is regarded as insecure due
	to its usage of SHA1 and its short key length, and is therfore deprecated.
	It is not available by default from OTP-23.
	</p>
    </section>

    <section>
      <title>Distributed Disk Logs</title>
      <p>
	As of OTP 23, the distributed
	<seeerl marker="kernel:disk_log"><c>disk_log</c></seeerl>
	feature has been deprecated and it has also been
	<seeguide marker="removed#otp-24">removed in OTP 24</seeguide>.
      </p>
    </section>

    <section>
      <title>erl_interface registry</title>
      <p>
	As of OTP 23, the <c>registry</c> functionality part of
	<c>erl_interface</c> has been deprecated and it has also been
	<seeguide marker="removed#otp-24">removed in OTP 24</seeguide>.
      </p>
    </section>
