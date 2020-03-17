    <section>
      <title>Crypto Old API</title>
      <p>The <seealso marker="crypto:new_api#the-old-api">Old API</seealso> is now deprecated
      and has also been
      <seealso marker="scheduled_for_removal#OTP-24">scheduled for removal</seealso>.</p>
      <p>For replacement functions see the
      <seealso marker="crypto:new_api#the-new-api">New API</seealso>.</p>
    </section>

    <section>
      <title>http_uri</title>
      <p>Since OTP 21 the recommended module to handle URIs is
      <seealso marker="stdlib:uri_string">uri_string</seealso>. The module
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
      <title>pg2</title>
      <p>
	As of OTP 23, a new process group implementation
	<seealso marker="kernel:pg">pg</seealso> is introduced.
	<c>pg</c> is similar to <seealso marker="kernel:pg2">pg2</seealso>,
	but with much better scalability properties. However, the API and behavior
	are not compatible.
      </p>
      <p>
	<c>pg2</c> is now deprecated and has also been
	<seealso marker="scheduled_for_removal#OTP-24">scheduled for removal</seealso>
	in OTP 24.
      </p>
    </section>

    <section>
      <title>Distributed Disk Logs</title>
      <p>
	As of OTP 23, the distributed
	<seealso marker="kernel:disk_log"><c>disk_log</c></seealso>
	feature has been deprecated and it has also been
	<seealso marker="scheduled_for_removal#OTP-24">scheduled for removal</seealso>
	in OTP 24.
      </p>
    </section>
