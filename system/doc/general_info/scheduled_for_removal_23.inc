    <section>
      <title>VxWorks Support</title>
      <p>Some parts of OTP has had limited VxWorks support, such as
      <seealso marker="erl_interface:index"><c>erl_interface</c></seealso>.
      This support will be removed as of OTP 23. This limited support
      was formally deprecated as of OTP 22.</p>
    </section>
    <section>
      <title>Legacy parts of erl_interface</title>
      <p>The old legacy <seealso marker="erl_interface:index"><c>erl_interface</c></seealso>
      library (functions with prefix <c>erl_</c>) will be removed as of
      OTP 23. These parts of <c>erl_interface</c> has been informally deprecated
      for a very long time, and was formally deprecated in OTP 22. You typically
      want to replace the usage of the <c>erl_interface</c> library with the use
      of the <c>ei</c> library which also is part of the <c>erl_interface</c>
      application.</p>
    </section>

    <section>
      <title>httpd_conf module</title>
      <p>
	API functions in the module called <c>httpd_conf</c> was deprecated in favor
	of standard modules such as <c>lists</c>, <c>string</c>,
	<c>filelib</c>, and <c>erlang</c>. Formally deprecated as of OTP 18.
      </p>
    </section>

    <section>
      <title>inets - httpd Apache config files</title>
      <p>
        Support for the Apache-compatible config files will be removed in
	OTP 23. A new config file format was introduced in OTP 12.
      </p>
    </section>

     <section>
       <title>SSL/TLS</title>
       <p>For security reasons SSL-3.0 is no longer supported at all.</p>
     </section>
