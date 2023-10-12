    <section>
      <title>erlang:now/0</title>
      <p>New time functionality and a new time API was introduced. For more information
      see the <seeguide marker="erts:time_correction">Time and Time Correction</seeguide>
      chapter in the ERTS User's guide and specifically the
      <seeguide marker="erts:time_correction#Dos_and_Donts">Dos and Donts</seeguide>
      section on how to replace usage of <c>erlang:now/0</c>.</p>
    </section>

    <section>
      <title>httpd_conf module</title>
      <p>
	API functions in the module <c>httpd_conf</c> was deprecated in favor
	of standard modules such as <c>lists</c>, <c>string</c>,
	<c>filelib</c>, and <c>erlang</c>.
      </p>
    </section>
