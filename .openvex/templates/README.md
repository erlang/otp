# OpenVEX Templates Directory

This directory contains the OpenVEX data for this repository.
The files stored in this directory are used as templates by
`vexctl generate` when generating VEX data for a release or
a specific artifact.

To add new statements to publish data about a vulnerability,
download [vexctl] and append new statements using
`vexctl add`. For example:

```
vexctl add --in-place main.openvex.json --product pkg:oci/test --vuln CVE-2014-1234567 --status under_investigation
```

That will add a new VEX statement expressing that the impact of
CVE-2014-1234567 is under investigation in the test image. When
cutting a new release, for `pkg:oci/test` the new file can be
incorporated to the release's VEX data.

## Read more about OpenVEX

To know more about generating, publishing and using VEX data
in your project, please check out the [vexctl repository and
documentation][vexctl].

OpenVEX also has an [examples repository] with samples and docs.


[vexctl]: https://github.com/openvex/vexctl
[examples repository]: https://github.com/openvex/examples
