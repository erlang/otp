{topcase, {dir, "../ssh_test"}}.
{require_nodenames, 1}.
{skip, {ssh_ssh_SUITE, ssh, "Current implementation is timingdependent and
hence will succeed/fail on a whim"}}.
{skip, {ssh_ssh_SUITE, ssh_compressed,
"Current implementation is timingdependent hence will succeed/fail on a whim"}}.
