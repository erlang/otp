{suites,"../ssh_test",all}.
{skip_cases,"../ssh_test",ssh_ssh_SUITE,
            [ssh],
            "Current implementation is timingdependent and\nhence will succeed/fail on a whim"}.
{skip_cases,"../ssh_test",ssh_ssh_SUITE,
            [ssh_compressed],
            "Current implementation is timingdependent hence will succeed/fail on a whim"}.
