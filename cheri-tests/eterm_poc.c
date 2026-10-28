
#include <assert.h>
#include <stdint.h>
#include <stddef.h>
#include <stdio.h>

/* Experimental 128-bit Eterm */
typedef __uintcap_t Eterm;

/* BEAM primary tags */
#define TAG_PRIMARY_MASK  0x3
#define TAG_PRIMARY_BOXED 0x2
#define TAG_IMMED_SMALL   0xF

/* Store a small integer directly */
static Eterm make_small(uint64_t value)
{
    uint64_t bits = (value << 4) | TAG_IMMED_SMALL;
    return (Eterm)bits;
}

/* Create a boxed term from an existing capability */
static Eterm make_boxed(int *ptr)
{
    Eterm cap = (Eterm)ptr;
    size_t address = __builtin_cheri_address_get(cap);

    assert((address & TAG_PRIMARY_MASK) == 0);

    return __builtin_cheri_address_set(
        cap, address + TAG_PRIMARY_BOXED);
}

/* Recover the original capability */
static int *boxed_val(Eterm term)
{
    size_t address = __builtin_cheri_address_get(term);

    assert((address & TAG_PRIMARY_MASK) == TAG_PRIMARY_BOXED);

    return (int *)__builtin_cheri_address_set(
        term, address - TAG_PRIMARY_BOXED);
}


/* Experiment 02: Float storage in a 128-bit Eterm */
typedef union {
    double fd;
    uint64_t fdw;
} FloatDef;

static void test_float(void)
{
    FloatDef original;
    FloatDef recovered;

    original.fd = 3.14;

    /* Simulate a two-slot BEAM float object */
    Eterm heap[2];

    heap[0] = (Eterm)0;             /* Placeholder header */
    heap[1] = (Eterm)original.fdw;  /* Store raw float bits */

    /* Recover the 64-bit bit pattern */
    recovered.fdw = (uint64_t)heap[1];

    printf("Original bits:  0x%016llx\n",
           (unsigned long long)original.fdw);
    printf("Recovered bits: 0x%016llx\n",
           (unsigned long long)recovered.fdw);
    printf("Recovered float: %.2f\n", recovered.fd);

    assert(original.fdw == recovered.fdw);
    assert(recovered.fd == 3.14);
    assert(!__builtin_cheri_tag_get(heap[1]));

    printf("Float round-trip passed!\n");
}


int main(void)
{
    int payload = 42;

    Eterm original = (Eterm)&payload;
    Eterm boxed = make_boxed(&payload);
    int *recovered = boxed_val(boxed);

    printf("sizeof(Eterm) = %zu\n", sizeof(Eterm));

    printf("Original tag valid: %d\n",
           __builtin_cheri_tag_get(original));

    printf("Boxed tag valid: %d\n",
           __builtin_cheri_tag_get(boxed));

    printf("Recovered tag valid: %d\n",
           __builtin_cheri_tag_get(recovered));

    assert(__builtin_cheri_tag_get(boxed));
    assert(__builtin_cheri_tag_get(recovered));

    assert(__builtin_cheri_base_get(original) ==
           __builtin_cheri_base_get(recovered));
    
    assert(__builtin_cheri_length_get(original) ==
           __builtin_cheri_length_get(recovered));

    assert(__builtin_cheri_perms_get(original) ==
           __builtin_cheri_perms_get(recovered));

    assert(recovered == &payload);
    assert(*recovered == 42);

    Eterm immediate = make_small(42);

    printf("Immediate capability valid: %d\n",
           __builtin_cheri_tag_get(immediate));

    assert(!__builtin_cheri_tag_get(immediate));
    assert((__builtin_cheri_address_get(immediate) & 0xF)
           == TAG_IMMED_SMALL);

    printf("All tests passed!\n");
    test_float();
    return 0;
}
