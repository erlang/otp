
#include <assert.h>
#include <stdint.h>
#include <stddef.h>
#include <stdio.h>
#include <string.h>

/* Experimental 128-bit Eterm */
typedef __uintcap_t Eterm;

/* BEAM primary tags */
#define TAG_PRIMARY_MASK  0x3
#define TAG_PRIMARY_BOXED 0x2
#define TAG_IMMED_SMALL   0xF
#define TAG_PRIMARY_LIST 0x1
#define TAG_PTR_MASK    0x7

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

static Eterm make_list(int *ptr)
{
    Eterm cap = (Eterm)ptr;

    return __builtin_cheri_address_set(
        cap,
        __builtin_cheri_address_get(cap) + TAG_PRIMARY_LIST);
}

static int *list_val(Eterm term)
{
    return (int *)__builtin_cheri_address_set(
        term,
        __builtin_cheri_address_get(term) & ~(size_t)TAG_PTR_MASK);
}

static void test_list(void)
{
   _Alignas(16) int payload = 99;

    Eterm original = (Eterm)&payload;
    Eterm tagged = make_list(&payload);
    int *recovered = list_val(tagged);

    assert(__builtin_cheri_tag_get(tagged));
    assert(__builtin_cheri_tag_get(recovered));
    assert(recovered == &payload);
    assert(*recovered == 99);

    assert(__builtin_cheri_base_get(original) ==
           __builtin_cheri_base_get(recovered));

    assert(__builtin_cheri_length_get(original) ==
           __builtin_cheri_length_get(recovered));

    assert(__builtin_cheri_perms_get(original) ==
           __builtin_cheri_perms_get(recovered));

    printf("List pointer round-trip passed!\n");
}




/* Experiment 04: Literal pointer tagging */
#define TAG_LITERAL_PTR 0x4

static void test_literal(void)
{
    _Alignas(16) int payload = 123;

    Eterm boxed = make_boxed(&payload);

    Eterm literal = __builtin_cheri_address_set(
        boxed,
        __builtin_cheri_address_get(boxed) | TAG_LITERAL_PTR);

    int *recovered = (int *)__builtin_cheri_address_set(
        literal,
        __builtin_cheri_address_get(literal) & ~(size_t)0x7);

    assert(__builtin_cheri_tag_get(literal));
    assert(__builtin_cheri_tag_get(recovered));
    assert((__builtin_cheri_address_get(literal) & 0x7) == 0x6);
    assert(recovered == &payload);
    assert(*recovered == 123);

    printf("Literal pointer round-trip passed!\n");
}

/* Experiment 05: Negative small integer */
static void test_negative_small(void)
{
    int64_t value = -42;

    uint64_t bits = ((uint64_t)value << 4) | 0xF;
    Eterm term = (Eterm)bits;

    int64_t recovered = ((int64_t)(uint64_t)term) >> 4;

    assert(recovered == value);
    assert(!__builtin_cheri_tag_get(term));

    printf("Negative small integer passed!\n");
}


static void test_is_byte(void)
{
    #define TEST_IS_BYTE(x) \
        ((((uint64_t)(x)) & \
          ((~(uint64_t)0 << 12) + 0xF)) == 0xF)

    assert(TEST_IS_BYTE(make_small(0)));
    assert(TEST_IS_BYTE(make_small(255)));
    assert(!TEST_IS_BYTE(make_small(256)));
    assert(!TEST_IS_BYTE(make_small((uint64_t)-1)));

    printf("is_byte boundary tests passed!\n");

    #undef TEST_IS_BYTE
}

static void test_capability_memcpy(void)
{
    int payload = 42;

    Eterm source[2] = {
        (Eterm)&payload,
        make_small(99)
    };

    Eterm destination[2] = {0};

    memcpy(destination, source, sizeof(source));

    assert(__builtin_cheri_tag_get(destination[0]));
    assert((int *)destination[0] == &payload);
    assert(*(int *)destination[0] == 42);

    assert(!__builtin_cheri_tag_get(destination[1]));

    printf("Capability memcpy test passed!\n");
}


/* Experiment 07: Relocating capabilities */
static void test_capability_relocation(void)
{
    Eterm source[4] = {0};
    Eterm destination[4] = {0};

    source[2] = make_small(42);

    /* A capability pointing to source[2] */
    Eterm old_cap = (Eterm)&source[2];

    /* Copy the object's contents */
    memcpy(destination, source, sizeof(source));

    /* Numerical location of the copied element */
    size_t new_address =
        __builtin_cheri_address_get(&destination[2]);

    /* Approach A: Derive from source capability */
    Eterm wrong_cap = __builtin_cheri_address_set(
        old_cap, new_address);

    /* Approach B: Derive from destination capability */
    Eterm dest_cap = (Eterm)&destination[0];

    Eterm new_cap = __builtin_cheri_address_set(
        dest_cap, new_address);

    assert(__builtin_cheri_tag_get(new_cap));
    assert((Eterm *)new_cap == &destination[2]);
    assert(*(Eterm *)new_cap == make_small(42));

    printf("Source-derived tag: %d\n",
           __builtin_cheri_tag_get(wrong_cap));

    printf("Destination-derived tag: %d\n",
           __builtin_cheri_tag_get(new_cap));

    printf("Capability relocation test passed!\n");
    printf("Source base:  0x%zx, length: %zu\n",
           (size_t)__builtin_cheri_base_get(old_cap),
           (size_t)__builtin_cheri_length_get(old_cap));

    printf("Wrong base:   0x%zx, length: %zu\n",
           (size_t)__builtin_cheri_base_get(wrong_cap),
           (size_t)__builtin_cheri_length_get(wrong_cap));

    printf("Dest base:    0x%zx, length: %zu\n",
           (size_t)__builtin_cheri_base_get(dest_cap),
           (size_t)__builtin_cheri_length_get(dest_cap));

    size_t wrong_addr = __builtin_cheri_address_get(wrong_cap);
    size_t wrong_base = __builtin_cheri_base_get(wrong_cap);
    size_t wrong_len = __builtin_cheri_length_get(wrong_cap);

    printf("Wrong capability address in bounds: %d\n",
           wrong_addr >= wrong_base &&
           wrong_addr - wrong_base < wrong_len);
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
    test_list();
    test_literal();
    test_negative_small();
    test_is_byte();
    test_capability_memcpy();
    test_capability_relocation();
    return 0;
}
