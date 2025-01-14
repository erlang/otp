/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 1999-2021. All Rights Reserved.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 * %CopyrightEnd%
 */

/*
 * BIFs logically belonging to the lists module.
 */

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

#include "sys.h"
#include "erl_vm.h"
#include "global.h"
#include "bif.h"
#include "erl_binary.h"

static Eterm keyfind(Export* Bif, Process* p, Eterm Key, Eterm Pos, Eterm List);

/* erlang:'++'/2
 *
 * Adds a list to another (LHS ++ RHS). For historical reasons this is
 * implemented by copying LHS and setting its tail to RHS without checking
 * that RHS is a proper list. [] ++ 'not_a_list' will therefore result in
 * 'not_a_list', and [1,2] ++ 3 will result in [1,2|3], and this is a bug that
 * we have to live with. */

/**
 * @struct ErtsAppendContext
 * @brief Represents the context for appending operations.
 *
 * This structure is used to manage the state during an appending operation.
 * It contains the original left-hand side (lhs) and right-hand side (rhs),
 * as well as an iterator for traversing elements and the result-related
 * components.
 *
 * @var ErtsAppendContext::lhs_original
 * The original left-hand side value for the append operation.
 *
 * @var ErtsAppendContext::rhs_original
 * The original right-hand side value for the append operation.
 *
 * @var ErtsAppendContext::iterator
 * An iterator used during the append operation.
 *
 * @var ErtsAppendContext::result
 * The resulting value of the append operation.
 *
 * @var ErtsAppendContext::result_cdr
 * A pointer to the cdr (tail) of the result list.
 *
 */
typedef struct {
    Eterm lhs_original;
    Eterm rhs_original;

    Eterm iterator;

    Eterm result;
    Eterm *result_cdr;
} ErtsAppendContext;

static int append_ctx_bin_dtor(Binary *context_bin) {
    return 1;
}

/**
 * @brief Processes the left-hand side (lhs) of an append operation when the right-hand side (rhs) is empty.
 *
 * This function iterates over the elements of the list in the append context's iterator, consuming a budget
 * of reductions, and updates the append context accordingly. It handles cases where the iterator reaches the
 * end of the list or encounters an unexpected state.
 *
 * @param p A pointer to the `Process` structure representing the current process.
 * @param context A pointer to the `ErtsAppendContext` structure managing the state of the append operation.
 * @return
 * - `0` if the operation is incomplete and further iterations are needed.
 * - `1` if the append operation is completed successfully.
 * - `-1` if an unexpected state is encountered.
 *
 * @details
 * The function calculates a budget of reductions to process based on the remaining reductions for the process.
 * It iterates through the list until the budget is exhausted or the end of the list is reached.
 *
 * @complexity O(n), where `n` is the number of list elements processed within the given budget.
 */
static int append_empty_rhs(Process *p, ErtsAppendContext *context) {
    static const Sint ELEMENTS_PER_RED = 32;
    Sint budget, count;

    budget = ELEMENTS_PER_RED * ERTS_BIF_REDS_LEFT(p);

#ifdef DEBUG
    budget = budget / 10 + 1;
#endif

    for (count = 0; count < budget && is_list(context->iterator); count++) {
        context->iterator = CDR(list_val(context->iterator));
    }

    BUMP_REDS(p, count / ELEMENTS_PER_RED);

    if (is_list(context->iterator)) {
        return 0;
    } else if (is_nil(context->iterator)) {
#ifdef DEBUG
        context->result_cdr = &context->rhs_original;
#endif
        context->result = context->lhs_original;

        return 1;
    }

    return -1;
}

/**
 * @brief Creates a trap state for an append operation.
 *
 * This function allocates a new append context, copies the state from the source
 * context, and sets up a magic binary reference to manage the lifecycle of the
 * allocated context.
 *
 * @param p A pointer to the `Process` structure representing the current process.
 * @param from_context A pointer to the `ErtsAppendContext` structure from which
 *                     the state is copied.
 * @return An `Eterm` value representing the created magic reference.
 *
 * @details
 * - A new binary object is created to store the `ErtsAppendContext`, and a destructor
 *   function (`append_ctx_bin_dtor`) is associated with the binary.
 * - The context's `result_cdr` is adjusted to point to the new context's `result` field
 *   if it previously pointed to the source context's `result`.
 * - The function allocates heap space for a magic reference and returns the reference.
 *
 * @complexity O(1), as the function performs fixed-size allocations and operations.
 */
static Eterm append_create_trap_state(Process *p,
                                      ErtsAppendContext *from_context) {
    ErtsAppendContext *to_context;
    Binary *state_bin;
    Eterm *hp;

    state_bin = erts_create_magic_binary(sizeof(ErtsAppendContext),
                                         append_ctx_bin_dtor);

    to_context = ERTS_MAGIC_BIN_DATA(state_bin);
    *to_context = *from_context;

    if (from_context->result_cdr == &from_context->result) {
        to_context->result_cdr = &to_context->result;
    }

    hp = HAlloc(p, ERTS_MAGIC_REF_THING_SIZE);
    return erts_mk_magic_ref(&hp, &MSO(p), state_bin);
}

/**
 * @brief Allocates memory and appends elements during an append operation.
 *
 * This function manages memory allocation and appends elements from the iterator in the
 * append context to the result list, while consuming a budget of reductions. It also handles
 * terminating the result list properly in case of incomplete or invalid input.
 *
 * @param p A pointer to the `Process` structure representing the current process.
 * @param context A pointer to the `ErtsAppendContext` structure managing the state of the append operation.
 * @return
 * - `1` if the append operation completes successfully.
 * - `0` if the operation needs to trap for more memory or reductions.
 * - `-1` if an error occurs (e.g., an incomplete or invalid list is encountered).
 *
 * @details
 * - The function calculates the number of list cells it can process within a given reduction budget.
 * - It iterates over the input list (`context->iterator`), appending elements to the result list and allocating
 *   memory as needed.
 * - If the operation is incomplete, the function terminates the result list and returns `0` to indicate trapping.
 * - Handles special cases where the input list is invalid or the `rhs_original` is `NIL`.
 *
 * @complexity O(n), where `n` is the number of list elements processed during the given budget.
 */
static BIF_RETTYPE lists_append_alloc(Process *p, ErtsAppendContext *context) {
    static const Uint CELLS_PER_RED = 40;

    Eterm *alloc_top, *alloc_end;
    Uint cells_left, max_cells;
    Eterm lookahead;

    cells_left = max_cells = CELLS_PER_RED * ERTS_BIF_REDS_LEFT(p);
    lookahead = context->iterator;

#ifdef DEBUG
    cells_left = max_cells = max_cells / 10 + 1;
#endif

    while (cells_left != 0 && is_list(lookahead)) {
        lookahead = CDR(list_val(lookahead));
        cells_left--;
    }

    BUMP_REDS(p, (max_cells - cells_left) / CELLS_PER_RED);

    if (is_not_list(lookahead) && is_not_nil(lookahead)) {
        /* It's possible that we're erroring out with an incomplete list, so it
         * must be terminated or we'll leave a hole in the heap. */
        *context->result_cdr = NIL;
        return -1;
    }

    alloc_top = HAlloc(p, 2 * (max_cells - cells_left));
    alloc_end = alloc_top + 2 * (max_cells - cells_left);

    while (alloc_top < alloc_end) {
        Eterm *cell = list_val(context->iterator);

        ASSERT(context->iterator != lookahead);

        *context->result_cdr = make_list(alloc_top);
        context->result_cdr = &CDR(alloc_top);
        CAR(alloc_top) = CAR(cell);

        context->iterator = CDR(cell);
        alloc_top += 2;
    }

    if (is_list(context->iterator)) {
        /* The result only has to be terminated when returning it to the user,
         * but we're doing it when trapping as well to prevent headaches when
         * debugging. */
        *context->result_cdr = NIL;
        ASSERT(cells_left == 0);
        return 0;
    }

    *context->result_cdr = context->rhs_original;
    ASSERT(is_nil(context->iterator));

    if (is_nil(context->rhs_original)) {
        /* The list we created was equal to the original, so we'll return that
         * in the hopes that the garbage we created can be removed soon. */
        context->result = context->lhs_original;
    }

    return 1;
}

/**
 * @brief Performs an append operation entirely on the heap.
 *
 * This function appends elements from the iterator in the append context to the result list,
 * using the process's heap memory. It processes elements within the available reduction budget
 * and heap space, and handles continuation or completion of the append operation.
 *
 * @param p A pointer to the `Process` structure representing the current process.
 * @param context A pointer to the `ErtsAppendContext` structure managing the state of the append operation.
 * @return
 * - `1` if the append operation completes successfully.
 * - `0` if the operation needs to trap for more reductions or heap memory.
 * - `-1` if an error occurs (e.g., an invalid or incomplete list is encountered).
 *
 * @details
 * - The function calculates the number of list cells it can process based on the reduction budget
 *   and the available heap memory.
 * - It iterates over the input list (`context->iterator`), appending elements to the result list
 *   and updating heap pointers accordingly.
 * - If the append operation is incomplete, it either traps for more heap memory or ends the result
 *   list temporarily, depending on the situation.
 * - Special cases include handling invalid lists or when the `rhs_original` is `NIL`.
 *
 * @complexity O(n), where `n` is the number of list elements processed during the given budget.
 */
static BIF_RETTYPE lists_append_onheap(Process *p, ErtsAppendContext *context) {
    static const Uint CELLS_PER_RED = 60;

    Eterm *alloc_start, *alloc_top, *alloc_end;
    Uint cells_left, max_cells;

    cells_left = max_cells = CELLS_PER_RED * ERTS_BIF_REDS_LEFT(p);

#ifdef DEBUG
    cells_left = max_cells = max_cells / 10 + 1;
#endif

    ASSERT(HEAP_LIMIT(p) >= HEAP_TOP(p) + 2);
    alloc_start = HEAP_TOP(p);
    alloc_end = HEAP_LIMIT(p) - 2;
    alloc_top = alloc_start;

    /* Don't process more cells than we have reductions for. */
    alloc_end = MIN(alloc_top + (cells_left * 2), alloc_end);

    while (alloc_top < alloc_end && is_list(context->iterator)) {
        Eterm *cell = list_val(context->iterator);

        *context->result_cdr = make_list(alloc_top);
        context->result_cdr = &CDR(alloc_top);
        CAR(alloc_top) = CAR(cell);

        context->iterator = CDR(cell);
        alloc_top += 2;
    }

    cells_left -= (alloc_top - alloc_start) / 2;
    HEAP_TOP(p) = alloc_top;

    ASSERT(cells_left >= 0 && cells_left <= max_cells);
    BUMP_REDS(p, (max_cells - cells_left) / CELLS_PER_RED);

    if (is_not_list(context->iterator) && is_not_nil(context->iterator)) {
        *context->result_cdr = NIL;
        return -1;
    }

    if (is_list(context->iterator)) {
        if (cells_left > CELLS_PER_RED) {
            return lists_append_alloc(p, context);
        }

        *context->result_cdr = NIL;
        return 0;
    }

    *context->result_cdr = context->rhs_original;
    ASSERT(is_nil(context->iterator));

    if (is_nil(context->rhs_original)) {
        context->result = context->lhs_original;
    }

    return 1;
}

/**
 * @brief Continues an append operation based on the current context and process state.
 *
 * This function determines the appropriate method to proceed with the append operation
 * by evaluating the right-hand side (rhs) of the append context and the available heap
 * space in the process. It delegates the operation to specific functions for handling
 * empty rhs, on-heap processing, or heap allocation.
 *
 * @param p A pointer to the `Process` structure representing the current process.
 * @param context A pointer to the `ErtsAppendContext` structure managing the state of the append operation.
 * @return
 * - `1` if the append operation completes successfully.
 * - `0` if the operation traps for additional resources (reductions or heap space).
 * - `-1` if an error occurs during the operation.
 *
 * @details
 * - If the right-hand side (`rhs_original`) is `NIL`, the function uses `append_empty_rhs` to
 *   quickly return the left-hand side (`lhs_original`).
 * - If sufficient heap space is available, the function proceeds with `lists_append_onheap` to
 *   perform the operation directly on the heap.
 * - If heap space is insufficient, it falls back to `lists_append_alloc`, which allocates
 *   additional space to continue the operation.
 *
 * @complexity O(n), where `n` is the number of list elements processed based on the available
 * resources during the function call.
 */
static int append_continue(Process *p, ErtsAppendContext *context) {
    /* Fast-lane when the rhs is nil: return lhs. */
    if (is_nil(context->rhs_original)) {
        return append_empty_rhs(p, context);
    }

    /* We build the result on the unused part of the heap if possible to save
     * us the trouble of having to figure out the list size. We fall back to
     * lists_append_alloc when we run out of space. */
    if (HeapWordsLeft(p) > 8) {
        return lists_append_onheap(p, context);
    }

    return lists_append_alloc(p, context);
}

/**
 * @brief Initializes and starts an append operation.
 *
 * This function initializes the `ErtsAppendContext` structure with the provided
 * left-hand side (lhs) and right-hand side (rhs) of the append operation and
 * then begins the operation by calling `append_continue`.
 *
 * @param p A pointer to the `Process` structure representing the current process.
 * @param lhs The left-hand side term for the append operation.
 * @param rhs The right-hand side term for the append operation.
 * @param context A pointer to the `ErtsAppendContext` structure to be initialized and used.
 * @return
 * - `1` if the append operation completes successfully.
 * - `0` if the operation traps for additional resources (reductions or heap space).
 * - `-1` if an error occurs during the operation.
 *
 * @details
 * - The function sets the original lhs and rhs terms in the context.
 * - Initializes the result-related fields (`result` and `result_cdr`) and sets the iterator to the lhs.
 * - Delegates the operation to `append_continue` to proceed with the append logic.
 *
 * @complexity O(n), where `n` is the number of list elements processed during the append operation.
 */
static int append_start(Process *p, Eterm lhs, Eterm rhs,
                        ErtsAppendContext *context) {
    context->lhs_original = lhs;
    context->rhs_original = rhs;

    context->result_cdr = &context->result;
    context->result = NIL;

    context->iterator = lhs;

    return append_continue(p, context);
}

/* erlang:'++'/2 */
/**
 * @brief Implements the `++/2` operation for appending two terms in Erlang.
 *
 * This function handles the append operation for the `++/2` BIF (built-in function),
 * supporting cases where the left-hand side (lhs) is a list, a magic reference (trap state),
 * or invalid input. It manages the context for the append operation and traps as necessary
 * to handle large lists or resource limitations.
 *
 * @param bif_entry A pointer to the `Export` structure representing the BIF entry.
 * @param BIF_ALIST_2 Macro for the BIF's argument list (lhs and rhs).
 * @return The resulting term after appending lhs and rhs.
 *
 * @details
 * - **Case 1**: If `lhs` is `NIL` (empty list), the function returns `rhs` directly.
 * - **Case 2**: If `lhs` is a list:
 *   - Initializes an `ErtsAppendContext` and attempts to append in a single operation.
 *   - If trapping is required, creates a trap state and defers execution.
 *   - Handles errors gracefully, such as invalid lists.
 * - **Case 3**: If `lhs` is an internal magic reference (trap state):
 *   - Resumes the append operation from the trap state.
 *   - Validates the magic reference to ensure it corresponds to an append operation.
 *   - Restores the garbage collection state after completing the operation.
 * - **Invalid Input**: If `lhs` is not a list, `NIL`, or a valid magic reference, the
 *   function raises a `BADARG` error.
 *
 * @complexity O(n), where `n` is the number of elements in `lhs` that need to be processed.
 */
static Eterm append(Export *bif_entry, BIF_ALIST_2) {
    Eterm lhs = BIF_ARG_1, rhs = BIF_ARG_2;

    if (is_nil(lhs)) {
        /* This is buggy but expected, `[] ++ 'not_a_list'` has always resulted
         * in 'not_a_list'. */
        return rhs;
    } else if (is_list(lhs)) {
        /* We start with the context on the stack in the hopes that we won't
         * have to trap. */
        ErtsAppendContext context;
        int res;

        res = append_start(BIF_P, lhs, rhs, &context);

        if (res == 0) {
            Eterm state_mref;

            state_mref = append_create_trap_state(BIF_P, &context);
            erts_set_gc_state(BIF_P, 0);

            BIF_TRAP2(bif_entry, BIF_P, state_mref, NIL);
        }

        if (res < 0) {
            ASSERT(is_nil(*context.result_cdr));
            BIF_ERROR(BIF_P, BADARG);
        }

        ASSERT(*context.result_cdr == context.rhs_original);
        BIF_RET(context.result);
    } else if (is_internal_magic_ref(lhs)) {
        ErtsAppendContext *context;
        int (*dtor)(Binary*);
        Binary *magic_bin;

        int res;

        magic_bin = erts_magic_ref2bin(lhs);
        dtor = ERTS_MAGIC_BIN_DESTRUCTOR(magic_bin);

        if (dtor != append_ctx_bin_dtor) {
            BIF_ERROR(BIF_P, BADARG);
        }

        ASSERT(BIF_P->flags & F_DISABLE_GC);
        ASSERT(rhs == NIL);

        context = ERTS_MAGIC_BIN_DATA(magic_bin);
        res = append_continue(BIF_P, context);

        if (res == 0) {
            BIF_TRAP2(bif_entry, BIF_P, lhs, NIL);
        }

        erts_set_gc_state(BIF_P, 1);

        if (res < 0) {
            ASSERT(is_nil(*context->result_cdr));
            ERTS_BIF_ERROR_TRAPPED2(BIF_P, BADARG, bif_entry,
                                    context->lhs_original,
                                    context->rhs_original);
        }

        ASSERT(*context->result_cdr == context->rhs_original);
        BIF_RET(context->result);
    }

    ASSERT(!(BIF_P->flags & F_DISABLE_GC));

    BIF_ERROR(BIF_P, BADARG);
}

/*
 * erlang:'++'/2
 */

Eterm
/**
 * @brief Entry point for the `++/2` BIF operation.
 *
 * This function acts as a wrapper around the `append` function to implement
 * the `++/2` append operation in Erlang. It prepares the `append` function
 * call by passing the appropriate BIF entry and argument list.
 *
 * @param BIF_ALIST_2 Macro for the BIF's argument list (lhs and rhs).
 * @return The resulting term after appending lhs and rhs, or traps if further
 *         processing is required.
 *
 * @details
 * - The function delegates the core logic to the `append` function, passing
 *   the `BIF_TRAP_EXPORT` of `ebif_plusplus_2` as the entry point and
 *   `BIF_CALL_ARGS` as the arguments.
 * - Ensures consistent handling of trapping, resource management, and errors
 *   by reusing the `append` implementation.
 *
 * @complexity O(n), where `n` is the number of elements in the list(s) being processed.
 */
ebif_plusplus_2(BIF_ALIST_2)
{
    return append(BIF_TRAP_EXPORT(BIF_ebif_plusplus_2), BIF_CALL_ARGS);
}

/**
 * @brief Entry point for the `append/2` BIF operation.
 *
 * This function serves as a wrapper for the `append` function to implement the
 * `append/2` operation in Erlang. It initializes the `append` function call with the
 * appropriate BIF entry and argument list.
 *
 * @param BIF_ALIST_2 Macro for the BIF's argument list (lhs and rhs).
 * @return The resulting term after appending lhs and rhs, or traps if additional
 *         processing is required.
 *
 * @details
 * - The function delegates the actual append logic to the `append` function, passing
 *   `BIF_TRAP_EXPORT(BIF_append_2)` as the entry point and `BIF_CALL_ARGS` as the arguments.
 * - This wrapper ensures that the `append` function handles all logic related to trapping,
 *   resource management, and error handling consistently.
 *
 * @complexity O(n), where `n` is the number of elements in the list(s) being processed.
 */
BIF_RETTYPE append_2(BIF_ALIST_2)
{
    return append(BIF_TRAP_EXPORT(BIF_append_2), BIF_CALL_ARGS);
}

/* erlang:'--'/2
 *
 * Subtracts a list from another (LHS -- RHS), removing the first occurrence of
 * each element in LHS from RHS. There is no type coercion so the elements must
 * match exactly.
 *
 * The BIF is broken into several stages that can all trap individually, and it
 * chooses its algorithm based on input size. If either input is small it will
 * use a linear scan tuned to which side it's on, and if both inputs are large
 * enough it will convert RHS into a multiset to provide good asymptotic
 * behavior. */

#define SUBTRACT_LHS_THRESHOLD 16
#define SUBTRACT_RHS_THRESHOLD 16

/**
 * @enum ErtsSubtractCtxStage
 * @brief Represents the stages in a subtract operation context.
 *
 * This enumeration defines the various stages involved in subtracting elements
 * between two lists or sets, optimizing for different scenarios based on the size
 * of the input and operation type.
 *
 * @var SUBTRACT_STAGE_START
 * Marks the beginning of the subtract operation.
 *
 * @var SUBTRACT_STAGE_LEN_LHS
 * Calculates the length of the left-hand side (LHS) list.
 *
 * @var SUBTRACT_STAGE_NAIVE_LHS
 * Performs a naive linear scan of the LHS list, optimized for cases where the
 * length of the LHS is less than or equal to a predefined threshold.
 *
 * @var SUBTRACT_STAGE_LEN_RHS
 * Calculates the length of the right-hand side (RHS) list.
 *
 * @var SUBTRACT_STAGE_NAIVE_RHS
 * Performs a naive linear scan of the RHS list, similar to the LHS scan.
 *
 * @var SUBTRACT_STAGE_SET_BUILD
 * Builds a multiset from the RHS list to enable faster lookups. The multiset
 * is implemented as a red-black tree, with duplicate elements managed using
 * counters in each node.
 *
 * @var SUBTRACT_STAGE_SET_FINISH
 * Finalizes the subtract operation, ensuring the results are properly prepared
 * after the multiset-based scanning.
 *
 */
typedef enum {
    SUBTRACT_STAGE_START,
    SUBTRACT_STAGE_LEN_LHS,

    /* Naive linear scan that's efficient when
     * LEN_LHS <= SUBTRACT_LHS_THRESHOLD. */
    SUBTRACT_STAGE_NAIVE_LHS,

    SUBTRACT_STAGE_LEN_RHS,

    /* As SUBTRACT_STAGE_NAIVE_LHS but for RHS. */
    SUBTRACT_STAGE_NAIVE_RHS,

    /* Creates a multiset from RHS for faster lookups before sweeping through
     * LHS. The set is implemented as a red-black tree and duplicate elements
     * are handled by a counter on each node. */
    SUBTRACT_STAGE_SET_BUILD,
    SUBTRACT_STAGE_SET_FINISH
} ErtsSubtractCtxStage;

/**
 * @struct subtract_tree_t
 * @brief Represents a node in a red-black tree used for subtraction operations.
 *
 * This structure defines the components of a node in a red-black tree, which is
 * used to manage a multiset for efficient subtraction operations. Each node
 * contains a key, a count for duplicate elements, and pointers to its parent
 * and child nodes.
 *
 * @var subtract_tree_t::parent
 * A pointer to the parent node in the tree.
 *
 * @var subtract_tree_t::left
 * A pointer to the left child node.
 *
 * @var subtract_tree_t::right
 * A pointer to the right child node.
 *
 * @var subtract_tree_t::is_red
 * A flag indicating whether the node is red (`1`) or black (`0`), as per red-black
 * tree properties.
 *
 * @var subtract_tree_t::key
 * The key value stored in the node, representing an element in the multiset.
 *
 * @var subtract_tree_t::count
 * The count of duplicate elements for the given key.
 *
 */
typedef struct subtract_node__ {
    struct subtract_node__ *parent;
    struct subtract_node__ *left;
    struct subtract_node__ *right;
    int is_red;

    Eterm key;
    Uint count;
} subtract_tree_t;

/**
 * @struct ErtsSubtractContext
 * @brief Represents the context for a subtract operation.
 *
 * This structure manages the state of a subtract operation, including the
 * current stage, the original input terms, and intermediate results. It also
 * supports both naive and optimized approaches, such as using a red-black
 * tree for faster lookups.
 *
 * @var ErtsSubtractContext::stage
 * The current stage of the subtract operation, as defined by `ErtsSubtractCtxStage`.
 *
 * @var ErtsSubtractContext::lhs_original
 * The original left-hand side (LHS) term of the subtract operation.
 *
 * @var ErtsSubtractContext::rhs_original
 * The original right-hand side (RHS) term of the subtract operation.
 *
 * @var ErtsSubtractContext::lhs_remaining
 * The remaining number of elements in the LHS to be processed.
 *
 * @var ErtsSubtractContext::rhs_remaining
 * The remaining number of elements in the RHS to be processed.
 *
 * @var ErtsSubtractContext::iterator
 * An iterator used to traverse the elements in the LHS or RHS.
 *
 * @var ErtsSubtractContext::result_cdr
 * A pointer to the tail of the result list being constructed.
 *
 * @var ErtsSubtractContext::result
 * The result of the subtract operation, typically a list.
 *
 * @var ErtsSubtractContext::u
 * A union containing structures or arrays for managing intermediate data:
 * - `lhs_elements`: Array to store LHS elements, used during naive subtraction.
 * - `rhs_elements`: Array to store RHS elements, used during naive subtraction.
 * - `rhs_set`: Structure for managing an optimized RHS multiset:
 *   - `tree`: A pointer to the root node of a red-black tree used for fast lookups.
 *   - `alloc_start`: A memory area for pre-allocated tree nodes.
 *   - `alloc`: A pointer to the next available node in the pre-allocated memory.
 *
 */
typedef struct {
    ErtsSubtractCtxStage stage;

    Eterm lhs_original;
    Eterm rhs_original;

    Uint lhs_remaining;
    Uint rhs_remaining;

    Eterm iterator;

    Eterm *result_cdr;
    Eterm result;

    union {
        Eterm lhs_elements[SUBTRACT_LHS_THRESHOLD];
        Eterm rhs_elements[SUBTRACT_RHS_THRESHOLD];

        struct {
            subtract_tree_t *tree;

            /* A memory area for the tree's nodes, saving us the need to have
             * one allocation per node. */
            subtract_tree_t *alloc_start;
            subtract_tree_t *alloc;
        } rhs_set;
    } u;
} ErtsSubtractContext;

#define ERTS_RBT_PREFIX subtract
#define ERTS_RBT_T subtract_tree_t
#define ERTS_RBT_KEY_T Eterm
#define ERTS_RBT_FLAGS_T int
#define ERTS_RBT_INIT_EMPTY_TNODE(T) \
    do { \
        (T)->parent = NULL; \
        (T)->left = NULL; \
        (T)->right = NULL; \
    } while(0)
#define ERTS_RBT_IS_RED(T) ((T)->is_red)
#define ERTS_RBT_SET_RED(T) ((T)->is_red = 1)
#define ERTS_RBT_IS_BLACK(T) (!ERTS_RBT_IS_RED(T))
#define ERTS_RBT_SET_BLACK(T) ((T)->is_red = 0)
#define ERTS_RBT_GET_FLAGS(T) ((T)->is_red)
#define ERTS_RBT_SET_FLAGS(T, F) ((T)->is_red = F)
#define ERTS_RBT_GET_PARENT(T) ((T)->parent)
#define ERTS_RBT_SET_PARENT(T, P) ((T)->parent = P)
#define ERTS_RBT_GET_RIGHT(T) ((T)->right)
#define ERTS_RBT_SET_RIGHT(T, R) ((T)->right = (R))
#define ERTS_RBT_GET_LEFT(T) ((T)->left)
#define ERTS_RBT_SET_LEFT(T, L) ((T)->left = (L))
#define ERTS_RBT_GET_KEY(T) ((T)->key)
#define ERTS_RBT_CMP_KEYS(KX, KY) subtract_term_cmp((KX), (KY))
#define ERTS_RBT_WANT_LOOKUP_INSERT
#define ERTS_RBT_WANT_LOOKUP
#define ERTS_RBT_WANT_DELETE
#define ERTS_RBT_UNDEF

/* erl_rbtree expects comparisons to return an int */
/**
 * @brief Compares two terms for ordering.
 *
 * This function compares two `Eterm` values and determines their relative order,
 * returning a result consistent with standard comparison functions.
 *
 * @param a The first `Eterm` value to compare.
 * @param b The second `Eterm` value to compare.
 * @return
 * - `-1` if `a` is less than `b`.
 * - `1` if `a` is greater than `b`.
 * - `0` if `a` is equal to `b`.
 *
 * @details
 * The comparison is performed using `CMP_TERM`, which evaluates the relative
 * order of two Erlang terms. This function ensures that the result conforms to
 * the expected semantics of a comparison operation.
 *
 * @complexity O(1) for primitive types, but may vary for complex terms based
 * on their structure and size.
 */
static int subtract_term_cmp(Eterm a, Eterm b) {
    Sint res = CMP_TERM(a, b);

    if (res < 0) {
        return -1;
    } else if (res > 0) {
        return 1;
    }

    return 0;
}

#include "erl_rbtree.h"

static int subtract_continue(Process *p, ErtsSubtractContext *context);

/**
 * @brief Destructor for an `ErtsSubtractContext`.
 *
 * This function releases resources associated with an `ErtsSubtractContext` when it is no longer needed.
 * It handles cleanup specific to the current stage of the subtract operation.
 *
 * @param context A pointer to the `ErtsSubtractContext` structure to be cleaned up.
 *
 * @details
 * - If the context is in `SUBTRACT_STAGE_SET_BUILD` or `SUBTRACT_STAGE_SET_FINISH`,
 *   memory allocated for the RHS red-black tree nodes is freed.
 * - For other stages, no additional cleanup is necessary.
 *
 * @complexity O(1), as it performs a single deallocation operation if applicable.
 */
static void subtract_ctx_dtor(ErtsSubtractContext *context) {
    switch (context->stage) {
        case SUBTRACT_STAGE_SET_BUILD:
        case SUBTRACT_STAGE_SET_FINISH:
            erts_free(ERTS_ALC_T_LIST_TRAP, context->u.rhs_set.alloc_start);
            break;
        default:
            break;
    }
}

/**
 * @brief Destructor for a binary holding an `ErtsSubtractContext`.
 *
 * This function releases resources associated with an `ErtsSubtractContext` stored in a magic binary.
 * It calls `subtract_ctx_dtor` to handle stage-specific cleanup.
 *
 * @param context_bin A pointer to the `Binary` structure containing the `ErtsSubtractContext`.
 * @return Always returns `1` to indicate successful destruction.
 *
 * @details
 * - The function retrieves the `ErtsSubtractContext` from the binary's data using `ERTS_MAGIC_BIN_DATA`.
 * - It invokes `subtract_ctx_dtor` to perform any required cleanup for the context.
 *
 * @complexity O(1), as it performs a fixed amount of work regardless of context size.
 */
static int subtract_ctx_bin_dtor(Binary *context_bin) {
    ErtsSubtractContext *context = ERTS_MAGIC_BIN_DATA(context_bin);
    subtract_ctx_dtor(context);
    return 1;
}

/**
 * @brief Moves an `ErtsSubtractContext` from one location to another.
 *
 * This function transfers the state of an `ErtsSubtractContext` from one
 * instance (`from`) to another (`to`). It ensures that the context is correctly
 * copied, including handling stage-specific data and maintaining pointer integrity.
 *
 * @param from A pointer to the source `ErtsSubtractContext` to be moved.
 * @param to A pointer to the destination `ErtsSubtractContext` where the state
 *           will be transferred.
 *
 * @details
 * - Copies all primary fields (`stage`, `lhs_original`, `rhs_original`, etc.) from `from` to `to`.
 * - Handles stage-specific data:
 *   - For `SUBTRACT_STAGE_NAIVE_LHS` and `SUBTRACT_STAGE_NAIVE_RHS`, it copies the `lhs_elements`
 *     or `rhs_elements` arrays, respectively.
 *   - For `SUBTRACT_STAGE_SET_BUILD` and `SUBTRACT_STAGE_SET_FINISH`, it transfers ownership of
 *     the red-black tree (`rhs_set`) memory areas, including `alloc_start`, `alloc`, and `tree`.
 * - Adjusts the `result_cdr` pointer to ensure it points to the correct location in the new context.
 * - Retains special handling for stages that utilize the `result_cdr` field.
 *
 * @complexity O(n), where `n` is the size of the stage-specific data being copied (e.g., number of elements
 * in `lhs_elements` or `rhs_elements`).
 */
static void subtract_ctx_move(ErtsSubtractContext *from,
                              ErtsSubtractContext *to) {
    int uses_result_cdr = 0;

    to->stage = from->stage;

    to->lhs_original = from->lhs_original;
    to->rhs_original = from->rhs_original;

    to->lhs_remaining = from->lhs_remaining;
    to->rhs_remaining = from->rhs_remaining;

    to->iterator = from->iterator;
    to->result = from->result;

    switch (to->stage) {
        case SUBTRACT_STAGE_NAIVE_LHS:
            sys_memcpy(to->u.lhs_elements,
                       from->u.lhs_elements,
                       sizeof(Eterm) * to->lhs_remaining);
            break;
        case SUBTRACT_STAGE_NAIVE_RHS:
            sys_memcpy(to->u.rhs_elements,
                       from->u.rhs_elements,
                       sizeof(Eterm) * to->rhs_remaining);

            uses_result_cdr = 1;
            break;
        case SUBTRACT_STAGE_SET_FINISH:
            uses_result_cdr = 1;
            /* FALL THROUGH */
        case SUBTRACT_STAGE_SET_BUILD:
            to->u.rhs_set.alloc_start = from->u.rhs_set.alloc_start;
            to->u.rhs_set.alloc = from->u.rhs_set.alloc;
            to->u.rhs_set.tree = from->u.rhs_set.tree;
            break;
        default:
            break;
    }

    if (uses_result_cdr) {
        if (from->result_cdr == &from->result) {
            to->result_cdr = &to->result;
        } else {
            to->result_cdr = from->result_cdr;
        }
    }
}

/**
 * @brief Creates a trap state for a subtract operation.
 *
 * This function allocates a new magic binary to store an `ErtsSubtractContext`,
 * moves the current context into the binary, and returns a magic reference
 * representing the state.
 *
 * @param p A pointer to the `Process` structure representing the current process.
 * @param context A pointer to the `ErtsSubtractContext` structure to be saved
 *                as a trap state.
 * @return An `Eterm` representing the created magic reference.
 *
 * @details
 * - Allocates a magic binary sized to hold an `ErtsSubtractContext`, using
 *   `subtract_ctx_bin_dtor` as its destructor.
 * - Transfers the state of `context` into the newly allocated magic binary
 *   using `subtract_ctx_move`.
 * - Allocates heap space for the magic reference and returns it.
 * - The resulting trap state can be used to resume the subtract operation later.
 *
 * @complexity O(n), where `n` depends on the size of the context data being moved.
 */static Eterm subtract_create_trap_state(Process *p,
                                        ErtsSubtractContext *context) {
    Binary *state_bin;
    Eterm *hp;

    state_bin = erts_create_magic_binary(sizeof(ErtsSubtractContext),
                                         subtract_ctx_bin_dtor);

    subtract_ctx_move(context, ERTS_MAGIC_BIN_DATA(state_bin));

    hp = HAlloc(p, ERTS_MAGIC_REF_THING_SIZE);

    return erts_mk_magic_ref(&hp, &MSO(p), state_bin);
}

/**
 * @brief Initializes the LHS length calculation stage in a subtract operation.
 *
 * This function sets up the `ErtsSubtractContext` to enter the `SUBTRACT_STAGE_LEN_LHS` stage,
 * where the length of the left-hand side (LHS) list is calculated. It prepares the iterator
 * and resets the count of remaining LHS elements before continuing the operation.
 *
 * @param p A pointer to the `Process` structure representing the current process.
 * @param context A pointer to the `ErtsSubtractContext` structure managing the state of the subtract operation.
 * @return
 * - `1` if the operation completes successfully.
 * - `0` if the operation traps for more reductions or resources.
 * - `-1` if an error occurs during the operation.
 *
 * @details
 * - Sets the `stage` to `SUBTRACT_STAGE_LEN_LHS`.
 * - Initializes the `iterator` to traverse the LHS list starting from `lhs_original`.
 * - Resets `lhs_remaining` to `0` to begin counting elements in the LHS.
 * - Calls `subtract_continue` to proceed with the operation.
 *
 * @complexity O(n), where `n` is the number of elements in the LHS list.
 */
static int subtract_enter_len_lhs(Process *p, ErtsSubtractContext *context) {
    context->stage = SUBTRACT_STAGE_LEN_LHS;

    context->iterator = context->lhs_original;
    context->lhs_remaining = 0;

    return subtract_continue(p, context);
}

/**
 * @brief Initializes the RHS length calculation stage in a subtract operation.
 *
 * This function sets up the `ErtsSubtractContext` to enter the `SUBTRACT_STAGE_LEN_RHS` stage,
 * where the length of the right-hand side (RHS) list is calculated. It prepares the iterator
 * and resets the count of remaining RHS elements before continuing the operation.
 *
 * @param p A pointer to the `Process` structure representing the current process.
 * @param context A pointer to the `ErtsSubtractContext` structure managing the state of the subtract operation.
 * @return
 * - `1` if the operation completes successfully.
 * - `0` if the operation traps for additional reductions or resources.
 * - `-1` if an error occurs during the operation.
 *
 * @details
 * - Sets the `stage` to `SUBTRACT_STAGE_LEN_RHS`.
 * - Initializes the `iterator` to traverse the RHS list starting from `rhs_original`.
 * - Resets `rhs_remaining` to `0` to begin counting elements in the RHS.
 * - Calls `subtract_continue` to proceed with the operation.
 *
 * @complexity O(n), where `n` is the number of elements in the RHS list.
 */
static int subtract_enter_len_rhs(Process *p, ErtsSubtractContext *context) {
    context->stage = SUBTRACT_STAGE_LEN_RHS;

    context->iterator = context->rhs_original;
    context->rhs_remaining = 0;

    return subtract_continue(p, context);
}

/**
 * @brief Computes the length of a list in a subtract operation, with a reduction budget.
 *
 * This function traverses a list to calculate its length, updating the iterator and count as it progresses.
 * It respects a reduction budget to ensure that the process can trap and resume if necessary.
 *
 * @param p A pointer to the `Process` structure representing the current process.
 * @param iterator_p A pointer to the current iterator, which is updated as the list is traversed.
 * @param count_p A pointer to the counter, which is incremented by the number of elements traversed.
 * @return
 * - `1` if the list traversal completes and the full length is determined.
 * - `0` if the traversal is incomplete and requires additional reductions.
 * - `-1` if an error occurs, such as encountering a non-list term.
 *
 * @details
 * - The function uses a budget of reductions (`ELEMENTS_PER_RED * ERTS_BIF_REDS_LEFT`) to limit the number of list elements processed in one invocation.
 * - Updates the `iterator_p` to point to the remaining unprocessed portion of the list.
 * - Increments `count_p` by the number of elements processed during the current invocation.
 * - Validates that the input is a proper list or `NIL`. If an improper list or non-list term is encountered, the function returns `-1`.
 * - The process's reduction count is adjusted based on the number of elements processed.
 *
 * @complexity O(n), where `n` is the number of list elements processed within the given budget.
 */
static int subtract_get_length(Process *p, Eterm *iterator_p, Uint *count_p) {
    static const Sint ELEMENTS_PER_RED = 32;

    Sint budget, count;
    Eterm iterator;

    budget = ELEMENTS_PER_RED * ERTS_BIF_REDS_LEFT(p);
    iterator = *iterator_p;

#ifdef DEBUG
    budget = budget / 10 + 1;
#endif

    for (count = 0; count < budget && is_list(iterator); count++) {
        iterator = CDR(list_val(iterator));
    }

    if (!is_list(iterator) && !is_nil(iterator)) {
        return -1;
    }

    BUMP_REDS(p, count / ELEMENTS_PER_RED);

    *iterator_p = iterator;
    *count_p += count;

    if (is_nil(iterator)) {
        return 1;
    }

    return 0;
}

/**
 * @brief Initializes the naive LHS subtraction stage.
 *
 * This function sets up the `ErtsSubtractContext` for the `SUBTRACT_STAGE_NAIVE_LHS` stage,
 * where the left-hand side (LHS) list is processed using a naive linear approach.
 * It extracts elements from the LHS list into an array for efficient processing.
 *
 * @param p A pointer to the `Process` structure representing the current process.
 * @param context A pointer to the `ErtsSubtractContext` structure managing the state of the subtract operation.
 * @return
 * - `1` if the operation completes successfully.
 * - `0` if the operation traps for additional reductions or resources.
 * - `-1` if an error occurs during the operation.
 *
 * @details
 * - Sets the `stage` to `SUBTRACT_STAGE_NAIVE_LHS`.
 * - Initializes the `iterator` to the RHS list and resets the result to `NIL`.
 * - Extracts elements from the LHS list (`lhs_original`) into the `lhs_elements` array in the context's union.
 * - Ensures that the number of elements extracted matches the previously calculated `lhs_remaining`.
 * - Calls `subtract_continue` to proceed with the operation.
 *
 * @complexity O(n), where `n` is the number of elements in the LHS list.
 */
static int subtract_enter_naive_lhs(Process *p, ErtsSubtractContext *context) {
    Eterm iterator;
    int i = 0;

    context->stage = SUBTRACT_STAGE_NAIVE_LHS;

    context->iterator = context->rhs_original;
    context->result = NIL;

    iterator = context->lhs_original;

    while (is_list(iterator)) {
        const Eterm *cell = list_val(iterator);

        ASSERT(i < SUBTRACT_LHS_THRESHOLD);

        context->u.lhs_elements[i++] = CAR(cell);
        iterator = CDR(cell);
    }

    ASSERT(i == context->lhs_remaining);

    return subtract_continue(p, context);
}

/**
 * @brief Performs naive subtraction on the LHS list.
 *
 * This function processes the left-hand side (LHS) list to remove elements that
 * match those in the right-hand side (RHS) list, using a naive linear search approach.
 * It preserves the order of remaining elements in the LHS while handling errors and
 * maintaining reduction limits.
 *
 * @param p A pointer to the `Process` structure representing the current process.
 * @param context A pointer to the `ErtsSubtractContext` structure managing the state of the subtract operation.
 * @return
 * - `1` if the operation completes successfully, with the result stored in `context->result`.
 * - `0` if the operation traps for additional reductions or resources.
 * - `-1` if an error occurs (e.g., invalid list encountered).
 *
 * @details
 * - Iterates over the RHS list (`context->iterator`), checking each element against the `lhs_elements` array.
 * - If a match is found, the matching element is removed from the `lhs_elements` array, preserving order.
 * - Continues processing within a reduction budget (`CHECKS_PER_RED`).
 * - Handles incomplete lists by trapping and resuming until the entire RHS is processed.
 * - After processing, any remaining elements in `lhs_elements` are reconstructed into a new result list.
 *
 * @complexity O(m * n), where `m` is the number of elements in the RHS list and `n` is the size of `lhs_elements`.
 */
static int subtract_naive_lhs(Process *p, ErtsSubtractContext *context) {
    const Sint CHECKS_PER_RED = 16;
    Sint checks, budget;

    budget = CHECKS_PER_RED * ERTS_BIF_REDS_LEFT(p);
    checks = 0;

    while (checks < budget && is_list(context->iterator)) {
        const Eterm *cell;
        Eterm value, next;
        int found_at;

        cell = list_val(context->iterator);

        value = CAR(cell);
        next = CDR(cell);

        for (found_at = 0; found_at < context->lhs_remaining; found_at++) {
            if (EQ(value, context->u.lhs_elements[found_at])) {
                /* We shift the array one step down as we have to preserve
                 * order.
                 *
                 * Note that we can't exit early as that would suppress errors
                 * in the right-hand side (this runs prior to determining the
                 * length of RHS). */

                context->lhs_remaining--;
                sys_memmove(&context->u.lhs_elements[found_at],
                            &context->u.lhs_elements[found_at + 1],
                            (context->lhs_remaining - found_at) * sizeof(Eterm));
                break;
            }
        }

        checks += MAX(1, context->lhs_remaining);
        context->iterator = next;
    }

    BUMP_REDS(p, MIN(checks, budget) / CHECKS_PER_RED);

    if (is_list(context->iterator)) {
        return 0;
    } else if (!is_nil(context->iterator)) {
        return -1;
    }

    if (context->lhs_remaining > 0) {
        Eterm *hp;
        int i;

        hp = HAlloc(p, context->lhs_remaining * 2);

        for (i = context->lhs_remaining - 1; i >= 0; i--) {
            Eterm value = context->u.lhs_elements[i];

            context->result = CONS(hp, value, context->result);
            hp += 2;
        }
    }

    ASSERT(context->lhs_remaining > 0 || context->result == NIL);

    return 1;
}

/**
 * @brief Initializes the naive RHS subtraction stage.
 *
 * This function sets up the `ErtsSubtractContext` for the `SUBTRACT_STAGE_NAIVE_RHS` stage,
 * where the right-hand side (RHS) list is processed using a naive linear approach.
 * It extracts elements from the RHS list into an array for efficient lookup during subtraction.
 *
 * @param p A pointer to the `Process` structure representing the current process.
 * @param context A pointer to the `ErtsSubtractContext` structure managing the state of the subtract operation.
 * @return
 * - `1` if the operation completes successfully.
 * - `0` if the operation traps for additional reductions or resources.
 * - `-1` if an error occurs during the operation.
 *
 * @details
 * - Sets the `stage` to `SUBTRACT_STAGE_NAIVE_RHS`.
 * - Initializes the `iterator` to the LHS list and sets the result pointers (`result` and `result_cdr`).
 * - Extracts elements from the RHS list (`rhs_original`) into the `rhs_elements` array in the context's union.
 * - Ensures that the number of elements extracted matches the previously calculated `rhs_remaining`.
 * - Calls `subtract_continue` to proceed with the operation.
 *
 * @complexity O(n), where `n` is the number of elements in the RHS list.
 */
static int subtract_enter_naive_rhs(Process *p, ErtsSubtractContext *context) {
    Eterm iterator;
    int i = 0;

    context->stage = SUBTRACT_STAGE_NAIVE_RHS;

    context->iterator = context->lhs_original;
    context->result_cdr = &context->result;
    context->result = NIL;

    iterator = context->rhs_original;

    while (is_list(iterator)) {
        const Eterm *cell = list_val(iterator);

        ASSERT(i < SUBTRACT_RHS_THRESHOLD);

        context->u.rhs_elements[i++] = CAR(cell);
        iterator = CDR(cell);
    }

    ASSERT(i == context->rhs_remaining);

    return subtract_continue(p, context);
}

/**
 * @brief Performs naive subtraction on the RHS list.
 *
 * This function processes the left-hand side (LHS) list to construct a result
 * list that excludes elements found in the right-hand side (RHS) list. It uses
 * a naive linear search approach, preserving the order of unmatched elements
 * in the result.
 *
 * @param p A pointer to the `Process` structure representing the current process.
 * @param context A pointer to the `ErtsSubtractContext` structure managing the state of the subtract operation.
 * @return
 * - `1` if the operation completes successfully, with the result stored in `context->result`.
 * - `0` if the operation traps for additional reductions or resources.
 * - `-1` if an error occurs (e.g., invalid list encountered).
 *
 * @details
 * - Iterates over the LHS list (`context->iterator`) and compares each element to the array of RHS elements.
 * - If a match is found:
 *   - The matching RHS element is removed from the array by swapping with the last remaining element.
 *   - If no more RHS elements remain, the remaining LHS list is appended to the result, and the operation completes.
 * - If no match is found, the LHS element is added to the result list.
 * - The function respects a reduction budget (`CHECKS_PER_RED`), trapping and resuming as needed.
 * - Ensures that the result is terminated when trapping to facilitate debugging.
 *
 * @complexity O(m * n), where `m` is the number of elements in the LHS list and `n` is the size of `rhs_elements`.
 */
static int subtract_naive_rhs(Process *p, ErtsSubtractContext *context) {
    const Sint CHECKS_PER_RED = 16;
    Sint checks, budget;

    budget = CHECKS_PER_RED * ERTS_BIF_REDS_LEFT(p);
    checks = 0;

#ifdef DEBUG
    budget = budget / 10 + 1;
#endif

    while (checks < budget && is_list(context->iterator)) {
        const Eterm *cell;
        Eterm value, next;
        int found_at;

        cell = list_val(context->iterator);
        value = CAR(cell);
        next = CDR(cell);

        for (found_at = context->rhs_remaining - 1; found_at >= 0; found_at--) {
            if (EQ(value, context->u.rhs_elements[found_at])) {
                break;
            }
        }

        if (found_at < 0) {
            /* Destructively add the value to the result. This is safe
             * since the GC is disabled and the unfinished term is never
             * leaked to the outside world. */
            Eterm *hp = HAllocX(p, 2, context->lhs_remaining * 2);

            *context->result_cdr = make_list(hp);
            context->result_cdr = &CDR(hp);

            CAR(hp) = value;
        } else if (found_at >= 0) {
            Eterm swap;

            if (context->rhs_remaining-- == 1) {
                /* We've run out of items to remove, so the rest of the
                 * result will be equal to the remainder of the input. We know
                 * that LHS is well-formed as any errors would've been reported
                 * during length determination. */
                *context->result_cdr = next;

                BUMP_REDS(p, MIN(budget, checks) / CHECKS_PER_RED);

                return 1;
            }

            swap = context->u.rhs_elements[context->rhs_remaining];
            context->u.rhs_elements[found_at] = swap;
        }

        checks += context->rhs_remaining;
        context->iterator = next;
        context->lhs_remaining--;
    }

    /* The result only has to be terminated when returning it to the user, but
     * we're doing it when trapping as well to prevent headaches when
     * debugging. */
    *context->result_cdr = NIL;

    BUMP_REDS(p, MIN(budget, checks) / CHECKS_PER_RED);

    if (is_list(context->iterator)) {
        ASSERT(context->lhs_remaining > 0 && context->rhs_remaining > 0);
        return 0;
    }

    return 1;
}

/**
 * @brief Initializes the set-building stage for the RHS list.
 *
 * This function sets up the `ErtsSubtractContext` to enter the `SUBTRACT_STAGE_SET_BUILD` stage,
 * where a red-black tree is constructed from the right-hand side (RHS) list to optimize element lookups.
 *
 * @param p A pointer to the `Process` structure representing the current process.
 * @param context A pointer to the `ErtsSubtractContext` structure managing the state of the subtract operation.
 * @return
 * - `1` if the operation completes successfully.
 * - `0` if the operation traps for additional reductions or resources.
 * - `-1` if an error occurs during the operation.
 *
 * @details
 * - Allocates memory for the red-black tree nodes using `erts_alloc`. The size of the allocation is
 *   based on the number of elements in the RHS list (`rhs_remaining`).
 * - Initializes pointers for the allocation (`alloc_start` and `alloc`) and sets the `tree` to `NULL`.
 * - Sets the `iterator` to traverse the RHS list for building the tree.
 * - Updates the `stage` to `SUBTRACT_STAGE_SET_BUILD` and calls `subtract_continue` to proceed.
 *
 * @complexity O(1) for setup; subsequent tree-building steps are handled in `subtract_continue`.
 */
static int subtract_enter_set_build(Process *p, ErtsSubtractContext *context) {
    context->stage = SUBTRACT_STAGE_SET_BUILD;

    context->u.rhs_set.alloc_start =
        erts_alloc(ERTS_ALC_T_LIST_TRAP,
                   context->rhs_remaining * sizeof(subtract_tree_t));

    context->u.rhs_set.alloc = context->u.rhs_set.alloc_start;
    context->u.rhs_set.tree = NULL;

    context->iterator = context->rhs_original;

    return subtract_continue(p, context);
}

/**
 * @brief Builds a red-black tree from the RHS list for optimized lookups.
 *
 * This function processes the right-hand side (RHS) list and constructs a red-black tree
 * (`rhs_set.tree`) to efficiently manage duplicates and perform fast lookups during the subtraction operation.
 * The tree nodes are allocated from a pre-allocated memory region.
 *
 * @param p A pointer to the `Process` structure representing the current process.
 * @param context A pointer to the `ErtsSubtractContext` structure managing the state of the subtract operation.
 * @return
 * - `1` if the tree-building process is complete.
 * - `0` if the operation traps for additional reductions or resources.
 *
 * @details
 * - A reduction budget (`INSERTIONS_PER_RED`) determines how many elements are processed in a single call.
 * - For each element in the RHS list:
 *   - A new node is created or fetched from the pre-allocated memory (`rhs_set.alloc`).
 *   - The node is inserted into the red-black tree using `subtract_rbt_lookup_insert`.
 *   - If the element already exists in the tree, its `count` is incremented; otherwise, a new node is added.
 * - The function continues processing until the list is fully traversed or the budget is exhausted.
 * - Adjusts the reduction count based on the number of insertions performed.
 *
 * @complexity O(n log n), where `n` is the number of elements in the RHS list.
 */
static int subtract_set_build(Process *p, ErtsSubtractContext *context) {
    const static Sint INSERTIONS_PER_RED = 16;
    Sint budget, insertions;

    budget = INSERTIONS_PER_RED * ERTS_BIF_REDS_LEFT(p);
    insertions = 0;

#ifdef DEBUG
    budget = budget / 10 + 1;
#endif

    while (insertions < budget && is_list(context->iterator)) {
        subtract_tree_t *existing_node, *new_node;
        const Eterm *cell;
        Eterm value, next;

        cell = list_val(context->iterator);
        value = CAR(cell);
        next = CDR(cell);

        new_node = context->u.rhs_set.alloc;
        new_node->key = value;
        new_node->count = 1;

        existing_node = subtract_rbt_lookup_insert(&context->u.rhs_set.tree,
                                                   new_node);

        if (existing_node != NULL) {
            existing_node->count++;
        } else {
            context->u.rhs_set.alloc++;
        }

        context->iterator = next;
        insertions++;
    }

    BUMP_REDS(p, insertions / INSERTIONS_PER_RED);

    ASSERT(is_list(context->iterator) || is_nil(context->iterator));
    ASSERT(context->u.rhs_set.tree != NULL);

    return is_nil(context->iterator);
}

/**
 * @brief Initializes the set-finishing stage for the subtraction operation.
 *
 * This function sets up the `ErtsSubtractContext` to enter the `SUBTRACT_STAGE_SET_FINISH` stage,
 * where the left-hand side (LHS) list is processed against the red-black tree built from the
 * right-hand side (RHS) list to produce the final result.
 *
 * @param p A pointer to the `Process` structure representing the current process.
 * @param context A pointer to the `ErtsSubtractContext` structure managing the state of the subtract operation.
 * @return
 * - `1` if the operation completes successfully.
 * - `0` if the operation traps for additional reductions or resources.
 * - `-1` if an error occurs during the operation.
 *
 * @details
 * - Sets the `stage` to `SUBTRACT_STAGE_SET_FINISH`.
 * - Resets the result list pointers (`result` and `result_cdr`) to prepare for constructing the final result.
 * - Initializes the `iterator` to traverse the LHS list (`lhs_original`).
 * - Calls `subtract_continue` to proceed with the operation.
 *
 * @complexity O(1) for setup; subsequent processing is handled in `subtract_continue`.
 */
static int subtract_enter_set_finish(Process *p, ErtsSubtractContext *context) {
    context->stage = SUBTRACT_STAGE_SET_FINISH;

    context->result_cdr = &context->result;
    context->result = NIL;

    context->iterator = context->lhs_original;

    return subtract_continue(p, context);
}

/**
 * @brief Finalizes the subtract operation using the red-black tree.
 *
 * This function processes the left-hand side (LHS) list against the red-black tree built from
 * the right-hand side (RHS) list. It constructs the result list by excluding elements found
 * in the tree and updating the tree as elements are matched and removed.
 *
 * @param p A pointer to the `Process` structure representing the current process.
 * @param context A pointer to the `ErtsSubtractContext` structure managing the state of the subtract operation.
 * @return
 * - `1` if the operation completes successfully, with the result stored in `context->result`.
 * - `0` if the operation traps for additional reductions or resources.
 * - `-1` if an error occurs during the operation.
 *
 * @details
 * - Iterates over the LHS list (`context->iterator`) and checks each element against the red-black tree (`rhs_set.tree`).
 * - If an element is not found in the tree:
 *   - Adds it to the result list.
 * - If an element is found in the tree:
 *   - Decrements the count for the element. If the count reaches zero, the node is removed from the tree.
 *   - If the RHS elements are fully processed (`rhs_remaining` reaches zero), the remainder of the LHS list is appended to the result, and the operation ends.
 * - The function respects a reduction budget (`CHECKS_PER_RED`) and traps if necessary.
 * - Ensures the result list is terminated when trapping to simplify debugging.
 *
 * @complexity O(m log n), where `m` is the number of elements in the LHS list and `n` is the number of elements in the red-black tree.
 */
static int subtract_set_finish(Process *p, ErtsSubtractContext *context) {
    const Sint CHECKS_PER_RED = 8;
    Sint checks, budget;

    budget = CHECKS_PER_RED * ERTS_BIF_REDS_LEFT(p);
    checks = 0;

#ifdef DEBUG
    budget = budget / 10 + 1;
#endif

    while (checks < budget && is_list(context->iterator)) {
        subtract_tree_t *node;
        const Eterm *cell;
        Eterm value, next;

        cell = list_val(context->iterator);
        value = CAR(cell);
        next = CDR(cell);

        ASSERT(context->rhs_remaining > 0);

        node = subtract_rbt_lookup(context->u.rhs_set.tree, value);

        if (node == NULL) {
            Eterm *hp = HAllocX(p, 2, context->lhs_remaining * 2);

            *context->result_cdr = make_list(hp);
            context->result_cdr = &CDR(hp);

            CAR(hp) = value;
        } else {
            if (context->rhs_remaining-- == 1) {
                *context->result_cdr = next;

                BUMP_REDS(p, checks / CHECKS_PER_RED);

                return 1;
            }

            if (node->count-- == 1) {
                subtract_rbt_delete(&context->u.rhs_set.tree, node);
            }
        }

        context->iterator = next;
        context->lhs_remaining--;
        checks++;
    }

    *context->result_cdr = NIL;

    BUMP_REDS(p, checks / CHECKS_PER_RED);

    if (is_list(context->iterator)) {
        ASSERT(context->lhs_remaining > 0 && context->rhs_remaining > 0);
        return 0;
    }

    return 1;
}

/**
 * @brief Continues the subtract operation based on the current stage.
 *
 * This function determines the next steps in a subtract operation by evaluating the
 * current stage stored in the `ErtsSubtractContext`. It delegates to stage-specific
 * functions to progress through the operation, handling tasks such as length calculation,
 * naive subtraction, and multiset creation.
 *
 * @param p A pointer to the `Process` structure representing the current process.
 * @param context A pointer to the `ErtsSubtractContext` structure managing the state of the subtract operation.
 * @return
 * - `1` if the operation completes successfully.
 * - `0` if the operation traps for additional reductions or resources.
 * - `-1` if an error occurs during the operation.
 *
 * @details
 * - **`SUBTRACT_STAGE_START`**: Begins the operation by entering the LHS length calculation stage.
 * - **`SUBTRACT_STAGE_LEN_LHS`**: Calculates the length of the LHS list. If the LHS is small and the RHS is a list,
 *   it transitions to `SUBTRACT_STAGE_NAIVE_LHS`. Otherwise, it moves to `SUBTRACT_STAGE_LEN_RHS`.
 * - **`SUBTRACT_STAGE_NAIVE_LHS`**: Performs naive subtraction on the LHS list.
 * - **`SUBTRACT_STAGE_LEN_RHS`**: Calculates the length of the RHS list. Depending on the lengths of LHS and RHS:
 *   - If the LHS is empty, the result is `NIL`.
 *   - If the RHS is empty, the result is the unchanged LHS.
 *   - Otherwise, transitions to either `SUBTRACT_STAGE_NAIVE_RHS` or `SUBTRACT_STAGE_SET_BUILD`.
 * - **`SUBTRACT_STAGE_NAIVE_RHS`**: Performs naive subtraction on the RHS list.
 * - **`SUBTRACT_STAGE_SET_BUILD`**: Builds a multiset from the RHS for efficient lookups and transitions to
 *   `SUBTRACT_STAGE_SET_FINISH` upon completion.
 * - **`SUBTRACT_STAGE_SET_FINISH`**: Finalizes the subtraction operation using the constructed multiset.
 *
 * - If an unrecognized stage is encountered, an internal error is triggered.
 *
 * @complexity Varies depending on the current stage:
 * - Length calculation: O(n) for the respective list.
 * - Naive subtraction: O(m * n) for LHS and RHS sizes `m` and `n`.
 * - Multiset-based subtraction: O(n log n + m log n).
 */
static int subtract_continue(Process *p, ErtsSubtractContext *context) {
    switch (context->stage) {
        case SUBTRACT_STAGE_START: {
            return subtract_enter_len_lhs(p, context);
        }

        case SUBTRACT_STAGE_LEN_LHS: {
            int res = subtract_get_length(p,
                                          &context->iterator,
                                          &context->lhs_remaining);

            if (res != 1) {
                return res;
            }

            /* If the lhs list is empty then there's nothing to do.
             * Returning early will be taken care of in the `SUBTRACT_STAGE_LEN_RHS`
             * stage after the rhs list has been scanned. */
            if (context->lhs_remaining > 0 &&
                context->lhs_remaining <= SUBTRACT_LHS_THRESHOLD &&
                is_list(context->rhs_original)) {
                return subtract_enter_naive_lhs(p, context);
            }

            return subtract_enter_len_rhs(p, context);
        }

        case SUBTRACT_STAGE_NAIVE_LHS: {
            return subtract_naive_lhs(p, context);
        }

        case SUBTRACT_STAGE_LEN_RHS: {
            int res = subtract_get_length(p,
                                          &context->iterator,
                                          &context->rhs_remaining);

            if (res != 1) {
                return res;
            }

            /* If the lhs list is empty then the subtraction must return nil. */
            if (context->lhs_remaining == 0) {
                ASSERT(is_nil(context->lhs_original));
                context->result = NIL;
                return 1;
            }
            /* If the rhs list is empty then the subtraction should return the
             * lhs list unchanged. */
            if (context->rhs_remaining == 0) {
                ASSERT(is_nil(context->rhs_original));
                context->result = context->lhs_original;
                return 1;
            }

            /* We've walked through both lists fully now so we no longer need
             * to check for errors past this point. */

            if (context->rhs_remaining <= SUBTRACT_RHS_THRESHOLD) {
                return subtract_enter_naive_rhs(p, context);
            }

            return subtract_enter_set_build(p, context);
        }

        case SUBTRACT_STAGE_NAIVE_RHS: {
            return subtract_naive_rhs(p, context);
        }

        case SUBTRACT_STAGE_SET_BUILD: {
            int res = subtract_set_build(p, context);

            if (res != 1) {
                return res;
            }

            return subtract_enter_set_finish(p, context);
        }

        case SUBTRACT_STAGE_SET_FINISH: {
            return subtract_set_finish(p, context);
        }
    }
    ERTS_INTERNAL_ERROR("unreachable");
}

/**
 * @brief Initializes and starts the subtract operation.
 *
 * This function initializes the `ErtsSubtractContext` structure with the provided
 * left-hand side (LHS) and right-hand side (RHS) terms, and begins the subtraction
 * operation by entering the first stage (`SUBTRACT_STAGE_START`).
 *
 * @param p A pointer to the `Process` structure representing the current process.
 * @param lhs The left-hand side term for the subtract operation.
 * @param rhs The right-hand side term for the subtract operation.
 * @param context A pointer to the `ErtsSubtractContext` structure to be initialized and used.
 * @return
 * - `1` if the operation completes successfully.
 * - `0` if the operation traps for additional reductions or resources.
 * - `-1` if an error occurs during the operation.
 *
 * @details
 * - Sets the `stage` to `SUBTRACT_STAGE_START`.
 * - Initializes the context with the original LHS and RHS terms.
 * - Enters the first stage of the operation by calling `subtract_continue`.
 * - Debug values are set for `lhs_remaining`, `rhs_remaining`, and `result` to silence static analysis tools in debug mode.
 *
 * @complexity O(1) for initialization; subsequent complexity depends on the operation stages.
 */
static int subtract_start(Process *p, Eterm lhs, Eterm rhs,
                          ErtsSubtractContext *context) {
    context->stage = SUBTRACT_STAGE_START;

    context->lhs_original = lhs;
    context->rhs_original = rhs;
#ifdef DEBUG
    /* Silence CodeChecker in subtract_ctx_move() */
    context->lhs_remaining = 17;
    context->rhs_remaining = 42;
    context->result = 99;
#endif
    return subtract_continue(p, context);
}

/**
 * @brief Implements the `--/2` operation for subtracting elements in Erlang.
 *
 * This function handles the subtract operation for the `--/2` BIF (built-in function),
 * supporting cases where the left-hand side (LHS) and right-hand side (RHS) are lists
 * or magic references (trap states). It manages the context for the operation, trapping
 * as necessary for large inputs or resource limitations.
 *
 * @param bif_entry A pointer to the `Export` structure representing the BIF entry.
 * @param BIF_ALIST_2 Macro for the BIF's argument list (LHS and RHS).
 * @return The resulting term after subtracting RHS elements from LHS, or traps if further
 *         processing is required.
 *
 * @details
 * - **Case 1**: If `lhs` and `rhs` are valid lists or `NIL`:
 *   - Initializes an `ErtsSubtractContext` on the stack and begins the subtraction operation
 *     with `subtract_start`.
 *   - If trapping is required, creates a trap state with `subtract_create_trap_state` and defers execution.
 *   - Frees any resources held by the context after completion.
 *   - Handles errors for improper lists or unexpected conditions with `BADARG`.
 * - **Case 2**: If `lhs` is a magic reference (trap state):
 *   - Resumes the subtraction operation from the trap state.
 *   - Validates the magic reference and ensures it corresponds to a subtract operation.
 *   - Resets garbage collection state after completing the operation.
 *   - Handles errors by restoring the original terms and raising `BADARG`.
 * - **Invalid Input**: If `lhs` or `rhs` is neither a list nor a valid magic reference, raises `BADARG`.
 *
 * @complexity
 * - List processing: O(m * n) for naive stages, where `m` is the size of LHS and `n` is the size of RHS.
 * - Set-based stages: O(n log n + m log n), assuming `n` is the size of RHS and `m` is the size of LHS.
 */
/* erlang:'--'/2 */
static Eterm subtract(Export *bif_entry, BIF_ALIST_2) {
    Eterm lhs = BIF_ARG_1, rhs = BIF_ARG_2;

    if ((is_list(lhs) || is_nil(lhs)) && (is_list(rhs) || is_nil(rhs))) {
        /* We start with the context on the stack in the hopes that we won't
         * have to trap. */
        ErtsSubtractContext context;
        int res;

        res = subtract_start(BIF_P, lhs, rhs, &context);

        if (res == 0) {
            Eterm state_mref;

            state_mref = subtract_create_trap_state(BIF_P, &context);
            erts_set_gc_state(BIF_P, 0);

            BIF_TRAP2(bif_entry, BIF_P, state_mref, NIL);
        }

        subtract_ctx_dtor(&context);

        if (res < 0) {
            BIF_ERROR(BIF_P, BADARG);
        }

        BIF_RET(context.result);
    } else if (is_internal_magic_ref(lhs)) {
        ErtsSubtractContext *context;
        int (*dtor)(Binary*);
        Binary *magic_bin;

        int res;

        magic_bin = erts_magic_ref2bin(lhs);
        dtor = ERTS_MAGIC_BIN_DESTRUCTOR(magic_bin);

        if (dtor != subtract_ctx_bin_dtor) {
            BIF_ERROR(BIF_P, BADARG);
        }

        ASSERT(BIF_P->flags & F_DISABLE_GC);
        ASSERT(rhs == NIL);

        context = ERTS_MAGIC_BIN_DATA(magic_bin);
        res = subtract_continue(BIF_P, context);

        if (res == 0) {
            BIF_TRAP2(bif_entry, BIF_P, lhs, NIL);
        }

        erts_set_gc_state(BIF_P, 1);

        if (res < 0) {
            ERTS_BIF_ERROR_TRAPPED2(BIF_P, BADARG, bif_entry,
                                    context->lhs_original,
                                    context->rhs_original);
        }

        BIF_RET(context->result);
    }

    ASSERT(!(BIF_P->flags & F_DISABLE_GC));

    BIF_ERROR(BIF_P, BADARG);
}

/**
 * @brief Entry point for the `--/2` BIF operation.
 *
 * This function serves as a wrapper around the `subtract` function to implement
 * the `--/2` operation in Erlang. It prepares the `subtract` function call by
 * passing the appropriate BIF entry and argument list.
 *
 * @param BIF_ALIST_2 Macro for the BIF's argument list (LHS and RHS).
 * @return The resulting term after subtracting RHS elements from LHS, or traps if
 *         additional processing is required.
 *
 * @details
 * - Delegates the subtraction logic to the `subtract` function.
 * - Passes `BIF_TRAP_EXPORT(BIF_ebif_minusminus_2)` as the entry point and `BIF_CALL_ARGS` as the arguments.
 * - Ensures consistent handling of trapping, resource management, and errors by utilizing the `subtract` function.
 *
 * @complexity
 * - List processing: O(m * n) for naive subtraction stages.
 * - Set-based subtraction: O(n log n + m log n), where `n` is the size of RHS and `m` is the size of LHS.
 */
BIF_RETTYPE ebif_minusminus_2(BIF_ALIST_2) {
    return subtract(BIF_TRAP_EXPORT(BIF_ebif_minusminus_2), BIF_CALL_ARGS);
}

/**
 * @brief Entry point for the `subtract/2` BIF operation.
 *
 * This function acts as a wrapper for the `subtract` function to implement the
 * `subtract/2` operation in Erlang. It initializes the `subtract` function call with
 * the appropriate BIF entry and argument list.
 *
 * @param BIF_ALIST_2 Macro for the BIF's argument list (LHS and RHS).
 * @return The resulting term after subtracting RHS elements from LHS, or traps if
 *         additional processing is required.
 *
 * @details
 * - Delegates the subtraction logic to the `subtract` function.
 * - Passes `BIF_TRAP_EXPORT(BIF_subtract_2)` as the entry point and `BIF_CALL_ARGS` as the arguments.
 * - Ensures consistent handling of trapping, resource management, and errors by relying on the `subtract` function.
 *
 * @complexity
 * - List processing: O(m * n) for naive subtraction stages.
 * - Set-based subtraction: O(n log n + m log n), where `n` is the size of RHS and `m` is the size of LHS.
 */
BIF_RETTYPE subtract_2(BIF_ALIST_2) {
    return subtract(BIF_TRAP_EXPORT(BIF_subtract_2), BIF_CALL_ARGS);
}


/**
 * @brief Implements the `lists:member/2` BIF for checking membership in a list.
 *
 * This function checks if a given term (`BIF_ARG_1`) is a member of the list (`BIF_ARG_2`).
 * It efficiently iterates through the list with a reduction budget and traps if the operation
 * exceeds the allowed reductions.
 *
 * @param BIF_ALIST_2 Macro for the BIF's argument list (term and list).
 * @return
 * - `am_true` if the term is found in the list.
 * - `am_false` if the term is not found in the list.
 * - Traps if the operation exceeds the reduction budget.
 * - Raises `BADARG` if the second argument is not a proper list or `NIL`.
 *
 * @details
 * - Checks if `BIF_ARG_2` is a valid list or `NIL`. If not, raises `BADARG`.
 * - Iterates over the list while:
 *   - Comparing each element with `BIF_ARG_1`.
 *   - Accounting for immediate and non-immediate terms for proper equality checks.
 *   - Respecting a reduction budget of `16 * ERTS_BIF_REDS_LEFT`.
 * - Traps if the iteration exceeds the budget, allowing the operation to resume.
 * - If the list is improper, raises `BADARG`.
 * - Returns the result (`am_true` or `am_false`) with the adjusted reduction count.
 *
 * @complexity O(n), where `n` is the number of elements in the list.
 */
BIF_RETTYPE lists_member_2(BIF_ALIST_2)
{
    Eterm term;
    Eterm list;
    Eterm item;
    int non_immed_key;
    int reds_left = ERTS_BIF_REDS_LEFT(BIF_P);
    int max_iter = 16 * reds_left;

    if (is_nil(BIF_ARG_2)) {
    BIF_RET(am_false);
    } else if (is_not_list(BIF_ARG_2)) {
    BIF_ERROR(BIF_P, BADARG);
    }
    
    term = BIF_ARG_1;
    non_immed_key = is_not_immed(term);
    list = BIF_ARG_2;
    while (is_list(list)) {
    if (--max_iter < 0) {
        BUMP_ALL_REDS(BIF_P);
        BIF_TRAP2(BIF_TRAP_EXPORT(BIF_lists_member_2), BIF_P, term, list);
    }
    item = CAR(list_val(list));
    if ((item == term) || (non_immed_key && eq(item, term))) {
        BIF_RET2(am_true, reds_left - max_iter/16);
    }
    list = CDR(list_val(list));
    }
    if (is_not_nil(list))  {
        BUMP_REDS(BIF_P, reds_left - max_iter/16);
    BIF_ERROR(BIF_P, BADARG);
    }
    BIF_RET2(am_false, reds_left - max_iter/16);
}

/**
 * @brief Reverses a list using heap allocation and reduction management.
 *
 * This function reverses the elements of a list (`list_in`), appending the reversed
 * portion to a specified tail (`tail_in`). The operation respects a reduction budget
 * and traps if the budget is exceeded, allowing for resumption.
 *
 * @param c_p A pointer to the `Process` structure representing the current process.
 * @param list_in The input list to reverse.
 * @param tail_in The tail to append the reversed elements to.
 * @return
 * - The reversed list appended to `tail_in`, if the operation completes within the budget.
 * - Traps and defers execution if the operation exceeds the reduction budget.
 * - Raises `BADARG` if the input list is improper.
 *
 * @details
 * - **Reduction Budget**: The function processes up to `CELLS_PER_RED * ERTS_BIF_REDS_LEFT` list cells
 *   per invocation. If the list exceeds this budget, the operation traps and resumes later.
 * - **Heap Allocation**: Allocates heap space in chunks to construct the reversed list efficiently.
 * - **Error Handling**: Validates that the input list is proper. If an improper list is encountered,
 *   raises `BADARG`.
 * - If the input list is fully traversed (`NIL`), the function returns the constructed reversed list.
 * - If the list is not fully processed, the operation traps and resumes with the remaining list and the
 *   partially constructed tail.
 *
 * @complexity O(n), where `n` is the number of elements in the input list. Trapping may occur for large lists.
 */
static BIF_RETTYPE lists_reverse_alloc(Process *c_p,
                                       Eterm list_in,
                                       Eterm tail_in)
{
    static const Uint CELLS_PER_RED = 40;

    Eterm *alloc_top, *alloc_end;
    Uint cells_left, max_cells;
    Eterm list, tail;
    Eterm lookahead;

    list = list_in;
    tail = tail_in;

    cells_left = max_cells = CELLS_PER_RED * ERTS_BIF_REDS_LEFT(c_p);
    lookahead = list;

    while (cells_left != 0 && is_list(lookahead)) {
        lookahead = CDR(list_val(lookahead));
        cells_left--;
    }

    BUMP_REDS(c_p, (max_cells - cells_left) / CELLS_PER_RED);

    if (is_not_list(lookahead) && is_not_nil(lookahead)) {
        BIF_ERROR(c_p, BADARG);
    }

    alloc_top = HAlloc(c_p, 2 * (max_cells - cells_left));
    alloc_end = alloc_top + 2 * (max_cells - cells_left);

    while (alloc_top < alloc_end) {
        Eterm *pair = list_val(list);

        tail = CONS(alloc_top, CAR(pair), tail);
        list = CDR(pair);

        ASSERT(is_list(list) || is_nil(list));

        alloc_top += 2;
    }

    if (is_nil(list)) {
        BIF_RET(tail);
    }

    ASSERT(is_list(tail) && cells_left == 0);
    BIF_TRAP2(BIF_TRAP_EXPORT(BIF_lists_reverse_2), c_p, list, tail);
}

/**
 * @brief Reverses a list using on-heap memory and reduction management.
 *
 * This function reverses the elements of a list (`list_in`), appending the reversed
 * portion to a specified tail (`tail_in`). It operates directly on the process's
 * heap and respects a reduction budget. If the heap or reduction budget is exhausted,
 * the operation traps or delegates to a heap-allocating variant.
 *
 * @param c_p A pointer to the `Process` structure representing the current process.
 * @param list_in The input list to reverse.
 * @param tail_in The tail to append the reversed elements to.
 * @return
 * - The reversed list appended to `tail_in`, if the operation completes within the heap and budget limits.
 * - Traps and defers execution if the operation exceeds the reduction budget or heap space.
 * - Raises `BADARG` if the input list is improper.
 *
 * @details
 * - **Reduction Budget**: The function processes up to `CELLS_PER_RED * ERTS_BIF_REDS_LEFT` list cells
 *   per invocation. If the list exceeds this budget, it either traps or falls back to `lists_reverse_alloc`.
 * - **Heap Management**: Operates directly on the process's heap, allocating space for list construction.
 *   Ensures no overflow of the heap limit (`HEAP_LIMIT`).
 * - **Error Handling**: Validates that the input list is proper. If an improper list is encountered,
 *   raises `BADARG`.
 * - If the input list is fully traversed (`NIL`), the function returns the constructed reversed list.
 * - If the list is not fully processed:
 *   - Attempts to switch to heap allocation with `lists_reverse_alloc` if reductions allow.
 *   - Traps if neither option is feasible, preserving the remaining list and tail for resumption.
 *
 * @complexity O(n), where `n` is the number of elements in the input list. Trapping or delegation may occur for large lists.
 */
static BIF_RETTYPE lists_reverse_onheap(Process *c_p,
                                        Eterm list_in,
                                        Eterm tail_in)
{
    static const Uint CELLS_PER_RED = 60;

    Eterm *alloc_start, *alloc_top, *alloc_end;
    Uint cells_left, max_cells;
    Eterm list, tail;

    list = list_in;
    tail = tail_in;

    cells_left = max_cells = CELLS_PER_RED * ERTS_BIF_REDS_LEFT(c_p);

    ASSERT(HEAP_LIMIT(c_p) >= HEAP_TOP(c_p) + 2);
    alloc_start = HEAP_TOP(c_p);
    alloc_end = HEAP_LIMIT(c_p) - 2;
    alloc_top = alloc_start;

    /* Don't process more cells than we have reductions for. */
    alloc_end = MIN(alloc_top + (cells_left * 2), alloc_end);

    while (alloc_top < alloc_end && is_list(list)) {
        Eterm *pair = list_val(list);

        tail = CONS(alloc_top, CAR(pair), tail);
        list = CDR(pair);

        alloc_top += 2;
    }

    cells_left -= (alloc_top - alloc_start) / 2;
    HEAP_TOP(c_p) = alloc_top;

    ASSERT(cells_left >= 0 && cells_left <= max_cells);
    BUMP_REDS(c_p, (max_cells - cells_left) / CELLS_PER_RED);

    if (is_nil(list)) {
        BIF_RET(tail);
    } else if (is_list(list)) {
        if (cells_left > CELLS_PER_RED) {
            return lists_reverse_alloc(c_p, list, tail);
        }

        BUMP_ALL_REDS(c_p);
        BIF_TRAP2(BIF_TRAP_EXPORT(BIF_lists_reverse_2), c_p, list, tail);
    }

    BIF_ERROR(c_p, BADARG);
}

/**
 * @brief Reverses a list (`lists:reverse/2` BIF) with an optional tail.
 *
 * This function implements the `lists:reverse/2` built-in function (BIF), which reverses
 * the elements of the first argument (`BIF_ARG_1`) and appends the reversed portion to
 * the second argument (`BIF_ARG_2`).
 *
 * @param BIF_ALIST_2 Macro for the BIF's argument list (list and tail).
 * @return
 * - The reversed list appended to `BIF_ARG_2` if the operation completes successfully.
 * - Raises `BADARG` if `BIF_ARG_1` is not a proper list or `NIL`.
 *
 * @details
 * - **Case 1**: If `BIF_ARG_1` is `NIL`, the function directly returns `BIF_ARG_2`.
 * - **Case 2**: If `BIF_ARG_1` is not a list, the function raises `BADARG`.
 * - **Case 3**: If there is sufficient heap space (`HeapWordsLeft > 8`), the function
 *   uses `lists_reverse_onheap` to reverse the list directly on the heap.
 * - **Case 4**: If heap space is insufficient, the function falls back to
 *   `lists_reverse_alloc`, which allocates additional space for the reversal.
 *
 * @complexity O(n), where `n` is the number of elements in `BIF_ARG_1`. The function
 * may trap or allocate additional memory for large lists or limited resources.
 */
BIF_RETTYPE lists_reverse_2(BIF_ALIST_2)
{
    /* Handle legal and illegal non-lists quickly. */
    if (is_nil(BIF_ARG_1)) {
        BIF_RET(BIF_ARG_2);
    } else if (is_not_list(BIF_ARG_1)) {
        BIF_ERROR(BIF_P, BADARG);
    }

    /* We build the reversal on the unused part of the heap if possible to save
     * us the trouble of having to figure out the list size. We fall back to
     * lists_reverse_alloc when we run out of space. */
    if (HeapWordsLeft(BIF_P) > 8) {
        return lists_reverse_onheap(BIF_P, BIF_ARG_1, BIF_ARG_2);
    }

    return lists_reverse_alloc(BIF_P, BIF_ARG_1, BIF_ARG_2);
}

/**
 * @brief Checks if a key is present in a list of tuples (`lists:keymember/3` BIF).
 *
 * This function implements the `lists:keymember/3` built-in function (BIF), which determines
 * whether a key (`BIF_ARG_1`) exists in a list of tuples (`BIF_ARG_3`) at a specific position
 * (`BIF_ARG_2`).
 *
 * @param BIF_ALIST_3 Macro for the BIF's argument list (key, position, and list).
 * @return
 * - `am_true` if a tuple containing the key exists in the specified position in the list.
 * - `am_false` if the key is not found.
 * - Traps or raises an error if invalid inputs or resource limits are encountered.
 *
 * @details
 * - Delegates the core logic to `keyfind`, passing the BIF entry, process, key, position,
 *   and list.
 * - If `keyfind` returns a tuple (`is_tuple(res)`), the function returns `am_true`.
 * - If `keyfind` indicates the key is not found or an error occurs, the function returns
 *   the result as-is (`res`).
 *
 * @complexity O(n), where `n` is the number of elements in the list. Trapping or resource
 * limitations may affect the function's behavior for large inputs or improper lists.
 */
BIF_RETTYPE
lists_keymember_3(BIF_ALIST_3)
{
    Eterm res;

    res = keyfind(BIF_TRAP_EXPORT(BIF_lists_keymember_3), BIF_P,
          BIF_ARG_1, BIF_ARG_2, BIF_ARG_3);
    if (is_value(res) && is_tuple(res)) {
    return am_true;
    } else {
    return res;
    }
}

/**
 * @brief Searches for a key in a list of tuples (`lists:keysearch/3` BIF).
 *
 * This function implements the `lists:keysearch/3` built-in function (BIF), which searches
 * for a key (`BIF_ARG_1`) in a list of tuples (`BIF_ARG_3`) at a specified position (`BIF_ARG_2`).
 * If the key is found, the function returns a tuple `{value, Tuple}` containing the matching tuple.
 *
 * @param BIF_ALIST_3 Macro for the BIF's argument list (key, position, and list).
 * @return
 * - `{value, Tuple}` if a tuple containing the key is found in the specified position.
 * - `am_false` if the key is not found.
 * - Traps or raises an error for invalid inputs or resource limitations.
 *
 * @details
 * - Delegates the core logic to `keyfind`, passing the BIF entry, process, key, position,
 *   and list.
 * - If `keyfind` returns a non-value or a non-tuple, the result (`res`) is returned as-is
 *   (e.g., `am_false` for a missing key).
 * - If `keyfind` returns a matching tuple, the function allocates heap space and constructs
 *   the result as `{value, Tuple}` using `TUPLE2`.
 *
 * @complexity O(n), where `n` is the number of elements in the list. The function may trap
 * for large inputs or improper lists.
 */
BIF_RETTYPE
lists_keysearch_3(BIF_ALIST_3)
{
    Eterm res;
    
    res = keyfind(BIF_TRAP_EXPORT(BIF_lists_keysearch_3), BIF_P,
          BIF_ARG_1, BIF_ARG_2, BIF_ARG_3);
    if (is_non_value(res) || is_not_tuple(res)) {
    return res;
    } else {            /* Tuple */
    Eterm* hp = HAlloc(BIF_P, 3);
    return TUPLE2(hp, am_value, res);
    }
}

/**
 * @brief Searches for a key in a list of tuples (`lists:keyfind/3` BIF).
 *
 * This function implements the `lists:keyfind/3` built-in function (BIF), which searches
 * for a key (`BIF_ARG_1`) in a list of tuples (`BIF_ARG_3`) at a specified position (`BIF_ARG_2`).
 * If the key is found, the function returns the matching tuple.
 *
 * @param BIF_ALIST_3 Macro for the BIF's argument list (key, position, and list).
 * @return
 * - The tuple containing the key if found.
 * - `am_false` if the key is not found in the list.
 * - Traps or raises `BADARG` for invalid inputs or resource limitations.
 *
 * @details
 * - Delegates the core logic to the `keyfind` function.
 * - Passes the BIF entry, process, key, position, and list to `keyfind`.
 * - Returns the result of `keyfind` directly:
 *   - A tuple if the key is found.
 *   - `am_false` if the key is not found.
 *   - Handles traps and errors raised by `keyfind`.
 *
 * @complexity O(n), where `n` is the number of elements in the list. Trapping may occur for large inputs.
 */
BIF_RETTYPE
lists_keyfind_3(BIF_ALIST_3)
{
    return keyfind(BIF_TRAP_EXPORT(BIF_lists_keyfind_3), BIF_P,
           BIF_ARG_1, BIF_ARG_2, BIF_ARG_3);
}

/**
 * @brief Finds a key in a list of tuples.
 *
 * This function searches for a key (`Key`) in a list of tuples (`List`) at a specified position (`Pos`).
 * It returns the first tuple where the key matches the element at the specified position.
 * If the key is not found, it returns `am_false`. The function respects a reduction budget and traps if
 * the search exceeds the allowed reductions.
 *
 * @param Bif A pointer to the `Export` structure representing the BIF entry.
 * @param p A pointer to the `Process` structure representing the current process.
 * @param Key The key to search for.
 * @param Pos The position within each tuple to compare against the key (1-based index).
 * @param List The list of tuples to search through.
 * @return
 * - The matching tuple if a key is found.
 * - `am_false` if the key is not found in the list.
 * - Traps if the search exceeds the reduction budget.
 * - Raises `BADARG` if `Pos` is invalid or if `List` is not a proper list.
 *
 * @details
 * - Validates that `Pos` is a small integer and greater than or equal to 1. Raises `BADARG` if invalid.
 * - Handles three cases for the key type:
 *   1. **Small integer keys**: Supports direct integer comparison and floating-point equivalence.
 *   2. **Immediate keys**: Compares the key directly to tuple elements.
 *   3. **Non-immediate keys**: Uses `CMP_EQ` for equivalence testing.
 * - Iterates over the list (`List`) while:
 *   - Extracting the tuple at each list element.
 *   - Comparing the element at position `Pos` within the tuple to `Key`.
 *   - Returning the tuple immediately if a match is found.
 * - Respects a reduction budget of `10 * CONTEXT_REDS`. Traps if the budget is exceeded, allowing resumption.
 * - If the list is improper, raises `BADARG`.
 * - If no match is found, returns `am_false`.
 *
 * @complexity O(n), where `n` is the number of elements in the list. The function may trap for large inputs.
 */
static Eterm
keyfind(Export *Bif, Process* p, Eterm Key, Eterm Pos, Eterm List)
{
    int max_iter = 10 * CONTEXT_REDS;
    Sint pos;
    Eterm term;

    if (!is_small(Pos) || (pos = signed_val(Pos)) < 1) {
    BIF_ERROR(p, BADARG);
    }

    if (is_small(Key)) {
    double float_key = (double) signed_val(Key);

    while (is_list(List)) {
        if (--max_iter < 0) {
        BUMP_ALL_REDS(p);
        BIF_TRAP3(Bif, p, Key, Pos, List);
        }
        term = CAR(list_val(List));
        List = CDR(list_val(List));
        if (is_tuple(term)) {
        Eterm *tuple_ptr = tuple_val(term);
        if (pos <= arityval(*tuple_ptr)) {
            Eterm element = tuple_ptr[pos];
            if (Key == element) {
            return term;
            } else if (is_float(element)) {
            FloatDef f;

            GET_DOUBLE(element, f);
            if (f.fd == float_key) {
                return term;
            }
            }
        }
        }
    }
    } else if (is_immed(Key)) {
    while (is_list(List)) {
        if (--max_iter < 0) {
        BUMP_ALL_REDS(p);
        BIF_TRAP3(Bif, p, Key, Pos, List);
        }
        term = CAR(list_val(List));
        List = CDR(list_val(List));
        if (is_tuple(term)) {
        Eterm *tuple_ptr = tuple_val(term);
        if (pos <= arityval(*tuple_ptr)) {
            Eterm element = tuple_ptr[pos];
            if (Key == element) {
            return term;
            }
        }
        }
    }
    } else {
    while (is_list(List)) {
        if (--max_iter < 0) {
        BUMP_ALL_REDS(p);
        BIF_TRAP3(Bif, p, Key, Pos, List);
        }
        term = CAR(list_val(List));
        List = CDR(list_val(List));
        if (is_tuple(term)) {
        Eterm *tuple_ptr = tuple_val(term);
        if (pos <= arityval(*tuple_ptr)) {
            Eterm element = tuple_ptr[pos];
            if (CMP_EQ(Key, element)) {
            return term;
            }
        }
        }
    }
    }

    if (is_not_nil(List))  {
    BIF_ERROR(p, BADARG);
    }
    return am_false;
}
