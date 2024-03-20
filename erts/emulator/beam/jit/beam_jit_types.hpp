/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2023-2024. All Rights Reserved.
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

#ifndef __BEAM_JIT_TYPES_HPP__
#define __BEAM_JIT_TYPES_HPP__

/* Type-safe wrapper around the definitions in beam_types.h. We've redefined it
 * as an `enum class` to force the usage of our helpers, which lets us check
 * for common usage errors at compile time. */
enum class BeamTypeId : int {
    None = BEAM_TYPE_NONE,

    Atom = BEAM_TYPE_ATOM,
    Bitstring = BEAM_TYPE_BITSTRING,
    Cons = BEAM_TYPE_CONS,
    Float = BEAM_TYPE_FLOAT,
    Fun = BEAM_TYPE_FUN,
    Integer = BEAM_TYPE_INTEGER,
    Map = BEAM_TYPE_MAP,
    Nil = BEAM_TYPE_NIL,
    Pid = BEAM_TYPE_PID,
    Port = BEAM_TYPE_PORT,
    Reference = BEAM_TYPE_REFERENCE,
    Tuple = BEAM_TYPE_TUPLE,

    Any = BEAM_TYPE_ANY,

    Identifier = Pid | Port | Reference,
    List = Cons | Nil,
    Number = Float | Integer,

    /** @brief Types that can be boxed, including those that may also be
     * immediates (e.g. pids, integers). */
    MaybeBoxed = Bitstring | Float | Fun | Integer | Map | Pid | Port |
                 Reference | Tuple,
    /** @brief Types that can be immediates, including those that may also be
     * boxed (e.g. pids, integers). */
    MaybeImmediate = Atom | Integer | Nil | Pid | Port,

    /** @brief Types that are _always_ boxed. */
    AlwaysBoxed = MaybeBoxed & ~(Cons | MaybeImmediate),
    /** @brief Types that are _always_ immediates. */
    AlwaysImmediate = MaybeImmediate & ~(Cons | MaybeBoxed),
};

template<BeamTypeId... T>
struct BeamTypeIdUnion;

template<>
struct BeamTypeIdUnion<> {
    static constexpr BeamTypeId value() {
        return BeamTypeId::None;
    }
};

template<BeamTypeId T, BeamTypeId... Rest>
struct BeamTypeIdUnion<T, Rest...> : BeamTypeIdUnion<Rest...> {
    using integral = std::underlying_type_t<BeamTypeId>;
    using super = BeamTypeIdUnion<Rest...>;

    /* Overlapping type specifications are redundant at best and a subtle error
     * at worst. We've had several bugs where `Integer | MaybeBoxed` was used
     * instead of `Integer | AlwaysBoxed` or similar, and erroneously drew the
     * conclusion that the value is always an integer when not boxed, when it
     * could also be a pid or port. */
    static constexpr bool no_overlap =
            (static_cast<integral>(super::value()) &
             static_cast<integral>(T)) == BEAM_TYPE_NONE;
    static constexpr bool no_boxed_overlap =
            no_overlap || (super::value() != BeamTypeId::MaybeBoxed &&
                           T != BeamTypeId::MaybeBoxed);
    static constexpr bool no_immed_overlap =
            no_overlap || (super::value() != BeamTypeId::MaybeImmediate &&
                           T != BeamTypeId::MaybeImmediate);

    static_assert(no_boxed_overlap,
                  "types must not overlap, did you mean to use "
                  "BeamTypeId::AlwaysBoxed here?");
    static_assert(no_immed_overlap,
                  "types must not overlap, did you mean to use "
                  "BeamTypeId::AlwaysImmediate here?");
    static_assert(no_overlap || no_boxed_overlap || no_immed_overlap,
                  "types must not overlap");

    static constexpr bool is_single_typed() {
        constexpr auto V = static_cast<integral>(value());
        return (static_cast<integral>(V) & (static_cast<integral>(V) - 1)) ==
               BEAM_TYPE_NONE;
    }

    static constexpr BeamTypeId value() {
        return static_cast<BeamTypeId>(static_cast<integral>(super::value()) |
                                       static_cast<integral>(T));
    }
};

struct BeamArgType : public BeamType {
    BeamTypeId type() const {
        return static_cast<BeamTypeId>(BeamType::type_union);
    }

    bool hasLowerBound() const {
        return metadata_flags & BEAM_TYPE_HAS_LOWER_BOUND;
    }

    bool hasUpperBound() const {
        return metadata_flags & BEAM_TYPE_HAS_UPPER_BOUND;
    }

    bool hasUnit() const {
        return metadata_flags & BEAM_TYPE_HAS_UNIT;
    }

    auto max() const {
        ASSERT(hasUpperBound());
        return BeamType::max;
    }

    auto min() const {
        ASSERT(hasLowerBound());
        return BeamType::min;
    }

    auto unit() const {
        ASSERT(hasUnit());
        return BeamType::size_unit;
    }
};

static_assert(std::is_standard_layout<BeamArgType>::value);

#endif
