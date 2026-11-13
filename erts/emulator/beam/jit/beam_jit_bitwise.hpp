/*
 * %CopyrightBegin%
 *
 * SPDX-License-Identifier: Apache-2.0
 *
 * Copyright Ericsson AB 2026. All Rights Reserved.
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

#ifndef __BEAM_JIT_BITWISE_HPP__
#define __BEAM_JIT_BITWISE_HPP__

#include <type_traits>
#include <bit>

namespace Bitwise {

    template<int N, typename T>
    constexpr bool fits_signed(T value) noexcept {
        static_assert(N > 0);

        if constexpr (std::is_unsigned_v<T>) {
            return std::bit_width(value) < N;
        } else if constexpr (std::is_signed_v<T>) {
            if constexpr (sizeof(T) * 8 > N) {
                using U = std::make_unsigned<T>::type;
                T sign = value >> (sizeof(T) * 8 - 1);
                return std::bit_width(std::bit_cast<U>(value ^ sign)) < N;
            }

            return true;
        }
    }

    template<int N, typename T>
    constexpr bool fits_unsigned(T value) noexcept {
        static_assert(N > 0);

        if constexpr (std::is_unsigned_v<T>) {
            return std::bit_width(value) <= N;
        } else {
            using U = std::make_unsigned<T>::type;
            return value >= 0 && std::bit_width(std::bit_cast<U>(value)) <= N;
        }
    }

    template<typename T>
    constexpr bool is_power_of_2(T value) noexcept {
        using U = std::make_unsigned<T>::type;
        return std::has_single_bit(std::bit_cast<U>(value));
    }

    template<typename T>
    constexpr int clz(T value) noexcept {
        using U = std::make_unsigned<T>::type;
        return std::countl_zero(std::bit_cast<U>(value));
    }

    template<typename T>
    constexpr int ctz(T value) noexcept {
        using U = std::make_unsigned<T>::type;
        return std::countr_zero(std::bit_cast<U>(value));
    }

} // namespace Bitwise

#endif /* __BEAM_JIT_BITWISE_HPP__ */