/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2024-2025. All Rights Reserved.
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

#ifndef __BEAM_JIT_REGISTER_CACHE_HPP__
#define __BEAM_JIT_REGISTER_CACHE_HPP__

#include <algorithm>
#include <limits>

template<int Size, typename Mem, typename Reg>
class RegisterCache {
    struct CacheEntry {
        Mem mem;
        Reg reg;
    };

    CacheEntry cache[Size];
    int entries;
    size_t position;
    const std::vector<Reg> temporaries;
    const Reg xregs_base;
    const Reg yregs_base;

    const CacheEntry *cbegin() const {
        return std::begin(cache);
    }

    const CacheEntry *cend() const {
        return cbegin() + entries;
    }

    CacheEntry *begin() {
        return std::begin(cache);
    }

    CacheEntry *end() {
        return begin() + entries;
    }

    bool isCached(const Reg &reg) const {
        return std::any_of(cbegin(), cend(), [&](const auto &entry) {
            return reg == entry.reg;
        });
    }

public:
    RegisterCache(Reg xregs, Reg yregs, std::initializer_list<Reg> &&tmps)
            : entries(0), position(std::numeric_limits<size_t>::max()),
              temporaries(tmps), xregs_base(xregs), yregs_base(yregs) {
    }

    void consolidate(size_t offset) {
        if (!validAt(offset)) {
            invalidate();
        }

        position = offset;
    }

    Reg find(size_t offset, Mem mem) {
        ASSERT(mem.hasBase());
        consolidate(offset);

        auto it = std::find_if(cbegin(), cend(), [&](const auto &entry) {
            return mem == entry.mem;
        });

        if (it != cend()) {
            ASSERT(it->reg.isValid());
            return it->reg;
        }

        return Reg();
    }

    void invalidate(Mem mem) {
        ASSERT(mem.hasBase());

        auto i = 0;
        while (i < entries) {
            auto &entry = cache[i];

            if (entry.mem == mem) {
                entry = cache[entries - 1];
                entries--;
                break;
            }

            i++;
        }
    }

    void invalidate(Reg reg) {
        ASSERT(reg.isValid());

        auto i = 0;
        while (i < entries) {
            auto &entry = cache[i];

            if (reg == entry.reg) {
                entry = cache[entries - 1];
                entries--;
                continue;
            }

            i++;
        }
    }

    void invalidate() {
        position = std::numeric_limits<size_t>::max();
        entries = 0;
    }

    void trim_yregs(int64_t offset) {
        for (int i = 0; i < entries; i++) {
            auto &entry = cache[i];

            if (entry.mem.hasBase() && entry.mem.baseReg() == yregs_base) {
                entry.mem = entry.mem.cloneAdjusted(offset);
            }
        }
    }

    template<typename Operand, typename... Operands>
    void invalidate(Operand op, Operands... rest) {
        invalidate(op);
        invalidate(rest...);
    }

    /* Pick a temporary register among the list passed to our constructor,
     * preferring those not present in the cache. */
    Reg allocate(size_t offset) {
        consolidate(offset);

        auto it = std::find_if(temporaries.cbegin(),
                               temporaries.cend(),
                               [&](const auto &reg) {
                                   return !isCached(reg);
                               });

        if (it != temporaries.cend()) {
            ASSERT(std::none_of(cbegin(), cend(), [&](const auto &entry) {
                return (*it == entry.reg) ||
                       (entry.mem.hasBase() && entry.mem.baseReg() == *it);
            }));

            return *it;
        }

        return temporaries.front();
    }

    void put(Mem mem, Reg reg) {
        ASSERT(mem.hasBase());

        /* Only allow caching of X and Y registers. */
        auto base = mem.baseReg();
        if (base != xregs_base && base != yregs_base) {
            return;
        }

        ASSERT(mem.baseReg() != reg);

        auto it = std::find_if(begin(), end(), [&](const auto &entry) {
            return mem == entry.mem;
        });

        if (it == end()) {
            if (it == std::end(cache)) {
                ASSERT(entries == Size);
                it = std::begin(cache);
            } else {
                ASSERT(entries < Size);
                entries++;
            }
        }

        it->reg = reg;
        it->mem = mem;
    }

    bool validAt(size_t offset) const {
        return position == offset;
    }

    void update(size_t offset) {
        position = offset;
    }
};

#endif
