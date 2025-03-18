/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2022-2025. All Rights Reserved.
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

#ifndef __BEAM_JIT_ARGS_HPP__
#define __BEAM_JIT_ARGS_HPP__

struct ArgVal : public BeamOpArg {
    enum class Type : UWord {
        Word = TAG_u,
        XReg = TAG_x,
        YReg = TAG_y,
        FReg = TAG_l,
        Label = TAG_f,
        Literal = TAG_q,

        BytePtr = 'M',
        Catch = 'H',
        Export = 'E',
        FunEntry = 'F',
        Immediate = 'I'
    };

    constexpr ArgVal(UWord t, UWord value) : BeamOpArg{t, value} {
        ASSERT(t == (UWord)Type::Word || t == (UWord)Type::XReg ||
               t == (UWord)Type::YReg || t == (UWord)Type::FReg ||
               t == (UWord)Type::Label || t == (UWord)Type::Literal ||
               t == (UWord)Type::BytePtr || t == (UWord)Type::Catch ||
               t == (UWord)Type::Export || t == (UWord)Type::FunEntry ||
               t == (UWord)Type::Immediate);
    }

    constexpr ArgVal(Type t, UWord value) : ArgVal((UWord)t, value) {
    }

    constexpr Type getType() const {
        return (Type)type;
    }

    /* */

    constexpr bool isAtom() const {
        return isImmed() && is_atom(val);
    }

    constexpr bool isBytePtr() const {
        return getType() == Type::BytePtr;
    }

    constexpr bool isCatch() const {
        return getType() == Type::Catch;
    }

    constexpr bool isConstant() const {
        return isImmed() || isLiteral();
    }

    constexpr bool isExport() const {
        return getType() == Type::Export;
    }

    constexpr bool isImmed() const {
        return getType() == Type::Immediate;
    }

    constexpr bool isLabel() const {
        return getType() == Type::Label;
    }

    constexpr bool isLambda() const {
        return getType() == Type::FunEntry;
    }

    constexpr bool isLiteral() const {
        return getType() == Type::Literal;
    }

    constexpr bool isNil() const {
        return isImmed() && val == NIL;
    }

    constexpr bool isSmall() const {
        return isImmed() && is_small(val);
    }

    constexpr bool isSource() const {
        return isConstant() || isRegister();
    }

    constexpr bool isRegister() const {
        return isXRegister() || isYRegister() || isFRegister();
    }

    constexpr bool isXRegister() const {
        return getType() == Type::XReg;
    }

    constexpr bool isYRegister() const {
        return getType() == Type::YReg;
    }

    constexpr bool isFRegister() const {
        return getType() == Type::FReg;
    }

    constexpr bool isWord() const {
        return getType() == Type::Word;
    }

    struct Hash {
        constexpr size_t operator()(const ArgVal &key) const {
            return ((size_t)key.getType() << 48) ^ (size_t)key.val;
        }
    };

    constexpr bool operator==(const ArgVal &other) const {
        return getType() == other.getType() && val == other.val;
    }

    constexpr bool operator!=(const ArgVal &other) const {
        return !(*this == other);
    }

    enum class Relation { none, consecutive, reverse_consecutive };

    static Relation memory_relation(const ArgVal &lhs, const ArgVal &rhs) {
        if (lhs.isRegister() && lhs.getType() == rhs.getType()) {
            if ((lhs.val & REG_MASK) + 1 == (rhs.val & REG_MASK)) {
                return Relation::consecutive;
            } else if ((lhs.val & REG_MASK) == (rhs.val & REG_MASK) + 1) {
                return Relation::reverse_consecutive;
            }
        }

        return Relation::none;
    };

    template<typename T>
    constexpr T as() const {
        return static_cast<T>(*this);
    }
};

struct ArgBytePtr : public ArgVal {
    template<typename T>
    constexpr ArgBytePtr(const T &other) : ArgVal(other) {
        ASSERT(isBytePtr());
    }

    constexpr auto get() const {
        return val;
    }
};

struct ArgCatch : public ArgVal {
    template<typename T>
    constexpr ArgCatch(const T &other) : ArgVal(other) {
        ASSERT(isCatch());
    }

    constexpr auto get() const {
        return val;
    }
};

struct ArgExport : public ArgVal {
    template<typename T>
    constexpr ArgExport(const T &other) : ArgVal(other) {
        ASSERT(isExport());
    }

    constexpr auto get() const {
        return val;
    }
};

struct ArgLabel : public ArgVal {
    template<typename T>
    constexpr ArgLabel(const T &other) : ArgVal(other) {
        ASSERT(isLabel());
    }

    constexpr auto get() const {
        return val;
    }
};

struct ArgLambda : public ArgVal {
    template<typename T>
    constexpr ArgLambda(const T &other) : ArgVal(other) {
        ASSERT(isLambda());
    }

    constexpr auto get() const {
        return val;
    }
};

struct ArgWord : public ArgVal {
    /* Allows explicit construction from any integral type. */
    template<typename T,
             std::enable_if_t<std::is_integral<T>::value, bool> = true>
    constexpr explicit ArgWord(T value) : ArgVal(ArgVal::Type::Word, value) {
    }

    template<typename T,
             std::enable_if_t<!std::is_integral<T>::value, bool> = true>
    constexpr ArgWord(const T &other) : ArgVal(other) {
        ASSERT(isWord());
    }

    template<typename T>
    constexpr ArgWord operator+(T value) const {
        return ArgWord(val + value);
    }

    constexpr auto get() const {
        return val;
    }
};

struct ArgSource : public ArgVal {
    template<typename T>
    constexpr ArgSource(const T &other) : ArgVal(other) {
        ASSERT(isSource());
    }
};

struct ArgRegister : public ArgSource {
    template<typename T>
    constexpr ArgRegister(const T &other) : ArgSource(other) {
        ASSERT(isRegister());
    }

    constexpr int typeIndex() const {
        return (int)(val >> 10);
    }

    constexpr ArgVal trimmed(int n) const {
        if (isYRegister()) {
            return ArgVal(Type::YReg, UWord((val & REG_MASK) - n));
        } else {
            return *this;
        }
    }

    template<typename T>
    constexpr T copy(int n) const {
        return T(n | (val & ~REG_MASK));
    }
};

struct ArgXRegister : public ArgRegister {
    /* Allows explicit construction from any integral type. */
    template<typename T,
             std::enable_if_t<std::is_integral<T>::value, bool> = true>
    constexpr explicit ArgXRegister(T reg)
            : ArgRegister(ArgVal(ArgVal::Type::XReg, reg)) {
    }

    template<typename T,
             std::enable_if_t<!std::is_integral<T>::value, bool> = true>
    constexpr ArgXRegister(const T &other) : ArgRegister(other) {
        ASSERT(isXRegister());
    }

    constexpr auto get() const {
        return val & REG_MASK;
    }
};

struct ArgYRegister : public ArgRegister {
    /* Allows explicit construction from any integral type. */
    template<typename T,
             std::enable_if_t<std::is_integral<T>::value, bool> = true>
    constexpr explicit ArgYRegister(T reg)
            : ArgRegister(ArgVal(ArgVal::Type::YReg, reg)) {
    }

    template<typename T,
             std::enable_if_t<!std::is_integral<T>::value, bool> = true>
    constexpr ArgYRegister(const T &other) : ArgRegister(other) {
        ASSERT(isYRegister());
    }

    constexpr auto get() const {
        return val & REG_MASK;
    }
};

struct ArgFRegister : public ArgRegister {
    template<typename T>
    constexpr ArgFRegister(const T &other) : ArgRegister(other) {
        ASSERT(isFRegister());
    }

    constexpr auto get() const {
        return val & REG_MASK;
    }
};

struct ArgConstant : public ArgSource {
    template<typename T>
    constexpr ArgConstant(const T &other) : ArgSource(other) {
        ASSERT(isConstant());
    }
};

struct ArgImmed : public ArgConstant {
    constexpr explicit ArgImmed(Eterm term)
            : ArgConstant(ArgVal(ArgVal::Type::Immediate, term)) {
        ASSERT(isImmed());
    }

    template<typename T>
    constexpr ArgImmed(const T &other) : ArgConstant(other) {
        ASSERT(isImmed());
    }

    constexpr auto get() const {
        return val;
    }
};

struct ArgLiteral : public ArgConstant {
    template<typename T>
    constexpr ArgLiteral(const T &other) : ArgConstant(other) {
        ASSERT(isLiteral());
    }

    constexpr auto get() const {
        return val;
    }
};

struct ArgAtom : public ArgImmed {
    template<typename T>
    constexpr ArgAtom(const T &other) : ArgImmed(other) {
        ASSERT(isAtom());
    }

    constexpr Eterm get() const {
        return val;
    }
};

struct ArgNil : public ArgImmed {
    constexpr explicit ArgNil()
            : ArgImmed(ArgVal(ArgVal::Type::Immediate, NIL)) {
    }

    template<typename T>
    constexpr ArgNil(const T &other) : ArgImmed(other) {
        ASSERT(isNil());
    }

    constexpr Eterm get() const {
        return val;
    }
};

struct ArgSmall : public ArgImmed {
    template<typename T>
    constexpr ArgSmall(const T &other) : ArgImmed(other) {
        ASSERT(isSmall());
    }

    constexpr Sint getSigned() const {
        /* `signed_val` is not constexpr in debug builds, so we'll do it
         * manually as we already know for certain that this is a small. */
        return ((Sint)val) >> _TAG_IMMED1_SIZE;
    }

    constexpr Uint getUnsigned() const {
        return ((Uint)val) >> _TAG_IMMED1_SIZE;
    }

    constexpr Eterm get() const {
        return val;
    }
};

#endif
