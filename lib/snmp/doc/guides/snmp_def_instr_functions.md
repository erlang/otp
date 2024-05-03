<!--
%CopyrightBegin%

Copyright Ericsson AB 2023-2024. All Rights Reserved.

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.

%CopyrightEnd%
-->
# Definition of Instrumentation Functions

The section _Definition of Instrumentation Functions_ describes the user defined
functions, which the agent calls at different times.

## Variable Instrumentation

For scalar variables, a function `f(Operation, ...)` must be defined.

The `Operation` can be `new`, `delete`, `get`, `is_set_ok`, `set`, or `undo`.

In case of an error, all instrumentation functions may return either an SNMPv1
or an SNMPv2 error code. If it returns an SNMPv2 code, it is converted into an
SNMPv1 code before it is sent to a SNMPv1 manager. It is recommended to use the
SNMPv2 error codes for all instrumentation functions, as these provide more
details. See [Appendix A](snmp_app_a.md) for a description of error code
conversions.

### f(new \[, ExtraArgs])

The function `f(new [, ExtraArgs])` is called for each variable in the MIB when
the MIB is loaded into the agent. This makes it possible to perform necessary
initialization.

This function is optional. The return value is discarded.

### f(delete \[, ExtraArgs])

The function `f(delete [, ExtraArgs])` is called for each object in an MIB when
the MIB is unloaded from the agent. This makes it possible to perform necessary
clean-up.

This function is optional. The return value is discarded.

### f(get \[, ExtraArgs])

The function `f(get [, ExtraArgs])` is called when a get-request or a get-next
request refers to the variable.

This function is mandatory.

#### Valid Return Values

- `{value, Value}`. The `Value` must be of correct type, length and within
  ranges, otherwise `genErr` is returned in the response PDU. If the object is
  an enumerated integer, the symbolic enum value may be used as an atom. If the
  object is of type BITS, the return value shall be an integer or a list of bits
  that are set.
- `{noValue, noSuchName}`(SNMPv1)
- `{noValue, noSuchObject | noSuchInstance} `(SNMPv2)
- `genErr`. Used if an error occurred. Note, this should be an internal
  processing error, e.g. a caused by a programming fault somewhere. If the
  variable does not exist, use `{noValue, noSuchName}` or
  `{noValue, noSuchInstance}`.

### f(is_set_ok, NewValue \[, ExtraArgs])

The function `f(is_set_ok, NewValue [, ExtraArgs])` is called in phase one of
the set-request processing so that the new value can be checked for
inconsistencies.

`NewValue` is guaranteed to be of the correct type, length and within ranges, as
specified in the MIB. If the object is an enumerated integer or of type BITS,
the integer value is used.

This function is optional.

If this function is called, it will be called again, either with `undo` or with
`set` as first argument.

#### Valid return values

- `noError`
- `badValue | noSuchName | genErr`(SNMPv1)
- `noAccess | noCreation | inconsistentValue | resourceUnavailable | inconsistentName | genErr`(SNMPv2)

### f(undo, NewValue \[, ExtraArgs])

If an error occurred, this function is called after the `is_set_ok` function is
called. If `set` is called for this object, `undo` is not called.

`NewValue` is guaranteed to be of the correct type, length and within ranges, as
specified in the MIB. If the object is an enumerated integer or of type BITS,
the integer value is used.

This function is optional.

#### Valid return values

- `noError`
- `genErr`(SNMPv1)
- `undoFailed | genErr`(SNMPv2)

### f(set, NewValue \[, ExtraArgs])

This function is called to perform the set in phase two of the set-request
processing. It is only called if the corresponding `is_set_ok` function is
present and returns `noError`.

`NewValue` is guaranteed to be of the correct type, length and within ranges, as
specified in the MIB. If the object is an enumerated integer or of type BITS,
the integer value is used.

This function is mandatory.

#### Valid return values

- `noError`
- `genErr`(SNMPv1)
- `commitFailed | undoFailed | genErr`(SNMPv2)

## Table Instrumentation

For tables, a `f(Operation, ...)` function should be defined (the function shown
is exemplified with `f`).

The `Operation` can be `new`, `delete`, `get`, `next`, `is_set_ok`, `undo` or
`set`.

In case of an error, all instrumentation functions may return either an SNMPv1
or an SNMPv2 error code. If it returns an SNMPv2 code, it is converted into an
SNMPv1 code before it is sent to a SNMPv1 manager. It is recommended to use the
SNMPv2 error codes for all instrumentation functions, as these provide more
details. See [Appendix A](snmp_app_a.md) for a description of error code
conversions.

### f(new \[, ExtraArgs])

The function `f(new [, ExtraArgs])` is called for each object in an MIB when the
MIB is loaded into the agent. This makes it possible to perform the necessary
initialization.

This function is optional. The return value is discarded.

### f(delete \[, ExtraArgs])

The function `f(delete [, ExtraArgs])` is called for each object in an MIB when
the MIB is unloaded from the agent. This makes it possible to perform any
necessary clean-up.

This function is optional. The return value is discarded.

### f(get, RowIndex, Cols \[, ExtraArgs])

The function `f(get, RowIndex, Cols [, ExtraArgs])` is called when a get-request
refers to a table.

This function is mandatory.

#### Arguments

- `RowIndex` is a list of integers which define the key values for the row. The
  `RowIndex` is the list representation (list of integers) which follow the
  `Cols` integer in the OBJECT IDENTIFIER.
- `Cols` is a list of integers which represent the column numbers. The `Cols`
  are sorted by increasing value and are guaranteed to be valid column numbers.

#### Valid Return Values

- A list with as many elements as the `Cols` list, where each element is the
  value of the corresponding column. Each element can be:

  - `{value, Value}`. The `Value` must be of correct type, length and within
    ranges, otherwise `genErr` is returned in the response PDU. If the object is
    an enumerated integer, the symbolic enum value may be used (as an atom). If
    the object is of type BITS, the return value shall be an integer or a list
    of bits that are set.
  - `{noValue, noSuchName}`(SNMPv1)
  - `{noValue, noSuchObject | noSuchInstance}`(SNMPv2)

- `{noValue, Error}`. If the row does not exist, because all columns have
  `{noValue, Error}`), the single tuple `{noValue, Error}` can be returned. This
  is a shorthand for a list with all elements `{noValue, Error}`.
- `genErr`. Used if an error occurred. Note that this should be an internal
  processing error, e.g. a caused by a programming fault somewhere. If some
  column does not exist, use `{noValue, noSuchName}` or
  `{noValue, noSuchInstance}`.

### f(get_next, RowIndex, Cols \[, ExtraArgs])

The function `f(get_next, RowIndex, Cols [, ExtraArgs])` is called when a
get-next- or a get-bulk-request refers to the table.

The `RowIndex` argument may refer to an existing row or a non-existing row, or
it may be unspecified. The `Cols` list may refer to inaccessible columns or
non-existing columns. For each column in the `Cols` list, the corresponding next
instance is determined, and the last part of its OBJECT IDENTIFIER and its value
is returned.

This function is mandatory.

#### Arguments

- `RowIndex` is a list of integers (possibly empty) that defines the key values
  for a row. The `RowIndex` is the list representation (list of integers), which
  follow the `Cols` integer in the OBJECT IDENTIFIER.
- `Cols` is a list of integers, greater than or equal to zero, which represents
  the column numbers.

#### Valid Return Values

- A list with as many elements as the `Cols` list Each element can be:

  - `{NextOid, NextValue}`, where `NextOid` is the lexicographic next OBJECT
    IDENTIFIER for the corresponding column. This should be specified as the
    OBJECT IDENTIFIER part following the table entry. This means that the first
    integer is the column number and the rest is a specification of the keys.
    `NextValue` is the value of this element.
  - `endOfTable` if there are no accessible elements after this one.

- `{genErr, Column}` where `Column` denotes the column that caused the error.
  `Column` must be one of the columns in the `Cols` list. Note that this should
  be an internal processing error, e.g. a caused by a programming fault
  somewhere. If some column does not exist, you must return the next accessible
  element (or `endOfTable`).

### f(is_set_ok, RowIndex, Cols \[, ExtraArgs])

The function `f(is_set_ok, RowIndex, Cols [, ExtraArgs])` is called in phase one
of the set-request processing so that new values can be checked for
inconsistencies.

If the function is called, it will be called again with `undo`, or with `set` as
first argument.

This function is optional.

#### Arguments

- `RowIndex` is a list of integers which define the key values for the row. The
  `RowIndex` is the list representation (list of integers) which follow the
  `Cols` integer in the OBJECT IDENTIFIER.
- `Cols` is a list of `{Column, NewValue}`, where `Column` is an integer, and
  `NewValue` is guaranteed to be of the correct type, length and within ranges,
  as specified in the MIB. If the object is an enumerated integer or of type
  BITS, the integer value is used. The list is sorted by `Column` (increasing)
  and each `Column` is guaranteed to be a valid column number.

#### Valid Return Values

- `{noError, 0}`
- `{Error, Column}`, where `Error` is the same as for `is_set_ok` for variables,
  and `Column` denotes the faulty column. `Column` must be one of the columns in
  the `Cols` list.

### f(undo, RowIndex, Cols \[, ExtraArgs])

If an error occurs, The function `f(undo, RowIndex, Cols [, ExtraArgs])` is
called after the `is_set_ok` function. If `set` is called for this object,
`undo` is not called.

This function is optional.

#### Arguments

- `RowIndex` is a list of integers which define the key values for the row. The
  `RowIndex` is the list representation (list of integers) which follow the
  `Cols` integer in the OBJECT IDENTIFIER.
- `Cols` is a list of `{Column, NewValue}`, where `Column` is an integer, and
  `NewValue` is guaranteed to be of the correct type, length and within ranges,
  as specified in the MIB. If the object is an enumerated integer or of type
  BITS, the integer value is used. The list is sorted by `Column` (increasing)
  and each `Column` is guaranteed to be a valid column number.

#### Valid Return Values

- `{noError, 0}`
- `{Error, Column}` where `Error` is the same as for `undo` for variables, and
  `Column` denotes the faulty column. `Column` must be one of the columns in the
  `Cols` list.

### f(set, RowIndex, Cols \[, ExtraArgs])

The function `f(set, RowIndex, Cols [, ExtraArgs])` is called to perform the set
in phase two of the set-request processing. It is only called if the
corresponding `is_set_ok` function did not exist, or returned `{noError, 0}`.

This function is mandatory.

#### Arguments

- `RowIndex` is a list of integers that define the key values for the row. The
  `RowIndex` is the list representation (list of integers) which follow the
  `Cols` integer in the OBJECT IDENTIFIER.
- `Cols` is a list of `{Column, NewValue}`, where `Column` is an integer, and
  `NewValue` is guaranteed to be of the correct type, length and within ranges,
  as specified in the MIB. If the object is an enumerated integer or of type
  BITS, the integer value is used. The list is sorted by `Column` (increasing)
  and each `Column` is guaranteed to be a valid column number.

#### Valid Return Values

- `{noError, 0}`
- `{Error, Column}` where `Error` is the same as `set` for variables, and
  `Column` denotes the faulty column. `Column` must be one of the columns in the
  `Cols` list.
