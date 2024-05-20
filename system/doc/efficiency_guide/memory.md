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
# Memory Usage

A good start when programming efficiently is to know how much memory different
data types and operations require. It is implementation-dependent how much
memory the Erlang data types and other items consume, but the following table
shows some figures for the `erts-8.0` system in OTP 19.0.

The unit of measurement is memory words. There exists both a 32-bit and a 64-bit
implementation. A word is therefore 4 bytes or 8 bytes, respectively. The value
for a running system can be determined by calling
[`erlang:system_info(wordsize)`](`m:erlang#system_info_wordsize`).


<table class="doc-table">
<tr>
  <td align="left" valign="middle"><strong>Data Type</strong></td>
  <td align="left" valign="middle"><strong>Memory Size</strong></td>
</tr>
<tr>
   <td align="left" valign="middle">Small integer</td>
   <td align="left" valign="middle">1 word.<br>
   On 32-bit architectures: -134217729 &lt; i &lt; 134217728
   (28 bits).<br>
   On 64-bit architectures: -576460752303423489 &lt; i &lt;
   576460752303423488 (60 bits).</td>
</tr>
<tr>
   <td align="left" valign="middle">Large integer</td>
   <td align="left" valign="middle">At least 3 words.</td>
</tr>
<tr>
   <td align="left" valign="middle">Atom</td>
   <td align="left" valign="middle">1 word.<br>
   An atom refers into an atom table, which also consumes memory.
   The atom text is stored once for each unique atom in this table.
   The atom table is <strong>not</strong> garbage-collected.</td>
</tr>
<tr>
   <td align="left" valign="middle">Float</td>
   <td align="left" valign="middle">On 32-bit architectures: 4 words.<br>
   On 64-bit architectures: 3 words.</td>
</tr>
<tr>
   <td align="left" valign="middle">Binary</td>
   <td align="left" valign="middle">3-6 words + data (can be shared).</td>
</tr>
<tr>
   <td align="left" valign="middle">List</td>
   <td align="left" valign="middle">1 word + 1 word per element + the size of each element.</td>
</tr>
<tr>
   <td align="left" valign="middle">String</td>
   <td align="left" valign="middle">(is the same as a list of integers)<br>
   1 word + 2 words per character.
   </td>
</tr>
<tr>
  <td align="left" valign="middle">Tuple</td>
  <td align="left" valign="middle">2 words + the size of each element.</td>
</tr>
<tr>
  <td align="left" valign="middle">Small Map</td>
  <td align="left" valign="middle">(up to 32 keys)<br>
    5 words + the size of all keys and values.</td>
</tr>
<tr>
  <td align="left" valign="middle">Large Map</td>
  <td align="left" valign="middle">
      (more than 32 keys)<br>
      <span class="code">N</span> x <span class="code">F</span> words + the size of all keys and values.<br>
      <span class="code">N</span> is the number of keys in the Map.<br>
      <span class="code">F</span> is a sparsity factor that varies between 1.6 and 1.8
      due to the probabilistic nature of the internal HAMT data structure.
  </td>
</tr>
<tr>
   <td align="left" valign="middle">Pid</td>
   <td align="left" valign="middle">1 word for a process identifier from the current local node.<br>
   On 32-bit: 6 words for a process identifier from another node.<br>
   On 64-bit: 5 words for a process identifier from another node.<br>
   A process identifier refers into a process table and a node table,
   which also consumes memory.</td>
</tr>
<tr>
   <td align="left" valign="middle">Port</td>
   <td align="left" valign="middle">1 word for a port identifier from the current local node.<br>
   5 words for a port identifier from another node.<br>
   A port identifier refers into a port table and a node table,
   which also consumes memory.</td>
</tr>
<tr>
   <td align="left" valign="middle">Reference</td>
   <td align="left" valign="middle">On 32-bit architectures: 4-7 words for a reference from the
   current local node, and 7-9 words for a reference from another
   node.<br>
   On 64-bit architectures: 4-6 words for a reference from the current
   local node, and 6-7 words for a reference from another node.<br>
   A reference also refers into more or less emulator internal data
   structures which also consumes memory. At a minimum it
   refers into the node tables.</td>
</tr>
<tr>
   <td align="left" valign="middle">Fun</td>
   <td align="left" valign="middle">9..13 words + the size of environment.<br>
   A fun refers into a fun table, which also consumes memory.</td>
</tr>
<tr>
   <td align="left" valign="middle">Ets table</td>
   <td align="left" valign="middle">Initially 768 words + the size of each element (6 words +
   the size of Erlang data). The table grows when necessary.</td>
</tr>
<tr>
   <td align="left" valign="middle">Erlang process</td>
   <td align="left" valign="middle">338 words when spawned, including a heap of 233 words.</td>
</tr>
</table>

_Table: Memory Size of Different Data Types_
