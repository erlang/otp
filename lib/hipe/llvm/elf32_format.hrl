%% -*- erlang-indent-level: 2 -*-

%%% @copyright 2011-2014 Yiannis Tsiouris <gtsiour@softlab.ntua.gr>,
%%%                      Chris Stavrakakis <hydralisk.r@gmail.com>
%%% @author Yiannis Tsiouris <gtsiour@softlab.ntua.gr>
%%%    [http://www.softlab.ntua.gr/~gtsiour/]

%%% @doc This header file contains very very useful macros for handling
%%%      various segments of an ELF-32 formated object file, such as sizes,
%%%      offsets and predefined constants. For further information about
%%%      each field take a quick look at
%%%      "[http://www.sco.com/developers/gabi/latest/contents.html]"
%%%      that contain the current HP/Intel definition of the ELF object
%%%      file format.

%%------------------------------------------------------------------------------
%% ELF-32 Data Types (in bytes)
%%------------------------------------------------------------------------------
-define(ELF_ADDR_SIZE,          4).
-define(ELF_OFF_SIZE,           4).
-define(ELF_HALF_SIZE,          2).
-define(ELF_WORD_SIZE,          4).
-define(ELF_SWORD_SIZE,         4).
-define(ELF_XWORD_SIZE,         ?ELF_WORD_SIZE). % for compatibility
-define(ELF_SXWORD_SIZE,        ?ELF_WORD_SIZE).
-define(ELF_UNSIGNED_CHAR_SIZE, 1).

%%------------------------------------------------------------------------------
%% ELF-32 Symbol Table Entries
%%------------------------------------------------------------------------------
%% Precomputed offset for Symbol Table entries in SymTab binary (needed because
%% of the different offsets in 32 and 64 bit formats).
-define(ST_NAME_OFFSET,  0).
-define(ST_VALUE_OFFSET, (?ST_NAME_OFFSET + ?ST_NAME_SIZE) ).
-define(ST_SIZE_OFFSET,  (?ST_VALUE_OFFSET + ?ST_VALUE_SIZE) ).
-define(ST_INFO_OFFSET,  (?ST_SIZE_OFFSET + ?ST_SIZE_SIZE) ).
-define(ST_OTHER_OFFSET, (?ST_INFO_OFFSET + ?ST_INFO_SIZE) ).
-define(ST_SHNDX_OFFSET, (?ST_OTHER_OFFSET + ?ST_OTHER_SIZE) ).

%%------------------------------------------------------------------------------
%% ELF-64 Relocation Entries
%%------------------------------------------------------------------------------
%% Useful macros to extract information from r_info field
-define(ELF_R_SYM(I),     (I bsr 8) ).
-define(ELF_R_TYPE(I),    (I band 16#ff) ).
-define(ELF_R_INFO(S, T), ((S bsl 8) + (T band 16#ff)) ).

%%------------------------------------------------------------------------------
%% ELF-64 Program Header Table
%%------------------------------------------------------------------------------
%% Offsets of various fields in a Program Header Table entry binary.
-define(P_TYPE_OFFSET,   0).
-define(P_OFFSET_OFFSET, (?P_FLAGS_OFFSET + ?P_FLAGS_SIZE) ).
-define(P_VADDR_OFFSET,  (?P_OFFSET_OFFSET + ?P_OFFSET_SIZE) ).
-define(P_PADDR_OFFSET,  (?P_VADDR_OFFSET + ?P_VADDR_SIZE) ).
-define(P_FILESZ_OFFSET, (?P_PVADDR_OFFSET + ?P_PVADDR_SIZE) ).
-define(P_MEMSZ_OFFSET,  (?P_FILESZ_OFFSET + ?P_FILESZ_SIZE) ).
-define(P_FLAGS_OFFSET,  (?P_TYPE_OFFSET + ?P_TYPE_SIZE) ).
-define(P_ALIGN_OFFSET,  (?P_MEMSZ_OFFSET + ?P_MEMSZ_SIZE) ).
