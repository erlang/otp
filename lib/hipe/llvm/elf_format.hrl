%% -*- erlang-indent-level: 2 -*-

%%% @copyright 2011-2014 Yiannis Tsiouris <gtsiour@softlab.ntua.gr>,
%%%                      Chris Stavrakakis <hydralisk.r@gmail.com>
%%% @author Yiannis Tsiouris <gtsiour@softlab.ntua.gr>
%%%    [http://www.softlab.ntua.gr/~gtsiour/]

%%------------------------------------------------------------------------------
%%
%%                        ELF Header File
%%
%%------------------------------------------------------------------------------

-ifdef(BIT32).
-include("elf32_format.hrl"). % ELF32-specific definitions.
-else.
-include("elf64_format.hrl"). % ELF64-specific definitions.
-endif.

%%------------------------------------------------------------------------------
%% ELF Data Types (in bytes)
%%------------------------------------------------------------------------------
%%XXX: Included in either elf32_format or elf64_format.

%%------------------------------------------------------------------------------
%% ELF File Header
%%------------------------------------------------------------------------------
-define(ELF_EHDR_SIZE, (?E_IDENT_SIZE + ?E_TYPE_SIZE + ?E_MACHINE_SIZE
			 +?E_VERSION_SIZE + ?E_ENTRY_SIZE + ?E_PHOFF_SIZE
			 +?E_SHOFF_SIZE + ?E_FLAGS_SIZE + ?E_EHSIZE_SIZE
			 +?E_PHENTSIZE_SIZE + ?E_PHNUM_SIZE + ?E_SHENTSIZE_SIZE
			 +?E_SHNUM_SIZE + ?E_SHSTRNDX_SIZE) ).

-define(E_IDENT_SIZE,     (16 * ?ELF_UNSIGNED_CHAR_SIZE) ).
-define(E_TYPE_SIZE,      ?ELF_HALF_SIZE).
-define(E_MACHINE_SIZE,   ?ELF_HALF_SIZE).
-define(E_VERSION_SIZE,   ?ELF_WORD_SIZE).
-define(E_ENTRY_SIZE,     ?ELF_ADDR_SIZE).
-define(E_PHOFF_SIZE,     ?ELF_OFF_SIZE).
-define(E_SHOFF_SIZE,     ?ELF_OFF_SIZE).
-define(E_FLAGS_SIZE,     ?ELF_WORD_SIZE).
-define(E_EHSIZE_SIZE,    ?ELF_HALF_SIZE).
-define(E_PHENTSIZE_SIZE, ?ELF_HALF_SIZE).
-define(E_PHNUM_SIZE,     ?ELF_HALF_SIZE).
-define(E_SHENTSIZE_SIZE, ?ELF_HALF_SIZE).
-define(E_SHNUM_SIZE,     ?ELF_HALF_SIZE).
-define(E_SHSTRNDX_SIZE,  ?ELF_HALF_SIZE).

%% Useful arithmetics for computing byte offsets for various File Header
%% entries from a File Header (erlang) binary
-define(E_IDENT_OFFSET,     0).
-define(E_TYPE_OFFSET,      (?E_IDENT_OFFSET + ?E_IDENT_SIZE) ).
-define(E_MACHINE_OFFSET,   (?E_TYPE_OFFSET + ?E_TYPE_SIZE) ).
-define(E_VERSION_OFFSET,   (?E_MACHINE_OFFSET + ?E_MACHINE_SIZE) ).
-define(E_ENTRY_OFFSET,     (?E_VERSION_OFFSET + ?E_VERSION_SIZE) ).
-define(E_PHOFF_OFFSET,     (?E_ENTRY_OFFSET + ?E_ENTRY_SIZE) ).
-define(E_SHOFF_OFFSET,     (?E_PHOFF_OFFSET + ?E_PHOFF_SIZE) ).
-define(E_FLAGS_OFFSET,     (?E_SHOFF_OFFSET + ?E_SHOFF_SIZE) ).
-define(E_EHSIZE_OFFSET,    (?E_FLAGS_OFFSET + ?E_FLAGS_SIZE) ).
-define(E_PHENTSIZE_OFFSET, (?E_EHSIZE_OFFSET + ?E_EHSIZE_SIZE) ).
-define(E_PHNUM_OFFSET,     (?E_PHENTSIZE_OFFSET + ?E_PHENTSIZE_SIZE) ).
-define(E_SHENTSIZE_OFFSET, (?E_PHNUM_OFFSET + ?E_PHNUM_SIZE) ).
-define(E_SHNUM_OFFSET,     (?E_SHENTSIZE_OFFSET + ?E_SHENTSIZE_SIZE) ).
-define(E_SHSTRNDX_OFFSET,  (?E_SHNUM_OFFSET + ?E_SHNUM_SIZE) ).

%% Name aliases of File Header fields information used in get_header_field
%% function of elf64_format module.
-define(E_IDENT,     {?E_IDENT_OFFSET, ?E_IDENT_SIZE}).
-define(E_TYPE,      {?E_TYPE_OFFSET, ?E_TYPE_SIZE}).
-define(E_MACHINE,   {?E_MACHINE_OFFSET, ?E_MACHINE_SIZE}).
-define(E_VERSION,   {?E_VERSION_OFFSET, ?E_VERSION_SIZE}).
-define(E_ENTRY,     {?E_ENTRY_OFFSET, ?E_ENTRY_SIZE}).
-define(E_PHOFF,     {?E_PHOFF_OFFSET, ?E_PHOFF_SIZE}).
-define(E_SHOFF,     {?E_SHOFF_OFFSET, ?E_SHOFF_SIZE}).
-define(E_FLAGS,     {?E_FLAGS_OFFSET, ?E_FLAGS_SIZE}).
-define(E_EHSIZE,    {?E_EHSIZE_OFFSET, ?E_EHSIZE_SIZE}).
-define(E_PHENTSIZE, {?E_PHENTSIZE_OFFSET, ?E_PHENTSIZE_SIZE}).
-define(E_PHNUM,     {?E_PHNUM_OFFSET, ?E_PHNUM_SIZE}).
-define(E_SHENTSIZE, {?E_SHENTSIZE_OFFSET, ?E_SHENTSIZE_SIZE}).
-define(E_SHNUM,     {?E_SHNUM_OFFSET, ?E_SHNUM_SIZE}).
-define(E_SHSTRNDX,  {?E_SHSTRNDX_OFFSET, ?E_SHSTRNDX_SIZE}).

%% ELF Identification (e_ident)
-define(EI_MAG0,       0).
-define(EI_MAG1,       1).
-define(EI_MAG2,       2).
-define(EI_MAG3,       3).
-define(EI_CLASS,      4).
-define(EI_DATA,       5).
-define(EI_VERSION,    6).
-define(EI_OSABI,      7).
-define(EI_ABIVERSION, 8).
-define(EI_PAD,        9).
-define(EI_NIDENT,     16).

%% Object File Classes (e_ident[EI_CLASS])
-define(ELFCLASSNONE, 0).
-define(ELFCLASS32,   1).
-define(ELFCLASS64,   2).

%% Data Encodings (e_ident[EI_DATA])
-define(ELFDATA2LSB, 1).
-define(ELFDATA2MSB, 2).

%% Operating System and ABI Identifiers (e_ident[EI_OSABI])
-define(ELFOSABI_SYSV,       0).
-define(ELFOSABI_HPUX,       1).
-define(ELFOSABI_STANDALONE, 255).

%% Object File Types (e_type)
-define(ET_NONE,   0).
-define(ET_REL,    1).
-define(ET_EXEC,   2).
-define(ET_DYN,    3).
-define(ET_CORE,   4).
-define(ET_LOOS,   16#FE00).
-define(ET_HIOS,   16#FEFF).
-define(ET_LOPROC, 16#FF00).
-define(ET_HIPROC, 16#FFFF).

%%------------------------------------------------------------------------------
%% ELF Section Header
%%------------------------------------------------------------------------------
-define(ELF_SHDRENTRY_SIZE, (?SH_NAME_SIZE + ?SH_TYPE_SIZE + ?SH_FLAGS_SIZE
			      +?SH_ADDR_SIZE + ?SH_OFFSET_SIZE + ?SH_SIZE_SIZE
			      +?SH_LINK_SIZE + ?SH_INFO_SIZE
			      +?SH_ADDRALIGN_SIZE + ?SH_ENTSIZE_SIZE) ).

-define(SH_NAME_SIZE,      ?ELF_WORD_SIZE).
-define(SH_TYPE_SIZE,      ?ELF_WORD_SIZE).
-define(SH_FLAGS_SIZE,     ?ELF_XWORD_SIZE).
-define(SH_ADDR_SIZE,      ?ELF_ADDR_SIZE).
-define(SH_OFFSET_SIZE,    ?ELF_OFF_SIZE).
-define(SH_SIZE_SIZE,      ?ELF_XWORD_SIZE).
-define(SH_LINK_SIZE,      ?ELF_WORD_SIZE).
-define(SH_INFO_SIZE,      ?ELF_WORD_SIZE).
-define(SH_ADDRALIGN_SIZE, ?ELF_XWORD_SIZE).
-define(SH_ENTSIZE_SIZE,   ?ELF_XWORD_SIZE).

%% Useful arithmetics for computing byte offsets for various fields from a
%% Section Header Entry (erlang) binary
-define(SH_NAME_OFFSET,      0).
-define(SH_TYPE_OFFSET,      (?SH_NAME_OFFSET + ?SH_NAME_SIZE) ).
-define(SH_FLAGS_OFFSET,     (?SH_TYPE_OFFSET + ?SH_TYPE_SIZE) ).
-define(SH_ADDR_OFFSET,      (?SH_FLAGS_OFFSET + ?SH_FLAGS_SIZE) ).
-define(SH_OFFSET_OFFSET,    (?SH_ADDR_OFFSET + ?SH_ADDR_SIZE) ).
-define(SH_SIZE_OFFSET,      (?SH_OFFSET_OFFSET + ?SH_OFFSET_SIZE) ).
-define(SH_LINK_OFFSET,      (?SH_SIZE_OFFSET + ?SH_SIZE_SIZE) ).
-define(SH_INFO_OFFSET,      (?SH_LINK_OFFSET + ?SH_LINK_SIZE) ).
-define(SH_ADDRALIGN_OFFSET, (?SH_INFO_OFFSET + ?SH_INFO_SIZE) ).
-define(SH_ENTSIZE_OFFSET,   (?SH_ADDRALIGN_OFFSET + ?SH_ADDRALIGN_SIZE) ).

%% Name aliases of Section Header Table entry information used in
%% get_shdrtab_entry function of elf64_format module.
-define(SH_NAME,      {?SH_NAME_OFFSET, ?SH_NAME_SIZE}).
-define(SH_TYPE,      {?SH_TYPE_OFFSET, ?SH_TYPE_SIZE}).
-define(SH_FLAGS,     {?SH_FLAGS_OFFSET, ?SH_FLAGS_SIZE}).
-define(SH_ADDR,      {?SH_ADDR_OFFSET, ?SH_ADDR_SIZE}).
-define(SH_OFFSET,    {?SH_OFFSET_OFFSET, ?SH_OFFSET_SIZE}).
-define(SH_SIZE,      {?SH_SIZE_OFFSET, ?SH_SIZE_SIZE}).
-define(SH_LINK,      {?SH_LINK_OFFSET, ?SH_LINK_SIZE}).
-define(SH_INFO,      {?SH_INFO_OFFSET, ?SH_INFO_SIZE}).
-define(SH_ADDRALIGN, {?SH_ADDRALIGN_OFFSET, ?SH_ADDRALIGN_SIZE}).
-define(SH_ENTSIZE,   {?SH_ENTSIZE_OFFSET, ?SH_ENTSIZE_SIZE}).

%% Section Indices
-define(SHN_UNDEF,    0).
-define(SHN_LOPROC,   16#FF00).
-define(SHN_HIPROC,   16#FF1F).
-define(SHN_LOOS,     16#FF20).
-define(SHN_HIOS,     16#FF3F).
-define(SHN_ABS,      16#FFF1).
-define(SHN_COMMON,   16#FFF2).

%% Section Types (sh_type)
-define(SHT_NULL,     0).
-define(SHT_PROGBITS, 1).
-define(SHT_SYMTAB,   2).
-define(SHT_STRTAB,   3).
-define(SHT_RELA,     4).
-define(SHT_HASH,     5).
-define(SHT_DYNAMIC,  6).
-define(SHT_NOTE,     7).
-define(SHT_NOBITS,   8).
-define(SHT_REL,      9).
-define(SHT_SHLIB,    10).
-define(SHT_DYNSYM,   11).
-define(SHT_LOOS,     16#60000000).
-define(SHT_HIOS,     16#6FFFFFFF).
-define(SHT_LOPROC,   16#70000000).
-define(SHT_HIPROC,   16#7FFFFFFF).

%% Section Attributes (sh_flags)
-define(SHF_WRITE,     16#1).
-define(SHF_ALLOC,     16#2).
-define(SHF_EXECINSTR, 16#4).
-define(SHF_MASKOS,    16#0F000000).
-define(SHF_MASKPROC,  16#F0000000).

%%
%% Standard Section names for Code and Data
%%
-define(BSS,        ".bss").
-define(DATA,       ".data").
-define(INTERP,     ".interp").
-define(RODATA,     ".rodata").
-define(TEXT,       ".text").
%% Other Standard Section names
-define(COMMENT,    ".comment").
-define(DYNAMIC,    ".dynamic").
-define(DYNSTR,     ".dynstr").
-define(GOT,        ".got").
-define(HASH,       ".hash").
-define(NOTE(Name), (".note" ++ Name)).
-define(PLT,        ".plt").
-define(REL(Name),  (".rel" ++ Name) ).
-define(RELA(Name), (".rela" ++ Name) ).
-define(SHSTRTAB,   ".shstrtab").
-define(STRTAB,     ".strtab").
-define(SYMTAB,     ".symtab").
-define(GCC_EXN_TAB, ".gcc_except_table").

%%------------------------------------------------------------------------------
%% ELF Symbol Table Entries
%%------------------------------------------------------------------------------
-define(ELF_SYM_SIZE, (?ST_NAME_SIZE + ?ST_INFO_SIZE + ?ST_OTHER_SIZE
			+?ST_SHNDX_SIZE + ?ST_VALUE_SIZE + ?ST_SIZE_SIZE) ).

-define(ST_NAME_SIZE,  ?ELF_WORD_SIZE).
-define(ST_INFO_SIZE,  ?ELF_UNSIGNED_CHAR_SIZE).
-define(ST_OTHER_SIZE, ?ELF_UNSIGNED_CHAR_SIZE).
-define(ST_SHNDX_SIZE, ?ELF_HALF_SIZE).
-define(ST_VALUE_SIZE, ?ELF_ADDR_SIZE).
-define(ST_SIZE_SIZE,  ?ELF_XWORD_SIZE).

%% Precomputed offset for Symbol Table entries in SymTab binary
%%XXX: Included in either elf32_format or elf64_format.

%% Name aliases for Symbol Table entry information
-define(ST_NAME,  {?ST_NAME_OFFSET, ?ST_NAME_SIZE}).
-define(ST_INFO,  {?ST_INFO_OFFSET, ?ST_INFO_SIZE}).
-define(ST_OTHER, {?ST_OTHER_OFFSET, ?ST_OTHER_SIZE}).
-define(ST_SHNDX, {?ST_SHNDX_OFFSET, ?ST_SHNDX_SIZE}).
-define(ST_VALUE, {?ST_VALUE_OFFSET, ?ST_VALUE_SIZE}).
-define(ST_SIZE,  {?ST_SIZE_OFFSET, ?ST_SIZE_SIZE}).

%% Macros to extract information from st_type
-define(ELF_ST_BIND(I), (I bsr 4) ).
-define(ELF_ST_TYPE(I), (I band 16#f) ).
-define(ELF_ST_INFO(B,T), (B bsl 4 + T band 16#f) ).

%% Symbol Bindings
-define(STB_LOCAL,  0).
-define(STB_GLOBAL, 1).
-define(STB_WEAK,   2).
-define(STB_LOOS,   10).
-define(STB_HIOS,   12).
-define(STB_LOPROC, 13).
-define(STB_HIPROC, 15).

%% Symbol Types
-define(STT_NOTYPE,  0).
-define(STT_OBJECT,  1).
-define(STT_FUNC,    2).
-define(STT_SECTION, 3).
-define(STT_FILE,    4).
-define(STT_LOOS,    10).
-define(STT_HIOS,    12).
-define(STT_LOPROC,  13).
-define(STT_HIPROC,  15).

%%------------------------------------------------------------------------------
%% ELF Relocation Entries
%%------------------------------------------------------------------------------
-define(ELF_REL_SIZE,  (?R_OFFSET_SIZE + ?R_INFO_SIZE) ).
-define(ELF_RELA_SIZE, (?R_OFFSET_SIZE + ?R_INFO_SIZE + ?R_ADDEND_SIZE) ).

-define(R_OFFSET_SIZE, ?ELF_ADDR_SIZE).
-define(R_INFO_SIZE,   ?ELF_XWORD_SIZE).
-define(R_ADDEND_SIZE, ?ELF_SXWORD_SIZE).

%% Arithmetics for computing byte offsets in a Relocation entry binary
-define(R_OFFSET_OFFSET, 0).
-define(R_INFO_OFFSET,   (?R_OFFSET_OFFSET + ?R_OFFSET_SIZE) ).
-define(R_ADDEND_OFFSET, (?R_INFO_OFFSET + ?R_INFO_SIZE) ).

%% Name aliases for Relocation field information
-define(R_OFFSET, {?R_OFFSET_OFFSET, ?R_OFFSET_SIZE}).
-define(R_INFO,   {?R_INFO_OFFSET, ?R_INFO_SIZE}).
-define(R_ADDEND, {?R_ADDEND_OFFSET, ?R_ADDEND_SIZE}).

%% Useful macros to extract information from r_info field
%%XXX: Included in either elf32_format or elf64_format.

%%------------------------------------------------------------------------------
%% ELF Program Header Table
%%------------------------------------------------------------------------------
-define(ELF_PHDR_SIZE, (?P_TYPE_SIZE + ?P_FLAGS_SIZE + ?P_OFFSET_SIZE
			 +?P_VADDR_SIZE + ?P_PADDR_SIZE + ?P_FILESZ_SIZE
			 +?P_MEMSZ_SIZE + ?P_ALIGN_SIZE) ).

-define(P_TYPE_SIZE,   ?ELF_WORD_SIZE).
-define(P_FLAGS_SIZE,  ?ELF_WORD_SIZE).
-define(P_OFFSET_SIZE, ?ELF_OFF_SIZE).
-define(P_VADDR_SIZE,  ?ELF_ADDR_SIZE).
-define(P_PADDR_SIZE,  ?ELF_ADDR_SIZE).
-define(P_FILESZ_SIZE, ?ELF_XWORD_SIZE).
-define(P_MEMSZ_SIZE,  ?ELF_XWORD_SIZE).
-define(P_ALIGN_SIZE,  ?ELF_XWORD_SIZE).

%% Offsets of various fields in a Program Header Table entry binary.
%%XXX: Included in either elf32_format or elf64_format.

%% Name aliases for each Program Header Table entry field information.
-define(P_TYPE,   {?P_TYPE_OFFSET, ?P_TYPE_SIZE} ).
-define(P_FLAGS,  {?P_FLAGS_OFFSET, ?P_FLAGS_SIZE} ).
-define(P_OFFSET, {?P_OFFSET_OFFSET, ?P_OFFSET_SIZE} ).
-define(P_VADDR,  {?P_VADDR_OFFSET, ?P_VADDR_SIZE} ).
-define(P_PADDR,  {?P_PADDR_OFFSET, ?P_PADDR_SIZE} ).
-define(P_FILESZ, {?P_FILESZ_OFFSET, ?P_FILESZ_SIZE} ).
-define(P_MEMSZ,  {?P_MEMSZ_OFFSET, ?P_MEMSZ_SIZE} ).
-define(P_ALIGN,  {?P_ALIGN_OFFSET, ?P_ALIGN_SIZE} ).

%% Segment Types (p_type)
-define(PT_NULL,    0).
-define(PT_LOAD,    1).
-define(PT_DYNAMIC, 2).
-define(PT_INTERP,  3).
-define(PT_NOTE,    4).
-define(PT_SHLIB,   5).
-define(PT_PHDR,    6).
-define(PT_LOOS,    16#60000000).
-define(PT_HIOS,    16#6FFFFFFF).
-define(PT_LOPROC,  16#70000000).
-define(PT_HIPROC,  16#7FFFFFFF).

%% Segment Attributes (p_flags)
-define(PF_X,        16#1).
-define(PF_W,        16#2).
-define(PF_R,        16#4).
-define(PF_MASKOS,   16#00FF0000).
-define(PF_MASKPROC, 16#FF000000).

%%------------------------------------------------------------------------------
%% ELF Dynamic Table
%%------------------------------------------------------------------------------
-define(ELF_DYN_SIZE, (?D_TAG_SIZE + ?D_VAL_PTR_SIZE) ).

-define(D_TAG_SIZE,     ?ELF_SXWORD_SIZE).
-define(D_VAL_PTR_SIZE, ?ELF_ADDR_SIZE).

%% Offsets of each field in Dynamic Table entry in binary
-define(D_TAG_OFFSET,     0).
-define(D_VAL_PTR_OFFSET, (?D_TAG_OFFSET + ?D_TAG_SIZE)).

%% Name aliases for each field of a Dynamic Table entry information
-define(D_TAG,     {?D_TAG_OFFSET, ?D_TAG_SIZE} ).
-define(D_VAL_PTR, {?D_VAL_PTR_OFFSET, ?D_VAL_PTR_SIZE} ).

%% Dynamic Table Entries
-define(DT_NULL,         0).
-define(DT_NEEDED,       1).
-define(DT_PLTRELSZ,     2).
-define(DT_PLTGOT,       3).
-define(DT_HASH,         4).
-define(DT_STRTAB,       5).
-define(DT_SYMTAB,       6).
-define(DT_RELA,         7).
-define(DT_RELASZ,       8).
-define(DT_RELAENT,      9).
-define(DT_STRSZ,        10).
-define(DT_SYMENT,       11).
-define(DT_INIT,         12).
-define(DT_FINI,         13).
-define(DT_SONAME,       14).
-define(DT_RPATH,        15).
-define(DT_SYMBOLIC,     16).
-define(DT_REL,          17).
-define(DT_RELSZ,        18).
-define(DT_RELENT,       19).
-define(DT_PLTREL,       20).
-define(DT_DEBUG,        21).
-define(DT_TEXTREL,      22).
-define(DT_JMPREL,       23).
-define(DT_BIND_NOW,     24).
-define(DT_INIT_ARRAY,   25).
-define(DT_FINI_ARRAY,   26).
-define(DT_INIT_ARRAYSZ, 27).
-define(DT_FINI_ARRAYSZ, 28).
-define(DT_LOOS,         16#60000000).
-define(DT_HIOS,         16#6FFFFFFF).
-define(DT_LOPROC,       16#700000000).
-define(DT_HIPROC,       16#7FFFFFFFF).

%%------------------------------------------------------------------------------
%% ELF GCC Exception Table
%%------------------------------------------------------------------------------

%% The DWARF Exception Header Encoding is used to describe the type of data used
%% in the .eh_frame_hdr (and .gcc_except_table) section. The upper 4 bits
%% indicate how the value is to be applied. The lower 4 bits indicate the format
%% of the data.

%% DWARF Exception Header value format
-define(DW_EH_PE_omit,    16#ff). % No value is present.
-define(DW_EH_PE_uleb128, 16#01). % Unsigned value encoded using LEB128.
-define(DW_EH_PE_udata2,  16#02). % A 2 bytes unsigned value.
-define(DW_EH_PE_udata4,  16#03). % A 4 bytes unsigned value.
-define(DW_EH_PE_udata8,  16#04). % An 8 bytes unsigned value.
-define(DW_EH_PE_sleb128, 16#09). % Signed value encoded using LEB128.
-define(DW_EH_PE_sdata2,  16#0a). % A 2 bytes signed value.
-define(DW_EH_PE_sdata4,  16#0b). % A 4 bytes signed value.
-define(DW_EH_PE_sdata8,  16#0c). % An 8 bytes signed value.

%% DWARF Exception Header application
-define(DW_EH_PE_absptr,  16#00). % Value is used with no modification.
-define(DW_EH_PE_pcrel,   16#10). % Value is relative to the current PC.
-define(DW_EH_PE_datarel, 16#30). % Value is relative to the beginning of the
				  %   section.

%%------------------------------------------------------------------------------
%% ELF Read-only data (constants, literlas etc.)
%%------------------------------------------------------------------------------
-define(RO_ENTRY_SIZE, 8).

%%------------------------------------------------------------------------------
%% Custom Note section: ".note.gc" for Erlang GC
%%------------------------------------------------------------------------------

%%      The structure of this section is the following:
%%
%%       .short <n>       # number of safe points in code
%%
%%       .long .L<label1> # safe point address               |
%%       .long .L<label2> # safe point address               |-> safe point addrs
%%          .....                                            |
%%       .long .L<label3> # safe point address               |
%%
%%       .short <n>       # stack frame size (in words)      |-> fixed-size part
%%       .short <n>       # stack arity                      |
%%       .short <n>       # number of live roots that follow |
%%
%%       .short <n>       # live root's stack index  |
%%          .....                                    |-> live root indices
%%       .short <n>       #          >>              |

%% The name of the custom Note Section
-define(NOTE_ERLGC_NAME, ".gc").

%% The first word of a Note Section for Erlang GC (".note.gc") is always the
%% number of safepoints in code.
-define(SP_COUNT,        {?SP_COUNT_OFFSET, ?SP_COUNT_SIZE}).
-define(SP_COUNT_SIZE,   ?ELF_HALF_SIZE).
-define(SP_COUNT_OFFSET, 0).                %(always the first entry in sdesc)

%% The fixed-size part of a safe point (SP) entry consists of 4 words: the SP
%% address (offset in code), the stack frame size of the function (where the SP
%% is located), the stack arity of the function (the registered values are *not*
%% counted), the number of live roots in the specific SP.
-define(SP_FIXED,     {?SP_FIXED_OFF, ?SP_FIXED_SIZE}).
-define(SP_FIXED_OFF, 0).
%%XXX: Exclude SP_ADDR_SIZE from SP_FIXED_SIZE in lew of new GC layout
-define(SP_FIXED_SIZE, (?SP_STKFRAME_SIZE + ?SP_STKARITY_SIZE
			 + ?SP_LIVEROOTCNT_SIZE)).

-define(SP_ADDR_SIZE,        ?ELF_WORD_SIZE).
-define(SP_STKFRAME_SIZE,    ?ELF_HALF_SIZE).
-define(SP_STKARITY_SIZE,    ?ELF_HALF_SIZE).
-define(SP_LIVEROOTCNT_SIZE, ?ELF_HALF_SIZE).

%%XXX: SP_STKFRAME is the first piece of information in the new GC layout
-define(SP_STKFRAME_OFFSET,    0).
-define(SP_STKARITY_OFFSET,    (?SP_STKFRAME_OFFSET + ?SP_STKFRAME_SIZE) ).
-define(SP_LIVEROOTCNT_OFFSET, (?SP_STKARITY_OFFSET + ?SP_STKARITY_SIZE) ).

%% Name aliases for safepoint fields.
-define(SP_STKFRAME,    {?SP_STKFRAME_OFFSET, ?SP_STKFRAME_SIZE}).
-define(SP_STKARITY,    {?SP_STKARITY_OFFSET, ?SP_STKARITY_SIZE}).
-define(SP_LIVEROOTCNT, {?SP_LIVEROOTCNT_OFFSET, ?SP_LIVEROOTCNT_SIZE}).

%% After the fixed-size part a variable-size part exists. This part holds the
%% stack frame index of every live root in the specific SP.
-define(LR_STKINDEX_SIZE, ?ELF_HALF_SIZE).

%%------------------------------------------------------------------------------
%% Misc.
%%------------------------------------------------------------------------------
-define(bits(Bytes), ((Bytes) bsl 3)).

%%------------------------------------------------------------------------------
%% Exported record and type declarations for 'elf_format' module
%%------------------------------------------------------------------------------

%% Section header entries
-record(elf_shdr,
	{name      :: elf_format:name()      % Section name
	,type      :: elf_format:shdr_type() % Section type
	,flags     :: elf_format:bitflags()  % Section attributes
	,addr      :: elf_format:offset()    % Virtual address in memory
	,offset    :: elf_format:offset()    % Offset in file
	,size      :: elf_format:size()      % Size of section
	,link      :: non_neg_integer()      % Link to other section
	,info      :: non_neg_integer()      % Miscellaneous information
	,addralign :: elf_format:size()      % Address align boundary
	,entsize   :: elf_format:size()      % Size of entries, if section has
					     % table
	}).
-type elf_shdr() :: #elf_shdr{}.

%% Symbol table entries
-record(elf_sym,
	{name    :: elf_format:name()         % Symbol name
	,bind    :: elf_format:sym_bind()     % Symbol binding
	,type    :: elf_format:sym_type()     % Symbol type
	,value   :: elf_format:valueoff()     % Symbol value
	,size    :: elf_format:size()         % Size of object
	,section :: undefined | abs | elf_shdr()
	}).
-type elf_sym() :: #elf_sym{}.

%% Relocations
-record(elf_rel,
	{offset :: elf_format:offset()
	,type   :: elf_format:reloc_type()
	,addend :: elf_format:addend()
	,symbol :: elf_sym()
	}).
-type elf_rel() :: #elf_rel{}.
