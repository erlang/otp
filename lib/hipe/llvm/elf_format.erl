%% -*- erlang-indent-level: 2 -*-

%%% @copyright 2011-2014 Yiannis Tsiouris <gtsiour@softlab.ntua.gr>,
%%%                      Chris Stavrakakis <hydralisk.r@gmail.com>,
%%%                      Kostis Sagonas <kostis@cs.ntua.gr>
%%% @author Yiannis Tsiouris <gtsiour@softlab.ntua.gr>
%%%    [http://www.softlab.ntua.gr/~gtsiour/]

%%% @doc This module contains functions for extracting various pieces of
%%%      information from an ELF formated Object file. To fully understand
%%%      the ELF format and the use of these functions please read
%%%      "[http://www.linuxjournal.com/article/1060?page=0,0]" carefully.

-module(elf_format).

-export([%% Relocations
         extract_rela/2,
         %% Note
         extract_note/2,
         %% Executable code
         extract_text/1,
         %% GCC Exception Table
	 get_exn_handlers/1,
	 %% Symbols
	 elf_symbols/1,
	 %% Sections
	 section_contents/2,
	 %% Main interface
	 read/1
        ]).

-include("elf_format.hrl").

%%------------------------------------------------------------------------------
%% Types
%%------------------------------------------------------------------------------

-export_type([elf/0
	     ,addend/0
	     ,bitflags/0
	     ,name/0
	     ,offset/0
	     ,reloc_type/0
	     ,shdr_type/0
	     ,size/0
	     ,sym_bind/0
	     ,sym_type/0
	     ,valueoff/0
	     ]).

-type bitflags()  :: non_neg_integer().
-type index()     :: non_neg_integer().
-type lp()        :: non_neg_integer().  % landing pad
-type num()       :: non_neg_integer().
-type offset()    :: non_neg_integer().
-type size()      :: non_neg_integer().
-type start()     :: non_neg_integer().

-type addend()    :: integer() | undefined.
-type name()      :: string().
-type shdr_type() :: 'null' | 'progbits' | 'symtab' | 'strtab' | 'rela'
		   | 'hash' | 'dynamic' | 'note' | 'nobits' | 'rel' | 'shlib'
		   | 'dynsym' | {os, ?SHT_LOOS..?SHT_HIOS}
		   | {proc, ?SHT_LOPROC..?SHT_HIPROC}.
-type sym_bind()  :: 'local' | 'global' | 'weak' | {os, ?STB_LOOS..?STB_HIOS}
		   | {proc, ?STB_LOPROC..?STB_HIPROC}.
-type sym_type()  :: 'notype' | 'object' | 'func' | 'section' | 'file'
		   | {os, ?STT_LOOS..?STT_HIOS}
		   | {proc, ?STT_LOPROC..?STT_HIPROC}.
-type valueoff()  :: offset().

-ifdef(BIT32).  % 386
-type reloc_type() :: '32' | 'pc32'.
-else.          % X86_64
-type reloc_type() :: '64' | 'pc32' | '32'.
-endif.

%%------------------------------------------------------------------------------
%% Abstract Data Types and Accessors for ELF Structures.
%%------------------------------------------------------------------------------

-record(elf, {file            :: binary()
	     ,sections        :: [elf_shdr()]
	     ,sec_nam         :: #{string() => elf_shdr()}
	     ,symbols         :: undefined | [elf_sym()]
	     }).
-opaque elf() :: #elf{}.

%% File header
-record(elf_ehdr, {ident,                  % ELF identification
		   type,                    % Object file type
		   machine,                 % Machine Type
		   version,                 % Object file version
		   entry,                   % Entry point address
		   phoff,                   % Program header offset
		   shoff      :: offset(),  % Section header offset
		   flags,                   % Processor-specific flags
		   ehsize     :: size(),    % ELF header size
		   phentsize  :: size(),    % Size of program header entry
		   phnum      :: num(),     % Number of program header entries
		   shentsize  :: size(),    % Size of section header entry
		   shnum      :: num(),     % Number of section header entries
		   shstrndx   :: index()    % Section name string table index
		  }).
-type elf_ehdr() :: #elf_ehdr{}.

-record(elf_ehdr_ident, {class,                   % File class
			 data,                     % Data encoding
			 version,                  % File version
			 osabi,                    % OS/ABI identification
			 abiversion,               % ABI version
			 pad,                      % Start of padding bytes
			 nident                    % Size of e_ident[]
			}).
%% -type elf_ehdr_ident() :: #elf_ehdr_ident{}.

%% %% Program header table
%% -record(elf_phdr, {type,   % Type of segment
%% 		   flags,  % Segment attributes
%% 		   offset, % Offset in file
%% 		   vaddr,  % Virtual address in memory
%% 		   paddr,  % Reserved
%% 		   filesz, % Size of segment in file
%% 		   memsz,  % Size of segment in memory
%% 		   align   % Alignment of segment
%% 		  }).

%% %% GCC exception table
%% -record(elf_gccexntab, {lpbenc,    % Landing pad base encoding
%% 			lpbase,    % Landing pad base
%% 			ttenc,     % Type table encoding
%% 			ttoff,     % Type table offset
%% 			csenc,     % Call-site table encoding
%% 			cstabsize, % Call-site table size
%% 			cstab     :: cstab() % Call-site table
%% 		       }).
%% -type elf_gccexntab() :: #elf_gccexntab{}.

-record(elf_gccexntab_callsite, {start :: start(), % Call-site start
				 size  :: size(),   % Call-site size
				 lp    :: lp(),     % Call-site landing pad
                                                   %  (exception handler)
				 onaction           % On action (e.g. cleanup)
				}).
%% -type elf_gccexntab_callsite() :: #elf_gccexntab_callsite{}.

%%------------------------------------------------------------------------------
%% Accessor Functions
%%------------------------------------------------------------------------------

%% File header
%% -spec mk_ehdr(...) -> elf_ehrd().
mk_ehdr(Ident, Type, Machine, Version, Entry, Phoff, Shoff, Flags, Ehsize,
	Phentsize, Phnum, Shentsize, Shnum, Shstrndx) ->
    #elf_ehdr{ident = Ident, type = Type, machine = Machine, version = Version,
	      entry = Entry, phoff = Phoff, shoff = Shoff, flags = Flags,
	      ehsize = Ehsize, phentsize = Phentsize, phnum = Phnum,
	      shentsize = Shentsize, shnum = Shnum, shstrndx = Shstrndx}.

%% -spec ehdr_shoff(elf_ehdr()) -> offset().
%% ehdr_shoff(#elf_ehdr{shoff = Offset}) -> Offset.
%% 
%% -spec ehdr_shentsize(elf_ehdr()) -> size().
%% ehdr_shentsize(#elf_ehdr{shentsize = Size}) -> Size.
%% 
%% -spec ehdr_shnum(elf_ehdr()) -> num().
%% ehdr_shnum(#elf_ehdr{shnum = Num}) -> Num.
%% 
%% -spec ehdr_shstrndx(elf_ehdr()) -> index().
%% ehdr_shstrndx(#elf_ehdr{shstrndx = Index}) -> Index.


%%-spec mk_ehdr_ident(...) -> elf_ehdr_ident().
mk_ehdr_ident(Class, Data, Version, OsABI, AbiVersion, Pad, Nident) ->
  #elf_ehdr_ident{class = Class, data = Data, version = Version, osabi = OsABI,
		  abiversion = AbiVersion, pad = Pad, nident = Nident}.

%%%-------------------------
%%% Section header entries
%%%-------------------------
mk_shdr(Name, Type, Flags, Addr, Offset, Size, Link, Info, AddrAlign, EntSize) ->
    #elf_shdr{name = Name, type = Type, flags = Flags, addr = Addr,
	      offset = Offset, size = Size, link = Link, info = Info,
	      addralign = AddrAlign, entsize = EntSize}.

%% -spec shdr_offset(elf_shdr()) -> offset().
%% shdr_offset(#elf_shdr{offset = Offset}) -> Offset.
%% 
%% -spec shdr_size(elf_shdr()) -> size().
%% shdr_size(#elf_shdr{size = Size}) -> Size.

%%%-------------------------
%%% Symbol Table Entries
%%%-------------------------
mk_sym(Name, Bind, Type, Section, Value, Size) ->
  #elf_sym{name = Name, bind = Bind, type = Type,
	   section = Section, value = Value, size = Size}.

%% -spec sym_name(elf_sym()) -> string().
%% sym_name(#elf_sym{name = Name}) -> Name.
%%
%% -spec sym_value(elf_sym()) -> valueoff().
%% sym_value(#elf_sym{value = Value}) -> Value.
%% 
%% -spec sym_size(elf_sym()) -> size().
%% sym_size(#elf_sym{size = Size}) -> Size.

%% %%%-------------------------
%% %%% GCC exception table
%% %%%-------------------------
%% -type cstab()  :: [elf_gccexntab_callsite()].
%%
%% mk_gccexntab(LPbenc, LPbase, TTenc, TToff, CSenc, CStabsize, CStab) ->
%%   #elf_gccexntab{lpbenc = LPbenc, lpbase = LPbase, ttenc = TTenc,
%% 		 ttoff = TToff, csenc = CSenc, cstabsize = CStabsize,
%% 		 cstab = CStab}.
%%
%% -spec gccexntab_cstab(elf_gccexntab()) -> cstab().
%% gccexntab_cstab(#elf_gccexntab{cstab = CSTab}) -> CSTab.

mk_gccexntab_callsite(Start, Size, LP, Action) ->
   #elf_gccexntab_callsite{start = Start, size=Size, lp=LP, onaction=Action}.

%% -spec gccexntab_callsite_start(elf_gccexntab_callsite()) -> start().
%% gccexntab_callsite_start(#elf_gccexntab_callsite{start = Start}) -> Start.
%% 
%% -spec gccexntab_callsite_size(elf_gccexntab_callsite()) -> size().
%% gccexntab_callsite_size(#elf_gccexntab_callsite{size = Size}) -> Size.
%% 
%% -spec gccexntab_callsite_lp(elf_gccexntab_callsite()) -> lp().
%% gccexntab_callsite_lp(#elf_gccexntab_callsite{lp = LP}) -> LP.

%%------------------------------------------------------------------------------
%% Main interface function
%%------------------------------------------------------------------------------

%% @doc Parses an ELF file.
-spec read(binary()) -> elf().
read(ElfBin) ->
  Header = extract_header(ElfBin),
  [_UndefinedSec|Sections] = extract_shdrtab(ElfBin, Header),
  SecNam = maps:from_list(
	     [{Name, Sec} || Sec = #elf_shdr{name=Name} <- Sections]),
  Elf0 = #elf{file=ElfBin, sections=Sections, sec_nam=SecNam},
  [_UndefinedSym|Symbols] = extract_symtab(Elf0, extract_strtab(Elf0)),
  Elf0#elf{symbols=Symbols}.

%%------------------------------------------------------------------------------
%% Functions to manipulate the ELF File Header
%%------------------------------------------------------------------------------

%% @doc Extracts the File Header from an ELF formatted object file. Also sets
%%      the ELF class variable in the process dictionary (used by many functions
%%      in this and hipe_llvm_main modules).
-spec extract_header(binary()) -> elf_ehdr().
extract_header(ElfBin) ->
  Ehdr_bin = get_binary_segment(ElfBin, 0, ?ELF_EHDR_SIZE),
  << %% Structural pattern matching on fields.
     Ident_bin:?E_IDENT_SIZE/binary,
     Type:?bits(?E_TYPE_SIZE)/integer-little,
     Machine:?bits(?E_MACHINE_SIZE)/integer-little,
     Version:?bits(?E_VERSION_SIZE)/integer-little,
     Entry:?bits(?E_ENTRY_SIZE)/integer-little,
     Phoff:?bits(?E_PHOFF_SIZE)/integer-little,
     Shoff:?bits(?E_SHOFF_SIZE)/integer-little,
     Flags:?bits(?E_FLAGS_SIZE)/integer-little,
     Ehsize:?bits(?E_EHSIZE_SIZE)/integer-little,
     Phentsize:?bits(?E_PHENTSIZE_SIZE)/integer-little,
     Phnum:?bits(?E_PHNUM_SIZE)/integer-little,
     Shentsize:?bits(?E_SHENTSIZE_SIZE)/integer-little,
     Shnum:?bits(?E_SHENTSIZE_SIZE)/integer-little,
     Shstrndx:?bits(?E_SHSTRNDX_SIZE)/integer-little
  >> = Ehdr_bin,
  <<16#7f, $E, $L, $F, Class, Data, Version, Osabi, Abiversion,
    Pad:6/binary, Nident
  >> = Ident_bin,
  Ident = mk_ehdr_ident(Class, Data, Version, Osabi,
			Abiversion, Pad, Nident),
  mk_ehdr(Ident, Type, Machine, Version, Entry, Phoff, Shoff, Flags,
	  Ehsize, Phentsize, Phnum, Shentsize, Shnum, Shstrndx).

%%------------------------------------------------------------------------------
%% Functions to manipulate Section Header Entries
%%------------------------------------------------------------------------------

-type shdrtab() :: [elf_shdr()].

%% @doc Extracts the Section Header Table from an ELF formated Object File.
-spec extract_shdrtab(binary(), elf_ehdr()) -> shdrtab().
extract_shdrtab(ElfBin, #elf_ehdr{shoff=ShOff, shentsize=?ELF_SHDRENTRY_SIZE,
				  shnum=ShNum, shstrndx=ShStrNdx}) ->
  %% Get actual Section header table (binary)
  ShdrBin = get_binary_segment(ElfBin, ShOff,  ShNum * ?ELF_SHDRENTRY_SIZE),
  %% We need to lookup the offset and size of the section header string table
  %% before we can fully parse the section table. We compute its offset and
  %% extract the fields we need here.
  ShStrEntryOffset = ShStrNdx * ?ELF_SHDRENTRY_SIZE,
  <<_:ShStrEntryOffset/binary, _:?SH_NAME_SIZE/binary,
    _:?SH_TYPE_SIZE/binary, _:?SH_FLAGS_SIZE/binary, _:?SH_ADDR_SIZE/binary,
    ShStrOffset:?bits(?SH_OFFSET_SIZE)/little,
    ShStrSize:?bits(?SH_SIZE_SIZE)/little,
    _/binary>> = ShdrBin,
  ShStrTab = parse_strtab(get_binary_segment(ElfBin, ShStrOffset, ShStrSize)),
  get_shdrtab_entries(ShdrBin, ShStrTab).

get_shdrtab_entries(<<>>, _ShStrTab) -> [];
get_shdrtab_entries(ShdrTab, ShStrTab) ->
  <<%% Structural pattern matching on fields.
    Name:?bits(?SH_NAME_SIZE)/integer-little,
    Type:?bits(?SH_TYPE_SIZE)/integer-little,
    Flags:?bits(?SH_FLAGS_SIZE)/integer-little,
    Addr:?bits(?SH_ADDR_SIZE)/integer-little,
    Offset:?bits(?SH_OFFSET_SIZE)/integer-little,
    Size:?bits(?SH_SIZE_SIZE)/integer-little,
    Link:?bits(?SH_LINK_SIZE)/integer-little,
    Info:?bits(?SH_INFO_SIZE)/integer-little,
    Addralign:?bits(?SH_ADDRALIGN_SIZE)/integer-little,
    Entsize:?bits(?SH_ENTSIZE_SIZE)/integer-little,
    Rest/binary
  >> = ShdrTab,
  Entry = mk_shdr(get_strtab_entry(Name, ShStrTab), decode_shdr_type(Type),
		  Flags, Addr, Offset, Size, Link, Info, Addralign, Entsize),
  [Entry | get_shdrtab_entries(Rest, ShStrTab)].

decode_shdr_type(?SHT_NULL)     -> 'null';
decode_shdr_type(?SHT_PROGBITS) -> 'progbits';
decode_shdr_type(?SHT_SYMTAB)   -> 'symtab';
decode_shdr_type(?SHT_STRTAB)   -> 'strtab';
decode_shdr_type(?SHT_RELA)     -> 'rela';
decode_shdr_type(?SHT_HASH)     -> 'hash';      %unused
decode_shdr_type(?SHT_DYNAMIC)  -> 'dynamic';   %unused
decode_shdr_type(?SHT_NOTE)     -> 'note';      %unused
decode_shdr_type(?SHT_NOBITS)   -> 'nobits';
decode_shdr_type(?SHT_REL)      -> 'rel';
decode_shdr_type(?SHT_SHLIB)    -> 'shlib';     %unused
decode_shdr_type(?SHT_DYNSYM)   -> 'dynsym';    %unused
decode_shdr_type(OS) when ?SHT_LOOS =< OS, OS =< ?SHT_HIOS -> {os, OS};
decode_shdr_type(Proc) when ?SHT_LOPROC =< Proc, Proc =< ?SHT_HIPROC ->
  {proc, Proc}.

-spec elf_section(non_neg_integer(), elf()) -> undefined | abs | elf_shdr().
elf_section(0, #elf{}) -> undefined;
elf_section(?SHN_ABS, #elf{}) -> abs;
elf_section(Index, #elf{sections=SecIdx}) ->
  lists:nth(Index, SecIdx).

%% Reads the contents of a section from an object
-spec section_contents(elf_shdr(), elf()) -> binary().
section_contents(#elf_shdr{offset=Offset, size=Size}, #elf{file=ElfBin}) ->
  get_binary_segment(ElfBin, Offset, Size).

%%------------------------------------------------------------------------------
%% Functions to manipulate Symbol Table
%%------------------------------------------------------------------------------

%% @doc Function that extracts Symbol Table from an ELF Object file.
extract_symtab(Elf, StrTab) ->
  Symtab = extract_segment_by_name(Elf, ?SYMTAB),
  [parse_sym(Sym, Elf, StrTab) || <<Sym:?ELF_SYM_SIZE/binary>> <= Symtab].

-ifdef(BIT32).
parse_sym(<<%% Structural pattern matching on fields.
	    Name:?bits(?ST_NAME_SIZE)/integer-little,
	    Value:?bits(?ST_VALUE_SIZE)/integer-little,
	    Size:?bits(?ST_SIZE_SIZE)/integer-little,
	    Info:?bits(?ST_INFO_SIZE)/integer-little,
	    _Other:?bits(?ST_OTHER_SIZE)/integer-little,
	    Shndx:?bits(?ST_SHNDX_SIZE)/integer-little>>,
	 Elf, StrTab) ->
  mk_sym(get_strtab_entry(Name, StrTab), decode_symbol_bind(?ELF_ST_BIND(Info)),
	 decode_symbol_type(?ELF_ST_TYPE(Info)), elf_section(Shndx, Elf), Value,
	 Size).
-else.
parse_sym(<<%% Same fields in different order:
	    Name:?bits(?ST_NAME_SIZE)/integer-little,
	    Info:?bits(?ST_INFO_SIZE)/integer-little,
	    _Other:?bits(?ST_OTHER_SIZE)/integer-little,
	    Shndx:?bits(?ST_SHNDX_SIZE)/integer-little,
	    Value:?bits(?ST_VALUE_SIZE)/integer-little,
	    Size:?bits(?ST_SIZE_SIZE)/integer-little>>,
	 Elf, StrTab) ->
  mk_sym(get_strtab_entry(Name, StrTab), decode_symbol_bind(?ELF_ST_BIND(Info)),
	 decode_symbol_type(?ELF_ST_TYPE(Info)), elf_section(Shndx, Elf), Value,
	 Size).
-endif.

decode_symbol_bind(?STB_LOCAL)  -> 'local';
decode_symbol_bind(?STB_GLOBAL) -> 'global';
decode_symbol_bind(?STB_WEAK)   -> 'weak';      %unused
decode_symbol_bind(OS) when ?STB_LOOS =< OS, OS =< ?STB_HIOS -> {os, OS};
decode_symbol_bind(Proc) when ?STB_LOPROC =< Proc, Proc =< ?STB_HIPROC ->
  {proc, Proc}.

decode_symbol_type(?STT_NOTYPE)  -> 'notype';
decode_symbol_type(?STT_OBJECT)  -> 'object';
decode_symbol_type(?STT_FUNC)    -> 'func';
decode_symbol_type(?STT_SECTION) -> 'section';
decode_symbol_type(?STT_FILE)    -> 'file';
decode_symbol_type(OS) when ?STT_LOOS =< OS, OS =< ?STT_HIOS -> {os, OS};
decode_symbol_type(Proc) when ?STT_LOPROC =< Proc, Proc =< ?STT_HIPROC ->
  {proc, Proc}.

%% @doc Extracts a specific entry from the Symbol Table.
-spec elf_symbol(0,             elf()) -> undefined;
		(pos_integer(), elf()) -> elf_sym().
elf_symbol(0, #elf{}) -> undefined;
elf_symbol(Index, #elf{symbols=Symbols}) -> lists:nth(Index, Symbols).

-spec elf_symbols(elf()) -> [elf_sym()].
elf_symbols(#elf{symbols=Symbols}) -> Symbols.

%%------------------------------------------------------------------------------
%% Functions to manipulate String Table
%%------------------------------------------------------------------------------

%% ADT: get_strtab_entry/1 must be used to consume this type.
-type strtab() :: binary().

%% @doc Extracts String Table from an ELF formated Object File.
-spec extract_strtab(elf()) -> strtab().
extract_strtab(Elf) ->
  parse_strtab(extract_segment_by_name(Elf, ?STRTAB)).

-spec parse_strtab(binary()) -> strtab().
parse_strtab(StrTabSectionBin) -> StrTabSectionBin.

%% @doc Returns the name of the symbol at the given offset.
-spec get_strtab_entry(non_neg_integer(), strtab()) -> string().
get_strtab_entry(Offset, StrTab) ->
  <<_:Offset/binary, StrBin/binary>> = StrTab,
  bin_get_string(StrBin).

%% @doc Extracts a null-terminated string from a binary.
-spec bin_get_string(binary()) -> string().
%% FIXME: No regard for encoding (just happens to work for ASCII and Latin-1)
bin_get_string(<<0, _/binary>>) -> [];
bin_get_string(<<Char, Rest/binary>>) -> [Char|bin_get_string(Rest)].

%%------------------------------------------------------------------------------
%% Functions to manipulate Relocations
%%------------------------------------------------------------------------------

%% @doc Extract the Relocations segment for section `Name' (that is passed
%%      as second argument) from an ELF formated Object file binary.
-spec extract_rela(elf(), name()) -> [elf_rel()].

-ifdef(BIT32).
extract_rela(Elf, Name) ->
  SecData = extract_segment_by_name(Elf, Name),
  [#elf_rel{offset=Offset, symbol=elf_symbol(?ELF_R_SYM(Info), Elf),
	    type=decode_reloc_type(?ELF_R_TYPE(Info)),
	    addend=read_implicit_addend(Offset, SecData)}
   || <<Offset:?bits(?R_OFFSET_SIZE)/little,
	Info:?bits(?R_INFO_SIZE)/little % 386 uses ".rel"
      >> <= extract_segment_by_name(Elf, ?REL(Name))].

%% The only types HiPE knows how to patch
decode_reloc_type(1) -> '32';
decode_reloc_type(2) -> 'pc32'.

read_implicit_addend(Offset, Section) ->
  %% All x86 relocation types uses 'word32' relocation fields; i.e. 32-bit LE.
  <<_:Offset/binary, Addend:32/signed-little, _/binary>> = Section,
  Addend.

-else. %% BIT32
extract_rela(Elf, Name) ->
  [#elf_rel{offset=Offset, symbol=elf_symbol(?ELF_R_SYM(Info), Elf),
	    type=decode_reloc_type(?ELF_R_TYPE(Info)), addend=Addend}
   || <<Offset:?bits(?R_OFFSET_SIZE)/little,
	Info:?bits(?R_INFO_SIZE)/little,
	Addend:?bits(?R_ADDEND_SIZE)/signed-little % X86_64 uses ".rela"
      >> <= extract_segment_by_name(Elf, ?RELA(Name))].

decode_reloc_type(1)  -> '64';
decode_reloc_type(2)  -> 'pc32';
decode_reloc_type(10) -> '32'.
-endif. %% BIT32

%%------------------------------------------------------------------------------
%% Functions to manipulate Executable Code segment
%%------------------------------------------------------------------------------

%% @doc This function gets as arguments an ELF formated binary file and
%%      returns the Executable Code (".text" segment) or an empty binary if it
%%      is not found.
-spec extract_text(elf()) -> binary().
extract_text(Elf) ->
  extract_segment_by_name(Elf, ?TEXT).

%%------------------------------------------------------------------------------
%% Functions to manipulate Note Section
%%------------------------------------------------------------------------------

%% @doc Extract specific Note Section from an ELF Object file. The function
%%      takes as first argument the object file (`Elf') and the `Name' of the
%%      wanted Note Section (<b>without</b> the ".note" prefix!). It returns
%%      the specified binary segment or an empty binary if no such section
%%      exists.
-spec extract_note(elf(), string()) -> binary().
extract_note(Elf, Name) ->
  extract_segment_by_name(Elf, ?NOTE(Name)).

%%------------------------------------------------------------------------------
%% Functions to manipulate GCC Exception Table segment
%%------------------------------------------------------------------------------

%% A description for the C++ exception table formats can be found at Exception
%% Handling Tables (http://www.codesourcery.com/cxx-abi/exceptions.pdf).

%% A list with `{Start, End, HandlerOffset}' for all call sites in the code
-spec get_exn_handlers(elf()) -> [{start(), start(), lp()}].
get_exn_handlers(Elf) ->
  CallSites = extract_gccexntab_callsites(Elf),
  [{Start, Start + Size, LP}
   || #elf_gccexntab_callsite{start = Start, size = Size, lp = LP} <- CallSites].

%% @doc This function gets as argument an ELF binary file and returns
%%      the table (list) of call sites which is stored in GCC
%%      Exception Table (".gcc_except_table") section.
%%      It returns an empty list if the Exception Table is not found.
%%      XXX: Assumes there is *no*  Action Record Table.
extract_gccexntab_callsites(Elf) ->
  case extract_segment_by_name(Elf, ?GCC_EXN_TAB) of
    <<>> ->
      [];
    ExnTab ->
      %% First byte of LSDA is Landing Pad base encoding.
      <<LBenc:8, More/binary>> = ExnTab,
      %% Second byte is the Landing Pad base (if its encoding is not
      %% DW_EH_PE_omit) (optional).
      {_LPBase, LSDACont} =
        case LBenc =:= ?DW_EH_PE_omit of
          true ->  % No landing pad base byte. (-1 denotes that)
            {-1, More};
          false -> % Landing pad base.
            <<Base:8, More2/binary>> = More,
            {Base, More2}
        end,
      %% Next byte of LSDA is the encoding of the Type Table.
      <<TTenc:8, More3/binary>> = LSDACont,
      %% Next byte is the Types Table offset encoded in U-LEB128 (optional).
      {_TTOff, LSDACont2} =
        case TTenc =:= ?DW_EH_PE_omit of
          true ->  % There is no Types Table pointer. (-1 denotes that)
            {-1, More3};
          false -> % The byte offset from this field to the start of the Types
                   % Table used for exception matching.
            leb128_decode(More3)
        end,
      %% Next byte of LSDA is the encoding of the fields in the Call-site Table.
      <<_CSenc:8, More4/binary>> = LSDACont2,
      %% Sixth byte is the size (in bytes) of the Call-site Table encoded in
      %% U-LEB128.
      {_CSTabSize, CSTab} = leb128_decode(More4),
      %% Extract all call site information
      get_gccexntab_callsites(CSTab, [])
  end.

get_gccexntab_callsites(<<>>, Acc) ->
  lists:reverse(Acc);
get_gccexntab_callsites(CSTab, Acc) ->
  %% We are only interested in the Landing Pad of every entry.
  <<Start:32/integer-little, Size:32/integer-little,
    LP:32/integer-little, OnAction:8, More/binary
  >> = CSTab,
  GccCS = mk_gccexntab_callsite(Start, Size, LP, OnAction),
  get_gccexntab_callsites(More, [GccCS | Acc]).

%%------------------------------------------------------------------------------
%% Helper functions
%%------------------------------------------------------------------------------

%% @doc Returns the binary segment starting at `Offset' with length `Size'
%%      (bytes) from a binary file. If `Offset' is bigger than the byte size of
%%      the binary, an empty binary (`<<>>') is returned.
-spec get_binary_segment(binary(), offset(), size()) -> binary().
get_binary_segment(Bin, Offset, _Size) when Offset > byte_size(Bin) ->
  <<>>;
get_binary_segment(Bin, Offset, Size) ->
  <<_Hdr:Offset/binary, BinSeg:Size/binary, _More/binary>> = Bin,
  BinSeg.

%% @doc This function gets as arguments an ELF formated binary object and
%%      a string with the segments' name and returns the specified segment or
%%      an empty binary (`<<>>') if there exists no segment with that name.
%%      There are handy macros defined in elf_format.hrl for all Standard
%%      Section Names.
-spec extract_segment_by_name(elf(), string()) -> binary().
extract_segment_by_name(#elf{file=ElfBin, sec_nam=SecNam}, SectionName) ->
  %% Find Section Header Table entry by name
  case SecNam of
    #{SectionName := #elf_shdr{offset=Offset, size=Size}} ->
      get_binary_segment(ElfBin, Offset, Size);
    #{} -> %% Not found.
      <<>>
  end.

%% @doc Little-Endian Base 128 (LEB128) Decoder
%%     This function extracts the <b>first</b> LEB128-encoded integer in a
%%     binary and returns that integer along with the remaining binary. This is
%%     done because a LEB128 number has variable bit-size and that is a way of
%%     extracting only one number in a binary and continuing parsing the binary
%%     for other kind of data (e.g. different encoding).
%% FIXME: Only decodes unsigned data!
-spec leb128_decode(binary()) -> {integer(), binary()}.
leb128_decode(LebNum) ->
  leb128_decode(LebNum, 0, <<>>).

-spec leb128_decode(binary(), integer(), binary()) -> {integer(), binary()}.
leb128_decode(LebNum, NoOfBits, Acc) ->
  <<Sentinel:1/bits, NextBundle:7/bits, MoreLebNums/bits>> = LebNum,
  case Sentinel of
    <<1:1>> -> % more bytes to follow
      leb128_decode(MoreLebNums, NoOfBits+7, <<NextBundle:7/bits, Acc/bits>>);
    <<0:1>> -> % byte bundle stop
      Size = NoOfBits+7,
      <<Num:Size/integer>> = <<NextBundle:7/bits, Acc/bits>>,
      {Num, MoreLebNums}
  end.
