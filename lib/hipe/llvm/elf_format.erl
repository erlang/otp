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

-export([get_tab_entries/1,
         %% Relocations
         get_rodata_relocs/1,
         get_text_relocs/1,
         extract_rela/2,
         get_rela_addends/1,
         %% Note
         extract_note/2,
         %% Executable code
         extract_text/1,
         %% GCC Exception Table
         get_exn_handlers/1,
         %% Misc.
         set_architecture_flag/1,
         is64bit/0
        ]).

-include("elf_format.hrl").

%%------------------------------------------------------------------------------
%% Types
%%------------------------------------------------------------------------------

-type elf()    :: binary().

-type lp()     :: non_neg_integer().  % landing pad
-type num()    :: non_neg_integer().
-type index()  :: non_neg_integer().
-type offset() :: non_neg_integer().
-type size()   :: non_neg_integer().
-type start()  :: non_neg_integer().

-type info()     :: index().
-type nameoff()  :: offset().
-type valueoff() :: offset().

-type name()       :: string().
-type name_size()  :: {name(), size()}.
-type name_sizes() :: [name_size()].

%%------------------------------------------------------------------------------
%% Abstract Data Types and Accessors for ELF Structures.
%%------------------------------------------------------------------------------

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

%% Section header entries
-record(elf_shdr, {name,                   % Section name
		   type,                    % Section type
		   flags,                   % Section attributes
		   addr,                    % Virtual address in memory
		   offset     :: offset(),  % Offset in file
		   size       :: size(),    % Size of section
		   link,                    % Link to other section
		   info,                    % Miscellaneous information
		   addralign,               % Address align boundary
		   entsize                  % Size of entries, if section has table
		  }).
%% -type elf_shdr() :: #elf_shdr{}.

%% Symbol table entries
-record(elf_sym, {name   :: nameoff(),     % Symbol name
		  info,                     % Type and Binding attributes
		  other,                    % Reserved
		  shndx,                    % Section table index
		  value  :: valueoff(),     % Symbol value
		  size   :: size()          % Size of object
		 }).
-type elf_sym() :: #elf_sym{}.

%% Relocations
-record(elf_rel, {r_offset  :: offset(),  % Address of reference
		  r_info    :: info()      % Symbol index and type of relocation
		 }).
-type elf_rel() :: #elf_rel{}.

-record(elf_rela, {r_offset  :: offset(), % Address of reference
		   r_info    :: info(),    % Symbol index and type of relocation
		   r_addend  :: offset()   % Constant part of expression
		  }).
-type elf_rela() :: #elf_rela{}.

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
mk_sym(Name, Info, Other, Shndx, Value, Size) ->
  #elf_sym{name = Name, info = Info, other = Other,
	   shndx = Shndx, value = Value, size = Size}.

-spec sym_name(elf_sym()) -> nameoff().
sym_name(#elf_sym{name = Name}) -> Name.

%% -spec sym_value(elf_sym()) -> valueoff().
%% sym_value(#elf_sym{value = Value}) -> Value.
%% 
%% -spec sym_size(elf_sym()) -> size().
%% sym_size(#elf_sym{size = Size}) -> Size.

%%%-------------------------
%%% Relocations
%%%-------------------------
-spec mk_rel(offset(), info()) -> elf_rel().
mk_rel(Offset, Info) ->
  #elf_rel{r_offset = Offset, r_info = Info}.

%% The following two functions capitalize on the fact that the two kinds of
%% relocation records (for 32- and 64-bit architectures have similar structure.

-spec r_offset(elf_rel() | elf_rela()) -> offset().
r_offset(#elf_rel{r_offset = Offset}) -> Offset;
r_offset(#elf_rela{r_offset = Offset}) -> Offset.

-spec r_info(elf_rel() | elf_rela()) -> info().
r_info(#elf_rel{r_info = Info}) -> Info;
r_info(#elf_rela{r_info = Info}) -> Info.

-spec mk_rela(offset(), info(), offset()) -> elf_rela().
mk_rela(Offset, Info, Addend) ->
  #elf_rela{r_offset = Offset, r_info = Info, r_addend = Addend}.

-spec rela_addend(elf_rela()) -> offset().
rela_addend(#elf_rela{r_addend = Addend}) -> Addend.

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
%% Functions to manipulate the ELF File Header
%%------------------------------------------------------------------------------

%% @doc Extracts the File Header from an ELF formatted object file. Also sets
%%      the ELF class variable in the process dictionary (used by many functions
%%      in this and hipe_llvm_main modules).
-spec extract_header(elf()) -> elf_ehdr().
extract_header(Elf) ->
  Ehdr_bin = get_binary_segment(Elf, 0, ?ELF_EHDR_SIZE),
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

%% @doc Extracts the Section Header Table from an ELF formated Object File.
extract_shdrtab(Elf) ->
  %% Extract File Header to get info about Section Header Offset (in bytes),
  %% Entry Size (in bytes) and Number of entries
  #elf_ehdr{shoff = ShOff, shentsize = ShEntsize, shnum = ShNum} =
    extract_header(Elf),
  %% Get actual Section header table (binary)
  ShdrBin = get_binary_segment(Elf, ShOff,  ShNum * ShEntsize),
  get_shdrtab_entries(ShdrBin, []).

get_shdrtab_entries(<<>>, Acc) ->
  lists:reverse(Acc);
get_shdrtab_entries(ShdrBin, Acc) ->
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
    MoreShdrE/binary
  >> = ShdrBin,
  ShdrE = mk_shdr(Name, Type, Flags, Addr, Offset,
		  Size, Link, Info, Addralign, Entsize),
  get_shdrtab_entries(MoreShdrE, [ShdrE | Acc]).

%% @doc Extracts a specific Entry of a Section Header Table. This function
%%      takes as argument the Section Header Table (`SHdrTab') and the entry's
%%      serial number (`EntryNum') and returns the entry (`shdr').
get_shdrtab_entry(SHdrTab, EntryNum) ->
  lists:nth(EntryNum + 1, SHdrTab).

%%------------------------------------------------------------------------------
%% Functions to manipulate Section Header String Table
%%------------------------------------------------------------------------------

%% @doc Extracts the Section Header String Table. This section is not a known
%%      ELF Object File section. It is just a "hidden" table storing the
%%      names of all sections that exist in current object file.
-spec extract_shstrtab(elf()) -> [name()].
extract_shstrtab(Elf) ->
  %% Extract Section Name String Table Index
  #elf_ehdr{shstrndx = ShStrNdx} = extract_header(Elf),
  ShHdrTab = extract_shdrtab(Elf),
  %% Extract Section header entry and get actual Section-header String Table
  #elf_shdr{offset = ShStrOffset, size = ShStrSize} =
    get_shdrtab_entry(ShHdrTab, ShStrNdx),
  case get_binary_segment(Elf, ShStrOffset, ShStrSize) of
    <<>> -> %% Segment empty
      [];
    ShStrTab -> %% Convert to string table
      [Name || {Name, _Size} <- get_names(ShStrTab)]
  end.

%%------------------------------------------------------------------------------

-spec get_tab_entries(elf()) -> [{name(), valueoff(), size()}].
get_tab_entries(Elf) ->
  SymTab = extract_symtab(Elf),
  Ts = [{Name, Value, Size div ?ELF_XWORD_SIZE}
	|| #elf_sym{name = Name, value = Value, size = Size} <- SymTab,
	   Name =/= 0],
  {NameIndices, ValueOffs, Sizes} = lists:unzip3(Ts),
  %% Find the names of the symbols.
  %% Get string table entries ([{Name, Offset in strtab section}]). Keep only
  %% relevant entries:
  StrTab = extract_strtab(Elf),
  Relevant = [get_strtab_entry(StrTab, Off) || Off <- NameIndices],
  %% Zip back to {Name, ValueOff, Size}
  lists:zip3(Relevant, ValueOffs, Sizes).

%%------------------------------------------------------------------------------
%% Functions to manipulate Symbol Table
%%------------------------------------------------------------------------------

%% @doc Function that extracts Symbol Table from an ELF Object file.
extract_symtab(Elf) ->
  Symtab_bin = extract_segment_by_name(Elf, ?SYMTAB),
  get_symtab_entries(Symtab_bin, []).

get_symtab_entries(<<>>, Acc) ->
  lists:reverse(Acc);
get_symtab_entries(Symtab_bin, Acc) ->
  <<SymE_bin:?ELF_SYM_SIZE/binary, MoreSymE/binary>> = Symtab_bin,
  case is64bit() of
    true ->
      <<%% Structural pattern matching on fields.
	Name:?bits(?ST_NAME_SIZE)/integer-little,
	Info:?bits(?ST_INFO_SIZE)/integer-little,
	Other:?bits(?ST_OTHER_SIZE)/integer-little,
	Shndx:?bits(?ST_SHNDX_SIZE)/integer-little,
	Value:?bits(?ST_VALUE_SIZE)/integer-little,
	Size:?bits(?ST_SIZE_SIZE)/integer-little
      >> = SymE_bin;
    false ->
      << %% Same fields in different order:
        Name:?bits(?ST_NAME_SIZE)/integer-little,
        Value:?bits(?ST_VALUE_SIZE)/integer-little,
        Size:?bits(?ST_SIZE_SIZE)/integer-little,
        Info:?bits(?ST_INFO_SIZE)/integer-little,
        Other:?bits(?ST_OTHER_SIZE)/integer-little,
        Shndx:?bits(?ST_SHNDX_SIZE)/integer-little
      >> = SymE_bin
  end,
  SymE = mk_sym(Name, Info, Other, Shndx, Value, Size),
  get_symtab_entries(MoreSymE, [SymE | Acc]).

%% @doc Extracts a specific entry from the Symbol Table (as binary).
%%      This function takes as arguments the Symbol Table (`SymTab')
%%      and the entry's serial number and returns that entry (`sym').
get_symtab_entry(SymTab, EntryNum) ->
  lists:nth(EntryNum + 1, SymTab).

%%------------------------------------------------------------------------------
%% Functions to manipulate String Table
%%------------------------------------------------------------------------------

%% @doc Extracts String Table from an ELF formated Object File.
-spec extract_strtab(elf()) -> [{string(), offset()}].
extract_strtab(Elf) ->
  Strtab_bin = extract_segment_by_name(Elf, ?STRTAB),
  NamesSizes = get_names(Strtab_bin),
  make_offsets(NamesSizes).

%% @doc Returns the name of the symbol at the given offset. The string table
%%      contains entries of the form {Name, Offset}. If no such offset exists
%%      returns the empty string (`""').
%%      XXX: There might be a bug here because of the "compact" saving the ELF
%%      format uses: e.g. only stores ".rela.text" for ".rela.text" and ".text".
get_strtab_entry(Strtab, Offset) ->
  case lists:keyfind(Offset, 2, Strtab) of
    {Name, Offset} -> Name;
    false -> ""
  end.

%%------------------------------------------------------------------------------
%% Functions to manipulate Relocations
%%------------------------------------------------------------------------------

%% @doc This function gets as argument an ELF binary file and returns a list
%%      with all .rela.rodata labels (i.e. constants and literals in code)
%%      or an empty list if no ".rela.rodata" section exists in code.
-spec get_rodata_relocs(elf()) -> [offset()].
get_rodata_relocs(Elf) ->
  case is64bit() of
    true ->
      %% Only care about the addends (== offsets):
      get_rela_addends(extract_rela(Elf, ?RODATA));
    false ->
      %% Find offsets hardcoded in ".rodata" entry
      %%XXX: Treat all 0s as padding and skip them!
      [SkipPadding || SkipPadding <- extract_rodata(Elf), SkipPadding =/= 0]
  end.

-spec get_rela_addends([elf_rela()]) -> [offset()].
get_rela_addends(RelaEntries) ->
  [rela_addend(E) || E <- RelaEntries].

%% @doc Extract a list of the form `[{SymbolName, Offset}]' with all relocatable
%%      symbols and their offsets in the code from the ".text" section.
-spec get_text_relocs(elf()) -> [{name(), offset()}].
get_text_relocs(Elf) ->
  %% Only care about the symbol table index and the offset:
  NameOffsetTemp = [{?ELF_R_SYM(r_info(E)), r_offset(E)}
                    || E <- extract_rela(Elf, ?TEXT)],
  {NameIndices, ActualOffsets} = lists:unzip(NameOffsetTemp),
  %% Find the names of the symbols:
  %%
  %% Get those symbol table entries that are related to Text relocs:
  Symtab = extract_symtab(Elf),
  SymtabEs = [get_symtab_entry(Symtab, Index) || Index <- NameIndices],
                                                %XXX: not zero-indexed!
  %% Symbol table entries contain the offset of the name of the symbol in
  %% String Table:
  SymtabEs2 = [sym_name(E) || E <- SymtabEs], %XXX: Do we need to sort SymtabE?
  %% Get string table entries ([{Name, Offset in strtab section}]). Keep only
  %% relevant entries:
  Strtab = extract_strtab(Elf),
  Relevant = [get_strtab_entry(Strtab, Off) || Off <- SymtabEs2],
  %% Zip back with actual offsets:
  lists:zip(Relevant, ActualOffsets).

%% @doc Extract the Relocations segment for section `Name' (that is passed
%%      as second argument) from an ELF formated Object file binary.
-spec extract_rela(elf(), name()) -> [elf_rel() | elf_rela()].
extract_rela(Elf, Name) ->
  SegName =
    case is64bit() of
      true -> ?RELA(Name); % ELF-64 uses ".rela"
      false -> ?REL(Name)  % ...while ELF-32 uses ".rel"
    end,
  Rela_bin = extract_segment_by_name(Elf, SegName),
  get_rela_entries(Rela_bin, []).

get_rela_entries(<<>>, Acc) ->
  lists:reverse(Acc);
get_rela_entries(Bin, Acc) ->
  E = case is64bit() of
	true ->
	  <<%% Structural pattern matching on fields of a Rela Entry.
	    Offset:?bits(?R_OFFSET_SIZE)/integer-little,
	    Info:?bits(?R_INFO_SIZE)/integer-little,
	    Addend:?bits(?R_ADDEND_SIZE)/integer-little,
	    Rest/binary
	  >> = Bin,
	  mk_rela(Offset, Info, Addend);
	false ->
	  <<%% Structural pattern matching on fields of a Rel Entry.
	    Offset:?bits(?R_OFFSET_SIZE)/integer-little,
	    Info:?bits(?R_INFO_SIZE)/integer-little,
	    Rest/binary
	  >> = Bin,
	  mk_rel(Offset, Info)
      end,
  get_rela_entries(Rest, [E | Acc]).

%% %% @doc Extract the `EntryNum' (serial number) Relocation Entry.
%% get_rela_entry(Rela, EntryNum) ->
%%   lists:nth(EntryNum + 1, Rela).

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
%% Functions to manipulate Read-only Data (.rodata)
%%------------------------------------------------------------------------------
extract_rodata(Elf) ->
  Rodata_bin = extract_segment_by_name(Elf, ?RODATA),
  get_rodata_entries(Rodata_bin, []).

get_rodata_entries(<<>>, Acc) ->
  lists:reverse(Acc);
get_rodata_entries(Rodata_bin, Acc) ->
  <<Num:?bits(?ELF_ADDR_SIZE)/integer-little, More/binary>> = Rodata_bin,
  get_rodata_entries(More, [Num | Acc]).

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
extract_segment_by_name(Elf, SectionName) ->
  %% Extract Section Header Table and Section Header String Table from binary
  SHdrTable = extract_shdrtab(Elf),
  Names     = extract_shstrtab(Elf),
  %% Zip to a list of (Name,ShdrE)
  [_Zero | ShdrEs] = lists:keysort(2, SHdrTable), % Skip first entry (zeros).
  L = lists:zip(Names, ShdrEs),
  %% Find Section Header Table entry by name
  case lists:keyfind(SectionName, 1, L) of
    {SectionName, ShdrE} -> %% Note: Same name.
      #elf_shdr{offset = Offset, size = Size} = ShdrE,
      get_binary_segment(Elf, Offset, Size);
    false -> %% Not found.
      <<>>
  end.

%% @doc Extracts a list of strings with (zero-separated) names from a binary.
%%      Returns tuples of `{Name, Size}'.
%%      XXX: Skip trailing 0.
-spec get_names(<<_:8,_:_*8>>) -> name_sizes().
get_names(<<0, Bin/binary>>) ->
  NamesSizes = get_names(Bin, []),
  fix_names(NamesSizes, []).

get_names(<<>>, Acc) ->
  lists:reverse(Acc);
get_names(Bin, Acc) ->
  {Name, MoreNames} = bin_get_string(Bin),
  get_names(MoreNames, [{Name, length(Name)} | Acc]).

%% @doc Fix names:
%%      e.g. If ".rela.text" exists, ".text" does not. Same goes for
%%           ".rel.text". In that way, the Section Header String Table is more
%%           compact. Add ".text" just *before* the corresponding rela-field,
%%           etc.
-spec fix_names(name_sizes(), name_sizes()) -> name_sizes().
fix_names([], Acc) ->
  lists:reverse(Acc);
fix_names([{Name, Size}=T | Names], Acc) ->
  case is64bit() of
    true ->
      case string:str(Name, ".rela") =:= 1 of
        true -> %% Name starts with ".rela":
          Section = string:substr(Name, 6),
          fix_names(Names, [{Section, Size - 5}
                            | [T | Acc]]); % XXX: Is order ok? (".text"
                                           % always before ".rela.text")
        false -> %% Name does not start with ".rela":
          fix_names(Names, [T | Acc])
      end;
    false ->
      case string:str(Name, ".rel") =:= 1 of
        true -> %% Name starts with ".rel":
          Section = string:substr(Name, 5),
          fix_names(Names, [{Section, Size - 4}
                            | [T | Acc]]); % XXX: Is order ok? (".text"
                                           % always before ".rela.text")
        false -> %% Name does not start with ".rel":
          fix_names(Names, [T | Acc])
      end
  end.


%% @doc A function that byte-reverses a binary. This might be needed because of
%%      little (fucking!) endianess.
-spec bin_reverse(binary()) -> binary().
bin_reverse(Bin) when is_binary(Bin) ->
  bin_reverse(Bin, <<>>).

-spec bin_reverse(binary(), binary()) -> binary().
bin_reverse(<<>>, Acc) ->
  Acc;
bin_reverse(<<Head, More/binary>>, Acc) ->
  bin_reverse(More, <<Head, Acc/binary>>).

%% @doc A function that extracts a null-terminated string from a binary. It
%%      returns the found string along with the rest of the binary.
-spec bin_get_string(binary()) -> {string(), binary()}.
bin_get_string(Bin) ->
  bin_get_string(Bin, <<>>).

bin_get_string(<<>>, BinAcc) ->
  Bin = bin_reverse(BinAcc), % little endian!
  {binary_to_list(Bin), <<>>};
bin_get_string(<<0, MoreBin/binary>>, BinAcc) ->
  Bin = bin_reverse(BinAcc), % little endian!
  {binary_to_list(Bin), MoreBin};
bin_get_string(<<Letter, Tail/binary>>, BinAcc) ->
  bin_get_string(Tail, <<Letter, BinAcc/binary>>).

%% @doc
make_offsets(NamesSizes) ->
  {Names, Sizes} = lists:unzip(NamesSizes),
  Offsets = make_offsets_from_sizes(Sizes, 1, []),
  lists:zip(Names, Offsets).

make_offsets_from_sizes([], _, Acc) ->
  lists:reverse(Acc);
make_offsets_from_sizes([Size | Sizes], Cur, Acc) ->
  make_offsets_from_sizes(Sizes, Size+Cur+1, [Cur | Acc]). % For the "."!

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

%% @doc Extract ELF Class from ELF header and export symbol to process
%%      dictionary.
-spec set_architecture_flag(elf()) -> 'ok'.
set_architecture_flag(Elf) ->
  %% Extract information about ELF Class from ELF Header
  <<16#7f, $E, $L, $F, EI_Class, _MoreHeader/binary>>
    = get_binary_segment(Elf, 0, ?ELF_EHDR_SIZE),
  put(elf_class, EI_Class),
  ok.

%% @doc Read from object file header if the file class is ELF32 or ELF64.
-spec is64bit() -> boolean().
is64bit() ->
  case get(elf_class) of
    ?ELFCLASS64 -> true;
    ?ELFCLASS32 -> false
  end.
