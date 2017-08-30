%% -*- erlang-indent-level: 2 -*-
%%------------------------------------------------------------------------------

%%====================================================================
%% Types
%%====================================================================

%% Code and Monitor servers' info.
-record(svs, {
  code    :: pid(),
  monitor :: pid()
}).

%% Tags of an AST's node.
-record(tags, {
  this = undefined :: cuter_cerl:tag() | undefined,
  next = undefined :: cuter_cerl:tag() | undefined
}).

-type loaded_ret_atoms() :: cover_compiled | preloaded | non_existing.
-type servers() :: #svs{}.
-type ast_tags() :: #tags{}.

%%====================================================================
%% Directories
%%====================================================================

-define(RELATIVE_TMP_DIR, "temp").
-define(PYTHON_CALL, ?PYTHON_PATH ++ " -u " ++ ?PRIV ++ "/cuter_interface.py").

%%====================================================================
%% Prefixes
%%====================================================================

-define(DEPTH_PREFIX, '__conc_depth').
-define(EXECUTION_PREFIX, '__conc_prefix').
-define(SYMBOLIC_PREFIX, '__s').
-define(CONCOLIC_PREFIX_MSG, '__concm').
-define(ZIPPED_VALUE_PREFIX, '__czip').
-define(CONCOLIC_PREFIX_PDICT, '__concp').
-define(FUNCTION_PREFIX, '__cfunc').
-define(UNBOUND_VAR_PREFIX, '__uboundvar').
-define(BRANCH_TAG_PREFIX, '__branch_tag').
-define(VISITED_TAGS_PREFIX, '__visited_tags').
-define(EXECUTION_COUNTER_PREFIX, '__exec_counter').

%%====================================================================
%% Flags & Default Values
%%====================================================================

-define(LOGGING_FLAG, ok).
-define(DELETE_TRACE, ok).
-define(LOG_UNSUPPORTED_MFAS, ok).
%%-define(VERBOSE_SCHEDULER, ok).
%%-define(VERBOSE_FILE_DELETION, ok).
%%-define(VERBOSE_SOLVING, ok).
%%-define(VERBOSE_MERGING, ok).
%%-define(VERBOSE_REPORTING, ok).
-define(USE_SPECS, ok).

%%====================================================================
%% Solver Responses
%%====================================================================

-define(RSP_MODEL_DELIMITER_START, <<"model_start">>).
-define(RSP_MODEL_DELIMITER_END, <<"model_end">>).

%%====================================================================
%% OpCodes for types in JSON objects
%%====================================================================

-define(JSON_TYPE_ANY, 0).
-define(JSON_TYPE_INT, 1).
-define(JSON_TYPE_FLOAT, 2).
-define(JSON_TYPE_ATOM, 3).
-define(JSON_TYPE_LIST, 4).
-define(JSON_TYPE_TUPLE, 5).
-define(JSON_TYPE_PID, 6).
-define(JSON_TYPE_REF, 7).

%%====================================================================
%% OpCodes for the commands to the solver
%%====================================================================

-define(JSON_CMD_LOAD_TRACE_FILE, 1).
-define(JSON_CMD_SOLVE, 2).
-define(JSON_CMD_GET_MODEL, 3).
-define(JSON_CMD_ADD_AXIOMS, 4).
-define(JSON_CMD_FIX_VARIABLE, 5).
-define(JSON_CMD_RESET_SOLVER, 6).
-define(JSON_CMD_STOP, 42).

%%====================================================================
%% OpCodes for constraint types
%%====================================================================

-define(CONSTRAINT_TRUE, 1).
-define(CONSTRAINT_FALSE, 2).
-define(NOT_CONSTRAINT, 3).

-define(CONSTRAINT_TRUE_REPR, 84).   %% $T
-define(CONSTRAINT_FALSE_REPR, 70).  %% $F

%%====================================================================
%% OpCodes of constraints & built-in operations
%%====================================================================

%% Empty tag ID
-define(EMPTY_TAG_ID, 0).

%% MFA's Parameters & Spec definitions.
-define(OP_PARAMS, 1).
-define(OP_SPEC, 2).
%% Constraints.
-define(OP_GUARD_TRUE, 3).
-define(OP_GUARD_FALSE, 4).
-define(OP_MATCH_EQUAL_TRUE, 5).
-define(OP_MATCH_EQUAL_FALSE, 6).
-define(OP_TUPLE_SZ, 7).
-define(OP_TUPLE_NOT_SZ, 8).
-define(OP_TUPLE_NOT_TPL, 9).
-define(OP_LIST_NON_EMPTY, 10).
-define(OP_LIST_EMPTY, 11).
-define(OP_LIST_NOT_LST, 12).
%% Information used for syncing & merging the traces of many processes.
-define(OP_SPAWN, 13).
-define(OP_SPAWNED, 14).
-define(OP_MSG_SEND, 15).
-define(OP_MSG_RECEIVE, 16).
-define(OP_MSG_CONSUME, 17).
%% Necessary operations for the evaluation of Core Erlang.
-define(OP_UNFOLD_TUPLE, 18).
-define(OP_UNFOLD_LIST, 19).
%% Bogus operation (operations interpreted as the identity function).
-define(OP_BOGUS, 48).
%% Type conversions.
-define(OP_FLOAT, 47).
-define(OP_LIST_TO_TUPLE, 52).
-define(OP_TUPLE_TO_LIST, 53).
%% Query types.
-define(OP_IS_INTEGER, 27).
-define(OP_IS_ATOM, 28).
-define(OP_IS_FLOAT, 29).
-define(OP_IS_LIST, 30).
-define(OP_IS_TUPLE, 31).
-define(OP_IS_BOOLEAN, 32).
-define(OP_IS_NUMBER, 33).
%% Arithmetic operations.
-define(OP_PLUS, 34).
-define(OP_MINUS, 35).
-define(OP_TIMES, 36).
-define(OP_RDIV, 37).
-define(OP_IDIV_NAT, 38).
-define(OP_REM_NAT, 39).
-define(OP_UNARY, 40).
%% Operations on atoms.
-define(OP_ATOM_NIL, 49).
-define(OP_ATOM_HEAD, 50).
-define(OP_ATOM_TAIL, 51).
%% Operations on lists.
-define(OP_HD, 25).
-define(OP_TL, 26).
-define(OP_CONS, 56).
%% Operations on tuples.
-define(OP_TCONS, 57).
%% Comparisons.
-define(OP_EQUAL, 41).
-define(OP_UNEQUAL, 42).
-define(OP_LT_INT, 54).
-define(OP_LT_FLOAT, 55).

%% Maps MFAs to their JSON Opcodes
-define(OPCODE_MAPPING,
  dict:from_list([ %% Simulated built-in operations
                   { {cuter_erlang, atom_to_list_bogus, 1}, ?OP_BOGUS         }
                 , { {cuter_erlang, is_atom_nil,        1}, ?OP_ATOM_NIL      }
                 , { {cuter_erlang, safe_atom_head,     1}, ?OP_ATOM_HEAD     }
                 , { {cuter_erlang, safe_atom_tail,     1}, ?OP_ATOM_TAIL     }
                 , { {cuter_erlang, safe_pos_div,       2}, ?OP_IDIV_NAT      }
                 , { {cuter_erlang, safe_pos_rem,       2}, ?OP_REM_NAT       }
                 , { {cuter_erlang, lt_int,             2}, ?OP_LT_INT        }
                 , { {cuter_erlang, lt_float,           2}, ?OP_LT_FLOAT      }
                 , { {cuter_erlang, safe_plus,          2}, ?OP_PLUS          }
                 , { {cuter_erlang, safe_minus,         2}, ?OP_MINUS         }
                 , { {cuter_erlang, safe_times,         2}, ?OP_TIMES         }
                 , { {cuter_erlang, safe_rdiv,          2}, ?OP_RDIV          }
                 , { {cuter_erlang, safe_float,         1}, ?OP_FLOAT         }
                 , { {cuter_erlang, safe_list_to_tuple, 1}, ?OP_LIST_TO_TUPLE }
                 , { {cuter_erlang, safe_tuple_to_list, 1}, ?OP_TUPLE_TO_LIST }
                 , { {bogus_erlang, cons,               2}, ?OP_CONS          }
                   %% Actual erlang BIFs
                 , { {erlang, hd,            1}, ?OP_HD            }
                 , { {erlang, tl,            1}, ?OP_TL            }
                 , { {erlang, is_integer,    1}, ?OP_IS_INTEGER    }
                 , { {erlang, is_atom,       1}, ?OP_IS_ATOM       }
                 , { {erlang, is_boolean,    1}, ?OP_IS_BOOLEAN    }
                 , { {erlang, is_float,      1}, ?OP_IS_FLOAT      }
                 , { {erlang, is_list,       1}, ?OP_IS_LIST       }
                 , { {erlang, is_tuple,      1}, ?OP_IS_TUPLE      }
                 , { {erlang, is_number,     1}, ?OP_IS_NUMBER     }
                 , { {erlang, '-',           1}, ?OP_UNARY         }
                 , { {erlang, '=:=',         2}, ?OP_EQUAL         }
                 , { {erlang, '=/=',         2}, ?OP_UNEQUAL       }
                 ])).

%% All the MFAs that are supported for symbolic evaluation.
-define(SUPPORTED_MFAS, gb_sets:from_list(dict:fetch_keys(?OPCODE_MAPPING))).

-define(UNSUPPORTED_MFAS,
  gb_sets:from_list([ {cuter_erlang, unsupported_lt, 2} ])).

%% The set of all the built-in operations that the solver can try to reverse.
-define (REVERSIBLE_OPERATIONS,
  gb_sets:from_list([ ?OP_HD, ?OP_TL
                    ])).
