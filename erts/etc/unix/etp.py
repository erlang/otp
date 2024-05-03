# coding=utf-8
#
# %CopyrightBegin%
# 
# Copyright Ericsson AB 2013-2023. All Rights Reserved.
# 
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
# 
# %CopyrightEnd%
#
#
# This script was orinally written by Anthony Ramine (aka nox) in 2013.
# A lot of things have changed since then, but the same base remains.
#

import re
import lldb
import shlex

unquoted_atom_re = re.compile(u'^[a-zß-öø-ÿ][a-zA-Zß-öø-ÿ0-9@]*$')
code_pointers = {}

def __lldb_init_module(debugger, internal_dict):
    debugger.HandleCommand('type format add -f hex Eterm')
    debugger.HandleCommand('type format add -f hex BeamInstr')
    debugger.HandleCommand('type summary add -F etp.eterm_summary Eterm')
    debugger.HandleCommand('command script add -f etp.processes_cmd etp-processes')
    debugger.HandleCommand('command script add -f etp.process_info_cmd etp-process-info')
    debugger.HandleCommand('command script add -f etp.stacktrace_cmd etp-stacktrace')
    debugger.HandleCommand('command script add -f etp.stackdump_cmd etp-stackdump')
    debugger.HandleCommand('command script add -f etp.eterm_cmd etp')

####################################
## Print all processes in the system
####################################
def processes_cmd(debugger, command, result, internal_dict):
    target = debugger.GetSelectedTarget()
    init(target)
    proc = erts_proc(target)
    proc_r_o = proc.GetChildMemberWithName('r').GetChildMemberWithName('o')
    proc_max_ix = proc_r_o.GetChildMemberWithName('max')
    proc_tab = proc_r_o.GetChildMemberWithName('tab').Cast(ProcessPtrPtr(target))
    proc_cnt = atomic_value(proc.GetChildMemberWithName('vola').GetChildMemberWithName('tile').GetChildMemberWithName('count'))
    invalid_proc = global_var('erts_invalid_process', target).address_of
    for proc_ix in range(0, proc_max_ix.unsigned):
        proc = offset(proc_ix, proc_tab).deref
        if proc.unsigned != 0 and proc.unsigned != invalid_proc.unsigned:
            print('---')
            print('  Pix: %d' % proc_ix)
            process_info(proc)
            proc_cnt -= 1
            if proc_cnt == 0:
                break

############################################
## Print process-info about a single process
############################################
def process_info_cmd(debugger, command, result, internal_dict):
    target = debugger.GetSelectedTarget()
    init(target)
    proc = target.process.selected_thread.GetSelectedFrame().EvaluateExpression(command).Cast(ProcessPtr(target))
    process_info(proc)

def process_info(proc):
    print('  Pid: %s' % eterm(proc.GetChildMemberWithName('common').GetChildMemberWithName('id')))
    print('  State: %s' % process_state(proc))
    print('  Flags: %s' % process_flags(proc))
    print_process_reg_name(proc)
    current = proc.GetChildMemberWithName('current')
    if current.unsigned != 0 and atomic_value(proc.GetChildMemberWithName('state')) & 0x800 == 0:
        print('  Current function: %s' % mfa(current))
    else:
        print('  Current function: %s' % 'unknown')
    i = proc.GetChildMemberWithName('i')
    if i.unsigned != 0:
        print('  I: %s' % eterm(i))
    else:
        print('  I: %s' % 'unknown')
    print('  Pointer: %#x' % proc.unsigned)

def print_process_reg_name(proc):
    state = proc.GetChildMemberWithName('state').unsigned
    if (state & 0x800) == 0:
        # Not exiting.
        reg = proc.GetChildMemberWithName('common').GetChildMemberWithName('u').GetChildMemberWithName('alive').GetChildMemberWithName('reg')
        if reg.unsigned != 0:
            print('  Registered name: %s' % eterm(reg.GetChildMemberWithName('name')))

def process_state(proc):
    state = proc.GetChildMemberWithName('state').unsigned
    res = ''
    if state & 0x80000000:
        res += "GARBAGE<0x80000000> | "
    if state & 0x40000000:
        res += "dirty-running-sys | "
    if state & 0x20000000:
        res += "dirty-running | "
    if state & 0x10000000:
        res += "dirty-active-sys | "
    if state & 0x8000000:
        res += "dirty-io-proc | "
    if state & 0x4000000:
        res += "dirty-cpu-proc | "
    if state & 0x2000000:
        res += "sig-q | "
    if state & 0x1000000:
        res += "off-heap-msgq | "
    if state & 0x800000:
        res += "delayed-sys | "
    if state & 0x400000:
        res += "proxy | "
        proxy_process = True
    else:
        proxy_process = False
    if state & 0x200000:
        res += "running-sys | "
    if state & 0x100000:
        res += "active-sys | "
    if state & 0x80000:
        res += "sig-in-q | "
    if state & 0x40000:
        res += "sys-tasks | "
    if state & 0x20000:
        res += "garbage-collecting | "
    if state & 0x10000:
        res += "suspended | "
    if state & 0x8000:
        res += "running | "
    if state & 0x4000:
        res += "in-run-queue | "
    if state & 0x2000:
        res += "active | "
    if state & 0x1000:
        res += "unused | "
    if state & 0x800:
        res += "exiting | "
    if state & 0x400:
        res += "free | "
    if state & 0x200:
        res += "in-prq-low | "
    if state & 0x100:
        res += "in-prq-normal | "
    if state & 0x80:
        res += "in-prq-high | "
    if state & 0x40:
        res += "in-prq-max | "
    if state & 0x30 == 0x0:
        res += "prq-prio-max | "
    elif state & 0x30 == 0x10:
      res += "prq-prio-high | "
    elif state & 0x30 == 0x20:
        res += "prq-prio-normal | "
    else:
        res += "prq-prio-low | "
    if state & 0xc == 0x0:
        res += "usr-prio-max | "
    elif state & 0xc == 0x4:
      res += "usr-prio-high | "
    elif state & 0xc == 0x8:
        res += "usr-prio-normal | "
    else:
        res += "usr-prio-low | "
    if state & 0x3 == 0x0:
        res += "act-prio-max"
    elif state & 0x3 == 0x1:
      res += "act-prio-high"
    elif state & 0x3 == 0x2:
        res += "act-prio-normal"
    else:
        res += "act-prio-low"
    return res

def process_flags(proc):
    flags = proc.GetChildMemberWithName('flags').unsigned
    res = ''
    if flags & ~((1 << 24)-1):
        res += "GARBAGE<%#x> " % (flags & ~((1 << 24)-1))
    if flags & (1 << 22):
        res += "trap-exit "
    if flags & (1 << 21):
        res += "hibernated "
    if flags & (1 << 20):
        res += "dirty-minor-gc "
    if flags & (1 << 19):
        res += "dirty-major-gc "
    if flags & (1 << 18):
        res += "dirty-gc-hibernate "
    if flags & (1 << 17):
        res += "dirty-cla "
    if flags & (1 << 16):
        res += "delayed-del-proc "
    if flags & (1 << 15):
        res += "have-blocked-nmsb "
    if flags & (1 << 14):
        res += "shdlr-onln-wait-q "
    if flags & (1 << 13):
        res += "delay-gc "
    if flags & (1 << 12):
        res += "abandoned-heap-use "
    if flags & (1 << 11):
        res += "disable-gc "
    if flags & (1 << 10):
        res += "force-gc "
    if flags & (1 << 9):
        res += "ets-super-user "
    if flags & (1 << 8):
        res += "have-blocked-msb "
    if flags & (1 << 7):
        res += "using-ddll "
    if flags & (1 << 6):
        res += "distribution "
    if flags & (1 << 5):
        res += "using-db "
    if flags & (1 << 4):
        res += "need-fullsweep "
    if flags & (1 << 3):
        res += "heap-grow "
    if flags & (1 << 2):
        res += "timo "
    if flags & (1 << 1):
        res += "inslpqueue "
    if flags & (1 << 0):
        res += "hibernate-sched "
    return res

############################################
## Print the stacktrace of a single process
############################################
def stacktrace_cmd(debugger, command, result, internal_dict):
    target = debugger.GetSelectedTarget()
    init(target)
    proc = target.process.selected_thread.GetSelectedFrame().EvaluateExpression(command).Cast(ProcessPtr(target))
    stackdump(proc, False)

############################################
## Print the stackdump of a single process
############################################
def stackdump_cmd(debugger, command, result, internal_dict):
    target = debugger.GetSelectedTarget()
    init(target)
    proc = target.process.selected_thread.GetSelectedFrame().EvaluateExpression(command).Cast(ProcessPtr(target))
    stackdump(proc, True)

def stackdump(proc, dump):
    stop = proc.GetChildMemberWithName('stop')
    send = proc.GetChildMemberWithName('hend')
    cnt = 0
    if atomic_value(proc.GetChildMemberWithName('state')) & 0x8000:
        print('%%%%%% WARNING: The process is currently running, so c_p->stop will not be correct')
    print(F'%% Stacktrace ({send.unsigned - stop.unsigned})');
    i = proc.GetChildMemberWithName('i')
    if i.unsigned != 0:
        print(F'I: {eterm(i)}')
    while stop.unsigned < send.unsigned:
        if stop.deref.unsigned & 0x3 == 0x0 or dump:
            print(F'{cnt}: {eterm(stop.deref)}')
        cnt += 1
        stop = offset(1, stop)

############################################
## Print an eterm
############################################
def eterm_cmd(debugger, command, result, internal_dict):
    args = shlex.split(command)
    target = debugger.GetSelectedTarget()
    init(target)
    term = target.process.selected_thread.GetSelectedFrame().EvaluateExpression(args[0]).Cast(EtermPtr(target))
    if len(args) >= 2:
        print(eterm(term, int(args[1])))
    else:
        print(eterm(term))

############################################
## Print the summary of an Eterm
############################################
def eterm_summary(valobj, internal_dict):
    if valobj.TypeIsPointerType():
        return ''
    return F'{valobj.unsigned:#x} {eterm(valobj, 5)}'

def eterm(valobj, depth = float('inf')):
    val = valobj.unsigned
    valobj = strip_literal_tag(valobj)
    tag = val & c('TAG_PRIMARY_MASK')
    if tag == c('TAG_PRIMARY_LIST'):
        return cons(valobj, depth)
    elif tag == c('TAG_PRIMARY_BOXED'):
        return boxed(valobj, depth)
    elif tag == c('TAG_PRIMARY_IMMED1'):
        return imm(valobj)
    elif val == c('THE_NON_VALUE'):
        return '<the non-value>'
    else:
        return cp(valobj)

def cons(valobj, depth = float('inf')):
    items = []
    cdr = strip_literal_tag(valobj)
    improper = False
    truncated = False
    depth *= 20

    ptr = cdr.CreateValueFromData(
        "unconsed",
        lldb.SBData.CreateDataFromInt(cdr.unsigned - c('TAG_PRIMARY_LIST')),
        EtermPtr(cdr.target))
    if ptr.deref.error.fail:
        return "#ConsError<%x>" % cdr.unsigned;

    while True:
        cdr = strip_literal_tag(cdr)
        ptr = cdr.CreateValueFromData(
            "unconsed",
            lldb.SBData.CreateDataFromInt(cdr.unsigned - c('TAG_PRIMARY_LIST')),
            EtermPtr(cdr.target))
        items.append((ptr.deref, depth // 20)); # Append depth, car
        if ptr.deref.unsigned & 0xF == 0xF:
            depth -= 1
        else:
            depth -= 20
        cdr = offset(1,ptr).deref
        if is_nil(cdr):
            break
        if cdr.unsigned & c('TAG_PRIMARY_LIST') == 0:
            improper = True
            break
        if depth <= 1:
            truncated = True
            break

    if improper:
        return '#ImproperList'

    ## Try to print as ascii first
    chars = ''
    isprintable = True
    for car, car_depth in items:
        if ((car.unsigned & c('TAG_IMMED1_MASK'))
             == c('TAG_IMMED1_SMALL')):
            if car.unsigned >> c('TAG_IMMED1_SIZE') == 10:
                chars += '\\n'
            elif car.unsigned >> c('TAG_IMMED1_SIZE') == 9:
                chars += '\\t'
            else:
                shift = c('TAG_IMMED1_SIZE')
                chars += f'{car.unsigned >> shift:c}'
        else:
            isprintable = False
            break
    isprintable = isprintable and chars.isprintable()
    if isprintable:
        if not truncated:
            return F'"{chars}"'
        else:
            return F'"{chars}..."'

    ## If not printable, we print the objects
    objs = []
    chars = '['
    for car, car_depth in items:
        objs.append(eterm(car, car_depth))
    if not truncated:
        return '[' + ','.join(objs) + ']'
    else:
        return '[' + ','.join(objs) + '|...]'

def boxed(valobj, depth = float('inf')):
    ptr = valobj.CreateValueFromData(
        "unboxed",
        lldb.SBData.CreateDataFromInt(valobj.unsigned - c('TAG_PRIMARY_BOXED')),
        EtermPtr(valobj.target))
    boxed_hdr = ptr.deref
    if boxed_hdr.error.fail:
        return "#BoxedError<%x>" % valobj.unsigned
    boxed_hdr = boxed_hdr.unsigned
    if boxed_hdr & c('TAG_HEADER_MASK') == c('TAG_HEADER_ARITYVAL'):
        arity = (boxed_hdr >> c('HEADER_ARITY_OFFS'))
        terms = []
        for x in range(1, arity+1):
            if depth <= 1:
                terms.append('...')
                break
            depth -= 1
            terms.append(eterm(offset(x, ptr).deref, depth))
        res = ','.join(terms)
        return F"{{{res}}}"
    masked_hdr = boxed_hdr & c('HEADER_SUBTAG_MASK')
    if masked_hdr == c('MAP_SUBTAG'):
        if ((boxed_hdr & c('HEADER_MAP_SUBTAG_MASK'))
             == c('MAP_SUBTAG_HEAD_FLATMAP')):
            return "#FlatMap"
        else:
            return "#HashMap"
    if masked_hdr == c('EXTERNAL_PID_SUBTAG'):
        return '#ExternalPid'
    if masked_hdr == c('EXTERNAL_PORT_SUBTAG'):
        return '#ExternalPort'
    if masked_hdr == c('EXTERNAL_REF_SUBTAG'):
        return '#ExternalRef'
    if (masked_hdr & BIG_TAG_MASK) == c('POS_BIG_SUBTAG'):
        return '#Bignum'
    if masked_hdr == c('FLOAT_SUBTAG'):
        return '#Float'
    if masked_hdr == c('REF_SUBTAG'):
        return '#Ref'
    if masked_hdr == c('FUN_SUBTAG'):
        return '#Fun'
    if masked_hdr == c('FUN_REF_SUBTAG'):
        return '#FunRef'
    if masked_hdr == c('HEAP_BITS_SUBTAG'):
        return '#HeapBits'
    if masked_hdr == c('SUB_BITS_SUBTAG'):
        return '#SubBits'
    return F'#Boxed<{valobj.unsigned}>'

def imm(valobj):
    val = valobj.unsigned
    if (val & c('TAG_PRIMARY_MASK')) != c('TAG_PRIMARY_IMMED1'):
        return '#NotImmediate<%#x>' % val
    tag = val & c('TAG_IMMED1_MASK')
    if tag == c('TAG_IMMED1_PID'):
        return pid(valobj)
    elif tag == c('TAG_IMMED1_PORT'):
        return port(valobj)
    elif tag == c('TAG_IMMED1_SMALL'):
        return str(val >> c('TAG_IMMED1_SIZE'))
    elif tag == c('TAG_IMMED1_IMMED2'):
        # Immediate2
        tag2 = val & c('TAG_IMMED2_MASK')
        if tag2 == c('TAG_IMMED2_ATOM'):
            return atom(valobj)
        elif tag2 == c('TAG_IMMED2_CATCH'):
            return F'#Catch<{val>>6:#x}>'
        elif is_nil(valobj):
            return '[]'
    return '#UnknownImmediate<%#x>' % val

# Continuation pointers

def cp(valobj):
    mfaptr = erts_lookup_function_info(valobj)
    if mfaptr == None:
        pointer = code_pointers.get(valobj.unsigned)
        if pointer != None:
            return '#Cp<%s>' % pointer
        else:
            return '#Cp<%#x>' % valobj.unsigned
    else:
        return '#Cp<%s>' % mfa(mfaptr)

# Pids and ports

def pid(valobj):
    val = valobj.unsigned
    if (val & c('TAG_IMMED1_MASK')) == c('TAG_IMMED1_PID'):
        target = valobj.target
        if etp_arch_bits(target) == 64:
            val = val >> c('TAG_IMMED1_SIZE')
            if etp_big_endian(target):
                data = (val >> 31) & 0x0FFFFFFF
            else:
                data = val & 0x0FFFFFFF
        else:
            data = pixdata2data(valobj)
        return '<0.%u.%u>' % (data & 0x7FFF, (data >> 15) & 0x1FFF)
    else:
        return '#NotPid<%#x>' % val

def port(valobj):
    val = valobj.unsigned
    if (val & c('TAG_IMMED1_MASK')) == c('TAG_IMMED1_PORT'):
        target = valobj.target
        if etp_arch_bits(target) == 64 and not etp_halfword(target):
            val = val >> c('TAG_IMMED1_SIZE')
            if etp_big_endian(target):
                data = (val >> 32) & 0x0FFFFFFF
            else:
                data = val & 0x0FFFFFFF
        else:
            data = pixdata2data(valobj)
        return '#Port<0.%u>' % data
    else:
        return '#NotPort<%#x>' % val

# Strings and atoms

def atom(valobj):
    val = valobj.unsigned
    if (val & c('TAG_IMMED2_MASK')) == c('TAG_IMMED2_ATOM'):
        name = atom_name(atom_tab(valobj))
        if unquoted_atom_re.match(name):
            return str(name)
        else:
            return quoted_name(name, "'")
    else:
        return '#NotAtom<%#x>' % val

def atom_name(entry):
    name = entry.GetChildMemberWithName('name')
    length = entry.GetChildMemberWithName('len').unsigned
    data = name.GetPointeeData(0, length).uint8s
    return ''.join(map(chr, data))

def quoted_name(name, quote):
    return quote + ''.join(map(lambda c: quoted_char(c, quote), name)) + quote

def quoted_char(c, quote):
    point = ord(c)
    if c == quote:
        return '\\' + quote
    elif point == 0x08:
        return '\\b'
    elif point == 0x09:
        return '\\t'
    elif point == 0x0A:
        return '\\n'
    elif point == 0x0B:
        return '\\v'
    elif point == 0x0C:
        return '\\f'
    elif point == 0x0D:
        return '\\e'
    elif point >= 0x20 and point <= 0x7E or point >= 0xA0:
        return c
    elif (point > 0xFF):
        return '#NotChar<%#x>' % c
    else:
        return '\\%03o' % point

# Constants

MI_FUNCTIONS = 13
MI_NUM_FUNCTIONS = 0

def is_nil(value):
    return (value.unsigned & c('TAG_IMMED2_MASK')) == c('TAG_IMMED2_NIL')

# Types

def Atom(target):
    return target.FindFirstType('Atom')

def Char(target):
    return target.FindFirstType('char')
def CharPtr(target):
    return Char(target).GetPointerType()

def Eterm(target):
    return target.FindFirstType('Eterm')
def EtermPtr(target):
    return Eterm(target).GetPointerType()
def EtermPtrPtr(target):
    return EtermPtr(target).GetPointerType()

def Range(target):
    return target.FindFirstType('Range')

def BeamInstr(target):
    return target.FindFirstType('BeamInstr')
def BeamInstrPtr(target):
    return BeamInstr(target).GetPointerType()

def ErtsCodeInfo(target):
    return target.FindFirstType('ErtsCodeInfo')
def ErtsCodeInfoPtr(target):
    return ErtsCodeInfo(target).GetPointerType()

def Process(target):
    return target.FindFirstType('Process')
def ProcessPtr(target):
    return Process(target).GetPointerType()
def ProcessPtrPtr(target):
    return ProcessPtr(target).GetPointerType()

# Globals

def erts_atom_table(target):
    return global_var('erts_atom_table', target)

def erts_proc(target):
    return global_var('erts_proc', target)

def etp_arch_bits(target):
    return global_var('etp_arch_bits', target).unsigned

def etp_big_endian(target):
    return global_var('etp_endianness', target).unsigned > 0

def etp_halfword(target):
    return False

def the_active_code_index(target):
    return global_var('the_active_code_index', target)

# Functions

def atom_tab(valobj):
    idx = valobj.unsigned
    target = valobj.target
    seg = erts_atom_table(target).GetChildMemberWithName('seg_table')
    slot = offset(idx >> 16, seg).deref
    entry = offset(idx >> 6 & 0x3FF, slot).deref
    return entry.Cast(Atom(target).GetPointerType())

def erts_lookup_function_info(valobj):
    r = find_range(valobj)
    if r is None:
        return None
    pc = valobj.unsigned
    target = valobj.target
    start = r.GetChildMemberWithName('start').Cast(EtermPtr(target))
    curr = offset(MI_FUNCTIONS, start).Cast(ErtsCodeInfoPtr(target).GetPointerType())
    prev = curr
    cnt = offset(MI_NUM_FUNCTIONS, start).deref.unsigned
    for x in range(0, cnt):
        prev = curr
        curr = offset(1, curr)
        if pc < curr.deref.unsigned:
            return prev.deref.GetChildMemberWithName('mfa')
    return None

def find_range(valobj):
    pc = valobj.unsigned
    target = valobj.target
    active = the_active_code_index(target).unsigned
    ranges = offset(active, global_var('r', target))
    n = ranges.GetChildMemberWithName('n').unsigned
    low = ranges.GetChildMemberWithName('modules')
    high = offset(n, low)
    range_type = Range(target)
    range_pointer_type = range_type.GetPointerType()
    mid = ranges.GetChildMemberWithName('mid').Cast(range_pointer_type)
    while low.unsigned < high.unsigned:
        start = mid.GetChildMemberWithName('start').unsigned
        end = atomic_value(mid.GetChildMemberWithName('end'))
        if pc < start:
            high = mid
        elif pc > end:
            low = offset(1, mid).Cast(range_pointer_type)
        else:
            return mid
        length = (high.unsigned - low.unsigned) // range_type.size
        mid = offset(length // 2, low)
    return None

def mfa(mfa):
    return '%s:%s/%d' % (eterm(mfa.GetChildMemberWithName('module')),
                         eterm(mfa.GetChildMemberWithName('function')),
                         mfa.GetChildMemberWithName('arity').unsigned)

def pixdata2data(valobj):
    pixdata = valobj.unsigned
    proc = erts_proc(target)
    ro = proc.GetChildMemberWithName('r').GetChildMemberWithName('o')
    pix_mask = ro.GetChildMemberWithName('pix_mask').unsigned
    pix_cl_mask = ro.GetChildMemberWithName('pix_cl_mask').unsigned
    pix_cl_shift = ro.GetChildMemberWithName('pix_cl_shift').unsigned
    pix_cli_mask = ro.GetChildMemberWithName('pix_cli_mask').unsigned
    pix_cli_shift = ro.GetChildMemberWithName('pix_cli_shift').unsigned
    data = pixdata & ~pix_mask
    data |= (pixdata >> pix_cl_shift) & pix_cl_mask
    data |= (pixdata & pix_cli_mask) << pix_cli_shift
    return data

def strip_literal_tag(valobj):
    if valobj.size == 4:
        # This is a 32-bit executable. There are no literal tags.
        return valobj

    # Strip literal tags from list and boxed terms.
    primary_tag = valobj.unsigned & c('TAG_PRIMARY_MASK')
    if ((primary_tag == c('TAG_PRIMARY_LIST') or
         primary_tag == c('TAG_PRIMARY_BOXED')) and
         (valobj.unsigned & c('TAG_LITERAL_PTR'))):
        valobj = valobj.CreateValueFromData(
            valobj.name,
            lldb.SBData.CreateDataFromInt(valobj.unsigned & c('PTR_MASK')),
            Eterm(valobj.target))

    return valobj

def init(target):
    names = ['beam_run_process', 'beam_normal_exit', 'beam_exit', 'beam_save_calls_export',
             'beam_save_calls_fun', 'beam_bif_export_trap', 'beam_export_trampoline',
             'beam_continue_exit', 'beam_return_to_trace', 'beam_return_trace',
             'beam_exception_trace', 'beam_call_trace_return']
    for name in names:
        code_pointers[global_var(name, target).unsigned] = name

# Lookup function for constants reflected in erl_etp.c, saving us from the
# massive headache of having to update this script every time the tag scheme
# changes.
constants = {}
def c(name):
    constant = constants.get(name)
    if constant != None:
        return constant
    else:
        target = lldb.debugger.GetSelectedTarget()
        constant = global_var('etp_' + name.lower(), target).unsigned
        constants[name] = constant
        return constant

# LLDB utils

def global_var(name, target):
    return target.FindGlobalVariables(name, 1)[0]

def offset(i, valobj):
    # print("offset(" + str(i) + ", " + str(valobj.unsigned) + ")")
    val = valobj.GetChildAtIndex(i, lldb.eNoDynamicValues, True)
    if valobj.TypeIsPointerType():
        return val.address_of.Cast(valobj.GetType())
    else:
        return val

def atomic_value(valobj):
    value = valobj.GetChildMemberWithName('counter')
    if value.IsValid():
        return value.unsigned
    else:
        return valobj.GetChildMemberWithName('value').unsigned
