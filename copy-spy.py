#!/usr/bin/env python

import sys, re

verbose = False

def inheap (ptr, heap):
    return heap[0] <= ptr and ptr < heap[1] or \
           heap[2] > 0 and heap[2] <= ptr and ptr < heap[3]

def check_safe (pid, entry):
    if entry['mangled'] or entry['tabled']:
        print "[pid={0}] safety invariant violated, WTF?".format(pid)
        print "  mangled:",
        for ptr in entry['mangled']:
            print "{0:x}".format(ptr),
        print
        print "  tabled:",
        for ptr in entry['tabled']:
            print "{0:x}".format(ptr),
        print
        sys.exit(1)

pending = {}

def store (pid, id, data, next, fun):
    try:
        entry = pending[pid]
    except KeyError:
        entry = {'last': 0, 'expecting': [], 'exclusive': False,
                 'mangled': set(), 'tabled': set()}
    fun(pid, entry, data)
    exclusive = entry['exclusive']
    expecting = entry['expecting']
    pending_exclusive = []
    for opid in pending:
        if pid != opid and pending[opid]['exclusive']:
            pending_exclusive.append(opid)
    if id == 0:
        if exclusive:
            print "[pid={0}] GC invoked while in exclusive state {1}".format(pid, last)
        if pending_exclusive and verbose:
            print "[pid={0}] GC invoked while {1} was/were pending in exclusive state".format(pid, pending_exclusive)
    elif id not in [10, 13, 14, 15, 16, 17, 18]:
        try:
            next_exclusive = next[0]
            next_expecting = next[1]
        except:
            next_exclusive = exclusive
            next_expecting = []
        try:
            expected = expecting.pop(0)
            if id != expected:
                print "[pid={0}] unexpected event {1}, expected {2}".format(pid, id, expected)
        except:
            if pending_exclusive and next_exclusive:
                print "[pid={0}] event {1} while {2} was/were pending in exclusive state".format(pid, id, pending_exclusive)
        expecting.extend(next_expecting)
        if expecting:
            entry['last'] = id
            entry['exclusive'] = next_exclusive
            pending[pid] = entry
        else:
            check_safe(pid, entry)
            try:
                del pending[pid]
            except:
                pass

def is_obj (ptr):
    return ptr % 4 in [1, 2]

obj = {}
own = {}
touched = {}

def check_pid (ptr, pid):
    if obj[ptr]['pid']:
        if not pid in obj[ptr]['pid']:
            print "[pid={0}] check_pid object {1:x} shared with".format(pid, ptr),
            for opid in obj[ptr]['pid']:
                print opid,
            print
	    try:
                print "  is", obj[ptr]['term']
	    except KeyError:
	        print "  is <unknown>"
    obj[ptr]['pid'].add(pid)
    try:
        own[pid].add(ptr)
    except KeyError:
        own[pid] = set([ptr])

def add_pid (ptr, pid):
    obj[ptr] = {'pid': set([pid])}
    try:
        own[pid].add(ptr)
    except KeyError:
        own[pid] = set([ptr])

def fun_nothing (pid, entry, data):
    pass
def fun_gc (pid, entry, data):
    check_safe(pid, entry)
    my_obj_list = []
    sh_obj_list = []
    try:
        for ptr in own[pid]:
            if pid not in obj[ptr]['pid']:
                print "[pid={0}] owned {1:x} but not in its pid list, WTF?".format(pid, ptr)
            if len(obj[ptr]['pid']) == 1:
                my_obj_list.append(ptr)
            else:
                sh_obj_list.append(ptr)
                obj[ptr]['pid'].remove(pid)
        for ptr in my_obj_list:
            del obj[ptr]
    except KeyError:
        pass
    try:
	for ptr in touched[pid]:
            if not inheap(ptr, data):
                print "[pid={0}] touched {1:x} but not in its heap {2:x} {3:x} {4:x} {5:x}, WTF?".format(pid, ptr, data[0], data[1], data[2], data[3])
    except KeyError:
        pass
    if verbose:
        print "[pid={0}] GC results".format(pid)
        print "  may have collected",
        for ptr in my_obj_list:
            print "{0:x}".format(ptr),
        print
        if sh_obj_list:
            print "  not shared anymore",
            for ptr in sh_obj_list:
                print "{0:x}".format(ptr),
            print
    own[pid] = set()
    touched[pid] = set()

def fun_size_object (pid, entry, data):
    check_safe(pid, entry)
    data = data[0]
    if is_obj(data):
        if data in obj:
            check_pid(data, pid)
        else:
            add_pid(data, pid)
    entry['original'] = data
    entry['type'] = 'flat'
def fun_size_was (pid, entry, data):
    data = data[0]
    ptr = entry['original']
    size_type = entry['type'] + "size"
    if is_obj(ptr):
        check_pid(ptr, pid)
        try:
            if obj[ptr][size_type] != data:
                print "[pid={0}] {1} differs for object {2:x}".format(pid, size_type, ptr)
                print "  was {0}".format(obj[ptr][size_type])
                print "  now {0}".format(data)
                obj[ptr][size_type] = data
        except:
            obj[ptr][size_type] = data
    entry[size_type] = data
def fun_copy_struct (pid, entry, data):
    check_safe(pid, entry)
    data = data[0]
    if is_obj(data):
        if data in obj:
            check_pid(data, pid)
        else:
            add_pid(data, pid)
    entry['original'] = data
    entry['type'] = 'flat'
def fun_result_is_at (pid, entry, data):
    data = data[0]
    ptr = entry['original']
    if is_obj(ptr):
        check_pid(ptr, pid)
        copy_type = entry['type'] + "copies"
        obj[data] = {'pid': set(), 'copy': ptr, 'type': copy_type}
        try:
            obj[ptr][copy_type] = obj[ptr][copy_type] + 1
        except:
            obj[ptr][copy_type] = 1
def fun_copy_shared_calculate (pid, entry, data):
    check_safe(pid, entry)
    data = data[0]
    if is_obj(data):
        if data in obj:
            check_pid(data, pid)
        else:
            add_pid(data, pid)
    entry['original'] = data
    entry['type'] = 'shared'
def fun_message_is (pid, entry, data):
    data = data[0]
    ptr = entry['original']
    if is_obj(ptr):
        check_pid(ptr, pid)
        try:
            if obj[ptr]['term'] != data:
                print "[pid={0}] term differs for object {1:x}".format(pid, ptr)
                print "  was {0}".format(obj[ptr]['term'])
                print "  now {0}".format(data)
                obj[ptr]['term'] = data
        except:
            obj[ptr]['term'] = data
    entry['original_term'] = data
def fun_copy_shared_perform (pid, entry, data):
    data = data[0]
    ptr = entry['original']
    entry['type'] = 'shared'
    if is_obj(ptr):
        check_pid(ptr, pid)
    if ptr != data:
        print "[pid={0}] copy_shared_{calculate/perform} mismatch for object{1:x}".format(pid, ptr)
        print "  was {0}".format(ptr)
        print "  now {0}".format(data)
def fun_original_was (pid, entry, data):
    check_safe(pid, entry)
    data = data[0]
    ptr = entry['original']
    if is_obj(ptr):
        check_pid(ptr, pid)
    if entry['original_term'] != data:
        print "[pid={0}] copy_shared_perform mismatch (original/after) for object {1:x}".format(pid, ptr)
        print "  was {0}".format(entry['original_term'])
        print "  now {0}".format(data)
    entry['original_term_after'] = data
def fun_copy_is (pid, entry, data):
    data = data[0]
    ptr = entry['original']
    if is_obj(ptr):
        check_pid(ptr, pid)
    if entry['original_term'] != data:
        print "[pid={0}] copy_shared_perform mismatch (original/copy) for object {1:x}".format(pid, ptr)
        print "  was {0}".format(entry['original_term'])
        print "  now {0}".format(data)
    if entry['original_term_after'] != data:
        print "[pid={0}] copy_shared_perform mismatch (after/copy) for object {1:x}".format(pid, ptr)
        print "  was {0}".format(entry['original_term_after'])
        print "  now {0}".format(data)
def fun_new_mbuf (pid, entry, data):
    #check_safe(pid, entry)
    pass
def fun_move_mbuf (pid, entry, data):
    check_safe(pid, entry)
def fun_delete_proc (pid, entry, data):
    fun_gc(pid, entry, data)
    del own[pid]
    del touched[pid]
def fun_mangle (pid, entry, data):
    data = data[0]
    if data in entry['mangled']:
        print "[pid={0}] already mangled {1:x}, WTF?".format(pid, data)
        sys.exit(0)
    entry['mangled'].add(data)
    try:
        touched[pid].add(data)
    except KeyError:
        touched[pid] = set([data])
def fun_unmangle (pid, entry, data):
    data = data[0]
    try:
        if data != entry['assoc'][0]:
            print "[pid={0}] tabling association mismatch {1:x} and {2:x}, WTF?".format(pid, data, entry['assoc'][0])
            sys.exit(0)
        data = entry['assoc'][1]
        del entry['assoc']
    except:
        pass
    if data not in entry['mangled']:
        print "[pid={0}] not mangled {1:x}, WTF?".format(pid, data)
        sys.exit(0)
    entry['mangled'].remove(data)
def fun_table (pid, entry, data):
    data = data[0]
    if data in entry['tabled']:
        print "[pid={0}] already tabled {1:x}, WTF?".format(pid, data)
        sys.exit(0)
    entry['tabled'].add(data)
def fun_table_assoc (pid, entry, data):
    if data[1] not in entry['tabled']:
        print "[pid={0}] not tabled {1:x}, when associating WTF?".format(pid, data)
        sys.exit(0)
    entry['assoc'] = data
def fun_untable (pid, entry, data):
    data = data[0]
    if data not in entry['tabled']:
        print "[pid={0}] not tabled {1:x}, when untabling WTF?".format(pid, data)
        sys.exit(0)
    entry['tabled'].remove(data)
def fun_bypassed (pid, entry, data):
    pass

prefix_list = [
    ( 0, "MINOR GC:", ["ptr", "ptr", "ptr", "ptr"], None, fun_gc),
    ( 0, "MAJOR GC:", ["ptr", "ptr", "ptr", "ptr"], None, fun_gc),
    ( 1, "size_object", ["ptr"], (False, [2]), fun_size_object),
    ( 2, "size was:", ["int"], None, fun_size_was),
    ( 3, "copy_struct", ["ptr"], (False, [4]), fun_copy_struct),
    ( 4, "result is at", ["ptr"], None, fun_result_is_at),
    ( 5, "copy_shared_calculate", ["ptr"], (True, [6, 2, 7]), fun_copy_shared_calculate),
    ( 6, "message is", ["term"], None, fun_message_is),
    ( 7, "copy_shared_perform", ["ptr"], (True, [8, 9, 4]), fun_copy_shared_perform),
    ( 8, "original was", ["term"], None, fun_original_was),
    ( 9, "copy is", ["term"], None, fun_copy_is),
    (10, "new message buffer", ["ptr"], None, fun_new_mbuf),
    (11, "moving message buffer", ["ptr"], None, fun_move_mbuf),
    (12, "delete process", ["ptr", "ptr", "ptr", "ptr"], None, fun_delete_proc),
    (13, "mangling", ["skip", "ptr"], None, fun_mangle),
    (14, "unmangling", ["skip", "ptr"], None, fun_unmangle),
    (15, "tabling", ["skip", "ptr"], None, fun_table),
    (16, "tabled", ["skip", "ptr", "skip", "ptr"], None, fun_table_assoc),
    (17, "untabling", ["skip", "ptr"], None, fun_untable),
    (18, "bypassed copying", ["ptr", "skip", "term"], None, fun_bypassed)
    ]

def parse_message(msg):
    for (id, prf, info, next, fun) in prefix_list:
        if msg.startswith(prf):
            n = len(prf) + 1
	    rest = msg[n:]
	    d = []
	    for wtype in info:
                if wtype == "term":
                    d.append(rest)
		    break
	        words = rest.split(None, 1)
	        try:
		    word = words[0]
		except:
		    word = ""
                try:
		    rest = words[1]
		except:
		    rest = ""
                if wtype == "skip":
		    pass
                elif wtype == "int":
                    d.append(int(word))
                elif wtype == "ptr":
                    d.append(int(word[2:], 16))
            return (id, d, next, fun)
    print "I don't know what this is:", msg

try:
    f = open(sys.argv[1], "rt")
except:
    f = sys.stdin

for line in f:
    re_line = re.compile("(.*)\[pid=([^]]+)\] (.*)\n")
    if verbose:
        print "I:", line.strip()
    g = re_line.match(line)
    try:
        prf = g.group(1)
        if prf != "":
            print "Ignoring prefix:", prf
        pid = g.group(2)
        (id, data, next, fun) = parse_message(g.group(3).rstrip())
    except:
        print "Ignoring line:", line.strip()
    else:
        store(pid, id, data, next, fun)

print "----------------------------------------------------"
print "POST MORTEM STUFF"
print "----------------------------------------------------"

for (pid, entry) in pending.items():
    print "[pid={0}]".format(pid)
    print "  mangled:",
    for ptr in entry['mangled']:
        print "{0:x}".format(ptr),
    print
    print "  tabled:",
    for ptr in entry['tabled']:
        print "{0:x}".format(ptr),
    print

f.close()
