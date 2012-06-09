#!/usr/bin/env python

from subprocess import call, Popen, PIPE

p1 = Popen(["ps", "-ww", "x"], stdout=PIPE)
p2 = Popen(["grep", "beam"], stdin=p1.stdout, stdout=PIPE)
p1.stdout.close()
output = p2.communicate()[0]

found = None

for line in output.splitlines():
    words = line.split()
    pid = words[0]
    execf = words[4]
    argsf = " ".join(words[5:])
    if execf.find("beam") == -1:
	continue
    print "Could debug pid {0}: {1} {2}".format(pid, execf, argsf)
    if found == None or argsf.find("-sname test_server") != -1:
        found = {'pid': pid, 'execf': execf}
if found != None:
    print "Choosing pid {0}".format(found['pid'])
    call(["gdb", found['execf'], found['pid']])
    exit(0)
else:
    print "Could not find beam process to debug"
    exit(1)
