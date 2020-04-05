#!/usr/bin/env python

"""
  This program generates random text that matches a given regex-pattern.
  The pattern is given via sys.argv and the generated text is passed to
  the binary 'tests/test_rand' to check if the generated text also matches
  the regex-pattern in the C implementation.
  The exit-code of the testing program, is used to determine test success.

  This script is called by the Makefile when doing 'make test'
"""


import re
import sys
import string
import random
from subprocess import call


prog = "./tests/test_rand_neg"

if len(sys.argv) < 2:
  print("")
  print("usage: %s pattern [nrepeat]" % sys.argv[0])
  print("  where [nrepeat] is optional")
  print("")
  sys.exit(-1)

own_prog = sys.argv[0]
pattern = sys.argv[1]
if len(sys.argv) > 2:
  ntests = int(sys.argv[2])
else:
  ntests = 10
nfails = 0
repeats = ntests


try:
  repeats = int(sys.argv[2])
except:
  pass

sys.stdout.write("%-35s" % ("  pattern '%s': " % pattern))




def gen_no_match(pattern, minlen=1, maxlen=50, maxattempts=500):
  nattempts = 0
  while True:
    nattempts += 1
    ret = "".join([random.choice(string.printable) for i in range(random.Random().randint(minlen, maxlen))])
    if re.findall(pattern, ret) == []:
      return ret
    if nattempts >= maxattempts:
      raise Exception("Could not generate string that did not match the regex pattern '%s' after %d attempts" % (pattern, nattempts))



while repeats >= 0:
  try:
    repeats -= 1
    example = gen_no_match(pattern)
    #print("%s %s %s" % (prog, pattern, example))
    ret = call([prog, "\"%s\"" % pattern, "\"%s\"" % example])
    if ret != 0:
      escaped = repr(example) # escapes special chars for better printing
      print("    FAIL : matches %s unexpectedly [%s]." % (escaped, ", ".join([("0x%02x" % ord(e)) for e in example]) ))
      nfails += 1

  except:
    #import traceback
    #print("EXCEPTION!")
    #raw_input(traceback.format_exc())
    ntests -= 1
    repeats += 1
    #nfails += 1

sys.stdout.write("%4d/%d tests succeeded \n" % (ntests - nfails, ntests))
#print("")
