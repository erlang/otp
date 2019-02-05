#
# %CopyrightBegin%
#
# Copyright Ericsson AB 2013-2016. All Rights Reserved.
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

has_exited = False

def stop_handler (event):
  global has_exited
  if isinstance(event, gdb.SignalEvent):
    print("exit code: %s" % (event.stop_signal))
    has_exited = True

gdb.events.stop.connect (stop_handler)

gdb.execute('continue')

while not has_exited:
  r = gdb.execute('when', to_string=True)
  m = re.match("[^0-9]*([0-9]+)", r)
  if m:
    event = int(m.group(1));
    gdb.execute('start ' + str(event + 1));
    gdb.execute('continue')

gdb.events.stop.disconnect (stop_handler)

gdb.execute('file ' + str(gdb.parse_and_eval("$etp_beam_executable")))
gdb.execute('break main')
gdb.execute('reverse-continue')
