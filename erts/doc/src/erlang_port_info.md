Returns information about `Port`.

If the port identified by `Port` is not open, `undefined` is returned. If the port is closed and the calling process was previously linked to the port, the exit signal from the port is guaranteed to be delivered before `port_info/2` returns `undefined`.

`Item` is one of the following and can be used to get various information about the `Port`.

- `connected` - returns `{connected, Pid}` where `Pid` is the process identifier of the process connected to the port.
- `id` - returns `{id, Index}` where `Index` is the internal index of the port. This index can be used to separate ports.
- `input` - returns `{input, Bytes}` where `Bytes` is the total number of bytes read from the port.
- `links` - returns `{links, Pids}` where `Pids` is a list of the process identifiers of the processes that the port is linked to.
- `locking` - returns `{locking, Locking}` where `Locking` is one of the following:
  * `port_level` (port-specific locking)
  * `driver_level` (driver-specific locking)
  Notice that these results are highly implementation-specific and can change in a future release.
  
  Since: OTP R16B
- `memory` {: #port_info_memory } - returns `{memory, Bytes}` where `Bytes` is the total number of bytes allocated for this port by the runtime system. The port itself can have allocated memory that is not included in `Bytes`.
  
  Since: OTP R16B
- `monitors` - returns `{monitors, Monitors}` where `Monitors` represent processes monitored by this port.
  
  Since: OTP R16B
- `monitored_by` - returns `{monitored_by, MonitoredBy}` where `MonitoredBy` is a list of pids that are monitoring given port at the moment.
  
  Since: OTP 19.0
- `name` - returns `{name, Name}` where `Name` is the command name set by `open_port/2`.
- `os_pid` - returns `{os_pid, OsPid}` where `OsPid` is the process identifier (or equivalent) of an OS process created with [`open_port({spawn | spawn_executable, Command}, Options)`](`open_port/2`). If the port is not the result of spawning an OS process, the value is `undefined`.
  
  Since: OTP R16B
- `output` - returns `{output, Bytes}` where `Bytes` is the total number of bytes written to the port from Erlang processes using `port_command/2`, `port_command/3`, or `Port ! {Owner, {command, Data}`.
- `parallelism` - returns `{parallelism, Boolean}` where `Boolean` corresponds to the port parallelism hint used by this port. For more information, see option [`parallelism`](`m:erlang#open_port_parallelism`) of `open_port/2`.
  
  Since: OTP R16B
- `queue_size` - returns `{queue_size, Bytes}` where `Bytes` is the total number of bytes queued by the port using the ERTS driver queue implementation.
  
  Since: OTP R16B
- `registered_name` - returns `{registered_name, RegisteredName}` where `RegisteredName` is the registered name of the port. If the port has no registered name, `[]` is returned.

Failure: `badarg` if `Port` is not a local port identifier, or an atom.
