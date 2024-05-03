<!--
%CopyrightBegin%

Copyright Ericsson AB 2023-2024. All Rights Reserved.

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.

%CopyrightEnd%
-->
# Usage

## Overview

This document focuses on the graphical parts of the tool. The concepts are
explained in the reference manual for the module `reltool`.

## System window

The system window is started with the function `reltool:start/1`. At startup the
tool will process all `beam` files and `app` files in order to find out
dependencies between applications and their modules. Once all this information
has been derived, it will be possible to explore the tool.

The system window consists of four main pages (tabs):

- Libraries
- System settings
- Applications
- Releases

Click on a name tag to display its page.

### Libraries

On the library page it is possible to control which sources the tool will use.
The page is organized as a tree which can be expanded and collapsed by clicking
on the little symbol in the beginning of the expandable/collapsible lines.

The `Root directory` can be edited by selecting the line where the path of the
root directory is displayed and clicking the right mouse button. Choose edit in
the menu that pops up.

Library directories can be added, edited or deleted. This is done by selecting
the line where the path to a library directory is displayed and clicking the
right mouse button. Choose add, edit or delete in the menu that pops up. New
library directories can also be added by selecting the line
`Library directories` and clicking the right mouse button. Choose add in the
menu that pops up.

Escript files can be added, edited or deleted. This is done by selecting the
line where the path to an escript file is displayed and clicking the right mouse
button. Choose add, edit or delete in the menu that pops up. New escripts can
also be added by selecting the line `Escript files` and clicking the right mouse
button. Choose add in the menu that pops up.

When libraries and escripts are expanded, the names of their contained
applications will be displayed. Double click on an application name to launch an
application window.

### System settings

On the system settings page it is possible to control some global settings that
are used as defaults for all applications. Set the
`Application inclusion policy` to `include` to include all applications that are
not explicitly excluded. See `incl_cond` (application inclusion) and `mod_cond`
(module inclusion) in the reference manual for the module `reltool` for more
info.

The system settings page is rather incomplete.

### Applications

There are four categories of applications on the applications page. `Included`
contains applications that are explicitly included. `Excluded` contains
applications that are explicitly excluded. `Derived` contains applications that
either are used directly by explicitly included applications or by other derived
applications. `Available` contains the remaining applications.

Select one or more applications and click on a button directly below the
application column to change application category. For example, select an
available application and click on its tick button to move the application to
the included category. Clicking on the tick symbol for included applications
will move the application back to the available category. The tick is undone.

The symbols in front of the application names are intended to describe the
status of the application. There are error and warning symbols to signalize that
there is something which needs attention. The tick symbol means that the
application is included or derived and no problem has been detected. The cross
symbol means that the application is excluded or available and no problem has
been detected. Applications with error symbols are listed first in each category
and are followed by the warnings and the normal ones (ticks and crosses) at the
end.

Double click on an application to launch its application window.

### Releases

The releases page is incomplete and very experimental.

### File menu

- `Display application dependency graph` \- Launches an application force graph
  window. All included and derived applications and their dependencies will be
  shown in a graph.
- `Display module dependency graph` \- Launch a module force graph window. All
  included and derived modules and their dependencies will be shown in a graph.
- `Reset configuration to default`
- `Undo configuration (toggle)`
- `Load configuration` \- Loads a new configuration from file.
- `Save configuration` \- Saves the current configuration to file. Normally,
  only the explicit configuration parameters with values that differ from their
  defaults are saved. But the configuration with or without default values and
  with or without derived values may also be saved.
- `Generate rel, script and boot files`
- `Generate target system`
- `Close` \- Close the system window and all its subwindows.

### Dependencies between applications or modules displayed as a graph

The dependency graph windows are launched from the file menu in the system
window. The graph depicts all included and derived applications/modules and
their dependencies.

It is possible to perform some limited manipulations of the graph. Nodes can be
moved, selected, locked or deleted. Move a single node or the entire graph by
moving the mouse while the left mouse button is pressed. A node can be locked
into a fix position by holding down the shift button when the left mouse button
is released. Select several nodes by moving the mouse while the control key and
the left mouse button are pressed. Selected nodes can be locked, unlocked or
deleted by clicking on a suitable button.

The algorithm that is used to draw a graph with as few crossed links as possible
is called force graph. A force graph consists of nodes and directed links
between nodes. Each node is associated with a repulsive force that pushes nodes
away from each other. This force can be adjusted with the left slider or with
the mouse wheel. Each link is associated with an attractive force that pulls the
nodes nearer to each other. This force can be adjusted with the right slider. If
this force becomes too strong, the graph will be unstable. The third parameter
that can be adjusted is the length of the links. It is adjusted with the middle
slider.

The `Freeze` button starts/stops the redrawing of the graph. `Reset` moves the
graph to the middle of the window and resets all graph settings to default, with
the exception of deleted nodes.

## Application window

The application window is started by double clicking on an application name. The
application window consists of four pages (tabs):

- Application settings
- Modules
- Application dependencies
- Module dependencies

Click on a name tag to display its page.

### Application settings

Select version of the application in the `Source selection policy` part of the
page. By default the latest version of the application is selected, but it is
possible to override this by explicitly selecting another version.

Note that in order for reltool to sort application versions and thereby be able
to select the latest, it is required that the version id for the application
consists of integers and dots only, for example `1`, `2.0` or `3.17.1`.

By default the `Application inclusion policy` on system level is used for all
applications. Set the value to `include` if you want to explicitly include one
particular application. Set it to `exclude` if you want to exclude the
application despite that it is used by another (explicitly or implicitly)
included application. `derived` means that the application automatically will be
included if some other (explicitly or implicitly) included application uses it.

By default the `Module inclusion policy` on system level is used for all
applications. Set it to `derived` if you only want actually used modules to be
included. Set it to `app` if you, besides derived modules, also want the modules
listed in the app file to be included. Set it to `ebin` if you, besides derived
modules, also want the modules that exist as beam files in the ebin directory to
be included. Set it to `all` if you want all modules to be included, that is the
union of modules found in the ebin directory and listed in the app file.

The application settings page is rather incomplete.

### Modules

There are four categories of modules on the modules page. `Included` contains
modules that are explicitly included. `Excluded` contains modules that are
explicitly excluded. `Derived` contains modules that either are used directly by
explicitly included modules or by other derived modules. `Available` contains
the remaining modules.

Select one or more modules and click on a button directly below the module
column to change module category. For example, select an available module and
click on its tick button to move the module to the included category. Clicking
on the tick symbol for included modules will move the module back to the
available category. The tick is undone.

The symbols in front of the module names are intended to describe the status of
the module. There are error and and warning symbols to signalize that there is
something that needs attention. The tick symbol means that the module is
included or derived and no problem has been detected. The cross symbol means
that the module is excluded or available and no problem has been detected.
Modules with error symbols are listed first in each category and are followed by
warnings and the normal ones (ticks and crosses) at the end.

Double click on a module to launch its module window.

### Application dependencies

There are four categories of applications on the `Application dependencies`
page. If the application is used by other applications, these are listed under
`Used by`. If the application requires other applications be started before it
can be started, these are listed under `Required`. These applications are listed
in the `applications` part of the `app` file. If the application includes other
applications, these are listed under `Included`. These applications are listed
in the `included_applications` part of the `app` file. If the application uses
other applications, these are listed under `Uses`.

Double click on an application name to launch an application window.

### Module dependencies

There are two categories of modules on the `Module dependencies` page. If the
module is used by other modules, these are listed under `Modules using this`. If
the module uses other modules, these are listed under `Used modules`.

Double click on an module name to launch a module window.

## Module window

The module window is started by double clicking on an module name. The module
window consists initially of two pages (tabs):

- Dependencies
- Code

Click on a name tag to display its page.

### Dependencies

There are two categories of modules on the `Dependencies` page. If the module is
used by other modules, these are listed under `Modules using this`. If the
module uses other modules, these are listed under `Used modules`.

Double click on an module name to launch a module window.

### Code

On the `Code` page the Erlang source code is displayed. It is possible to search
forwards and backwards for text in the module. Enter a regular expression in the
`Find` field and press enter. It is also possible to go to a certain line in the
module. The `Back` button can be used to go back to the previous position.

Put the marker on a function name and double click to go to the definition of
the function. If the function is defined in another module, that module will be
loaded and added to the page list.
