AXL - Auxiliary Library
-----------------------

AXL is a lightweight library written for the purpose of embedding it in projects in a way to feel like it's part
of the project itself. The goal of the library is to provide low-level primitives for building high-performance
applications and to wrap the standard C++ library functionality where possible to create code that doesn't depend
on the standard library much (it still uses some bits of it, like <type_traits>, but that's it).

AXL Goals
---------

  - provides a lightweight foundation that high-performance projects can be built on
  - not a standalone library - requires a minimum integration to fit - for example to use your own result type
  - no use of exceptions - we consider exceptions a mistake in a C++ language design, thus AXL doesn't use them
  - everything that can fail returns a result-type, which can be propagated by XXX_PROPAGATE macro
  - arena allocators and arena/in-place containers - no generic containers are provided

AXL Functionality
-----------------

  - `commons.h` - common low-level functionality
  - `algorithm.h` - algorithms like `lower_bound()` and `sort()`
  - `span.h` - span
  - `inplace_*.h` - in-place containers
  - `arena*.h` - arena and arena-based containers
  - `string*.h` - string and related utilities

AXL Integration
---------------

-- TODO --
