Fills an already created ETS table with the objects in the already opened Dets
table `DetsTab`. Existing objects in the ETS table are kept unless overwritten.

If any of the tables does not exist or the Dets table is not open, a `badarg`
exception is raised.
