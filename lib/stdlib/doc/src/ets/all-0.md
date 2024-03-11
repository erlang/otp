Returns a list of all tables at the node. Named tables are specified by their
names, unnamed tables are specified by their table identifiers.

There is no guarantee of consistency in the returned list. Tables created or
deleted by other processes "during" the `ets:all()` call either are or are not
included in the list. Only tables created/deleted _before_ `ets:all()` is called
are guaranteed to be included/excluded.
