# ERL-1112 - Record already defined

## Example

```erlang
-record(person, {name, age}).
-record(person, {name, age, city}).
```

```

```

## Explanation

This error occurs when you attempt to define a record with the same name
more than once in the same module.

Each record name must be unique within a module. If you need to modify a
record definition, you should update the original definition rather than
creating a new one with the same name.

To fix this error, either:

- Remove the duplicate record definition
- Rename one of the records to have a different name
- Combine the field definitions into a single record definition

```erlang
%% Correct approach - single record definition
-record(person, {name, age, city}).
```