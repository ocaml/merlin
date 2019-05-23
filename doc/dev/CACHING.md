# File\_id

The basic abstraction to check if a file has changed on disk is `File_id.t`.

These can be computed using `File_id.get`.  After that, `File_id.check` returns
true if and only if the contents of the file didn't change:
- file was missing and is still missing,
- contents of the file changed.

## Caching file identities

Since the state of the disk is not supposed during the execution of a command,
the results of `File_id.get` can be cached in some scope.

Using `File_id.with_cache (fun () -> <body>)`, the results of calls to
`File_id.get` will be memoized during the execution of `<body>`.

# Caching file contents, the `File_cache` functor

The `File_cache` functor caches the contents of a computation based on a
filename for as long as the file don't change (as determined by `File_id`).

For instance `Cmi_format.read` loads a cmi file from the disk. The OCaml
compiler calls it directly as a cmi is not supposed to change while the
compiler is working.

Merlin will live for a long time and should reload files that have changed on
disk. At the same time, not reloading files that haven't changed provide a
significant speed up.

`File_cache.Make(Cmi_format)` gives just that.

- for .cmi, there is `Cmi_cache = File_cache.Make(Cmi_format)`
- for .cmt, there is `Cmt_cache = File_cache.Make(Cmt_format)`
- for .merlin, there is `Mconfig_dot.Cache`
- existence of files (see below), there is `Misc.Exists_in_directory`

# File existence

To discover files on disk, Merlin follow OCaml approach of checking the load
path in order for file existence.

Doing this results in the lookup phase being quadratic: with n modules and m
paths, there can be up to n * m calls to stat/file\_exists.

In normal cases, a call to stat is cheap and this is insignificant. However,
under some cicumstances this degenerates:
- in some configurations (selinux?) we observed stat being up to two magnitude
  orders more expensive (the same applies to NFS and other networked, although
  supporting this situation is not of prime importance)
- big projects with a naive .merlin tend to have a huge load path (hundreds of
  entries, in part because of .merlin lack of expressivity).

To speed up computations, determination of file existence is split in two steps:
- first the `File_id` of the directory in which the file is stored is computed
- the existence of the file is cached based on the id of the directory.

While in the worst case this doesn't bring back a linear behavior, as most
directories don't change this is fast enough in practice (the quadratic part
happens all in memory).

The important parts:
- existence of a file depends on the id of its parent directory (because adding
  or removing a file affect the contents of the directory)
- contents of a file depends on the id of the file itself.

# Refreshing cache

A new function `Env.check_state_consistency` compares all global modules loaded
in the environment to the version on disk.  If it returns false, the `Env.t`
should be discarded and recomputed from zero.

# Cache flush policy

As time passes, the cache grows. Some files are kept in memory but aren't going
to be used anymore.

`Mocaml.flush_caches` remove all files that have changed on disk or that
haven't been used for some time.  By default, `ocamlmerlin_server` remove
entries that haven't been used in the last 300 seconds.

Since this involve stating each entry, the check is done after answering.

The policy is still quite naive, improvements are welcome (IDEA?).

# Type environment cache

Since the user is likely to ask many queries on the same environment in a row,
the last 5 environments are cached (`Mtyper.cache`).

This number might be adjusted... ? Also, entries could be filtered by time of
last use.
