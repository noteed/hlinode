# hlinode - Haskell bindings to the Linode API

The library is in early stage. Currently only the following DNS methods are
available:

  - domain.list (the optional DomainID can not be given)
  - domain.resource.list (the optional ResourceID can not be given)
  - domain.resource.create (only name and target can be given, type is
    either hardcoded to "A" or "CNAME")
  - domain.resource.delete

A command-line tool (also called `hlinode`) exposes the methods.
