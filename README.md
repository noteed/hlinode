# hlinode - Haskell bindings to the Linode API

The library is in early stage. Functions return JSON values (using Aeson's
Value data type). Currently only the following DNS methods are available:

  - domain.list (the optional DomainID can not be given)

