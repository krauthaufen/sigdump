sigdump
=======

sigdump is a small tool creating C#-signatures for assemblies. It's printing all public types/methods/properties/etc. of the given dll and is also able to compute a hash for that signature.

This is useful for determining if the public API of a dll has changed.

sigdump can be included via nuget:
[sigdump nuget package](https://www.nuget.org/packages/sigdump/1.0.0)
