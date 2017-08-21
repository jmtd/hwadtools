# HWadTools

## About

Doom WAD file manipulation tools, written in Haskell

Copyright © Jonathan Dowland 2017

## `lswad`

So far the only tool included is `lswad`, a simple tool to list the contents of WAD files.

### Usage

```
$ lswad some.wad
$ lswad < some.wad
```

### Example

```
$ lswad jonhex.wad
MAP01
THINGS
LINEDEFS
SIDEDEFS
VERTEXES
SEGS
SSECTORS
NODES
SECTORS
REJECT
BLOCKMAP
GL_MAP01
GL_VERT
GL_SEGS
GL_SSECT
GL_NODES
GL_PVS
WADCSRC
```


