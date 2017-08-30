# HWadTools

## About

Doom WAD file manipulation tools, written in Haskell.

Copyright © Jonathan Dowland 2017

## `lswad`

A simple tool to list the contents of WAD files.

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

## `wadxtract`

A basic WAD extractor.

### Usage

```
$ wadxtract some.wad out-dir
```

WAD lumps are written out to files within out-dir, with filenames matching their lump names.
Lumps with the same name will overwrite each other.

A list of lumps and labels (0-length lumps) in the order they appear in the WAD's directory
is written to a file `wadinfo.txt`.
