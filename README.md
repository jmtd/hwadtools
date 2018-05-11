# HWadTools

> *&lt;zokum&gt; wadcat, it does exactly what I think it does?*<br />
> *&lt;zokum&gt; or is it a tool to replace all textures with images of cats*

## About

Doom WAD file manipulation tools, written in Haskell.

*hwadtools* is inspired by *xwadtools*. See [xwadtools](xwadtools.md) for
a tool-by-tool comparison.

Copyright © Jonathan Dowland 2018

<https://jmtd.net/software/hwadtools>

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

## `waddiff`

A WAD-difference calculator.

### Usage

```
$ waddiff a.wad b.wad
```

### Example

```
--- jonhex.wad
+++ jonhex2.wad
@@
 MAP01    (0 bytes)
 THINGS   (10 bytes)
-LINEDEFS (47950 bytes)
+LINEDEFS (47950 bytes)
@@
 SIDEDEFS (205080 bytes)
-VERTEXES (9048 bytes)
+VERTEXES (9360 bytes)
+SEGS     (83136 bytes)
+SSECTORS (11024 bytes)
+NODES    (77140 bytes)
@@
 SECTORS  (30264 bytes)
+REJECT   (0 bytes)
+BLOCKMAP (16124 bytes)
@@
 WADCSRC  (3275 bytes)
```

## `wadcat`

Concatenate WAD files.

### Usage

```
$ wadcat 1.wad 2.wad 3.wad … > out.wad
```

The input files `1.wad`, `2.wad`, `3.wad` (…) are read in and their lumps
concatenated into the output PWAD `out.wad`. The output PWAD's directory is
assembled from the inputs with appropriate offsets adjustments applied.

## `wadstats`

Prints out some statistics about the input WAD, in YAML format.

### Usage

```
$ wadstats input.wad
- E1M1:
    vertices: 4
    sectors: 1
    linedefs: 4
    things:
      count: 1
      Singleplayer:
       "player 1 start": 1
```

## `wadxtract`

A basic WAD extractor. Experimental.

### Usage

```
$ wadxtract some.wad out-dir
```

WAD lumps are written out to files within out-dir, with filenames matching their lump names.
Lumps with the same name will overwrite each other.

A list of lumps and labels (0-length lumps) in the order they appear in the WAD's directory
is written to a file `wadinfo.txt`.
