This table lists the tools within the *xwadtools* distribution, describes them,
and then indicates what the plans are within *hwadtools* for that functionality
(if any)

xwadtool              | description           | hwadtool
--------------------- | --------------------- | ------------------------------
acc                   | acs compiler          | ×
bsp                   | node builder          | planned
deutex                | xtract/build          | wadxtract/wadbuild
dm2au                 | convert snd sun       | ×
dmpsmu                | map→postscript        | planned (sort-of; SVG)
idbsp                 | node builder          | planned
idmultigen            | state builder         | ×
listacs               | disassembler          | maybe
lswad                 | list wad              | ✓
mergewad              | superpose maps        | ×
mkgipal               | make GIMP palettes    | ×
mkpopal               | make Povray palettes  | ×
mkqmap                | …ppmqant…             | ×
mktran                | make TRANMAP lumps    | maybe
mkwad                 | wad builder           | wadbuild
mkxppal               | xpaint palette        | ×
pal2c                 | makes C struct palette| ×
qmus2mid              | mus→mid               | probably not
raw2ppm               | flat→ppm              | maybe (PNG)
raw2sfx               | raw→doom sfx          | maybe (WAV→SFX)
reject                | REJECT builder        | probably (part of hbsp?)
slige                 | random gen            | ×
swantbls              | SWITCHES/ANIMATED     | probably
tkwadcad              | GUI map editor        | ×
trigcalc              | gen linedef calc      | probably not
wadcat                | concatenate           | ✓
wadext                | extract directory     | ×
wadflat               | flats→PPM             | wadxtract
wadgc                 | [PPM]→PWAD            | wadbuild + some PNG→flat tool
wadlc                 | txt→PWAD(!)           | × (see wadc or hwadc)
wadldc                | map→txt               | possibly
wadpat                | patches→PPM           | maybe (PNG)
wadps                 | map→postscript        | maybe
wadsprit              | sprites→PPM           | maybe (PNG)
wadtex                | textures→PPM          | ×
wadtxls               | list textures in use  | wadstats?
wadwhat               | stats                 | wadstats| heuristics tool
warm                  | node builder          | planned
xew                   | wintex-like           | × (use slade)
