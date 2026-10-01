#!/usr/bin/env python3
"""Regenerate the whiteboard fonts and their advance-width tables.

Usage: python3 fonts.py EXCALIDRAW_CHECKOUT

Reads Excalidraw's unicode-range subsets from
packages/excalidraw/fonts/{Excalifont,Nunito,ComicShanns}/*.woff2, merges
each family into one font, and writes FAMILY.ttf (host renderer), FAMILY.woff2
(browser editor) and font-metrics.json (shared text measurement), which also
covers the bundled Noto Sans fallback, font.ttf.  Needs fontTools and brotli.
"""
import glob, json, os, sys
from fontTools.ttLib import TTFont
from fontTools.merge import Merger

here = os.path.dirname(os.path.abspath(__file__))
source = os.path.join(sys.argv[1], 'packages/excalidraw/fonts')
families = {'Excalifont': 'Excalifont', 'Nunito': 'Nunito', 'ComicShanns': 'Comic Shanns'}


def metrics(font, name):
    cmap, hmtx = font.getBestCmap(), font['hmtx'].metrics
    advances = {str(cp): hmtx[glyph][0] for cp, glyph in sorted(cmap.items()) if cp >= 32}
    return {'name': name, 'unitsPerEm': font['head'].unitsPerEm,
            'fallback': round(sum(advances.values()) / len(advances)), 'advances': advances}


tables = {}
for file, name in families.items():
    parts = []
    for i, path in enumerate(sorted(glob.glob(os.path.join(source, file, '*.woff2')),
                                    key=lambda p: -os.path.getsize(p))):
        font = TTFont(path)
        font.flavor = None
        parts.append(os.path.join(here, f'.{file}-{i}.ttf'))
        font.save(parts[-1])
    merged = Merger().merge(parts)
    for part in parts:
        os.remove(part)
    # Renderers select fonts by family name; subsets carry inconsistent ones.
    for record in merged['name'].names:
        if record.nameID in (1, 16):
            record.string = name
        elif record.nameID in (4, 6):
            record.string = name if record.nameID == 4 else name.replace(' ', '') + '-Regular'
        elif record.nameID in (2, 17):
            record.string = 'Regular'
    merged.save(os.path.join(here, f'{file}.ttf'))
    merged.flavor = 'woff2'
    merged.save(os.path.join(here, f'{file}.woff2'))
    tables[name] = metrics(TTFont(os.path.join(here, f'{file}.ttf')), name)
tables['Noto Sans'] = metrics(TTFont(os.path.join(here, 'font.ttf')), 'Noto Sans')
with open(os.path.join(here, 'font-metrics.json'), 'w') as out:
    json.dump(tables, out, separators=(',', ':'))
    out.write('\n')
