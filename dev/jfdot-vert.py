#!/usr/bin/env python3
"""Make the site's copy of JF Dot M+Q 10, the pixel face of the Japanese labels
in the running header, the running footer and the gutters.

Two things are done to the font (needs fonttools):

  - It has no GSUB table, so in vertical text ー lies down, 、 and 。 sit in the
    wrong corner, brackets face sideways and small kana hang low. This adds a
    vertical form of each such glyph and a `vert` feature that swaps them in,
    as dev/kkm-vert.py does for KKM Analog TV.
  - It is 1.3MB, with every kanji and its bitmaps. The site loads a subset:
    the kana, Japanese punctuation, full-width forms and the kanji the labels
    use, outlines only.

It writes two files:

    static/assets/fonts/jf-dot-mplusq10-vert.ttf   the font the site loads
    dev/jfdot-vert.html                            a page showing every form

    python3 dev/jfdot-vert.py [path/to/JF-Dot-MPlusQ10.ttf]

To change a form, edit the tables below, run this again and reload the page.
A label with a new kanji needs it added to KANJI. The font is free to use,
change and redistribute: see static/assets/fonts/jf-dot-mplusq10-LICENSE.txt.
"""
import base64
import copy
import html
import io
import math
import sys
from pathlib import Path

from fontTools import subset
from fontTools.feaLib.builder import addOpenTypeFeaturesFromString
from fontTools.pens.svgPathPen import SVGPathPen
from fontTools.ttLib import TTFont
from fontTools.ttLib.tables._g_l_y_f import GlyphCoordinates

ROOT = Path(__file__).resolve().parent.parent
SOURCE = Path.home() / "Library/Fonts/JF-Dot-MPlusQ10.ttf"
TARGET = ROOT / "static/assets/fonts/jf-dot-mplusq10-vert.ttf"
PAGE = ROOT / "dev/jfdot-vert.html"

# The kanji in *jp-labels* (hypermedia/sheet.lisp).
KANJI = "戦勝主"

# What the subset keeps besides KANJI: space, dashes and quotes, arrows,
# Japanese punctuation, hiragana, katakana, full- and half-width forms.
RANGES = [(0x20, 0x20), (0x2010, 0x2027), (0x2190, 0x2193), (0x3000, 0x303F),
          (0x3040, 0x309F), (0x30A0, 0x30FF), (0xFF00, 0xFFEF)]

# The em is 10 pixels wide and the line 11 pixels tall: a row below the
# baseline area, then the 10 x 10 cell a full-width glyph is drawn in. A
# half-width glyph has the left 5 x 10 of it. Everything below is in pixels,
# x rightwards from the left edge and y upwards from the foot of the cell.
EM = 10
ROWS = 11
CELL = 10
HALF_CELL = 5

# Turned a quarter clockwise, as dashes and brackets are in vertical text.
ROTATE = "ー―…－＝｜＿：「」『』【】（）［］｛｝ｰ"
# Turned, then mirrored left to right: the vertical wave dash.
ROTATE_MIRROR = "～"
# Moved from the lower left of the cell to its upper right.
CORNER = "、。，．｡､"
# Small kana: pushed to the right edge and halfway up the room above them.
SMALL = "ぁぃぅぇぉっゃゅょゎァィゥェォッャュョヮヵヶ"

# (dx, dy) added after the rule above, for forms the rule gets wrong. The
# half-width long-vowel mark is turned about the full cell, which leaves it
# in the upper right; this brings it back to the middle of its column.
NUDGE = {
    "ｰ": (-2, -3),
}

# Shown on the test page for comparison; nothing is done to these.
UNCHANGED = "・！？；＜＞／＼‘’”←↑→↓"

SAMPLES = [
    "主なシリーズ", "オールマイティ", "戦わなければ勝てない", "ブリーフィング",
    "チェック、ワン。", "「ロード」かんりょう…", "【ニュース】（ア）",
    "『ちょっと』［イ］｛ウ｝", "ァィゥェォッャュョ", "ぁぃぅぇぉっゃゅょ",
    "Ａ－Ｂ＝Ｃ：Ｄ～Ｅ", "＜タグ＞＿，．―", "ｼﾘｰｽﾞ｡ ｱ､ｲ",
]

UNIT = 1024 / EM
FOOT = -144     # the foot of the cell, in font units: one row above the descent
TOP = 819       # where a glyph's top side bearing is measured from


def to_pixel(value, origin=0):
    """VALUE in pixels from ORIGIN: a whole number on the grid, a fraction for
    the few points that are off it."""
    pixel = round((value - origin) / UNIT)
    return pixel if abs(pixel * UNIT + origin - value) < 1.5 else (value - origin) / UNIT


def to_units(pixel, origin=0):
    return round(pixel * UNIT + origin)


def vertical_form(char, glyph, glyf, width):
    """A copy of GLYPH as CHAR should look in vertical text, and the rule
    used. WIDTH is its advance in pixels."""
    points = [(to_pixel(x), to_pixel(y, FOOT)) for x, y in glyph.coordinates]
    x_min, x_max = min(x for x, _ in points), max(x for x, _ in points)
    y_min, y_max = min(y for _, y in points), max(y for _, y in points)
    mirrored = False
    if char in ROTATE:
        rule = "rotate"
        move = lambda x, y: (y, CELL - x)
    elif char in ROTATE_MIRROR:
        rule, mirrored = "rotate + mirror", True
        move = lambda x, y: (CELL - y, CELL - x)
    elif char in CORNER:
        rule = "corner"
        dx, dy = width - x_max - x_min, CELL - y_max - y_min
        move = lambda x, y: (x + dx, y + dy)
    else:
        rule = "small kana"
        dx, dy = width - x_max, math.ceil((CELL - y_max) / 2)
        move = lambda x, y: (x + dx, y + dy)
    nudge_x, nudge_y = NUDGE.get(char, (0, 0))

    new = copy.deepcopy(glyph)
    points = [move(x, y) for x, y in points]
    points = [(to_units(x + nudge_x), to_units(y + nudge_y, FOOT)) for x, y in points]
    flags = list(glyph.flags)
    if mirrored:
        # Mirroring turns the outlines inside out; trace them the other way.
        start = 0
        for end in glyph.endPtsOfContours:
            points[start:end + 1] = points[start:end + 1][::-1]
            flags[start:end + 1] = flags[start:end + 1][::-1]
            start = end + 1
    new.coordinates = GlyphCoordinates(points)
    new.flags = bytearray(flags)
    new.recalcBounds(glyf)
    return new, rule


def subset_font(font):
    """Cut FONT down to RANGES and KANJI, and whatever `vert` swaps them for."""
    options = subset.Options()
    options.drop_tables += ["EBDT", "EBLC", "FFTM"]
    options.layout_features = ["*"]
    options.hinting = False
    options.notdef_outline = True
    options.name_IDs = ["*"]
    options.name_languages = ["*"]
    subsetter = subset.Subsetter(options)
    wanted = [code for low, high in RANGES for code in range(low, high + 1)]
    subsetter.populate(unicodes=wanted + [ord(char) for char in KANJI])
    subsetter.subset(font)
    return font


def font_bytes(font):
    out = io.BytesIO()
    font.save(out)
    return out.getvalue()


def build(source):
    """Write the font. Returns (char, rule, old glyph name, new glyph name)s,
    their drawings, the cmap and the subset font as it was before."""
    before = font_bytes(subset_font(TTFont(source)))

    font = TTFont(source)
    glyf, hmtx, vmtx, cmap = font["glyf"], font["hmtx"], font["vmtx"], font.getBestCmap()
    order = font.getGlyphOrder()
    made = []
    for char in ROTATE + ROTATE_MIRROR + CORNER + SMALL:
        name = cmap.get(ord(char))
        if name is None:
            continue
        glyph = glyf[name]
        assert glyph.numberOfContours > 0, f"{char} is empty or a composite"
        new, rule = vertical_form(char, glyph, glyf, round(hmtx[name][0] / UNIT))
        new_name = name + ".vert"
        order.append(new_name)
        glyf[new_name] = new
        hmtx[new_name] = (hmtx[name][0], new.xMin)
        vmtx[new_name] = (vmtx[name][0], TOP - new.yMax)
        made.append((char, rule, name, new_name))
    font.setGlyphOrder(order)
    font["maxp"].numGlyphs = len(order)

    swaps = "\n".join(f"    sub {old} by {new};" for _, _, old, new in made)
    addOpenTypeFeaturesFromString(font, f"""
languagesystem DFLT dflt;
languagesystem kana dflt;
feature vert {{
{swaps}
}} vert;
""")

    # Read these before subsetting, while the glyphs still have their names.
    drawings = {name: drawing(font, name)
                for _, _, old, new in made for name in (old, new)}
    drawings.update((cmap[ord(c)], drawing(font, cmap[ord(c)]))
                    for c in UNCHANGED if ord(c) in cmap)
    subset_font(font).save(TARGET)
    return made, drawings, cmap, before


# The test page.

def drawing(font, name):
    """NAME's outline on the pixel grid, as an SVG."""
    pen = SVGPathPen(font.getGlyphSet())
    font.getGlyphSet()[name].draw(pen)
    width, height = EM * UNIT, ROWS * UNIT
    lines = "".join(f'<path d="M{i * UNIT:.1f} 0V{height:.1f}"/>' for i in range(EM + 1))
    lines += "".join(f'<path d="M0 {i * UNIT:.1f}H{width:.1f}"/>' for i in range(ROWS + 1))
    cell = CELL * UNIT
    return (f'<svg class="glyph" viewBox="0 0 {width:.1f} {height:.1f}" role="img">'
            f'<rect class="gap" width="{width:.1f}" height="{height:.1f}"/>'
            f'<rect class="cell" width="{cell:.1f}" height="{cell:.1f}"/>'
            f'<path class="ink" transform="translate(0 {height + FOOT - UNIT:.1f}) scale(1 -1)" '
            f'd="{pen.getCommands()}"/>'
            f'<g class="grid">{lines}</g></svg>')


def data_uri(data):
    return "data:font/ttf;base64," + base64.b64encode(data).decode()


def live(char, family):
    return f'<td><span class="live v {family}" lang="ja">ア{html.escape(char)}ア</span></td>'


def page(made, drawings, cmap, before):
    rows = []
    for char, rule, old, new in made:
        rows.append(
            f'<tr><th>{html.escape(char)}<small>U+{ord(char):04X}</small></th>'
            f'<td class="rule">{rule}</td>'
            f'<td>{drawings[old]}</td><td>{drawings[new]}</td>'
            f'{live(char, "before")}{live(char, "after")}{live(char, "system")}</tr>')
    for char in UNCHANGED:
        if ord(char) not in cmap:
            continue
        rows.append(
            f'<tr><th>{html.escape(char)}<small>U+{ord(char):04X}</small></th>'
            f'<td class="rule">left alone</td>'
            f'<td>{drawings[cmap[ord(char)]]}</td><td></td>'
            f'{live(char, "before")}{live(char, "after")}{live(char, "system")}</tr>')

    def samples(family, size):
        return "".join(
            f'<span class="v {family}" lang="ja" style="font-size:{size}px">{html.escape(s)}</span>'
            for s in SAMPLES)

    return f"""<!doctype html>
<html lang="en">
<meta charset="utf-8">
<meta name="viewport" content="width=device-width, initial-scale=1">
<title>JF Dot M+Q 10: vertical forms</title>
<style>
@font-face {{ font-family: "JF Dot Before"; src: url("{data_uri(before)}") format("truetype"); }}
@font-face {{ font-family: "JF Dot After"; src: url("{data_uri(TARGET.read_bytes())}") format("truetype"); }}
:root {{ --bg: #f4f1ea; --fg: #1b1b1b; --dim: #77726a; --line: #d6d0c4; --cell: #fff; --gap: #e6e0d3; }}
@media (prefers-color-scheme: dark) {{
  :root {{ --bg: #17171a; --fg: #ecebe6; --dim: #8d8a84; --line: #36353a; --cell: #222226; --gap: #131315; }}
}}
body {{ margin: 0; padding: 24px 16px 64px; background: var(--bg); color: var(--fg);
       font: 14px/1.5 ui-monospace, Menlo, monospace; }}
main {{ max-width: 1000px; margin: 0 auto; }}
h1 {{ font-size: 18px; margin: 0 0 4px; }}
h2 {{ font-size: 14px; margin: 40px 0 8px; text-transform: uppercase; letter-spacing: 0.08em; }}
p {{ margin: 0 0 12px; color: var(--dim); max-width: 70ch; }}
.before {{ font-family: "JF Dot Before"; }}
.after {{ font-family: "JF Dot After"; }}
.system {{ font-family: "Hiragino Kaku Gothic ProN", "Yu Gothic", Meiryo, sans-serif; }}
.v {{ writing-mode: vertical-rl; text-orientation: upright; line-height: 1; white-space: nowrap; }}
.scroll {{ overflow-x: auto; }}
table {{ border-collapse: collapse; }}
th, td {{ padding: 8px 12px; border-bottom: 1px solid var(--line); text-align: center; vertical-align: middle; }}
thead th {{ color: var(--dim); font-weight: normal; position: sticky; top: 0; background: var(--bg); }}
tbody th {{ font: 22px/1.2 "Hiragino Kaku Gothic ProN", sans-serif; }}
tbody th small {{ display: block; color: var(--dim); font: 11px/1.4 ui-monospace, Menlo, monospace; }}
.rule {{ color: var(--dim); text-align: left; white-space: nowrap; }}
.glyph {{ display: block; width: 100px; height: 110px; }}
.glyph .gap {{ fill: var(--gap); }}
.glyph .cell {{ fill: var(--cell); }}
.glyph .ink {{ fill: var(--fg); }}
.glyph .grid {{ stroke: var(--line); stroke-width: 4; fill: none; }}
.live {{ display: inline-block; font-size: 40px; }}
.sizes {{ display: flex; flex-wrap: wrap; gap: 32px; align-items: flex-start; }}
.sizes figure {{ margin: 0; }}
.sizes figcaption {{ color: var(--dim); margin-bottom: 8px; }}
.strip {{ display: flex; gap: 14px; align-items: flex-start; }}
</style>
<main>
<h1>JF Dot M+Q 10: vertical forms</h1>
<p>Made by dev/jfdot-vert.py, which also writes the font. Edit the tables at the
top of that script, run it and reload.</p>

<h2>Labels and sample text</h2>
<div class="sizes">
  <figure><figcaption>After, 15px (the gutter label)</figcaption><div class="strip">{samples("after", 15)}</div></figure>
  <figure><figcaption>After, 30px</figcaption><div class="strip">{samples("after", 30)}</div></figure>
  <figure><figcaption>Before, 30px</figcaption><div class="strip">{samples("before", 30)}</div></figure>
  <figure><figcaption>System font, 30px</figcaption><div class="strip">{samples("system", 30)}</div></figure>
</div>

<h2>Every form</h2>
<p>The drawings are the outlines in the font: the white square is the 10 x 10
cell, the darker row under it the room below the baseline. The last three columns are the browser setting
ア, the character, ア down a column.</p>
<div class="scroll">
<table>
<thead><tr><th></th><th>Rule</th><th>Horizontal</th><th>Vertical</th>
<th>Before</th><th>After</th><th>System</th></tr></thead>
<tbody>
{chr(10).join(rows)}
</tbody>
</table>
</div>
</main>
</html>
"""


if __name__ == "__main__":
    source = Path(sys.argv[1]) if len(sys.argv) > 1 else SOURCE
    made, drawings, cmap, before = build(source)
    PAGE.write_text(page(made, drawings, cmap, before), encoding="utf-8")
    print(f"Wrote {TARGET.relative_to(ROOT)} ({TARGET.stat().st_size} bytes, {len(made)} vertical forms)")
    print(f"Wrote {PAGE.relative_to(ROOT)}")
