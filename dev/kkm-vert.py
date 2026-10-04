#!/usr/bin/env python3
"""Give KKM Analog TV the glyphs vertical text needs.

The font has no GSUB table, so in vertical text ー lies down, 、 and 。 sit in
the wrong corner, brackets face sideways and small kana hang low. This adds a
vertical form of each such glyph and a `vert` feature that swaps them in, and
writes two files (needs fonttools):

    static/assets/fonts/kkm_analogtv_v2-vert.ttf   the font the site loads
    dev/kkm-vert.html                              a page showing every form

    python3 dev/kkm-vert.py

To change a form, edit the tables below, run this again and reload the page.
"""
import base64
import copy
import html
import math
from pathlib import Path

from fontTools.feaLib.builder import addOpenTypeFeaturesFromString
from fontTools.pens.svgPathPen import SVGPathPen
from fontTools.ttLib import TTFont
from fontTools.ttLib.tables._g_l_y_f import GlyphCoordinates

ROOT = Path(__file__).resolve().parent.parent
FONTS = ROOT / "static/assets/fonts"
SOURCE = FONTS / "kkm_analogtv_v2.ttf"
TARGET = FONTS / "kkm_analogtv_v2-vert.ttf"
PAGE = ROOT / "dev/kkm-vert.html"

# The em is 11 pixels square. A full-width glyph is drawn in the lower-left
# 10 x 10, a half-width one in the lower-left 6 x 10; the pixel left over to
# the right and above is the gap to the next glyph. Everything below is in
# pixels, x rightwards and y upwards from the lower-left corner.
EM = 11
CELL = 10
HALF_CELL = 6

# Turned a quarter clockwise, as dashes and brackets are in vertical text.
ROTATE = "ー―…－＝｜＿：「」『』【】（）［］｛｝ｰ"
# Turned, then mirrored left to right: the vertical wave dash.
ROTATE_MIRROR = "～"
# Moved from the lower left of the cell to its upper right.
CORNER = "、。，．｡､"
# Small kana: pushed to the right edge and halfway up the room above them.
SMALL = "ぁぃぅぇぉっゃゅょゎァィゥェォッャュョヮヵヶ"

# Glyphs wider or narrower than the usual cell.
CELL_WIDTH = {"―": EM, "ｰ": HALF_CELL, "｡": HALF_CELL, "､": HALF_CELL}

# (dx, dy) added after the rule above, for forms the rule gets wrong. The
# long-vowel marks sit below the middle of the line, so turning them leaves
# them left of the middle of the column.
NUDGE = {
    "ー": (1, 0),
    "ｰ": (-2, 2),
}

# Shown on the test page for comparison; nothing is done to these.
UNCHANGED = "・！？；＜＞／＼‘’”←↑→↓"

SAMPLES = [
    "シリーズ", "オールマイティ", "ブリーフィング", "おもなシリーズ",
    "チェック、ワン。", "「ロード」かんりょう…", "【ニュース】（ア）",
    "『ちょっと』［イ］｛ウ｝", "ァィゥェォッャュョ", "ぁぃぅぇぉっゃゅょ",
    "Ａ－Ｂ＝Ｃ：Ｄ～Ｅ", "＜タグ＞＿，．―", "ｼﾘｰｽﾞ｡ ｱ､ｲ",
]

UNIT = 1024 / EM


def to_pixel(value):
    """VALUE in pixels: a whole number on the grid, a fraction for the few
    points that are off it."""
    pixel = round(value / UNIT)
    return pixel if abs(pixel * UNIT - value) < 1.5 else value / UNIT


def to_units(pixel):
    return round(pixel * UNIT)


def pixel_box(glyph):
    return tuple(to_pixel(v) for v in (glyph.xMin, glyph.yMin, glyph.xMax, glyph.yMax))


def vertical_form(char, glyph, glyf):
    """A copy of GLYPH as CHAR should look in vertical text, and the rule used."""
    width = CELL_WIDTH.get(char, CELL)
    x_min, y_min, x_max, y_max = pixel_box(glyph)
    mirrored = False
    if char in ROTATE:
        rule = "rotate"
        move = lambda x, y: (y, width - x)
    elif char in ROTATE_MIRROR:
        rule, mirrored = "rotate + mirror", True
        move = lambda x, y: (width - y, width - x)
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
    points = [move(to_pixel(x), to_pixel(y)) for x, y in glyph.coordinates]
    points = [(to_units(x + nudge_x), to_units(y + nudge_y)) for x, y in points]
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


def build():
    """Write the font. Returns (char, rule, old glyph name, new glyph name)s."""
    font = TTFont(SOURCE)
    glyf, hmtx, cmap = font["glyf"], font["hmtx"], font.getBestCmap()
    order = font.getGlyphOrder()
    made = []
    for char in ROTATE + ROTATE_MIRROR + CORNER + SMALL:
        name = cmap.get(ord(char))
        if name is None:  # The font has no ゎ, ヮ, ヵ or ヶ.
            continue
        glyph = glyf[name]
        assert glyph.numberOfContours > 0, f"{char} is empty or a composite"
        new, rule = vertical_form(char, glyph, glyf)
        new_name = name + ".vert"
        order.append(new_name)
        glyf[new_name] = new
        hmtx[new_name] = (hmtx[name][0], new.xMin)
        made.append((char, rule, name, new_name))
    font.setGlyphOrder(order)

    swaps = "\n".join(f"    sub {old} by {new};" for _, _, old, new in made)
    addOpenTypeFeaturesFromString(font, f"""
languagesystem DFLT dflt;
languagesystem kana dflt;
feature vert {{
{swaps}
}} vert;
""")

    # CC BY-SA asks that a changed font say so.
    for record in font["name"].names:
        if record.nameID == 5:
            record.string = record.toUnicode() + "; vertical forms added"

    # Read these before saving: the font has no glyph names to save (post
    # format 3), so the saved file knows the new glyphs only by number.
    drawings = {name: drawing(font, name)
                for _, _, old, new in made for name in (old, new)}
    drawings.update((cmap[ord(c)], drawing(font, cmap[ord(c)]))
                    for c in UNCHANGED if ord(c) in cmap)
    font.save(TARGET)
    return made, drawings, cmap


# The test page.

def drawing(font, name):
    """NAME's outline on the pixel grid, as an SVG."""
    pen = SVGPathPen(font.getGlyphSet())
    font.getGlyphSet()[name].draw(pen)
    lines = "".join(
        f'<path d="M{i * UNIT:.1f} 0V1024M0 {i * UNIT:.1f}H1024"/>' for i in range(EM + 1))
    cell = to_units(CELL)
    return (f'<svg class="glyph" viewBox="0 0 1024 1024" role="img">'
            f'<rect class="gap" width="1024" height="1024"/>'
            f'<rect class="cell" y="{1024 - cell}" width="{cell}" height="{cell}"/>'
            f'<path class="ink" transform="translate(0 1024) scale(1 -1)" d="{pen.getCommands()}"/>'
            f'<g class="grid">{lines}</g></svg>')


def data_uri(path):
    return "data:font/ttf;base64," + base64.b64encode(path.read_bytes()).decode()


def live(char, family):
    return f'<td><span class="live v {family}" lang="ja">ア{html.escape(char)}ア</span></td>'


def page(made, drawings, cmap):
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
<title>KKM Analog TV: vertical forms</title>
<style>
@font-face {{ font-family: "KKM Before"; src: url("{data_uri(SOURCE)}") format("truetype"); }}
@font-face {{ font-family: "KKM After"; src: url("{data_uri(TARGET)}") format("truetype"); }}
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
.before {{ font-family: "KKM Before"; }}
.after {{ font-family: "KKM After"; }}
.system {{ font-family: "Hiragino Kaku Gothic ProN", "Yu Gothic", Meiryo, sans-serif; }}
.v {{ writing-mode: vertical-rl; text-orientation: upright; line-height: 1; white-space: nowrap; }}
.scroll {{ overflow-x: auto; }}
table {{ border-collapse: collapse; }}
th, td {{ padding: 8px 12px; border-bottom: 1px solid var(--line); text-align: center; vertical-align: middle; }}
thead th {{ color: var(--dim); font-weight: normal; position: sticky; top: 0; background: var(--bg); }}
tbody th {{ font: 22px/1.2 "Hiragino Kaku Gothic ProN", sans-serif; }}
tbody th small {{ display: block; color: var(--dim); font: 11px/1.4 ui-monospace, Menlo, monospace; }}
.rule {{ color: var(--dim); text-align: left; white-space: nowrap; }}
.glyph {{ display: block; width: 110px; height: 110px; }}
.glyph .gap {{ fill: var(--gap); }}
.glyph .cell {{ fill: var(--cell); }}
.glyph .ink {{ fill: var(--fg); }}
.glyph .grid {{ stroke: var(--line); stroke-width: 4; fill: none; }}
.live {{ display: inline-block; font-size: 44px; }}
.sizes {{ display: flex; flex-wrap: wrap; gap: 32px; align-items: flex-start; }}
.sizes figure {{ margin: 0; }}
.sizes figcaption {{ color: var(--dim); margin-bottom: 8px; }}
.strip {{ display: flex; gap: 14px; align-items: flex-start; }}
</style>
<main>
<h1>KKM Analog TV: vertical forms</h1>
<p>Made by dev/kkm-vert.py, which also writes the font. Edit the tables at the
top of that script, run it and reload.</p>

<h2>Labels and sample text</h2>
<div class="sizes">
  <figure><figcaption>After, 15px (the gutter label)</figcaption><div class="strip">{samples("after", 15)}</div></figure>
  <figure><figcaption>After, 33px</figcaption><div class="strip">{samples("after", 33)}</div></figure>
  <figure><figcaption>Before, 33px</figcaption><div class="strip">{samples("before", 33)}</div></figure>
  <figure><figcaption>System font, 33px</figcaption><div class="strip">{samples("system", 33)}</div></figure>
</div>

<h2>Every form</h2>
<p>The drawings are the outlines in the font: the white square is the 10 x 10
cell, the darker edge the gap. The last three columns are the browser setting
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
    made, drawings, cmap = build()
    PAGE.write_text(page(made, drawings, cmap), encoding="utf-8")
    print(f"Wrote {TARGET.relative_to(ROOT)} ({len(made)} vertical forms)")
    print(f"Wrote {PAGE.relative_to(ROOT)}")
