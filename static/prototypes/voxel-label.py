"""Render a button label as isometric pixel art, for the .voxel-btn theme.

    python3 voxel-label.py "READ THE ARTICLE" > label.svg

Each lit pixel of a 3x5 font becomes a cube, and each letter is outlined in
black like a cartoon. A and R have square tops; the
pointed versions break apart once they have depth. The letters stand up off
the page, facing the reader, and their depth runs back along the 2:1
pixel-art isometric angle (two pixels across for every one up), so the words
still read left to right.

The drawing is rasterised rather than built from polygons: every layer of
depth is stamped from the back to the front, and each nearer stamp covers
the one behind it. What survives of each stamp is its top row (the lit top
face) and its right two columns (the side in shadow). Runs of same-coloured
pixels are then written out as one path per colour, so the SVG stays small
and every edge lands on a whole pixel.

The back of every letter is fixed to the page; only the front moves. The SVG
holds three drawings on one canvas, which the CSS switches between:
  rest — the letters stand HEIGHT layers off the page
  lift — one layer taller, under the pointer
  held — one layer shorter, while the button is pressed
"""
import sys

FONT = {
    "A": ["###", "#.#", "###", "#.#", "#.#"],
    "C": [".##", "#..", "#..", "#..", ".##"],
    "D": ["##.", "#.#", "#.#", "#.#", "##."],
    "E": ["###", "#..", "##.", "#..", "###"],
    "H": ["#.#", "#.#", "###", "#.#", "#.#"],
    "I": ["###", ".#.", ".#.", ".#.", "###"],
    "L": ["#..", "#..", "#..", "#..", "###"],
    "R": ["###", "#.#", "##.", "#.#", "#.#"],
    "T": ["###", ".#.", ".#.", ".#.", ".#."],
    " ": [".", ".", ".", ".", "."],
}

CELL = 4          # screen pixels per font pixel
HEIGHT = 2        # layers the letters stand off the page at rest; each
                  # layer steps 2 across and 1 up
TALLEST = HEIGHT + 1
GAP = 2           # font pixels between letters: the side faces take one
                  # of them, so a single pixel would close the gap entirely
# Cartoon shading, lit from above: a near-white top, a bright front and a
# mid-grey side, all inside a black outline. The outline carries the contrast
# against white paper, so the faces can stay light; the side stays well clear
# of black, or the extrusion merges with the outline into one dark mass.
TOP, FRONT, SIDE = "#e8e8e8", "#bdbdbd", "#6a6a6a"
INK = "#000000"   # the cartoon outline
LINE = 1          # outline width in screen pixels


def mask(text):
    rows = [""] * 5
    for i, ch in enumerate(text.upper()):
        glyph = FONT[ch]
        for r in range(5):
            rows[r] += ("." * GAP if i else "") + glyph[r]
    return [[c == "#" for c in row] for row in rows]


def render(m, height):
    """Paint the label standing `height` layers off the page, outlined.

    Three passes, back to front, which is also the layer order in Figma:
      1. every side slice's silhouette grown by LINE, in ink
      2. the side slices themselves, back layer first
      3. the front's silhouette grown by LINE, in ink, then the front
    Pass 2 covers all of pass 1 except a LINE-wide rim, so the whole letter
    gets one contour rather than a line round every slice; each side slice
    also carries its share of the ridge between top and side. Pass 3 draws
    the line where the front face meets its own extrusion."""
    h, w = len(m), len(m[0])
    W = w * CELL + 2 * TALLEST + 2 * LINE
    H = h * CELL + TALLEST + 2 * LINE
    grid = [[None] * W for _ in range(H)]

    def lit(r, c):
        return 0 <= r < h and 0 <= c < w and m[r][c]

    def stamp(layer, colour, grow=0):
        ox, oy = 2 * layer + LINE, TALLEST - layer + LINE
        for r in range(h):
            for c in range(w):
                if not m[r][c]:
                    continue
                for y in range(-grow, CELL + grow):
                    for x in range(-grow, CELL + grow):
                        grid[r * CELL + y + oy][c * CELL + x + ox] = colour(r, c, y, x)

    # Layer TALLEST sits on the page; the front is `height` layers nearer.
    front = TALLEST - height
    sides = range(TALLEST, front, -1)               # back layer first
    ink = lambda r, c, y, x: INK

    def side(r, c, y, x):
        if y == 0 and not lit(r - 1, c):
            # The ridge where the top face turns into the side face: the top
            # row's last two pixels at a corner with nothing to the right.
            # Stacked two across and one up, they join into a 2:1 line.
            if x >= CELL - 2 and not lit(r, c + 1):
                return INK
            return TOP
        return SIDE
    for layer in sides:
        stamp(layer, ink, LINE)
    for layer in sides:
        stamp(layer, side)
    stamp(front, ink, LINE)
    stamp(front, lambda r, c, y, x: FRONT)
    return grid, W, H


def paths(grid):
    runs = {}
    for y, row in enumerate(grid):
        x = 0
        while x < len(row):
            colour = row[x]
            if colour is None:
                x += 1
                continue
            start = x
            while x < len(row) and row[x] == colour:
                x += 1
            runs.setdefault(colour, []).append(f"M{start} {y}h{x - start}v1h-{x - start}z")
    order = [INK, SIDE, TOP, FRONT]
    return "".join(f'<path fill="{c}" d="{"".join(runs[c])}"/>' for c in order if c in runs)


def svg(text):
    m = mask(text)
    rest, W, H = render(m, HEIGHT)
    lift, _, _ = render(m, HEIGHT + 1)
    held, _, _ = render(m, HEIGHT - 1)
    return (f'<svg class="voxel" viewBox="0 0 {W} {H}" width="{W}" height="{H}" '
            f'shape-rendering="crispEdges" aria-hidden="true" focusable="false">'
            f'<g class="voxel__rest">{paths(rest)}</g>'
            f'<g class="voxel__lift">{paths(lift)}</g>'
            f'<g class="voxel__held">{paths(held)}</g></svg>')


if __name__ == "__main__":
    print(svg(sys.argv[1] if len(sys.argv) > 1 else "READ THE ARTICLE"))
