// The Almighty logo as a 3D model.
//
// The logo's silhouette is extruded into one solid, with the logo's
// triangular holes cut through it. The mesh is the model; viewer.js draws
// it in outline, and the same mesh is what the model files hold.
//
//   geometry(THREE, { depth, hole }) -> BufferGeometry, in the logo's units
//   model(THREE, { depth, hole })    -> a Mesh for export, 1 unit tall

// The silhouette of static/assets/images/magazine/almighty-logo.svg: the
// outer edge of its 5-wide stroke, y up. The body, then the loose triangle.
const OUTLINE = [
    [[0, -11.539], [9.621, -28.246], [46.961, -28.346], [28.376, -60.815], [38.003, -77.535],
     [74.219, -14.247], [66.043, -0.049], [46.868, 0], [55.044, -14.2], [51.258, -20.836], [45.977, -11.665]],
    [[13.334, -34.923], [24.472, -54.265], [35.502, -34.986]]
];
// The lattice triangles the stroke runs along. Each has a hole at its centre.
const CELLS = [
    [[57.932, -14.204], [51.2, -2.513], [64.599, -2.551]], [[57.932, -14.204], [64.599, -2.551], [71.332, -14.243]],
    [[57.932, -14.204], [71.332, -14.243], [64.665, -25.897]], [[57.932, -14.204], [64.665, -25.897], [51.265, -25.858]],
    [[64.665, -25.897], [51.265, -25.858], [57.998, -37.55]], [[51.265, -25.858], [57.998, -37.55], [44.597, -37.512]],
    [[51.265, -25.858], [37.866, -25.82], [44.533, -14.167]], [[57.998, -37.55], [44.597, -37.512], [51.331, -49.204]],
    [[44.597, -37.512], [51.331, -49.204], [37.931, -49.166]], [[37.866, -25.82], [24.465, -25.782], [31.133, -14.128]],
    [[37.866, -25.82], [31.133, -14.128], [44.533, -14.167]], [[24.465, -25.782], [11.065, -25.744], [17.732, -14.09]],
    [[24.465, -25.782], [17.732, -14.09], [31.133, -14.128]], [[11.065, -25.744], [4.332, -14.051], [17.732, -14.09]],
    [[44.664, -60.858], [51.331, -49.204], [37.931, -49.166]], [[44.664, -60.858], [37.996, -72.513], [31.263, -60.82]],
    [[44.664, -60.858], [31.263, -60.82], [37.931, -49.166]], [[24.465, -49.243], [17.666, -37.435], [31.198, -37.474]]
];
const HALF_STROKE = 2.5;
const CENTRE = [37.1, -38.77], HEIGHT = 77.535;

export const DEFAULTS = {
    depth: 12,   // how far the logo is extruded, in the logo's units (it is 77.5 tall)
    hole: 1      // size of the holes: 1 as in the logo, 0 for none, 2.5 the most
};

// A cell's hole: the triangle shrunk about its incentre by half a stroke.
function hole(tri, size) {
    const [a, b, c] = tri;
    const la = Math.hypot(b[0] - c[0], b[1] - c[1]), lb = Math.hypot(a[0] - c[0], a[1] - c[1]),
          lc = Math.hypot(a[0] - b[0], a[1] - b[1]), per = la + lb + lc;
    const x = (la * a[0] + lb * b[0] + lc * c[0]) / per, y = (la * a[1] + lb * b[1] + lc * c[1]) / per;
    const area = Math.abs((b[0] - a[0]) * (c[1] - a[1]) - (b[1] - a[1]) * (c[0] - a[0])) / 2;
    const r = 2 * area / per, k = Math.min(size, 2.5) * (r - HALF_STROKE) / r;
    return { at: [x, y], pts: tri.map(p => [x + (p[0] - x) * k, y + (p[1] - y) * k]) };
}

function inside(ring, p) {
    let hit = false;
    for (let i = 0, j = ring.length - 1; i < ring.length; j = i++) {
        const a = ring[i], b = ring[j];
        if ((a[1] > p[1]) !== (b[1] > p[1]) &&
            p[0] < (b[0] - a[0]) * (p[1] - a[1]) / (b[1] - a[1]) + a[0]) hit = !hit;
    }
    return hit;
}

// The solid, centred on the origin, facing +z. Material group 0 is the
// front and back faces, group 1 the walls.
export function geometry(THREE, options) {
    const o = Object.assign({}, DEFAULTS, options);
    const path = (Kind, pts) => new Kind(pts.map(p => new THREE.Vector2(p[0] - CENTRE[0], p[1] - CENTRE[1])));
    const holes = o.hole > 0 ? CELLS.map(c => hole(c, o.hole)) : [];
    const shapes = OUTLINE.map(ring => {
        const shape = path(THREE.Shape, ring);
        holes.forEach(h => { if (inside(ring, h.at)) shape.holes.push(path(THREE.Path, h.pts)); });
        return shape;
    });
    const g = new THREE.ExtrudeGeometry(shapes, { depth: o.depth, bevelEnabled: false });
    g.translate(0, 0, -o.depth / 2);
    return g;
}

// The model as it goes into a file: one mesh, 1 unit tall, with a black
// material on the faces and a white one on the walls.
export function model(THREE, options) {
    const g = geometry(THREE, options);
    g.scale(1 / HEIGHT, 1 / HEIGHT, 1 / HEIGHT);
    const face = new THREE.MeshStandardMaterial({ name: "Face", color: 0x000000, roughness: 1, metalness: 0 });
    const wall = new THREE.MeshStandardMaterial({ name: "Wall", color: 0xffffff, roughness: 1, metalness: 0 });
    const mesh = new THREE.Mesh(g, [face, wall]);
    mesh.name = "AlmightyLogo";
    return mesh;
}
