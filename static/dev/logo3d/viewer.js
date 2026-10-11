// The viewer for the logo model: the panel, and saving the model to a file.
import * as THREE from "three";
import { GLTFExporter } from "./vendor/GLTFExporter.js";
import { OBJExporter } from "./vendor/OBJExporter.js";
import { STLExporter } from "./vendor/STLExporter.js";
import { geometry, model, DEFAULTS } from "./logo3d.js";
import { outline, THEMES } from "./outline.js";

const o = Object.assign({ theme: "paper", face: "outline", line: 2 }, DEFAULTS);
const q = new URLSearchParams(location.search);
Object.keys(o).forEach(k => { if (q.has(k)) o[k] = typeof o[k] === "number" ? +q.get(k) : q.get(k); });
if (q.has("bare")) document.body.className = "bare";

const view = outline(document.getElementById("stage"));
const rebuild = () => view.show(geometry(THREE, o));
const dress = () => view.style(Object.assign({}, THEMES[o.theme] || THEMES.paper, o));
dress(); rebuild();

const inputs = [...document.querySelectorAll("[data-k]")];
function show() {
    inputs.forEach(i => {
        i.value = o[i.dataset.k];
        const out = i.parentNode.querySelector("output");
        if (out) out.textContent = Math.round(o[i.dataset.k] * 100) / 100;
    });
}
inputs.forEach(i => i.addEventListener("input", () => {
    const k = i.dataset.k;
    o[k] = i.type === "range" ? +i.value : i.value;
    if (k === "depth" || k === "hole") rebuild();
    dress(); show();
}));
document.getElementById("reset").onclick = () => {
    Object.assign(o, DEFAULTS, { face: "outline", line: 2 });
    rebuild(); dress(); view.home(); show();
};
document.getElementById("front").onclick = () => view.front();

function save(name, data, type) {
    const a = document.createElement("a");
    a.href = URL.createObjectURL(new Blob([data], { type }));
    a.download = name;
    a.click();
}
document.getElementById("glb").onclick = () =>
    new GLTFExporter().parse(model(THREE, o), data => save("almighty-logo.glb", data, "model/gltf-binary"),
                             e => console.error(e), { binary: true });
document.getElementById("obj").onclick = () =>
    save("almighty-logo.obj", new OBJExporter().parse(model(THREE, o)), "text/plain");
document.getElementById("stl").onclick = () =>
    save("almighty-logo.stl", new STLExporter().parse(model(THREE, o), { binary: true }), "model/stl");
show();
