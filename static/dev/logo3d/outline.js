// A mesh drawn in outline: faces painted the colour of the background, and
// the mesh's hard edges drawn over them as lines.
//
// A line is hidden by what stands in front of the edge it follows, not by
// what stands in front of the line's own width. The faces are drawn once
// into a depth map; each point of a line then looks up the depth at the
// middle of the line there, and is drawn or not as that middle is in view.
// So every line that shows, shows at its full width.
//
//   outline(stage) -> { show(geometry), style({ bg, ink, face, line }),
//                       home(), front() }
//
// `geometry` may have two material groups, faces then walls, as the logo
// model has; `face: "solid"` then paints the faces in ink.
import * as THREE from "three";
import { OrbitControls } from "./vendor/OrbitControls.js";

const VIEW = [0.494, 0.469, 0.732];   // where the camera stands, as a direction

export function outline(stage) {
    const renderer = new THREE.WebGLRenderer({ antialias: true, preserveDrawingBuffer: true });
    renderer.setPixelRatio(Math.min(window.devicePixelRatio, 2));
    stage.appendChild(renderer.domElement);
    const scene = new THREE.Scene();
    const camera = new THREE.OrthographicCamera(-1, 1, 1, -1, 1, 1000);
    const controls = new OrbitControls(camera, renderer.domElement);
    controls.enablePan = false;
    let mesh = null, edges = null, radius = 1;

    const faces = new THREE.MeshBasicMaterial();
    const walls = faces.clone();
    const depth = new THREE.WebGLRenderTarget(1, 1, { depthTexture: new THREE.DepthTexture(1, 1, THREE.UnsignedIntType) });
    const lines = new THREE.ShaderMaterial({
        transparent: true, depthTest: false, depthWrite: false, side: THREE.DoubleSide,
        uniforms: {
            color: { value: new THREE.Color() }, width: { value: 2 },   // width in device px
            resolution: { value: new THREE.Vector2(1, 1) }, depthMap: { value: depth.depthTexture }
        },
        // One quad for each edge, from `a` to `b`, with room for a round end.
        vertexShader: `
            uniform vec2 resolution; uniform float width;
            attribute vec3 a; attribute vec3 b;
            varying vec3 vA; varying vec3 vB; varying vec2 vAt; varying float vLength;
            void main() {
                vec4 ca = projectionMatrix * modelViewMatrix * vec4(a, 1.0);
                vec4 cb = projectionMatrix * modelViewMatrix * vec4(b, 1.0);
                vA = ca.xyz / ca.w; vB = cb.xyz / cb.w;
                vec2 pa = vA.xy * resolution * 0.5, pb = vB.xy * resolution * 0.5;
                float len = length(pb - pa);
                vec2 dir = len > 1e-5 ? (pb - pa) / len : vec2(1.0, 0.0), across = vec2(-dir.y, dir.x);
                float half_ = width * 0.5 + 1.0, end = position.y * 2.0 - 1.0;
                vec2 p = mix(pa, pb, position.y) + dir * end * half_ + across * position.x * half_;
                vAt = vec2(position.x * half_, position.y * len + end * half_);
                vLength = len;
                gl_Position = vec4(p / (resolution * 0.5), 0.0, 1.0);
            }`,
        fragmentShader: `
            uniform vec2 resolution; uniform float width; uniform vec3 color; uniform sampler2D depthMap;
            varying vec3 vA; varying vec3 vB; varying vec2 vAt; varying float vLength;
            void main() {
                // The middle of the line beside this point, and whether it is in view.
                vec3 mid = mix(vA, vB, clamp(vAt.y / max(vLength, 1e-5), 0.0, 1.0));
                vec2 uv = mid.xy * 0.5 + 0.5;
                float behind = 0.0;
                for (int y = -1; y <= 1; y++) for (int x = -1; x <= 1; x++)
                    behind = max(behind, texture2D(depthMap, uv + vec2(float(x), float(y)) / resolution).r);
                if (mid.z * 0.5 + 0.5 > behind + 1e-3) discard;
                float past = vAt.y < 0.0 ? vAt.y : max(vAt.y - vLength, 0.0);
                float d = length(vec2(vAt.x, past));
                gl_FragColor = vec4(color, 1.0 - smoothstep(width * 0.5 - 0.5, width * 0.5 + 0.5, d));
            }`
    });
    function segments(positions) {
        const g = new THREE.InstancedBufferGeometry();
        g.setAttribute("position", new THREE.Float32BufferAttribute([-1, 0, 0, 1, 0, 0, -1, 1, 0, 1, 1, 0], 3));
        g.setIndex([0, 1, 2, 2, 1, 3]);
        const pairs = new THREE.InstancedInterleavedBuffer(new Float32Array(positions), 6);
        g.setAttribute("a", new THREE.InterleavedBufferAttribute(pairs, 3, 0));
        g.setAttribute("b", new THREE.InterleavedBufferAttribute(pairs, 3, 3));
        g.instanceCount = positions.length / 6;
        return g;
    }

    function stand(dir) {
        camera.position.set(dir[0] * radius * 4, dir[1] * radius * 4, dir[2] * radius * 4);
        camera.zoom = 1;
        controls.target.set(0, 0, 0);
        fit();
        controls.update();
        draw();
    }
    function fit() {
        const w = stage.clientWidth, h = stage.clientHeight, reach = radius * 1.15;
        renderer.setSize(w, h);
        const k = Math.max(1, w / h), j = Math.max(1, h / w);
        camera.left = -reach * k; camera.right = reach * k; camera.top = reach * j; camera.bottom = -reach * j;
        camera.near = radius * 2; camera.far = radius * 6;
        camera.updateProjectionMatrix();
        renderer.getDrawingBufferSize(lines.uniforms.resolution.value);
        depth.setSize(lines.uniforms.resolution.value.x, lines.uniforms.resolution.value.y);
    }
    function draw() {
        if (!mesh) return;
        edges.visible = false;
        renderer.setRenderTarget(depth);
        renderer.render(scene, camera);
        renderer.setRenderTarget(null);
        edges.visible = true;
        renderer.render(scene, camera);
    }
    new ResizeObserver(() => { fit(); draw(); }).observe(stage);
    controls.addEventListener("change", draw);

    return {
        // Show this geometry, centred, in place of whatever was shown.
        show(g) {
            const first = !mesh;
            if (mesh) { scene.remove(mesh, edges); mesh.geometry.dispose(); edges.geometry.dispose(); }
            g.center();
            g.computeBoundingSphere();
            radius = g.boundingSphere.radius;
            mesh = new THREE.Mesh(g, g.groups.length ? [faces, walls] : walls);
            edges = new THREE.Mesh(segments(new THREE.EdgesGeometry(g, 1).attributes.position.array), lines);
            edges.frustumCulled = false;
            scene.add(mesh, edges);
            if (first) stand(VIEW); else { fit(); draw(); }
        },
        style(o) {
            scene.background = new THREE.Color(o.bg);
            faces.color.set(o.face === "solid" ? o.ink : o.bg);
            walls.color.set(o.bg);
            lines.uniforms.color.value.set(o.ink);
            lines.uniforms.width.value = o.line * renderer.getPixelRatio();
            draw();
        },
        home() { stand(VIEW); },
        front() { stand([0, 0, 1]); }
    };
}

export const THEMES = {
    paper: { bg: "#e5e5e5", ink: "#000000" },
    white: { bg: "#ffffff", ink: "#000000" },
    ink:   { bg: "#000000", ink: "#e5e5e5" }
};
