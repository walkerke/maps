const STYLE_URL = "https://tiles.openfreemap.org/styles/positron";
const EARTH_RADIUS = 6378137;
const DATA_VERSION = "2024-acs-v2";

const DEFAULTS = {
  sourceState: "CO",
  sourcePlace: "Denver",
  targetState: "TX",
  targetPlace: "Houston"
};

const els = {
  sourceState: document.getElementById("source-state"),
  sourcePlace: document.getElementById("source-place"),
  targetState: document.getElementById("target-state"),
  targetPlace: document.getElementById("target-place"),
  isolateTarget: document.getElementById("isolate-target"),
  sourceArea: document.getElementById("source-area"),
  targetArea: document.getElementById("target-area")
};

const stateCache = new Map();

let map;
let sourceCollection = null;
let targetCollection = null;
let selectedSource = null;
let selectedTarget = null;
let currentDrop = null;
let currentOverlay = null;
let currentOverlayCenter = null;
let dragging = false;
let movedDuringDrag = false;
let startPoint = null;
let startCenterPoint = null;
let startOverlay = null;
let targetPopup = null;

function featureCollection(features) {
  return {
    type: "FeatureCollection",
    features: Array.isArray(features) ? features : [features]
  };
}

function clone(value) {
  return JSON.parse(JSON.stringify(value));
}

function placeName(feature) {
  return feature.properties.NAME;
}

function placeLabel(feature) {
  return placeName(feature)
    .replace(/,\s+[A-Za-z .]+$/, "")
    .replace(/\s+(city|town|village|borough|municipality|CDP|urban county|metro township|unified government)$/i, "");
}

function placeGeoid(feature) {
  return feature.properties.GEOID;
}

function placePopulation(feature) {
  return Number(feature.properties.population || feature.properties.value || 0);
}

function placeType(feature) {
  return feature.properties.place_type ||
    (feature.properties.LSAD === "57" ? "CDP" : "Incorporated place");
}

function isCdp(feature) {
  return placeType(feature) === "CDP" || feature.properties.LSAD === "57";
}

function formatPopulation(value) {
  const number = Number(value || 0);
  if (number >= 1000000) return `${(number / 1000000).toFixed(1)}M`;
  if (number >= 10000) return `${Math.round(number / 1000)}K`;
  if (number >= 1000) return `${(number / 1000).toFixed(1)}K`;
  return number.toLocaleString();
}

function formatArea(squareMeters) {
  return `${(squareMeters / 2589988.110336).toFixed(1)} sq mi`;
}

function escapeHtml(value) {
  return String(value)
    .replace(/&/g, "&amp;")
    .replace(/</g, "&lt;")
    .replace(/>/g, "&gt;")
    .replace(/"/g, "&quot;")
    .replace(/'/g, "&#039;");
}

function collectionArea(collection) {
  return collection.features.reduce((total, feature) => total + turf.area(feature), 0);
}

function lonLatToMercator(coord) {
  const lon = Math.max(-180, Math.min(180, coord[0]));
  const lat = Math.max(-85.05112878, Math.min(85.05112878, coord[1]));
  const lambda = (lon * Math.PI) / 180;
  const phi = (lat * Math.PI) / 180;

  return [
    EARTH_RADIUS * lambda,
    EARTH_RADIUS * Math.log(Math.tan(Math.PI / 4 + phi / 2))
  ];
}

function mercatorToLonLat(coord) {
  const lon = (coord[0] / EARTH_RADIUS) * (180 / Math.PI);
  const lat = (2 * Math.atan(Math.exp(coord[1] / EARTH_RADIUS)) - Math.PI / 2) *
    (180 / Math.PI);

  return [lon, lat];
}

function transformCoordinates(coords, transform) {
  if (typeof coords[0] === "number") {
    return transform(coords);
  }

  return coords.map((child) => transformCoordinates(child, transform));
}

function transformGeometry(geometry, transform) {
  return {
    type: geometry.type,
    coordinates: transformCoordinates(geometry.coordinates, transform)
  };
}

function transformCollection(collection, transform) {
  const next = clone(collection);
  next.features.forEach((feature) => {
    feature.geometry = transformGeometry(feature.geometry, transform);
  });
  return next;
}

function centroidCoord(collection) {
  return turf.centroid(collection).geometry.coordinates;
}

function translateAndScaleToPoint(sourceFeature, lngLat) {
  const source = featureCollection(sourceFeature);
  const originalArea = collectionArea(source);
  const sourceCenter = lonLatToMercator(centroidCoord(source));
  const targetCenter = lonLatToMercator(lngLat);
  const dx = targetCenter[0] - sourceCenter[0];
  const dy = targetCenter[1] - sourceCenter[1];

  const shifted = transformCollection(source, (coord) => {
    const mercator = lonLatToMercator(coord);
    return mercatorToLonLat([mercator[0] + dx, mercator[1] + dy]);
  });

  const shiftedArea = collectionArea(shifted);
  const scaleFactor = Math.sqrt(originalArea / shiftedArea);

  return transformCollection(shifted, (coord) => {
    const mercator = lonLatToMercator(coord);
    return mercatorToLonLat([
      targetCenter[0] + (mercator[0] - targetCenter[0]) * scaleFactor,
      targetCenter[1] + (mercator[1] - targetCenter[1]) * scaleFactor
    ]);
  });
}

function mapCoordinate(coord, dx, dy) {
  const point = map.project(coord);
  point.x += dx;
  point.y += dy;
  const lngLat = map.unproject(point);
  return [lngLat.lng, lngLat.lat];
}

function translatedOverlay(dx, dy) {
  return transformCollection(startOverlay, (coord) => mapCoordinate(coord, dx, dy));
}

function selectedFeature(collection, geoid) {
  return collection.features.find((feature) => placeGeoid(feature) === geoid);
}

function sortedFeatures(collection) {
  return [...collection.features].sort((a, b) => {
    return placePopulation(b) - placePopulation(a) ||
      placeLabel(a).localeCompare(placeLabel(b)) ||
      placeGeoid(a).localeCompare(placeGeoid(b));
  });
}

function populateStates(states) {
  for (const select of [els.sourceState, els.targetState]) {
    select.textContent = "";
    states.forEach((state) => {
      const option = document.createElement("option");
      option.value = state.abbr;
      option.textContent = state.abbr;
      option.title = state.name;
      select.appendChild(option);
    });
  }

  els.sourceState.value = DEFAULTS.sourceState;
  els.targetState.value = DEFAULTS.targetState;
}

function populatePlaces(select, collection, preferredName) {
  select.textContent = "";
  const features = sortedFeatures(collection);
  const nameCounts = features.reduce((counts, feature) => {
    counts.set(placeName(feature), (counts.get(placeName(feature)) || 0) + 1);
    return counts;
  }, new Map());

  let preferredValue = null;

  features.forEach((feature) => {
    const option = document.createElement("option");
    option.value = placeGeoid(feature);
    const label = nameCounts.get(placeName(feature)) > 1
      ? `${placeLabel(feature)} (${placeGeoid(feature)})`
      : placeLabel(feature);
    const typeLabel = isCdp(feature) ? " CDP" : "";
    option.textContent = `${label}${typeLabel} - ${formatPopulation(placePopulation(feature))}`;
    select.appendChild(option);

    if (placeLabel(feature) === preferredName && preferredValue === null) {
      preferredValue = placeGeoid(feature);
    }
  });

  select.value = preferredValue || features[0]?.properties.GEOID || "";
}

async function loadPlaces(state) {
  if (!stateCache.has(state)) {
    const response = await fetch(`data/places-${state}.geojson?v=${DATA_VERSION}`);
    if (!response.ok) {
      throw new Error(`Could not load places for ${state}`);
    }
    stateCache.set(state, await response.json());
  }

  return stateCache.get(state);
}

function ensureSource(id, data) {
  if (map.getSource(id)) {
    map.getSource(id).setData(data);
  } else {
    map.addSource(id, { type: "geojson", data, promoteId: "GEOID" });
  }
}

function addLayerIfMissing(layer) {
  if (!map.getLayer(layer.id)) {
    map.addLayer(layer);
  }
}

function updateTargetLayer() {
  const targetData = els.isolateTarget.checked
    ? featureCollection(selectedTarget)
    : targetCollection;

  ensureSource("targets", targetData);
}

function updateLabels() {
  const overlayLabel = turf.point(
    centroidCoord(currentOverlay),
    {
      name: placeName(selectedSource),
      label: placeLabel(selectedSource),
      color: "#df941d"
    }
  );
  const targetLabel = turf.point(
    centroidCoord(featureCollection(selectedTarget)),
    {
      name: placeName(selectedTarget),
      label: placeLabel(selectedTarget),
      color: "#168ea6"
    }
  );

  ensureSource("labels", featureCollection([targetLabel, overlayLabel]));
}

function updateStats() {
  els.sourceArea.textContent = formatArea(collectionArea(featureCollection(selectedSource)));
  els.targetArea.textContent = formatArea(collectionArea(featureCollection(selectedTarget)));
}

function fitComparison() {
  const bbox = turf.bbox(turf.featureCollection([
    selectedTarget,
    ...currentOverlay.features
  ]));

  map.fitBounds(
    [[bbox[0], bbox[1]], [bbox[2], bbox[3]]],
    { padding: 72, maxZoom: 10.6, duration: 600 }
  );
}

function updateOverlay({ fit = false } = {}) {
  currentOverlay = translateAndScaleToPoint(
    selectedSource,
    [currentDrop.lng, currentDrop.lat]
  );
  currentOverlayCenter = centroidCoord(currentOverlay);

  ensureSource("overlay", currentOverlay);
  updateLabels();
  updateStats();

  if (fit) fitComparison();
}

function setTargetFeature(feature, { isolate = false, fit = true } = {}) {
  selectedTarget = selectedFeature(targetCollection, placeGeoid(feature));
  if (!selectedTarget) return;

  els.targetPlace.value = placeGeoid(selectedTarget);
  els.isolateTarget.checked = isolate;
  currentDrop = lngLatObject(centroidCoord(featureCollection(selectedTarget)));

  updateTargetLayer();
  updateOverlay({ fit });
}

function addMapLayers() {
  addLayerIfMissing({
    id: "targets-fill",
    type: "fill",
    source: "targets",
    paint: {
      "fill-color": "#7cc7d8",
      "fill-opacity": 0.14
    }
  });
  addLayerIfMissing({
    id: "targets-outline",
    type: "line",
    source: "targets",
    paint: {
      "line-color": [
        "case",
        ["==", ["get", "LSAD"], "57"],
        "#62b8c8",
        "#1f9bb4"
      ],
      "line-width": [
        "case",
        ["==", ["get", "LSAD"], "57"],
        1.4,
        1.8
      ],
      "line-opacity": [
        "case",
        ["==", ["get", "LSAD"], "57"],
        0.5,
        0.68
      ],
      "line-dasharray": [
        "case",
        ["==", ["get", "LSAD"], "57"],
        ["literal", [2, 2]],
        ["literal", [1, 0]]
      ]
    }
  });
  addLayerIfMissing({
    id: "overlay-fill",
    type: "fill",
    source: "overlay",
    paint: {
      "fill-color": "#f2a93b",
      "fill-opacity": 0.32
    }
  });
  addLayerIfMissing({
    id: "overlay-outline",
    type: "line",
    source: "overlay",
    paint: {
      "line-color": "#df941d",
      "line-width": 3.5
    }
  });
  addLayerIfMissing({
    id: "city-labels",
    type: "symbol",
    source: "labels",
    layout: {
      "text-field": ["get", "label"],
      "text-size": 13,
      "text-allow-overlap": true
    },
    paint: {
      "text-color": ["get", "color"],
      "text-halo-color": "#ffffff",
      "text-halo-width": 1.5
    }
  });
}

async function setSourceState(preferredName = null) {
  sourceCollection = await loadPlaces(els.sourceState.value);
  populatePlaces(
    els.sourcePlace,
    sourceCollection,
    preferredName || DEFAULTS.sourcePlace
  );
  selectedSource = selectedFeature(sourceCollection, els.sourcePlace.value);
  updateOverlay({ fit: true });
}

async function setTargetState(preferredName = null) {
  targetCollection = await loadPlaces(els.targetState.value);
  populatePlaces(
    els.targetPlace,
    targetCollection,
    preferredName || DEFAULTS.targetPlace
  );
  selectedTarget = selectedFeature(targetCollection, els.targetPlace.value);
  currentDrop = lngLatObject(centroidCoord(featureCollection(selectedTarget)));
  updateTargetLayer();
  updateOverlay({ fit: true });
}

function lngLatObject(coord) {
  return { lng: coord[0], lat: coord[1] };
}

function finishDrag(point) {
  if (!dragging || !startCenterPoint) return;

  dragging = false;
  startPoint = null;
  startCenterPoint = null;
  startOverlay = null;
  map.dragPan.enable();
  map.getCanvas().style.cursor = "";

  if (movedDuringDrag) {
    const dropped = dropPointFromDrag(point);
    currentDrop = { lng: dropped.lng, lat: dropped.lat };
    updateOverlay({ fit: false });
  }
}

function dropPointFromDrag(point) {
  return map.unproject({
    x: startCenterPoint.x + point.x - startPoint.x,
    y: startCenterPoint.y + point.y - startPoint.y
  });
}

function wireMapDrag() {
  map.on("mouseenter", "overlay-fill", () => {
    map.getCanvas().style.cursor = "grab";
  });

  map.on("mouseleave", "overlay-fill", () => {
    if (!dragging) map.getCanvas().style.cursor = "";
  });

  map.on("mousedown", "overlay-fill", (event) => {
    if (!currentOverlay || !currentOverlayCenter) return;
    event.preventDefault();
    dragging = true;
    movedDuringDrag = false;
    startPoint = map.project(event.lngLat);
    startCenterPoint = map.project(currentOverlayCenter);
    startOverlay = clone(currentOverlay);
    map.dragPan.disable();
    map.getCanvas().style.cursor = "grabbing";
  });

  map.on("mousemove", (event) => {
    if (!dragging || !startCenterPoint || !startOverlay) return;
    const point = map.project(event.lngLat);
    const dx = point.x - startPoint.x;
    const dy = point.y - startPoint.y;
    if (Math.abs(dx) + Math.abs(dy) > 2) movedDuringDrag = true;
    map.getSource("overlay").setData(translatedOverlay(dx, dy));
  });

  map.on("mouseup", (event) => {
    finishDrag(map.project(event.lngLat));
  });

  map.on("mouseout", (event) => {
    if (dragging) finishDrag(event.point);
  });
}

function wireTargetPopups() {
  map.on("mouseenter", "targets-fill", () => {
    if (!dragging) map.getCanvas().style.cursor = "pointer";
  });

  map.on("mouseleave", "targets-fill", () => {
    if (!dragging) map.getCanvas().style.cursor = "";
  });

  map.on("click", "targets-fill", (event) => {
    if (dragging || !event.features.length) return;

    const feature = event.features[0];
    const geoid = feature.properties.GEOID;
    const label = placeLabel(feature);
    const population = formatPopulation(placePopulation(feature));
    const type = placeType(feature);

    if (targetPopup) targetPopup.remove();

    targetPopup = new maplibregl.Popup({
      closeButton: true,
      closeOnClick: true,
      maxWidth: "260px"
    })
      .setLngLat(event.lngLat)
      .setHTML(`
        <div class="place-popup">
          <div class="place-popup-title">${escapeHtml(label)}</div>
          <div class="place-popup-meta">
            ${escapeHtml(type)} · Population ${escapeHtml(population)}
          </div>
          <button type="button" data-compare-geoid="${escapeHtml(geoid)}">
            Compare here
          </button>
        </div>
      `)
      .addTo(map);
  });

  document.addEventListener("click", (event) => {
    const button = event.target.closest("[data-compare-geoid]");
    if (!button) return;

    const feature = selectedFeature(targetCollection, button.dataset.compareGeoid);
    if (!feature) return;

    setTargetFeature(feature, { isolate: true, fit: true });
    if (targetPopup) {
      targetPopup.remove();
      targetPopup = null;
    }
  });
}

async function initControls() {
  const response = await fetch("data/states.json");
  populateStates(await response.json());

  sourceCollection = await loadPlaces(els.sourceState.value);
  targetCollection = await loadPlaces(els.targetState.value);
  populatePlaces(els.sourcePlace, sourceCollection, DEFAULTS.sourcePlace);
  populatePlaces(els.targetPlace, targetCollection, DEFAULTS.targetPlace);

  selectedSource = selectedFeature(sourceCollection, els.sourcePlace.value);
  selectedTarget = selectedFeature(targetCollection, els.targetPlace.value);
  currentDrop = lngLatObject(centroidCoord(featureCollection(selectedTarget)));
}

function wireControls() {
  els.sourceState.addEventListener("change", () => setSourceState());
  els.targetState.addEventListener("change", () => setTargetState());

  els.sourcePlace.addEventListener("change", () => {
    selectedSource = selectedFeature(sourceCollection, els.sourcePlace.value);
    updateOverlay({ fit: false });
  });

  els.targetPlace.addEventListener("change", () => {
    const feature = selectedFeature(targetCollection, els.targetPlace.value);
    setTargetFeature(feature, { isolate: els.isolateTarget.checked, fit: true });
  });

  els.isolateTarget.addEventListener("change", () => {
    updateTargetLayer();
  });
}

async function init() {
  map = new maplibregl.Map({
    container: "map",
    style: STYLE_URL,
    center: [-95.3698, 29.7604],
    zoom: 8.7,
    dragRotate: false,
    touchPitch: false
  });
  map.addControl(new maplibregl.NavigationControl({ visualizePitch: false }), "top-right");
  map.addControl(new maplibregl.ScaleControl({ unit: "imperial" }), "bottom-left");

  await initControls();
  wireControls();

  const addInitialData = () => {
    updateTargetLayer();
    updateOverlay({ fit: false });
    addMapLayers();
    wireMapDrag();
    wireTargetPopups();
    fitComparison();
  };

  if (map.loaded()) {
    addInitialData();
  } else {
    map.on("load", addInitialData);
  }
}

init().catch((error) => {
  console.error(error);
  alert(error.message);
});
