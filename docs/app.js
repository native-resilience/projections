/* ============================================================================
   Native Resilience · CMIP6 Agricultural Climate Projections map · app.js
   Built on mco-web-style (window.MCO, window.MCO.map) + MapLibre GL 5.
   Section references (§) are to the kit's HOUSE-STYLE.md. Classic script,
   external file so the page's CSP can pin script-src 'self'.

   Data: data/native-land-index.json (id, slug, name, label, pdf_url, zip_url,
   bbox) and data/native-land.geojson (same properties + geometry), both
   written by ../projections-map.R. URLs arrive fully percent-encoded and are
   assigned to href verbatim — never re-encode them here.
   ========================================================================== */
(function () {
  'use strict';

  /* ── Constants ─────────────────────────────────────────────────────────── */

  // Full extent of the dataset: Hawaiʻi to Maine, Aleutians to the North Slope.
  const BOUNDS = [[-174.24, 18.91], [-67.04, 71.34]];
  const FIT_OPTS = { padding: 24, animate: false };
  const SRC = 'native';
  const LABEL_MINZOOM = 6;

  /* ── DOM ───────────────────────────────────────────────────────────────── */

  const $ = (id) => document.getElementById(id);
  const card = $('land-card');
  const tooltip = $('tooltip');
  const noteEl = $('app-note');
  const input = $('land-search');
  const datalist = $('land-names');
  const srSection = $('sr-land-section');
  const srTable = $('sr-land-table');

  const params = MCO.urlParams();
  const live = MCO.createLiveRegion();            // §5.1

  /* ── State ─────────────────────────────────────────────────────────────── */

  let index = [];
  const byId = new Map();
  const bySlug = new Map();
  const byName = new Map();
  let landFC = null;
  let selectedId = null;
  let hoverId = null;
  let cardOpener = null;

  const token = (name) =>
    getComputedStyle(document.documentElement).getPropertyValue(name).trim();

  function note(html) {
    noteEl.hidden = !html;
    noteEl.innerHTML = html || '';
  }

  /* ── Map init (§7) ─────────────────────────────────────────────────────── */

  let map = null;
  let zoomFloor = null;
  try {
    map = new maplibregl.Map({
      container: 'map',
      style: MCO.map.cartoStyleUrl(),
      ...MCO.map.initialCamera(params, { bounds: BOUNDS, fitOpts: FIT_OPTS }),
    });
    MCO.map.addNavigation(map);                  // house default: no compass
    MCO.map.addFitControl(map, {
      bounds: BOUNDS, fitOpts: FIT_OPTS,
      title: 'Zoom to all Native lands',
      onBeforeFit: closeCard,
    });
    zoomFloor = MCO.map.installZoomFloor(map, { bounds: BOUNDS, fitOpts: FIT_OPTS });
  } catch (e) {
    map = null;                                  // no WebGL → table fallback
  }

  // Everything map.setStyle() wipes gets re-added here (theme switch — §4).
  // Layer order (§7): basemap → hillshade → fill → basemap labels → line →
  // selection → our labels.
  function addCustomLayers() {
    if (!landFC) return;
    if (map.getLayer('boundary_county')) {
      map.setLayoutProperty('boundary_county', 'visibility', 'none');
    }
    MCO.map.addHillshade(map);
    const paints = MCO.map.overlayPaints();

    if (!map.getSource(SRC)) {
      map.addSource(SRC, { type: 'geojson', data: landFC, promoteId: 'id' });
    }
    const firstSymbol = MCO.map.firstSymbolLayerId(map);
    const baseOpacity = paints.tribalFill['fill-opacity'];
    // Hover boost: the fill is decorative — the tooltip and live region carry
    // the name (§5.8) and no text sits on the fill, so contrast rules for
    // text don't apply. Capped so basemap detail still reads through.
    map.addLayer({
      id: 'native-fill', type: 'fill', source: SRC,
      paint: {
        ...paints.tribalFill,
        'fill-opacity': ['case',
          ['boolean', ['feature-state', 'hover'], false], Math.min(0.6, baseOpacity * 3),
          baseOpacity],
      },
    }, firstSymbol);
    map.addLayer({ id: 'native-line', type: 'line', source: SRC, paint: paints.tribalLine });
    // Selection outline keyed by feature filter, coloured by the
    // --selection-ring token (§2).
    map.addLayer({
      id: 'native-selected', type: 'line', source: SRC,
      filter: ['==', ['get', 'id'], selectedId ?? -1],
      paint: { 'line-color': token('--selection-ring'), 'line-width': 2.5 },
    });
    // Kit label layout, but our own text-field: the kit's is a Montana-only
    // name shortener. `label` is precomputed in R (agency / division name).
    map.addLayer({
      id: 'native-label', type: 'symbol', source: SRC, minzoom: LABEL_MINZOOM,
      layout: { ...MCO.map.TRIBAL_LABEL_LAYOUT, 'text-field': ['get', 'label'] },
      paint: paints.tribalLabelPaint,
    });
  }

  /* ── Detail card (docked panel, not <dialog> — see index.html) ─────────── */

  function openLand(id, { fly = false } = {}) {
    const r = byId.get(id);
    if (!r) return;
    selectedId = id;
    cardOpener = document.activeElement;

    $('card-title').textContent = r.name;
    $('card-pdf').href = r.pdf_url;              // pre-encoded in R — verbatim
    $('card-zip').href = r.zip_url;
    card.hidden = false;
    card.focus();                                // focus lands on the card

    if (map) {
      map.getLayer('native-selected') &&
        map.setFilter('native-selected', ['==', ['get', 'id'], id]);
      if (fly) {
        // Camera animation gated on the LIVE reduced-motion flag — §5.3
        map.fitBounds(r.bbox, { padding: 60, maxZoom: 9, animate: !MCO.reducedMotion() });
      }
    }
    live.announce(`${r.name} selected. PDF report and data download links are in the details panel.`);
    pushState();
  }

  function closeCard() {
    if (card.hidden) return;
    card.hidden = true;
    selectedId = null;
    if (map && map.getLayer('native-selected')) {
      map.setFilter('native-selected', ['==', ['get', 'id'], -1]);
    }
    // Restore focus — §5.12. Fall back to the canvas when the opener was the
    // map itself (a click) or has gone away.
    const target = cardOpener && cardOpener.isConnected && cardOpener !== document.body
      ? cardOpener : (map ? map.getCanvas() : null);
    if (target && target.focus) target.focus();
    cardOpener = null;
    pushState();
  }
  $('card-close').addEventListener('click', closeCard);

  /* ── URL state (§4): mirror every mutation; clean URL at defaults ──────── */

  function pushState() {
    const p = {};
    if (selectedId && byId.has(selectedId)) p.land = byId.get(selectedId).slug;
    const theme = MCO.getTheme();
    if (theme) p.theme = theme;
    if (map) Object.assign(p, MCO.map.cameraParams(map));
    MCO.replaceUrlState(p);
  }
  if (map) map.on('moveend', pushState);

  // ?land= is a slug; a bare integer (legacy id) is tolerated. Validated
  // against the loaded index before use.
  function resolveLandParam() {
    const raw = MCO.getParamLower('land', params);
    if (!raw) return null;
    if (bySlug.has(raw)) return bySlug.get(raw);
    if (/^\d+$/.test(raw) && byId.has(Number(raw))) return byId.get(Number(raw));
    return null;
  }

  /* ── Search (keyboard path to every polygon — §5.8) ────────────────────── */

  const searchCtl = MCO.initSearchCollapse({
    wrap: $('search-wrap'),
    toggle: $('btn-search-toggle'),
    input,
  });

  function submitSearch({ exactOnly }) {
    const q = input.value.trim();
    if (!q) return;
    const lower = q.toLowerCase();
    let r = byName.get(lower);
    if (!r && !exactOnly) {
      const hits = index.filter((x) => x.name.toLowerCase().includes(lower));
      if (hits.length === 1) {
        r = hits[0];
      } else if (hits.length > 1) {
        MCO.showToast(`${hits.length} matches — keep typing or pick from the list.`);
        live.announce(`${hits.length} Native lands match ${q}.`);
        return;
      }
    }
    if (!r) {
      if (!exactOnly) {
        MCO.showToast(`No Native land matches “${q}”.`);
        live.announce(`No Native land matches ${q}.`);
      }
      return;
    }
    input.value = '';
    searchCtl.close({ restoreFocus: false });    // the card is about to take focus
    openLand(r.id, { fly: true });
  }
  input.addEventListener('change', () => submitSearch({ exactOnly: true }));   // datalist pick
  input.addEventListener('keydown', (e) => {
    if (e.key === 'Enter') { e.preventDefault(); submitSearch({ exactOnly: false }); }
  });

  // Esc is always live (not a printable-key shortcut, so §5.9 doesn't apply).
  // The app owns precedence: search overlay first, then the card.
  document.addEventListener('keydown', (e) => {
    if (e.key !== 'Escape') return;
    if (searchCtl.isOpen()) { searchCtl.close(); return; }
    if (!card.hidden) closeCard();
  });

  /* ── Map pointer interactions ──────────────────────────────────────────── */

  if (map) {
    map.on('mousemove', 'native-fill', (e) => {
      const f = e.features && e.features[0];
      if (!f) return;
      if (hoverId !== null && hoverId !== f.id) {
        map.setFeatureState({ source: SRC, id: hoverId }, { hover: false });
      }
      hoverId = f.id;
      map.setFeatureState({ source: SRC, id: hoverId }, { hover: true });
      map.getCanvas().style.cursor = 'pointer';
      tooltip.innerHTML = `<span class="tooltip-name">${MCO.escapeHTML(f.properties.name)}</span>`;
      // .mco-tooltip is position:fixed — use viewport coordinates.
      tooltip.style.left = (e.originalEvent.clientX + 14) + 'px';
      tooltip.style.top = (e.originalEvent.clientY + 14) + 'px';
      tooltip.classList.add('visible');
    });
    map.on('mouseleave', 'native-fill', () => {
      if (hoverId !== null) map.setFeatureState({ source: SRC, id: hoverId }, { hover: false });
      hoverId = null;
      map.getCanvas().style.cursor = '';
      tooltip.classList.remove('visible');
    });
    map.on('click', 'native-fill', (e) => {
      const f = e.features && e.features[0];
      if (f) openLand(f.properties.id);
    });
  }

  /* ── Theme (§4): swap style, then re-add everything setStyle wiped ─────── */

  MCO.initThemeToggle({
    button: $('btn-theme'),
    iconSun: $('icon-sun'),
    iconMoon: $('icon-moon'),
    onChange: () => {
      if (map) {
        hoverId = null;
        map.setStyle(MCO.map.cartoStyleUrl());
        map.once('style.load', addCustomLayers);
      }
      pushState();
    },
  });

  MCO.initInfoModal({ dialog: $('info-modal'), trigger: $('btn-info') });
  // No first-visit auto-open: this page usually runs inside a cross-site
  // iframe where storage is partitioned, so a "seen" flag would never stick.

  /* ── Screen-reader table twin (§5.2) — static layer, built once ────────── */

  function renderSRTable(rows) {
    const body = rows.map((r) =>
      `<tr><th scope="row">${MCO.escapeHTML(r.name)}</th>` +
      `<td><a href="${MCO.escapeHTML(r.pdf_url)}" target="_blank" rel="noopener noreferrer">PDF report</a></td>` +
      `<td><a href="${MCO.escapeHTML(r.zip_url)}" rel="noopener noreferrer">Data (.zip)</a></td></tr>`).join('');
    srTable.innerHTML =
      '<caption>Native American, Alaska Native, and Native Hawaiian lands with CMIP6 climate projections</caption>' +
      '<thead><tr><th scope="col">Native land</th><th scope="col">Report</th><th scope="col">Data</th></tr></thead>' +
      `<tbody>${body}</tbody>`;
  }

  function showFallback() {
    document.documentElement.classList.add('is-fallback');
    srSection.classList.remove('sr-only');
    srSection.classList.add('is-visible');
    note('This browser cannot display the interactive map. All reports and data are listed below.');
  }

  /* ── Boot ──────────────────────────────────────────────────────────────── */

  function loadAll() {
    note('Loading Native lands…');
    Promise.all([
      MCO.fetchJSON('data/native-land-index.json'),
      MCO.fetchJSON('data/native-land.geojson', { timeoutMs: 60000 }),
    ]).then(([idx, fc]) => {
      index = idx;
      landFC = fc;
      for (const r of idx) {
        byId.set(r.id, r);
        bySlug.set(r.slug, r);
        byName.set(r.name.toLowerCase(), r);
      }
      datalist.replaceChildren(...idx.map((r) => {
        const o = document.createElement('option');
        o.value = r.name;
        return o;
      }));
      renderSRTable(idx);
      note('');

      if (!map) { showFallback(); return; }

      const start = () => {
        addCustomLayers();
        zoomFloor && zoomFloor.refresh();
        // Deep-linked land — fly only when the URL didn't already pin a camera.
        const r = resolveLandParam();
        if (r) openLand(r.id, { fly: !params.has('lng') });
        live.announce(`Map loaded with ${idx.length} Native lands.`);
      };
      if (map.isStyleLoaded()) start(); else map.once('load', start);
    }).catch(() => {
      note('Failed to load boundary data. <button type="button" class="nav-btn" id="btn-retry">Retry</button>');
      $('btn-retry').addEventListener('click', loadAll);
      MCO.showToast('Failed to load data.', 4000);
    });
  }

  if (map) {
    map.on('error', (e) => {
      // Style/tile failures surface here; keep the console message, but tell
      // the user once if the basemap itself never arrives.
      if (e && e.error && /style/i.test(String(e.error.message || ''))) {
        note('Basemap failed to load. The boundaries will still draw once data arrives.');
      }
    });
  }
  loadAll();
})();
