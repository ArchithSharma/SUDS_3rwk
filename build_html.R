# build_html.R
# Builds dmr_map.html: a self-contained HTML replica of app.R.
# Run from the folder containing app.R, edmr_csvs/ and www/.
# Needs: jsonlite, base64enc (plus everything app.R already uses).
library(jsonlite)

# 1) Run the app's own data pipeline (everything in app.R before the UI helpers),
#    so the HTML uses exactly the same cleaning, importance-sampling imputation,
#    percentiles, deltas, palette and branding settings.
src <- readLines("app.R", warn = FALSE)
eval(parse(text = src[seq_len(grep("^# ---- Marker helpers", src)[1] - 1)]))

# 2) Export the data the page needs
mi   <- function(d) (year(d) - year(months[1])) * 12 + month(d) - month(months[1])
fin  <- function(x) { x <- round(x, 3); x[!is.finite(x)] <- NA; x }
facs <- dat |>
  distinct(Primary_Facility_ID, Latitude, Longitude, .keep_all = TRUE) |>
  transmute(id = Primary_Facility_ID, name = Primary_Facility_Name, client = Client_Name,
            county = County, muni = Municipality, lat = Latitude, lon = Longitude)
fi   <- match(paste(dat$Primary_Facility_ID, dat$Latitude, dat$Longitude),
              paste(facs$id, facs$lat, facs$lon)) - 1L
pidx <- match(dat$Parameter_Name, counts$Parameter_Name) - 1L
recs <- pmap(list(pidx, fi, mi(dat$begin_date), mi(dat$end_date), signif(dat$value, 4),
                  as.integer(dat$censored), dat$label, round(dat$pctl, 1),
                  fin(dat$lr_prev), fin(dat$lr_first), dat$Units,
                  replace_na(dat$pool_n, -1L),
                  format(dat$begin_date, "%m/%d/%Y"), format(dat$end_date, "%m/%d/%Y")),
             list)
flags <- flag_rows |>
  group_by(id = Primary_Facility_ID, param = Parameter_Name) |>
  summarise(name = first(Primary_Facility_Name), client = first(Client_Name),
            E = sum(flag == "E"), FF = sum(flag == "FF"), GG = sum(flag == "GG"),
            .groups = "drop")
json <- toJSON(list(params = transmute(counts, name = Parameter_Name, n = n),
                    facs = facs, recs = recs, months = month_labels, flags = flags),
               auto_unbox = TRUE, na = "null", digits = NA, dataframe = "rows")

# 3) Logo embedded so the HTML is one file (falls back to a relative path)
logo_path <- file.path("www", LOGO_FILE)
LOGO_SRC  <- if (file.exists(logo_path))
  paste0("data:image/avif;base64,", base64enc::base64encode(logo_path)) else logo_path

# starting month: mirrors the `selected =` choice of the month slider in app.R
start_idx <- if (any(grepl("selected = month_labels[length(month_labels)]", src, fixed = TRUE)))
  length(month_labels) - 1 else 0

# 4) Fill the template
fill <- function(t, key, val) {
  while (!is.na(p <- regexpr(key, t, fixed = TRUE)[1]) && p > 0)
    t <- paste0(substr(t, 1, p - 1), val, substr(t, p + nchar(key), nchar(t)))
  t
}
html <- r"-----(<!DOCTYPE html>
<html lang="en">
<head>
<meta charset="utf-8">
<meta name="referrer" content="origin">
<meta name="viewport" content="width=device-width, initial-scale=1">
<title>__TITLE__</title>
<link rel="stylesheet" href="https://cdnjs.cloudflare.com/ajax/libs/leaflet/1.9.4/leaflet.css">
<style>
  *{box-sizing:border-box}
  html,body{margin:0;font:14px/1.43 "Helvetica Neue",Helvetica,Arial,sans-serif;color:#333;background:#fff}
  .wrap{padding:0 15px;max-width:1600px;margin:0 auto}
  header{display:flex;justify-content:space-between;align-items:center;gap:16px;margin:12px 0}
  header a.logo{display:inline-block}
  header img{max-height:90px;display:block;margin-bottom:8px}
  header h1{font-size:30px;font-weight:500;margin:0;line-height:1.2}
  .btn{display:inline-block;background:#337ab7;color:#fff;text-decoration:none;border:1px solid #2e6da4;border-radius:4px}
  .btn:hover{background:#286090}
  .btn.big{padding:14px 28px;font-size:20px;font-weight:600}
  .btn.small{padding:1px 5px;font-size:12px;margin-top:14px}
  #app{display:flex;gap:20px;align-items:flex-start}
  aside{width:25%;min-width:270px;flex:none;padding:19px;background:#f5f5f5;border:1px solid #e3e3e3;border-radius:4px}
  main{flex:1;min-width:0}
  h2{font-size:14px;font-weight:700;margin:14px 0 6px}
  aside h2:first-child{margin-top:0}
  h4{font-size:18px;font-weight:500;margin:12px 0 8px}
  label.opt{display:block;padding:1px 0;cursor:pointer;font-weight:400}
  #month{width:100%}
  button{font:inherit;padding:4px 10px;border:1px solid #ccc;background:#fff;border-radius:4px;cursor:pointer}
  .help{font-size:13px;color:#737373;margin:10px 0}
  #summary{margin-top:10px}
  #map{height:70vh;min-height:360px}
  .leg{background:#fff;padding:6px 8px;border-radius:4px;font-size:12px;line-height:1.5;box-shadow:0 0 4px #0003}
  .grad{height:10px;width:150px}
  .grad+div{display:flex;justify-content:space-between}
  hr{border:0;border-top:1px solid #eee;margin:20px 0}
  summary{font-size:14px;font-style:italic;cursor:pointer}
  details{margin-bottom:10px}
  .bar{display:flex;gap:10px;margin:8px 0}
  .bar input{padding:3px 8px;border:1px solid #ccc;border-radius:4px}
  table{border-collapse:collapse;width:100%}
  th,td{padding:5px 8px;border-bottom:1px solid #ddd;text-align:left}
  td.n,th.n{text-align:right}
  th{cursor:pointer;background:#f7f7f7;position:sticky;top:0;white-space:nowrap}
  .twrap{max-height:380px;overflow:auto;border:1px solid #ddd}
  footer{display:flex;justify-content:space-between;align-items:center;padding-bottom:20px}
  footer a{color:#337ab7}
  @media(max-width:760px){#app{flex-direction:column}aside{width:100%}header{flex-direction:column;align-items:flex-start}}
</style>
</head>
<body>
<div class="wrap">
<header>
  <div>
    <a class="logo" href="__LOGO_URL__" target="_blank"><img src="__LOGO_SRC__" alt="" onerror="this.style.display='none'"></a>
    <h1>__TITLE__</h1>
  </div>
  <a class="btn big" href="__LINK_URL__" target="_blank">__LINK_TEXT__</a>
</header>
<div id="app">
  <aside>
    <h2>Contaminant (ordered by # observations)</h2>
    <div id="params"></div>
    <h2>Month: <span id="mlabel"></span></h2>
    <input id="month" type="range" min="0" value="0" step="1">
    <button id="play">&#9654; Play</button>
    <h2>Trend arrows</h2>
    <label class="opt"><input type="radio" name="trend" value="off" checked> Off</label>
    <label class="opt"><input type="radio" name="trend" value="prev"> Change vs. previous reporting period</label>
    <label class="opt"><input type="radio" name="trend" value="first"> Change since first reported</label>
    <div class="help">Point color = percentile of the value within the contaminant (all dates). Circles are measured values; diamonds are '&lt;X' values, replaced by the expected value (importance sampling) of the smoothed distribution of measured values below X. Each month shows the reporting period covering it; the most extreme record per facility is used. Arrows start at the facility: red leans up-right = increase, blue leans up-left = decrease; length scales with size of change (none if within &plusmn;10%).</div>
    <div id="phnote" class="help" hidden>For pH, percentile and arrows are based on distance from 7 (|pH - 7|), not the pH value itself.</div>
    <div id="summary"></div>
    <a class="btn small" href="__LINK_URL_2__" target="_blank">__LINK_TEXT_2__</a>
  </aside>
  <main>
    <div id="map"></div>
    <hr>
    <h4>Records without data (NODI codes)</h4>
    <details>
      <summary>What do these codes mean? (Click me!)</summary>
      <p>Where results are not available, a NO Data Indicator (NODI) Code instead of a numerical value is entered into EDMR system. E and FF NODI codes are red flags:</p>
      <ol>
        <li><strong>E &ndash; Analysis Not Conducted/No Sample.</strong> All samples or results are not available due to analytical equipment failure, because a sample collection was overlooked, or samples could not be collected for the parameter during the reporting period. Use of this NODI code results in a violation.</li>
        <li><strong>FF &ndash; Other (See Comments).</strong> No Data, not covered by NODI codes &ldquo;E&rdquo; or &ldquo;GG.&rdquo; Use only in extenuating circumstances where the reason for the absence of data is not covered by NODI codes &ldquo;E&rdquo; or &ldquo;GG.&rdquo; Use of this NODI code also results in a violation.</li>
        <li><strong>GG &ndash; Conditional Monitoring &ndash; Not Required.</strong> If the permit requires sample collection and analysis only under certain conditions and those conditions were not met during the monitoring period. This includes non-representative outfalls. Use of this NODI code does not result in a violation.</li>
      </ol>
    </details>
    <label class="opt"><input type="checkbox" id="onlysel"> Only the selected contaminant (otherwise all 15)</label>
    <div class="bar"><button id="dl">Download CSV</button><input id="q" type="search" placeholder="Search"></div>
    <div class="twrap"><table><thead></thead><tbody></tbody></table></div>
  </main>
</div>
<hr style="border-top:2px solid #ccc;margin-top:30px">
<footer>
  <em>Made in collaboration with <a href="https://cmusuds.org" target="_blank">Carnegie Mellon University Students Using Data for Social Good (SUDS)</a></em>
  <a href="https://github.com/ArchithSharma/SUDS_3rwk" target="_blank" title="View the code on GitHub" aria-label="GitHub repository" style="color:#333"><svg width="28" height="28" viewBox="0 0 16 16" fill="currentColor" xmlns="http://www.w3.org/2000/svg"><path d="M8 0C3.58 0 0 3.58 0 8c0 3.54 2.29 6.53 5.47 7.59.4.07.55-.17.55-.38 0-.19-.01-.82-.01-1.49-2.01.37-2.53-.49-2.69-.94-.09-.23-.48-.94-.82-1.13-.28-.15-.68-.52-.01-.53.63-.01 1.08.58 1.23.82.72 1.21 1.87.87 2.33.66.07-.52.28-.87.51-1.07-1.78-.2-3.64-.89-3.64-3.95 0-.87.31-1.59.82-2.15-.08-.2-.36-1.02.08-2.12 0 0 .67-.21 2.2.82.64-.18 1.32-.27 2-.27.68 0 1.36.09 2 .27 1.53-1.04 2.2-.82 2.2-.82.44 1.1.16 1.92.08 2.12.51.56.82 1.27.82 2.15 0 3.07-1.87 3.75-3.65 3.95.29.25.54.73.54 1.48 0 1.07-.01 1.93-.01 2.2 0 .21.15.46.55.38A8.013 8.013 0 0016 8c0-4.42-3.58-8-8-8z"/></svg></a>
</footer>
</div>
<script src="https://cdnjs.cloudflare.com/ajax/libs/leaflet/1.9.4/leaflet.min.js"></script>
<script>
const D = __DATA__;
const {params, facs, recs, months, flags} = D;
const FLAT = Math.log2(1.10), CAP = 3, ANG = 45;
const VIR = __ZION__;   // Zion palette from nationalparkcolors (low -> high)
const rgb = s => [1,3,5].map(i => parseInt(s.substr(i,2),16));
function color(p){
  const x = Math.max(0,Math.min(100,p))/100*(VIR.length-1), i = Math.min(Math.floor(x),VIR.length-2), t = x-i;
  const a = rgb(VIR[i]), b = rgb(VIR[i+1]);
  return 'rgb('+a.map((v,k)=>Math.round(v+(b[k]-v)*t)).join(',')+')';
}
const esc = s => String(s ?? '').replace(/[&<>"]/g, c => ({'&':'&amp;','<':'&lt;','>':'&gt;','"':'&quot;'}[c]));
const chg = lr => lr == null ? 'n/a' : ((Math.pow(2,lr)-1)*100 >= 0 ? '+' : '') + ((Math.pow(2,lr)-1)*100).toFixed(0) + '%';

// records by contaminant
const byP = params.map(() => []);
recs.forEach(r => byP[r[0]].push(r));

// ---- controls ----
let P = 0, trend = 'off', timer = null;
const $ = id => document.getElementById(id);
$('params').innerHTML = params.map((p,i) =>
  `<label class="opt"><input type="radio" name="param" value="${i}" ${i===0?'checked':''}> ${esc(p.name)} (n = ${p.n.toLocaleString()})</label>`).join('');
const slider = $('month');
slider.max = months.length - 1;
slider.value = __START__;   // same starting month as app.R
const sync = () => { $('mlabel').textContent = months[+slider.value]; };

// ---- map ----
const map = L.map('map');
// Key-free basemaps. Esri tiles need no API key and work from file:// and iframes.
// OpenStreetMap's own server only serves pages that send a Referer, so it is
// offered as an option for when the page is hosted over https.
const esriAttr = 'Tiles &copy; Esri &mdash; Source: Esri, HERE, Garmin, OpenStreetMap contributors';
const base = {
  'Light gray (Esri)': L.tileLayer('https://server.arcgisonline.com/ArcGIS/rest/services/Canvas/World_Light_Gray_Base/MapServer/tile/{z}/{y}/{x}', {maxZoom:16, attribution:esriAttr}),
  'Streets (Esri)': L.tileLayer('https://server.arcgisonline.com/ArcGIS/rest/services/World_Street_Map/MapServer/tile/{z}/{y}/{x}', {maxZoom:19, attribution:esriAttr}),
  'OpenStreetMap (hosted pages only)': L.tileLayer('https://tile.openstreetmap.org/{z}/{x}/{y}.png', {maxZoom:19, attribution:'&copy; <a href="https://www.openstreetmap.org/copyright">OpenStreetMap</a> contributors'})
};
base['Light gray (Esri)'].addTo(map);
L.control.layers(base, null, {collapsed:true}).addTo(map);
map.fitBounds([[Math.min(...facs.map(f=>f.lat)), Math.min(...facs.map(f=>f.lon))],
               [Math.max(...facs.map(f=>f.lat)), Math.max(...facs.map(f=>f.lon))]]);
const pts = L.layerGroup().addTo(map), arrows = L.layerGroup().addTo(map);

const pctlLeg = L.control({position:'bottomright'});
pctlLeg.onAdd = () => { const d = L.DomUtil.create('div','leg'); d.id = 'pleg'; return d; };
pctlLeg.addTo(map);
const shapeLeg = L.control({position:'bottomleft'});
shapeLeg.onAdd = () => { const d = L.DomUtil.create('div','leg');
  d.innerHTML = '&#9679; measured value<br>&#9670; below detection (&lt;X, imputed)<br><span style="color:#c0392b">&#9650;</span> increase &nbsp;<span style="color:#2471a3">&#9660;</span> decrease'; return d; };
shapeLeg.addTo(map);

function pointSvg(fill, cens){
  const s = cens
    ? `<polygon points="10,1.5 18.5,10 10,18.5 1.5,10" fill="${fill}" stroke="#222" stroke-width="1.6"/>`
    : `<circle cx="10" cy="10" r="7.5" fill="${fill}" stroke="white" stroke-width="1.5"/>`;
  return `<svg xmlns="http://www.w3.org/2000/svg" width="20" height="20">${s}</svg>`;
}
// tail sits at the point; increases lean up-right, decreases lean up-left
function arrowSvg(up, L_, col){
  const S = 2*L_ + 20, c = S/2, hl = Math.min(14, L_*0.5), st = 'stroke="white" stroke-width="0.8"';
  return `<svg xmlns="http://www.w3.org/2000/svg" width="${S}" height="${S}"><g transform="rotate(${up?ANG:-ANG} ${c} ${c})">`+
    `<rect x="${c-2.5}" y="${c-L_+hl}" width="5" height="${L_-hl}" fill="${col}" ${st}/>`+
    `<polygon points="${c},${c-L_} ${c-8},${c-L_+hl} ${c+8},${c-L_+hl}" fill="${col}" ${st}/></g></svg>`;
}

function render(){
  const isPH = params[P].name === 'pH';
  $('phnote').hidden = !isPH;
  $('pleg').innerHTML = 'Percentile' + (isPH ? '<br><small>(distance from 7)</small>' : '') +
    '<div class="grad" style="background:linear-gradient(to right,'+VIR.join(',')+')"></div><div><span>0</span><span>100</span></div>';
  pts.clearLayers(); arrows.clearLayers();
  const m = +slider.value;
  const list = byP[P].filter(r => r[2] <= m && r[3] >= m);
  $('summary').textContent = `${list.length} facilities shown (${list.filter(r=>r[5]).length} below detection)`;
  list.forEach(r => {
    const f = facs[r[1]], cens = r[5] === 1;
    const imp = cens ? (r[11] >= 5
      ? `<br><i>Imputed: expected value (importance sampling) from ${r[11]} measured values up to X</i>`
      : '<br><i>Too few measured values below X; used X/2</i>') : '';
    const sfx = isPH ? ' in |pH - 7|' : '';
    const html = `<b>${esc(f.name)}</b><br>${esc(f.client)}<br><small>Facility ID ${esc(f.id)} &middot; ${esc(f.county)} Co. &middot; ${esc(f.muni)}</small><hr style="margin:4px 0">`+
      `<b>${esc(params[P].name)}</b> (${r[12]} to ${r[13]})<br>Reported: ${esc(r[6])} ${esc(r[10])}<br>Value used: ${r[4]}<br>`+
      `${isPH ? 'Percentile of |pH - 7|' : 'Percentile'}: ${Math.round(r[7])}<br>`+
      `Change vs. previous${sfx}: ${chg(r[8])}<br>Change since first${sfx}: ${chg(r[9])}${imp}`;
    L.marker([f.lat,f.lon], {icon: L.divIcon({html: pointSvg(color(r[7]), cens), className:'', iconSize:[20,20], iconAnchor:[10,10]})})
      .bindPopup(html).bindTooltip(f.name).addTo(pts);
    const lr = trend === 'prev' ? r[8] : trend === 'first' ? r[9] : null;
    if (lr != null && Math.abs(lr) >= FLAT){
      const len = Math.round(12 + 14*Math.min(Math.abs(lr), CAP)), up = lr > 0;
      L.marker([f.lat,f.lon], {interactive:false, zIndexOffset:1000,
        icon: L.divIcon({html: arrowSvg(up, len, up ? '#c0392b' : '#2471a3'), className:'', iconSize:[2*len+20,2*len+20], iconAnchor:[len+10,len+10]})})
        .addTo(arrows);
    }
  });
}

// ---- E/FF/GG table ----
const cols = [['Facility ID','id'],['Facility Name','name'],['Client Name','client'],['E','E',1],['FF','FF',1],['E + FF','EFF',1],['GG','GG',1],['Total','Total',1]];
let sort = {k:'Total', dir:-1}, tableRows = [];
function buildTable(){
  const src = $('onlysel').checked ? flags.filter(x => x.param === params[P].name) : flags;
  const g = {};
  src.forEach(x => { const o = g[x.id] ??= {id:x.id,name:x.name,client:x.client,E:0,FF:0,GG:0};
    o.E += x.E; o.FF += x.FF; o.GG += x.GG; });
  tableRows = Object.values(g).map(o => ({...o, EFF:o.E+o.FF, Total:o.E+o.FF+o.GG}));
  const q = $('q').value.trim().toLowerCase();
  if (q) tableRows = tableRows.filter(r => [r.id,r.name,r.client].join(' ').toLowerCase().includes(q));
  tableRows.sort((a,b) => (a[sort.k] > b[sort.k] ? 1 : a[sort.k] < b[sort.k] ? -1 : 0) * sort.dir);
  const th = document.querySelector('thead');
  th.innerHTML = '<tr>' + cols.map(c => `<th class="${c[2]?'n':''}" data-k="${c[1]}">${c[0]}${sort.k===c[1]?(sort.dir>0?' &#9650;':' &#9660;'):''}</th>`).join('') + '</tr>';
  th.querySelectorAll('th').forEach(h => h.onclick = () => {
    const k = h.dataset.k; sort = {k, dir: sort.k===k ? -sort.dir : (cols.find(c=>c[1]===k)[2] ? -1 : 1)}; buildTable(); });
  document.querySelector('tbody').innerHTML = tableRows.map(r =>
    '<tr>' + cols.map(c => `<td class="${c[2]?'n':''}">${esc(r[c[1]])}</td>`).join('') + '</tr>').join('');
}
$('dl').onclick = () => {   // exports every row currently in the table, in the current sort order
  const q = v => '"' + String(v ?? '').replace(/"/g,'""') + '"';
  const csv = [cols.map(c => q(c[0])).join(',')]
    .concat(tableRows.map(r => cols.map(c => q(r[c[1]])).join(','))).join('\n');
  const a = document.createElement('a');
  a.href = URL.createObjectURL(new Blob([csv], {type:'text/csv'}));
  a.download = 'efgg_by_facility.csv'; a.click();
};

// ---- events ----
document.querySelectorAll('input[name=param]').forEach(e => e.onchange = () => { P = +e.value; render(); if ($('onlysel').checked) buildTable(); });
document.querySelectorAll('input[name=trend]').forEach(e => e.onchange = () => { trend = e.value; render(); });
slider.oninput = () => { sync(); render(); };
$('onlysel').onchange = buildTable;
$('q').oninput = buildTable;
$('play').onclick = () => {
  if (timer) { clearInterval(timer); timer = null; $('play').innerHTML = '&#9654; Play'; return; }
  $('play').innerHTML = '&#10074;&#10074; Pause';
  timer = setInterval(() => { slider.value = (+slider.value + 1) % months.length; sync(); render(); }, 1200);
};
sync(); render(); buildTable();
</script>
</body>
</html>
)-----"
for (kv in list(c("__TITLE__", APP_TITLE), c("__LOGO_URL__", LOGO_URL), c("__LOGO_SRC__", LOGO_SRC),
                c("__LINK_URL__", LINK_URL), c("__LINK_TEXT__", LINK_TEXT),
                c("__START__", start_idx), c("__LINK_URL_2__", LINK_URL_2), c("__LINK_TEXT_2__", LINK_TEXT_2),
                c("__ZION__", as.character(toJSON(zion))), c("__DATA__", as.character(json))))
  html <- fill(html, kv[1], kv[2])
writeLines(html, "dmr_map.html", useBytes = TRUE)
message("Wrote dmr_map.html (", round(file.size("dmr_map.html") / 1024), " KB)")

writeLines(html, "index.html", useBytes = TRUE)
message("Wrote index.html for GitHub Pages (", round(file.size("index.html") / 1024), " KB)")