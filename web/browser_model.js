// GitHub Pages serves files only. WebR runs the existing R model on the visitor's
// device, in a worker, while the interface remains responsive.
const encoder = new TextEncoder();
const decoder = new TextDecoder();
const jobs = new Map();
let runtime;
let running = false;
let sequence = 0;

const jsonResponse = (value, status = 200) => new Response(JSON.stringify(value), {
  status, headers: {'Content-Type': 'application/json'}
});

async function loadRuntime(job) {
  if (runtime) return runtime;
  job.progress = 'Loading R for your browser. The first visit can take a minute…';
  const manifest = await fetch('./model.json').then(r => r.json());
  const baseUrl = `https://webr.r-wasm.org/v${manifest.webr_version}/`;
  const {WebR, ChannelType} = await import(baseUrl + 'webr.mjs');
  // PostMessage works on static hosts without special cross-origin headers.
  const R = new WebR({baseUrl, channelType: ChannelType.PostMessage, interactive: false});
  try {
    await R.init();
    await R.FS.mkdir('/brp');
    await R.FS.mkdir('/brp/model');
    const sources = await Promise.all(manifest.files.map(async name => {
      const response = await fetch('./model/' + name);
      if (!response.ok) throw new Error(`Could not load ${name}`);
      return [name, new Uint8Array(await response.arrayBuffer())];
    }));
    for (const [name, bytes] of sources) await R.FS.writeFile('/brp/model/' + name, bytes);
    runtime = {R, manifest};
    return runtime;
  } catch (error) {
    R.close();
    throw error;
  }
}

// Match the key/value CSV used by the Python server. Values are data, not R code.
function configurationCSV(settings) {
  const rows = [['key', 'value']];
  for (let [key, value] of Object.entries(settings)) {
    if (key === 'seasonal_movement') {
      if (value) for (const direction of ['m12', 'm21'])
        rows.push(['seasonal_' + direction, value.map(s => s[direction]).join(';')]);
      continue;
    }
    if (key === 'biology') {
      rows.push(['custom_biology', String(Boolean(value))]);
      if (value) for (const [field, values] of Object.entries(value))
        rows.push(['bio_' + field, values.join(';')]);
      continue;
    }
    if (Array.isArray(value)) value = value.join(';');
    rows.push([key, value ?? '']);
  }
  return rows.map(row => row.map(value => '"' + String(value).replaceAll('"', '""') + '"').join(',')).join('\n') + '\n';
}

// R writes standard quoted CSV. Preserve labels/booleans; convert numeric cells.
function readCSV(text) {
  const rows = [];
  let row = [], value = '', quoted = false;
  for (let i = 0; i < text.length; i++) {
    const c = text[i];
    if (c === '"') {
      if (quoted && text[i + 1] === '"') {value += '"'; i++;}
      else quoted = !quoted;
    } else if (c === ',' && !quoted) {row.push(value); value = '';}
    else if (c === '\n' && !quoted) {row.push(value); rows.push(row); row = []; value = '';}
    else if (c !== '\r' || quoted) value += c;
  }
  if (value || row.length) {row.push(value); rows.push(row);}
  const headers = rows.shift() || [];
  return rows.filter(row => row.length === headers.length).map(row => Object.fromEntries(
    headers.map((key, i) => {
      const value = row[i];
      if (value === '' || ['NA', 'NaN', 'Inf', '-Inf'].includes(value)) return [key, null];
      if (!['rule', 'status', 'note'].includes(key) && Number.isFinite(Number(value)))
        return [key, Number(value)];
      return [key, value];
    })
  ));
}

const tables = {
  brps: 'brps', trajectories: 'trajectories', conditioning: 'conditioning',
  history: 'history_diagnostics', biology: 'biology', surveys: 'surveys',
  movement_schedule: 'movement', benchmarks: 'benchmarks', benchmark_summary: 'benchmark_summary',
  msy: 'msy', msy_summary: 'msy_summary', msy_joint: 'msy_joint', msy_components: 'msy_components'
};

async function calculate(job, settings) {
  const started = performance.now();
  try {
    const {R, manifest} = await loadRuntime(job);
    job.progress = settings.msy?.length
      ? 'Searching equilibrium MSY comparisons. This can take longer than SPR…'
      : 'Calculating reference points and projecting the population…';
    // Clear only the virtual output directory. Each run starts from fresh settings.
    await R.evalRVoid('unlink("/brp/output", recursive = TRUE)');
    await R.FS.writeFile('/brp/settings.csv', encoder.encode(configurationCSV(settings)));
    await R.evalRVoid('source("/brp/model/run_browser.R", local = new.env())');
    const result = {settings};
    const files = {'settings.csv': encoder.encode(configurationCSV(settings))};
    for (const [key, name] of Object.entries(tables)) {
      let bytes;
      try {bytes = await R.FS.readFile('/brp/output/' + name + '.csv');}
      catch (error) {
        if (!['benchmarks', 'benchmark_summary', 'msy', 'msy_summary', 'msy_joint', 'msy_components'].includes(key)) throw error;
      }
      result[key] = bytes ? readCSV(decoder.decode(bytes)) : [];
      if (bytes) files[name + '.csv'] = bytes;
    }
    result.seconds = Math.round((performance.now() - started) / 100) / 10;
    result.model_sha256 = manifest.model_sha256;
    result.engine = 'webR ' + manifest.webr_version;
    files['settings.json'] = encoder.encode(JSON.stringify(settings, null, 2));
    files['results.json'] = encoder.encode(JSON.stringify(result, null, 2));
    job.files = files;
    job.result = result;
    job.progress = 'Complete';
    job.state = 'complete';
  } catch (error) {
    job.state = 'error';
    job.error = error.message || String(error);
  } finally {running = false;}
}

window.brpBrowser = {
  async request(path, options = {}) {
    if (path === '/api/defaults') return fetch('./defaults.json');
    if (path === '/api/runs' && options.method === 'POST') {
      if (running) return jsonResponse({error: 'A calculation is already running.'}, 409);
      const settings = JSON.parse(options.body);
      const job = {id: String(++sequence), state: 'running', progress: 'Starting browser R…'};
      // Retain the previous result for download if a later calculation fails.
      if (jobs.size > 2) jobs.delete(jobs.keys().next().value);
      jobs.set(job.id, job);
      running = true;
      void calculate(job, settings);
      return jsonResponse({id: job.id, state: job.state}, 202);
    }
    const job = jobs.get(path.split('/').pop());
    if (!job) return jsonResponse({error: 'Calculation not found.'}, 404);
    const {files, ...status} = job;
    return jsonResponse(status);
  },
  async download(id) {
    const job = jobs.get(id);
    if (!job?.files) throw new Error('Calculate a scenario first.');
    const {zipSync} = await import('https://cdn.jsdelivr.net/npm/fflate@0.8.2/esm/browser.js');
    const url = URL.createObjectURL(new Blob([zipSync(job.files)], {type: 'application/zip'}));
    const link = document.createElement('a');
    link.href = url;
    link.download = `spatial-brp-${id}.zip`;
    link.click();
    setTimeout(() => URL.revokeObjectURL(url), 10000);
  }
};

await import('./app.js');
