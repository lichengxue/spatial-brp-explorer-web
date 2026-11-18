"""Build the public browser app. Run: python web/build_site.py"""
from pathlib import Path
import hashlib
import importlib.util
import json
import shutil
import zipfile

root = Path(__file__).resolve().parents[1]
destination = root / 'dist'
destination.mkdir(exist_ok=True)
(destination / 'model').mkdir(exist_ok=True)

spec = importlib.util.spec_from_file_location('brp_app', root / 'app.py')
app = importlib.util.module_from_spec(spec)
spec.loader.exec_module(app)
defaults = dict(defaults=app.DEFAULTS, rules=app.RULES, benchmarks=app.BENCHMARKS,
    biology_defaults=json.loads((root / 'model/default_biology.json').read_text()),
    audience_scenarios=app.audience_scenarios(), opening_scenario='regional-global-40')
(destination / 'defaults.json').write_text(json.dumps(defaults), encoding='utf-8')

html = (root / 'static/index.html').read_text(encoding='utf-8')
html = html.replace('href="/style.css"', 'href="./style.css"')
html = html.replace('<script src="/app.js" defer></script>',
                    '<script type="module" src="./browser_model.js"></script>')
html = html.replace('Local model <span', 'Browser model <span')
html = html.replace('<body>', '<body class="browser-app">')
html = html.replace('A localhost link works only on this computer. Share the app ZIP for others to run, or copy a scenario link from a hosted copy of this app.',
                    'Share this page or copy a scenario link. Visitors can calculate directly in their browser without installing R or signing in.')
html = html.replace('Preparing the default scenario…', 'Loading the browser model. The first visit takes a little longer…')
html = html.replace('</header>', '<nav class="public-links" aria-label="App information">'
    '<a href="./guide.html">Quick start &amp; model guide</a>'
    '<a href="./spatial-brp-R.zip" download>Download the R scripts</a>'
    '<span>No installation or sign-in · calculations run on your device</span></nav></header>', 1)
(destination / 'index.html').write_text(html, encoding='utf-8')
for name in ('app.js', 'style.css'):
    shutil.copyfile(root / 'static' / name, destination / name)
shutil.copyfile(root / 'web/browser_model.js', destination / 'browser_model.js')
shutil.copyfile(root / 'web/guide.html', destination / 'guide.html')
(destination / '.nojekyll').touch()

# Keep the exact R model. Only the Rscript command-line startup is replaced.
runner = (root / 'model/run_model.R').read_text(encoding='utf-8')
body = runner[runner.index('source(file.path(model_dir, "spatial_brp_core.R"))'):]
browser_runner = ('# Same GUI workflow, with paths in the browser virtual filesystem.\n'
                  'args <- c("/brp/settings.csv", "/brp/output")\n'
                  'model_dir <- "/brp/model"\n' + body)
(destination / 'model/run_browser.R').write_text(browser_runner, encoding='utf-8')
files = ['run_browser.R']
for file in sorted((root / 'model').glob('*.R')):
    if file.name == 'run_model.R':
        continue
    shutil.copyfile(file, destination / 'model' / file.name)
    files.append(file.name)
manifest = dict(files=files, webr_version='0.6.1',
    model_sha256=hashlib.sha256((root / 'model/spatial_brp_core.R').read_bytes()).hexdigest())
(destination / 'model.json').write_text(json.dumps(manifest), encoding='utf-8')
with zipfile.ZipFile(destination / 'spatial-brp-R.zip', 'w', zipfile.ZIP_DEFLATED) as archive:
    for file in sorted((root / 'R').iterdir()):
        if file.is_file() and (file.suffix == '.R' or file.name == 'README.md'):
            archive.write(file, 'spatial-brp-R/' + file.name)
print('Browser app built in dist/')
