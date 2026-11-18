"""Start the Codespaces development server once per container session."""
from pathlib import Path
import subprocess
import sys
import urllib.error
import urllib.request

root = Path(__file__).resolve().parents[1]
try:
    with urllib.request.urlopen("http://127.0.0.1:8765/api/defaults", timeout=2) as response:
        if response.status == 200:
            raise SystemExit(0)
except (urllib.error.URLError, TimeoutError):
    pass
with (root / "server.log").open("a") as log:
    subprocess.Popen([sys.executable, "app.py", "--no-browser"], cwd=root,
        stdin=subprocess.DEVNULL, stdout=log, stderr=subprocess.STDOUT, start_new_session=True)
print("Spatial BRP explorer is starting on forwarded port 8765.")
