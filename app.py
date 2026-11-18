"""Local two-region BRP GUI. Uses only Python's standard library and base R."""
from __future__ import annotations

import argparse
import csv
import hashlib
import io
import json
import math
import mimetypes
import os
from pathlib import Path
import secrets
import shutil
import subprocess
import threading
import time
from http.server import BaseHTTPRequestHandler, ThreadingHTTPServer
import webbrowser
import zipfile

ROOT = Path(__file__).resolve().parent
DEFAULT_RULES = ["SpE-RecW", "SpE-EqW", "SpE-SurvW", "SpE-CatchW", "SpD-RecW", "Natal-RecW", "Pop-NoMove", "Pop-Move", "Reg-Move"]
RULES = DEFAULT_RULES + ["SpD-EqW", "SpD-SurvW", "SpD-CatchW"]
BENCHMARKS = ["Yield-SPR", "Yield-Max"]
MSY_OPTIONS = ["MSY-Pattern", "MSY-System", "MSY-Joint"]
DEFAULTS = dict(rules=DEFAULT_RULES, structure="NH", movement="high", recruitment="mean",
    life_history="short", fishing_share1=70, historical_F=0.4, historical_years=50, R01=700, R02=300,
    target=40, years=60, stochastic=False, sigmaR=0.4, seed=2026,
    bh_calibration="slope", alpha=12, steepness=0.75, projection="fixed",
    surveys=[f"{region}-{season}" for region in (1, 2) for season in (1, 2, 3, 4)],
    survey_ensemble=True, benchmarks=[], msy=[], biology=None, seasonal_movement=None,
    regional_weighting="recruitment", regional_share1=50)
JOBS: dict[str, dict] = {}
LOCK = threading.Lock()


def find_r() -> str:
    candidates = [os.environ.get("RSCRIPT"), shutil.which("Rscript")]
    candidates += sorted((Path(os.environ.get("ProgramFiles", "C:/Program Files")) / "R").glob("R-*/bin/Rscript.exe"), reverse=True)
    for candidate in candidates:
        if candidate and Path(candidate).is_file():
            return str(candidate)
    raise RuntimeError("Rscript was not found. Install R or set the RSCRIPT environment variable to Rscript.exe.")


def validate(raw: dict) -> dict:
    if not isinstance(raw, dict):
        raise ValueError("Settings must be an object.")
    cfg = dict(DEFAULTS)
    cfg.update({k: v for k, v in raw.items() if k in DEFAULTS})
    for key, choices in dict(structure=["NH", "MPO", "MPD"], movement=["none", "low", "high", "custom"],
        recruitment=["mean", "sr"], life_history=["short", "long"],
        bh_calibration=["slope", "steepness"], projection=["fixed", "feedback"],
        regional_weighting=["recruitment", "equal", "survey", "catch", "custom"]).items():
        if cfg[key] not in choices:
            raise ValueError(f"Invalid {key}.")
    if cfg["movement"] == "custom":
        schedule = cfg["seasonal_movement"]
        if not isinstance(schedule, list) or len(schedule) != 4:
            raise ValueError("Custom movement needs exactly four seasonal transfers.")
        cleaned_schedule = []
        for season, transfer in enumerate(schedule, 1):
            if not isinstance(transfer, dict) or set(transfer) != {"m12", "m21"}:
                raise ValueError(f"Season {season} needs m12 and m21 percentages.")
            cleaned_transfer = {}
            for direction in ("m12", "m21"):
                try:
                    value = float(transfer[direction])
                except (TypeError, ValueError):
                    raise ValueError(f"Season {season} {direction} must be a percentage.") from None
                if isinstance(transfer[direction], bool) or not math.isfinite(value) or not 0 <= value <= 100:
                    raise ValueError(f"Season {season} {direction} must be between 0 and 100%.")
                cleaned_transfer[direction] = value
            cleaned_schedule.append(cleaned_transfer)
        cfg["seasonal_movement"] = cleaned_schedule
    else:
        cfg["seasonal_movement"] = None
    # Retain compatibility with saved settings and older open GUI tabs.
    if isinstance(cfg["benchmarks"], bool):
        cfg["benchmarks"] = list(BENCHMARKS) if cfg["benchmarks"] else []
    if not isinstance(cfg["benchmarks"], list) or any(b not in BENCHMARKS for b in cfg["benchmarks"]):
        raise ValueError("Select valid yield benchmarks: Yield-SPR and/or Yield-Max.")
    cfg["benchmarks"] = [b for b in BENCHMARKS if b in cfg["benchmarks"]]
    if not isinstance(cfg["msy"], list) or any(b not in MSY_OPTIONS for b in cfg["msy"]):
        raise ValueError("Select valid experimental MSY options.")
    cfg["msy"] = [b for b in MSY_OPTIONS if b in cfg["msy"]]
    if cfg["msy"] and cfg["recruitment"] != "sr":
        raise ValueError("Experimental MSY requires Beverton–Holt recruitment.")
    if not isinstance(cfg["rules"], list) or any(r not in RULES for r in cfg["rules"]):
        raise ValueError("Select valid reference point formulations.")
    cfg["rules"] = [r for r in RULES if r in cfg["rules"]]
    if not cfg["rules"] and not cfg["benchmarks"] and not cfg["msy"]:
        raise ValueError("Select at least one BRP, yield benchmark or MSY comparison.")
    if isinstance(cfg["regional_share1"], bool):
        raise ValueError("regional_share1 must be a percentage between 1 and 99.")
    for key, lower, upper in [("fishing_share1", 1, 99), ("historical_F", 0.001, 2),
        ("R01", 1, 1e7), ("R02", 1, 1e7), ("target", 1, 99), ("regional_share1", 1, 99), ("years", 10, 200), ("historical_years", 5, 500),
        ("sigmaR", 0, 1.5), ("seed", 0, 2147483646), ("alpha", 0.01, 1000), ("steepness", 0.21, 0.99)]:
        try:
            value = float(cfg[key])
        except (TypeError, ValueError):
            raise ValueError(f"{key} must be a number.") from None
        if not math.isfinite(value) or not lower <= value <= upper:
            raise ValueError(f"{key} must be between {lower:g} and {upper:g}.")
        if key in ("years", "historical_years", "seed") and value != int(value):
            raise ValueError(f"{key} must be an integer.")
        cfg[key] = int(value) if key in ("years", "historical_years", "seed") else value
    for key in ("stochastic", "survey_ensemble"):
        if not isinstance(cfg[key], bool):
            raise ValueError(f"{key} must be true or false.")
    allowed_surveys = DEFAULTS["surveys"]
    if not isinstance(cfg["surveys"], list) or any(s not in allowed_surveys for s in cfg["surveys"]):
        raise ValueError("Invalid survey selection.")
    cfg["surveys"] = [s for s in allowed_surveys if s in cfg["surveys"]]
    if any(not any(s.startswith(f"{r}-") for s in cfg["surveys"]) for r in (1, 2)):
        raise ValueError("Select at least one survey in each region to calculate regional survey weights.")
    biology = cfg["biology"]
    if biology is not None:
        fields = {"selectivity1", "selectivity2", "weight1", "weight2", "maturity1", "maturity2"}
        ages = 10 if cfg["life_history"] == "short" else 30
        if not isinstance(biology, dict) or set(biology) != fields:
            raise ValueError("Custom biology needs two selectivity, weight and maturity vectors.")
        cleaned = {}
        for field in fields:
            vector = biology[field]
            if not isinstance(vector, list) or len(vector) != ages:
                raise ValueError(f"{field} must have exactly {ages} age values.")
            try:
                values = [float(v) for v in vector]
            except (TypeError, ValueError):
                raise ValueError(f"{field} must contain numeric values.") from None
            upper = 1e6 if field.startswith("weight") else 1
            if any(not math.isfinite(v) or v < 0 or v > upper for v in values):
                raise ValueError(f"{field} values must be finite and between 0 and {upper:g}.")
            cleaned[field] = values
        for r in (1, 2):
            if not any(cleaned[f"selectivity{r}"]):
                raise ValueError(f"Fleet {r} must select at least one age.")
            if not any(w * m > 0 for w, m in zip(cleaned[f"weight{r}"], cleaned[f"maturity{r}"])):
                raise ValueError(f"Biology type {r} must have positive spawning output at some age.")
        cfg["biology"] = cleaned
    return cfg


def write_configuration(path: Path, cfg: dict) -> None:
    """Serialize the settings without making R depend on a JSON package."""
    with path.open("w", encoding="utf-8", newline="") as stream:
        writer = csv.writer(stream)
        writer.writerow(["key", "value"])
        for key, value in cfg.items():
            if key == "seasonal_movement":
                if value is not None:
                    for direction in ("m12", "m21"):
                        writer.writerow(["seasonal_" + direction, ";".join(str(s[direction]) for s in value)])
                continue
            if key == "biology":
                writer.writerow(["custom_biology", str(value is not None).lower()])
                if value:
                    for field, vector in value.items():
                        writer.writerow(["bio_" + field, ";".join(str(v) for v in vector)])
                continue
            if isinstance(value, list):
                value = ";".join(value)
            elif isinstance(value, bool):
                value = str(value).lower()
            writer.writerow([key, value])


def read_csv(path: Path) -> list[dict]:
    with path.open(encoding="utf-8-sig", newline="") as stream:
        rows = list(csv.DictReader(stream))
    for row in rows:
        for key, value in row.items():
            if value == "":
                row[key] = None
            elif key not in ("rule", "status", "note"):
                try:
                    number = float(value)
                    row[key] = number if math.isfinite(number) else None
                except ValueError:
                    pass
    return rows


def audience_scenarios() -> list[dict]:
    scenarios = json.loads((ROOT / "model" / "audience_scenarios.json").read_text(encoding="utf-8"))
    return [dict(scenario, settings=validate(scenario["settings"])) for scenario in scenarios]


def run_model(job_id: str, cfg: dict, rscript: str) -> None:
    folder = ROOT / ".runs" / job_id
    folder.mkdir(parents=True)
    try:
        (folder / "settings.json").write_text(json.dumps(cfg, indent=2), encoding="utf-8")
        write_configuration(folder / "settings.csv", cfg)
        started = time.monotonic()
        with (folder / "model.log").open("w", encoding="utf-8") as log:
            process = subprocess.run([rscript, "--vanilla", str(ROOT / "model" / "run_model.R"),
                str(folder / "settings.csv"), str(folder)], cwd=ROOT,
                stdout=log, stderr=subprocess.STDOUT, timeout=900,
                creationflags=getattr(subprocess, "CREATE_NO_WINDOW", 0))
        if process.returncode:
            tail = (folder / "model.log").read_text(encoding="utf-8", errors="replace")[-3000:]
            raise RuntimeError(tail.strip() or f"R exited with status {process.returncode}.")
        result = dict(settings=cfg, brps=read_csv(folder / "brps.csv"),
            trajectories=read_csv(folder / "trajectories.csv"), conditioning=read_csv(folder / "conditioning.csv"), history=read_csv(folder / "history_diagnostics.csv"),
            biology=read_csv(folder / "biology.csv"), surveys=read_csv(folder / "surveys.csv"),
            movement_schedule=read_csv(folder / "movement.csv"),
            benchmarks=read_csv(folder / "benchmarks.csv") if (folder / "benchmarks.csv").exists() else [],
            benchmark_summary=read_csv(folder / "benchmark_summary.csv") if (folder / "benchmark_summary.csv").exists() else [],
            msy=read_csv(folder / "msy.csv") if (folder / "msy.csv").exists() else [],
            msy_summary=read_csv(folder / "msy_summary.csv") if (folder / "msy_summary.csv").exists() else [],
            msy_joint=read_csv(folder / "msy_joint.csv") if (folder / "msy_joint.csv").exists() else [],
            msy_components=read_csv(folder / "msy_components.csv") if (folder / "msy_components.csv").exists() else [],
            seconds=round(time.monotonic() - started, 1),
            model_sha256=hashlib.sha256((ROOT / "model" / "spatial_brp_core.R").read_bytes()).hexdigest())
        (folder / "results.json").write_text(json.dumps(result, allow_nan=False), encoding="utf-8")
        with LOCK:
            JOBS[job_id].update(state="complete", result=result)
    except Exception as exc:
        with LOCK:
            JOBS[job_id].update(state="error", error=str(exc))


class Handler(BaseHTTPRequestHandler):
    def log_message(self, fmt, *args):
        pass

    def send(self, data: bytes, mime="application/json", code=200, extra=None):
        self.send_response(code)
        self.send_header("Content-Type", mime)
        self.send_header("Content-Length", str(len(data)))
        self.send_header("Cache-Control", "no-store")
        self.send_header("X-Content-Type-Options", "nosniff")
        if extra:
            for key, value in extra.items():
                self.send_header(key, value)
        self.end_headers()
        self.wfile.write(data)

    def json(self, value, code=200):
        self.send(json.dumps(value, allow_nan=False).encode(), code=code)

    def do_GET(self):
        path = self.path.split("?", 1)[0]
        if path == "/api/defaults":
            biology_defaults = json.loads((ROOT / "model" / "default_biology.json").read_text(encoding="utf-8"))
            return self.json(dict(defaults=DEFAULTS, rules=RULES, benchmarks=BENCHMARKS, biology_defaults=biology_defaults,
                audience_scenarios=audience_scenarios(), opening_scenario="regional-global-40"))
        if path.startswith("/api/runs/"):
            parts = path.strip("/").split("/")
            job_id = parts[2]
            with LOCK:
                job = dict(JOBS.get(job_id, {}))
            if not job:
                return self.json(dict(error="Run not found."), 404)
            if len(parts) == 4 and parts[3] == "download" and job["state"] == "complete":
                buffer = io.BytesIO()
                with zipfile.ZipFile(buffer, "w", zipfile.ZIP_DEFLATED) as archive:
                    for name in ["settings.json", "settings.csv", "brps.csv", "trajectories.csv", "conditioning.csv", "history_diagnostics.csv", "biology.csv", "surveys.csv", "movement.csv", "benchmarks.csv", "benchmark_summary.csv", "msy.csv", "msy_summary.csv", "msy_joint.csv", "msy_components.csv", "results.json", "model.log"]:
                        file = ROOT / ".runs" / job_id / name
                        if file.exists():
                            archive.write(file, name)
                return self.send(buffer.getvalue(), "application/zip", extra={"Content-Disposition": f'attachment; filename="spatial-brp-{job_id}.zip"'})
            progress = ROOT / ".runs" / job_id / "progress.txt"
            try:
                job["progress"] = progress.read_text(encoding="utf-8").strip() if progress.exists() else "Starting R model…"
            except OSError:
                job["progress"] = "Calculating…"
            return self.json(job)
        public = {"/": "index.html", "/app.js": "app.js", "/style.css": "style.css"}
        if path not in public:
            return self.json(dict(error="Not found."), 404)
        file = ROOT / "static" / public[path]
        return self.send(file.read_bytes(), mimetypes.guess_type(file.name)[0] or "application/octet-stream")

    def do_POST(self):
        if self.path != "/api/runs":
            return self.json(dict(error="Not found."), 404)
        # Accept same-origin requests only. The server is bound to loopback.
        origin = self.headers.get("Origin")
        expected = f"http://{self.headers.get('Host')}"
        forwarded_origin = f"https://{self.headers.get('X-Forwarded-Host')}" if self.headers.get('X-Forwarded-Host') else None
        codespace_name = os.environ.get("CODESPACE_NAME")
        codespace_domain = os.environ.get("GITHUB_CODESPACES_PORT_FORWARDING_DOMAIN", "app.github.dev")
        codespace_origin = f"https://{codespace_name}-{self.server.server_port}.{codespace_domain}" if codespace_name else None
        if origin and origin not in (expected, forwarded_origin, codespace_origin):
            return self.json(dict(error="Origin not permitted."), 403)
        try:
            length = int(self.headers.get("Content-Length", "0"))
            if length < 1 or length > 32768:
                raise ValueError("Invalid request size.")
            cfg = validate(json.loads(self.rfile.read(length)))
            with LOCK:
                if any(j["state"] == "running" for j in JOBS.values()):
                    return self.json(dict(error="A model run is already in progress."), 409)
                job_id = secrets.token_hex(6)
                JOBS[job_id] = dict(id=job_id, state="running")
            threading.Thread(target=run_model, args=(job_id, cfg, self.server.rscript), daemon=True).start()
            return self.json(dict(id=job_id, state="running"), 202)
        except (ValueError, TypeError) as exc:
            return self.json(dict(error=str(exc)), 400)


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--port", type=int, default=8765)
    parser.add_argument("--no-browser", action="store_true")
    args = parser.parse_args()
    rscript = find_r()
    try:
        server = ThreadingHTTPServer(("127.0.0.1", args.port), Handler)
    except OSError as exc:
        raise SystemExit(f"Could not open port {args.port}. Try --port 8766. ({exc})") from exc
    server.rscript = rscript
    url = f"http://127.0.0.1:{server.server_port}"
    print(f"Two-region BRP explorer: {url}\nUsing R: {rscript}\nClose this process to stop the GUI.", flush=True)
    if not args.no_browser:
        webbrowser.open(url)
    try:
        server.serve_forever()
    except KeyboardInterrupt:
        pass
    finally:
        server.server_close()


if __name__ == "__main__":
    main()
