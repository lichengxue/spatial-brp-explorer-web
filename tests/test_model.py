"""A few regression checks for the GUI model. Uses Python's standard library."""
from pathlib import Path
import importlib.util
import math
import subprocess
import tempfile
import unittest

ROOT = Path(__file__).resolve().parents[1]
spec = importlib.util.spec_from_file_location("brp_app", ROOT / "app.py")
app = importlib.util.module_from_spec(spec)
spec.loader.exec_module(app)


class ModelTest(unittest.TestCase):
    def run_model(self, **changes):
        settings = dict(years=10, historical_years=100)
        settings.update(changes)
        settings = app.validate(settings)
        with tempfile.TemporaryDirectory(prefix="spatial_brp_") as directory:
            folder = Path(directory)
            app.write_configuration(folder / "settings.csv", settings)
            process = subprocess.run([app.find_r(), "--vanilla", str(ROOT / "model/run_model.R"),
                str(folder / "settings.csv"), str(folder)], cwd=ROOT,
                stdout=subprocess.PIPE, stderr=subprocess.STDOUT, text=True, timeout=180)
            self.assertEqual(process.returncode, 0, process.stdout)
            return {name: app.read_csv(folder / (name + ".csv"))
                    for name in ("brps", "trajectories", "conditioning", "surveys", "benchmarks", "msy")
                    if (folder / (name + ".csv")).exists()}

    def test_default_spr_and_surveys(self):
        result = self.run_model()
        rows = {row["rule"]: row for row in result["brps"]}
        self.assertEqual(len(rows), 9)
        self.assertTrue(all(row["status"] != "unavailable" for row in rows.values()))
        # Known F values with the default biology.
        self.assertAlmostEqual(rows["SpE-RecW"]["F1"], 0.386809466745056, places=8)
        self.assertAlmostEqual(rows["Reg-Move"]["F2"], 0.249285960208459, places=8)
        for field in ("F1", "F2"):
            self.assertAlmostEqual(rows["Pop-Move"][field], rows["Reg-Move"][field], places=8)
            self.assertAlmostEqual(rows["Natal-RecW"][field], rows["SpE-RecW"][field], places=8)
        for field in ("pop_spr1", "pop_spr2", "regional_spr1", "regional_spr2", "global_spr_rec"):
            self.assertAlmostEqual(rows["Reg-Move"][field], .4, places=8)
        self.assertEqual(len(result["trajectories"]), 90)
        self.assertEqual(len(result["surveys"]), 8)
        indices = [sum(s["index"] for s in result["surveys"] if s["region"] == r) / 4 for r in (1, 2)]
        for r in (1, 2):
            times = [s["time"] for s in result["surveys"] if s["region"] == r]
            self.assertEqual(times, [.125, .375, .625, .875])
            self.assertAlmostEqual(result["conditioning"][r-1]["survey_weight"], indices[r-1] / sum(indices))

    def test_feedback_and_yield_projections(self):
        result = self.run_model(structure="MPO", recruitment="sr", projection="feedback",
            rules=["SpE-RecW", "Reg-Move"], historical_F=.8,
            regional_weighting="custom", regional_share1=25,
            stochastic=True, sigmaR=.2, seed=2026,
            benchmarks=["Yield-SPR", "Yield-Max"], msy=["MSY-Pattern"])
        rows = {r["rule"]: r for r in result["brps"] + result["benchmarks"] + result["msy"]}
        self.assertTrue(all(r["status"] != "unavailable" for r in rows.values()), rows)
        for r in rows.values():
            self.assertAlmostEqual(r["regional_weight1"], .25)
            curve = [t for t in result["trajectories"] if t["rule"] == r["rule"]]
            self.assertEqual(len(curve), 10)
            for field in ("F1", "F2"):
                if r["status"] == "benchmark":
                    self.assertTrue(all(math.isclose(t[field], r[field]) for t in curve))
                else:
                    self.assertTrue(all(0 <= t[field] <= r[field] + 1e-10 for t in curve))
        reg = rows["Reg-Move"]
        self.assertAlmostEqual(reg["regional_spr1"], .4, places=8)
        self.assertAlmostEqual(reg["regional_spr2"], .4, places=8)
        protected = rows["Yield-SPR"]
        self.assertGreaterEqual(min(protected["regional_spr1"], protected["regional_spr2"]), .4 - 1e-7)
        self.assertGreaterEqual(rows["Yield-Max"]["eq_catch1"] + rows["Yield-Max"]["eq_catch2"],
                                protected["eq_catch1"] + protected["eq_catch2"] - 1e-6)
        self.assertTrue(any(t["F1"] < rows[t["rule"]]["F1"] - 1e-8 for t in result["trajectories"]
                            if t["rule"] in ("SpE-RecW", "Reg-Move")))

    def test_invalid_settings(self):
        for settings in (dict(target=0), dict(target=float("nan")), dict(surveys=["1-1"]),
                         dict(regional_share1=0), dict(rules=[])):
            with self.subTest(settings=settings), self.assertRaises(ValueError):
                app.validate(settings)


if __name__ == "__main__":
    unittest.main()
