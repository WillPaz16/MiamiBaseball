# Pitcher Controllables Dashboard

A Dash web app that scores pitchers on "controllable" metrics against 2024 reference percentiles, using a trained xBABIP model.

Formerly its own repo (`miami-pitcher-controllables`); folded in here so the whole codebase lives in one place.

## Structure

- `app.py` — Dash app entrypoint (must expose `server = app.server` for gunicorn)
- `xBABIP_Model.cbm` — trained CatBoost xBABIP model
- `Processed CSVs/` — 2024 reference percentile tables by pitch type / batter handedness (generated via `../python/preprocessing/preprocess_percentiles.py`)
- `Demo CSVs/` — sample raw Trackman game files used to demo the app

## Deployment

Deployed on Render as a standalone service pointed at this subdirectory (`rootDir: controllables-app` in `render.yaml`).

**Action needed:** the Render service currently deploys from the old `miami-pitcher-controllables` repo. To switch it to this monorepo, update the service's Root Directory to `controllables-app` (or re-create it from this `render.yaml` via a Render Blueprint) and point it at the `MiamiBaseball` repo before decommissioning the old one.

## Local run

```bash
pip install -r requirements.txt
python app.py
```
