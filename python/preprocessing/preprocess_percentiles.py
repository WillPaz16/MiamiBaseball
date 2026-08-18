import pandas as pd
import numpy as np
from catboost import CatBoostClassifier
import os

# ------------------------
# Load raw 2024 pitch-by-pitch data and model
# ------------------------
data_2024 = pd.read_csv("2024TMDataWhole.csv", low_memory=False)
xBABIP_Model = CatBoostClassifier()
xBABIP_Model.load_model("xBABIP_Model.cbm")  # adjust path if needed

# ------------------------
# Define metrics
# ------------------------
# NOTE: this box (0.708 = 8.5in ball-radius-adjusted half-width) is intentionally
# different from the display strike zone used in r/reports and r/shiny — it must
# stay identical between this file and preprocess_percentiles.py, since the
# reference percentile CSVs were computed with this same definition.
def define_strike_zone(df, top=3.5, bottom=1.5, left=-0.708, right=0.708):
    df = df.copy()
    df['zone'] = (
        (df['platelocheight'] >= bottom) &
        (df['platelocheight'] <= top) &
        (df['platelocside'] >= left) &
        (df['platelocside'] <= right)
    )
    return df

def calc_strike_metrics(df):
    df = define_strike_zone(df)
    df['strike'] = df['pitchcall'].isin(['StrikeCalled', 'StrikeSwinging', 'FoulBallFieldable', 'FoulBallNotFieldable'])
    df['chase'] = df['strike'] & (~df['zone'])
    df['swing'] = df['pitchcall'].isin(['BallInPlay', 'StrikeSwinging', 'FoulBallFieldable', 'FoulBallNotFieldable'])
    df['called_strike'] = df['pitchcall'] == 'StrikeCalled'

    agg = df.groupby('pitcher').agg(
        strike_pct=('strike', 'mean'),
        zone_pct=('zone', 'mean'),
        chase_pct=('chase', 'mean'),
        swing_pct=('swing', 'mean'),
        called_strike_pct=('called_strike', 'mean')
    ).reset_index()
    return agg

def calc_miss_metrics(df):
    df = define_strike_zone(df)
    df['miss'] = df['pitchcall'] == 'StrikeSwinging'
    df['in_zone_miss'] = df['miss'] & df['zone']
    df['out_zone_miss'] = df['miss'] & (~df['zone'])

    agg = df.groupby('pitcher').agg(
        miss_pct=('miss', 'mean'),
        in_zone_miss_pct=('in_zone_miss', 'mean'),
        out_zone_miss_pct=('out_zone_miss', 'mean')
    ).reset_index()
    return agg

def calc_weak_contact(df, model):
    df = df.copy()
    hitCols = ['exitspeed', 'angle', 'hitspinrate', 'distance', 'bearing']
    df['xbabip'] = np.nan
    inplay_mask = df['pitchcall'] == 'InPlay'
    inplay_data = df.loc[inplay_mask, :]

    valid_mask = inplay_data[hitCols].notnull().all(axis=1)
    X_valid = inplay_data.loc[valid_mask, hitCols].apply(pd.to_numeric, errors='coerce')

    if len(X_valid) > 0:
        predictions = model.predict_proba(X_valid)[:, 1]
        df.loc[inplay_mask & valid_mask, 'xbabip'] = predictions

    df['gb'] = df['taggedhittype'] == 'GroundBall'

    agg = df.groupby('pitcher').agg(
        xbabip=('xbabip', 'mean'),
        gb_pct=('gb', 'mean')
    ).reset_index()
    return agg

def calc_all_metrics(df, model):
    return (
        calc_strike_metrics(df)
        .merge(calc_miss_metrics(df), on='pitcher')
        .merge(calc_weak_contact(df, model), on='pitcher')
    )

# ------------------------
# Compute overall metrics (reference)
# ------------------------
metrics = [
    'strike_pct', 'zone_pct', 'chase_pct', 'swing_pct', 'called_strike_pct',
    'miss_pct', 'in_zone_miss_pct', 'out_zone_miss_pct', 'xbabip', 'gb_pct'
]

def add_percentiles_relative(df, reference_df, metric_cols):
    df = df.copy()
    for col in metric_cols:
        ref_values = reference_df[col].dropna().values
        df[col + '_pct'] = df[col].apply(lambda x: (ref_values < x).mean() * 100 if pd.notna(x) else np.nan)
    return df

# ------------------------
# Compute metrics for full 2024 dataset
# ------------------------
overall_metrics = calc_all_metrics(data_2024, xBABIP_Model)
overall_metrics = add_percentiles_relative(overall_metrics, overall_metrics, metrics)

# LHH / RHH splits
df_lhh = data_2024[data_2024['batterside'] == 'Left']
df_rhh = data_2024[data_2024['batterside'] == 'Right']

metrics_lhh = add_percentiles_relative(calc_all_metrics(df_lhh, xBABIP_Model), overall_metrics, metrics)
metrics_rhh = add_percentiles_relative(calc_all_metrics(df_rhh, xBABIP_Model), overall_metrics, metrics)

# Pitch-type specific metrics
pitch_types = data_2024['taggedpitchtype'].dropna().unique()
pitchtype_metrics = {}
for ptype in pitch_types:
    df_pt = data_2024[data_2024['taggedpitchtype'] == ptype]
    m_pt = add_percentiles_relative(calc_all_metrics(df_pt, xBABIP_Model), overall_metrics, metrics)
    pitchtype_metrics[ptype] = m_pt

# ------------------------
# Save small CSVs for Dash
# ------------------------
os.makedirs("Processed CSVs", exist_ok=True)

overall_metrics.to_csv("Processed CSVs/pitcher_metrics_overall.csv", index=False)
metrics_lhh.to_csv("Processed CSVs/pitcher_metrics_lhh.csv", index=False)
metrics_rhh.to_csv("Processed CSVs/pitcher_metrics_rhh.csv", index=False)

for ptype, df_pt in pitchtype_metrics.items():
    safe_name = ptype.replace("/", "_").replace(" ", "_")
    df_pt.to_csv(f"Processed CSVs/pitcher_metrics_{safe_name}.csv", index=False)

print("✅ Preprocessing complete. CSVs saved in 'Processed CSVs/'")
print(f"Overall, LHH, RHH, and {len(pitch_types)} pitch-type CSVs generated.")
