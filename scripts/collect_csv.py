import pandas as pd
import glob
import sys

def collect_tps_stat(folder):
    tps_files = glob.glob(folder + '/*-tps.csv')
    tpss = []
    for tfile in tps_files:
        tpss.append(pd.read_csv(tfile))
        
    for i in range(0, len(tpss)):
        tpss[i] = tpss[i].rename(columns={"tps": "tps_node{}".format(i)})
    
    all_tps = tpss[0]
    for i in range(1, len(tpss)):
        all_tps = all_tps.merge(tpss[i], on='time')
    
    tps_cols = ["tps_node{}".format(i) for i in range(0, len(tpss))]
    all_tps['tps_avg'] = all_tps[tps_cols].mean(axis=1)
    
    all_tps['time'] = pd.to_datetime(all_tps['time'])
    return all_tps

def strip_zeros(df):
    return df[df['tps_avg'] > 0]

all_tps = collect_tps_stat('.')
all_tps = strip_zeros(all_tps).sort_values('time')
all_tps.to_csv('measurements.csv', index=False)
