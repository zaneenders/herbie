import argparse

import requests
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt

def plot_speed_graph(outcomes, ax):
    baseline_cmp = outcomes.loc[outcomes['tool_name'] == "valid-baseline"]
    rival_cmp = outcomes.loc[outcomes['tool_name'] == "valid-rival"]
    sollya_cmp = outcomes.loc[outcomes['tool_name'] == "valid-sollya"]
    def add_values(row):
        return int(row['rival_iter']) + 1, (row['number_of_points'] / row['time']) * 1000

    def tool_cmp2speed(x):
        return x.sort_values(by=['rival_iter']).apply(add_values, axis=1, result_type='expand')

    ax.plot(tool_cmp2speed(baseline_cmp)[0][:5], tool_cmp2speed(baseline_cmp)[1][:5], '.-', linewidth=2.0, color='g',
            label='baseline')
    ax.plot(tool_cmp2speed(rival_cmp)[0][:5], tool_cmp2speed(rival_cmp)[1][:5], '.-', linewidth=2.0, color='r', label='rival')
    ax.plot(tool_cmp2speed(sollya_cmp)[0][:5], tool_cmp2speed(sollya_cmp)[1][:5], '.-', linewidth=2.0, color='b',
            label='sollya')

    ax.axhline(y=baseline_cmp['number_of_points'].sum() / baseline_cmp['time'].sum() * 1000, color='g', linestyle='--',
               linewidth=2.0, label='baseline average')
    ax.axhline(y=rival_cmp['number_of_points'].sum() / rival_cmp['time'].sum() * 1000, color='r', linestyle='--',
               linewidth=2.0, label='rival average')
    ax.axhline(y=sollya_cmp['number_of_points'].sum() / sollya_cmp['time'].sum() * 1000, color='b', linestyle='--',
               linewidth=2.0, label='sollya average')

    ax.legend()
    ax.set_xlabel("Difficulty")
    ax.set_ylabel("Evaluations per second")
    ax.yaxis.grid(True, linestyle='-', which='major', color='grey', alpha=0.3)

def load_outcomes(url):
    timeline_json = requests.get(url).json()
    outcomes = timeline_json[0]["outcomes"]
    outcomes = pd.DataFrame(outcomes, columns=['time', 'rival_iter', 'tool_name', 'number_of_points'])
    return outcomes

parser = argparse.ArgumentParser(prog='histograms.py', description='Script outputs mixed precision histograms for a Herbie run')
parser.add_argument('-t', '--timeline', dest='timeline', default="https://nightly.cs.washington.edu/reports/herbie/1719834219:nightly:artem-popl-eval:db6536e941/timeline.json")
args = parser.parse_args()

outcomes_20ms = load_outcomes(args.timeline)

fig, ax = plt.subplots(figsize=(5, 4.3))
fig.tight_layout(pad=4.0)
plot_speed_graph(outcomes_20ms, ax)
plt.savefig("speed_graph.pdf", format="pdf")