import argparse

import requests
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt

def plot_points_graph(outcomes, title, ax):
    series_labels = ['zero', 'real number']
    colors = ['tab:blue',  'tab:orange', 'tab:green']

    category_labels = ['Rival', 'Sollya', 'Baseline']


    data = np.zeros((len(series_labels), len(category_labels)))

    # Zeros difference
    data[0][0] = outcomes.loc[outcomes['tool_name'] == 'valid-rival+baseline-zero']['number_of_points'].sum()
    data[0][0] += outcomes.loc[outcomes['tool_name'] == 'valid-rival-only-zero']['number_of_points'].sum()

    data[0][1] = outcomes.loc[outcomes['tool_name'] == 'valid-rival+sollya-zero']['number_of_points'].sum()
    data[0][1] += outcomes.loc[outcomes['tool_name'] == 'valid-sollya-only-zero']['number_of_points'].sum()

    data[0][2] = outcomes.loc[outcomes['tool_name'] == 'valid-rival+baseline-zero']['number_of_points'].sum()
    data[0][2] += outcomes.loc[outcomes['tool_name'] == 'valid-baseline-only-zero']['number_of_points'].sum()

    # Real number difference
    data[1][0] = outcomes.loc[outcomes['tool_name'] == 'valid-rival+baseline-real']['number_of_points'].sum()
    data[1][0] += outcomes.loc[outcomes['tool_name'] == 'valid-rival-only-real']['number_of_points'].sum()

    data[1][1] = outcomes.loc[outcomes['tool_name'] == 'valid-rival+sollya-real']['number_of_points'].sum()
    data[1][1] += outcomes.loc[outcomes['tool_name'] == 'valid-sollya-only-real']['number_of_points'].sum()

    data[1][2] = outcomes.loc[outcomes['tool_name'] == 'valid-rival+baseline-real']['number_of_points'].sum()
    data[1][2] += outcomes.loc[outcomes['tool_name'] == 'valid-baseline-only-real']['number_of_points'].sum()


    # Plotting top part of the bar
    bottom = np.zeros(len(category_labels))
    for label, weight, color in zip(series_labels, data, colors):
        ax.bar(np.arange(len(category_labels)), weight, label=label, bottom=bottom, color=color)
        bottom += weight

    y_offset = 100
    print(bottom)
    for i, total in enumerate(bottom):
        ax.text(bottom[i], total + y_offset, total, ha='center',
                weight='bold')

    for bar in ax.patches:
        if bar.get_height() > 200:
            ax.text(
                bar.get_x() + bar.get_width() / 2,
                bar.get_height() + bar.get_y() - bar.get_height()/2 - 200,
                int(bar.get_height()),
                ha='center',
                color='black'
        )

    ax.set_xticks(np.arange(len(category_labels)), category_labels)
    ax.legend()
    ax.yaxis.grid(True, linestyle='-', which='major', color='grey', alpha=0.3)
    ax.set_xlabel("Tool")
    ax.set_ylabel("Number of points")


def load_outcomes(url):
    timeline_json = requests.get(url).json()
    outcomes = timeline_json[0]["outcomes"]
    outcomes = pd.DataFrame(outcomes, columns=['time', 'rival_iter', 'tool_name', 'number_of_points'])
    return outcomes

parser = argparse.ArgumentParser(prog='histograms.py', description='Script outputs mixed precision histograms for a Herbie run')
parser.add_argument('-t', '--timeline', dest='timeline', default="https://nightly.cs.washington.edu/reports/herbie/1719402193:nightly:artem-popl-eval:2cccdc887a/timeline.json")
args = parser.parse_args()

outcomes = load_outcomes(args.timeline)
fig, ax = plt.subplots(figsize=(5, 4.3))
fig.tight_layout(pad=5.0)
plot_points_graph(outcomes, "20 ms timeout", ax)
plt.savefig("point_graph.pdf", format="pdf")
