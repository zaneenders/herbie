import numpy as np
import requests
from matplotlib import pyplot as plt
import pandas as pd
import json
import argparse

def load_mixsample(url, tool):
    timeline_json = requests.get(url).json()
    # outcomes = timeline_json[0]["mixsample-" + str(tool)]
    outcomes = timeline_json[0]["mixsample"]
    outcomes = pd.DataFrame(outcomes, columns=['time', 'op', 'precision'])
    return outcomes

def plot_histograms(args):
    baseline = load_mixsample(args.timeline, "base")
    rival = load_mixsample(args.timeline, "rival")

    fig, ax = plt.subplots(len(args.ops), 1, figsize=(5.5, 2*len(args.ops)))
    for i, op in enumerate(args.ops):
        plot_histogram_for_operation(op_name=op, ax=ax[i], rival=rival, baseline=baseline)

    handles, labels = ax[0].get_legend_handles_labels()
    fig.legend(handles, labels)
    plt.tight_layout()
    plt.show()


def bucket_precisions_by_bins(data, bins):
    x = [0] * len(bins)
    for i in range(len(bins) - 1):
        time_per_bucket = data.loc[(data["precision"] >= bins[i]) & (data["precision"] < bins[i + 1]), "time"].sum()
        time_fraction_per_bucket = time_per_bucket/1000
        x[i] = time_fraction_per_bucket
    return np.array(x)


def plot_histogram_for_operation(op_name="ival-sin", ax=None, rival=None, baseline=None):
    bins = np.arange(0, 10001, 500)

    data_base = baseline.loc[baseline['op'] == op_name]
    buckets_base = bucket_precisions_by_bins(data_base, bins)
    data_rival = rival.loc[rival['op'] == op_name]
    buckets_rival = bucket_precisions_by_bins(data_rival, bins)

    ax.bar(np.arange(len(bins)) + 0.4, buckets_base, color="green", alpha=1, width=0.6, label='baseline')
    ax.bar(np.arange(len(bins)) + 0.6, buckets_rival, color="red", alpha=0.7, width=0.6, label='rival')
    ax.yaxis.grid(True, linestyle='-', which='major', color='grey', alpha=0.3)

    ax.set_xticks(np.arange(len(bins)), bins)
    ax.set_xticklabels([str(x) if x % 1000 == 0 else "" for x in bins])

    ax.margins(x=0.02)
    ax.set_ylabel("Seconds spent")
    ax.set_xlabel("Precision (number of bits)")
    ax.set_title("Precision tuning for " + op_name)

parser = argparse.ArgumentParser(
    prog='histograms.py',
    description='Script outputs mixed precision histograms for a Herbie run')
parser.add_argument('-t', '--timeline-url', dest='timeline', default="https://nightly.cs.washington.edu/reports/herbie/1719410524:nightly:main:aefdd770dd/timeline.json")
parser.add_argument('-o', '--ops', action='store', dest='ops',
                    type=str, nargs='*', default=['ival-sin', 'ival-tan', 'ival-cos', 'ival-pow'],
                    help="Examples: -o ival-sin ival-sub ival-add")
args = parser.parse_args()
plot_histograms(args)