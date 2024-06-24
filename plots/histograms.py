import numpy as np
from matplotlib import pyplot as plt
from matplotlib.ticker import MultipleLocator, ScalarFormatter
import pandas as pd
import json
import argparse

def plot_histograms(args):
    baseline_json = json.load(open(args.baseline))
    baseline = pd.DataFrame(baseline_json, columns=['time', 'op', 'precision'])

    rival_json = json.load(open(args.rival))
    rival = pd.DataFrame(rival_json, columns=['time', 'op', 'precision'])

    fig, ax = plt.subplots(3, 1, figsize=(5.5, 6))
    plot_histogram_for_operation(op_name="ival-sin", ax=ax[0], rival=rival, baseline=baseline)
    plot_histogram_for_operation(op_name="ival-add", ax=ax[1], rival=rival, baseline=baseline)
    plot_histogram_for_operation(op_name="ival-sub", ax=ax[2], rival=rival, baseline=baseline)

    handles, labels = ax[0].get_legend_handles_labels()
    fig.legend(handles, labels)
    plt.tight_layout()
    plt.show()

def bucket_precisions_by_bins(data, bins):
    total_time = data["time"].sum()
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

    ax.set_xticks(np.arange(len(bins)), bins)
    ax.set_xticklabels([str(x) if x % 1000 == 0 else "" for x in bins])

    ax.margins(x=0.02)
    ax.set_ylabel("Seconds spent")
    ax.set_xlabel("Precision (number of bits)")
    ax.set_title("Precision tuning for " + op_name)


parser = argparse.ArgumentParser(
                    prog='histograms.py',
                    description='Script outputs mixed precision histograms for a Herbie run')
parser.add_argument('-c', '--rival', default="rival.json")
parser.add_argument('-v', '--baseline', default="baseline.json")
args = parser.parse_args()
plot_histograms(args)