import numpy as np
import requests
from matplotlib import pyplot as plt, ticker
import pandas as pd
import json
import argparse

def load_mixsample(url, tool):
    timeline_json = requests.get(url).json()
    outcomes = timeline_json[0]["mixsample-" + str(tool)]
    # outcomes = timeline_json[0]["mixsample"]
    outcomes = pd.DataFrame(outcomes, columns=['time', 'op', 'precision'])
    return outcomes

def plot_histogram(args):
    baseline = load_mixsample(args.timeline, "base")
    rival = load_mixsample(args.timeline, "rival")

    rival = rival[rival["op"] != 'adjust']

    fig, ax = plt.subplots(figsize=(6.5, 2.5))

    # bins = np.concatenate([np.arange(0, 1000, 73), np.arange(1000, 19001, 1000)])
    bins = 2 ** np.arange(6, 16, 1)

    buckets_base = bucket_precisions_by_bins(baseline, bins)
    buckets_rival = bucket_precisions_by_bins(rival, bins)

    ax.bar(np.arange(len(bins)) + 0.4, buckets_base, color="green", alpha=1, width=0.6, label='baseline', hatch='/')
    ax.bar(np.arange(len(bins)) + 0.6, buckets_rival, color="red", alpha=0.7, width=0.6, label='rival')
    ax.yaxis.grid(True, linestyle='-', which='major', color='grey', alpha=0.3)

    ax.set_xticks(np.arange(len(bins)), bins)
    ax.set_xticklabels(["$2^{" + str(i+6) + "}$" for i, x in enumerate(bins)])
    # sec = ax.secondary_xaxis(location=0)
    # sec.set_xticks([7, 18], labels=['\nStep = 73', '\nStep = 2000'])

    # ax.vlines(x=14, ymin=0, ymax=max(buckets_base.max(), buckets_rival.max()), colors='black', ls='--', lw=1)

    ax.margins(x=0.02)
    ax.set_ylabel("Seconds spent")
    ax.set_xlabel("Precision (number of bits)")

    plt.legend()
    plt.tight_layout()
    plt.savefig("histogram.pdf", format="pdf")


def bucket_precisions_by_bins(data, bins):
    x = [0] * len(bins)
    for i in range(len(bins) - 1):
        time_per_bucket = data.loc[(data["precision"] >= bins[i]) & (data["precision"] < bins[i + 1]), "time"].sum()
        time_fraction_per_bucket = time_per_bucket/1000
        x[i] = time_fraction_per_bucket
    return np.array(x)

parser = argparse.ArgumentParser(prog='histograms.py', description='Script outputs mixed precision histograms for a Herbie run')
parser.add_argument('-t', '--timeline-url', dest='timeline', default="https://nightly.cs.washington.edu/reports/herbie/1720270631:nightly:artem-popl-eval-histograms:6f0787f921/timeline.json")
args = parser.parse_args()
plot_histogram(args)