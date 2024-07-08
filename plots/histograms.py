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

    adjust_time = round(rival[rival["op"] == 'adjust']['time'].sum()/1000, 2)
    rival = rival[rival["op"] != 'adjust']

    print("\\newcommand{\TuningTime}{" + str(adjust_time) + "\\xspace}")
    print("\\newcommand{\TuningTimePercentage}{" + str(round(adjust_time/rival['time'].sum()*100, 4)) + "\\xspace}")

    fig, ax = plt.subplots(figsize=(6.5, 2.5))

    bins = 2 ** np.arange(6, 17, 1)

    buckets_base = bucket_precisions_by_bins(baseline, bins)
    buckets_rival = bucket_precisions_by_bins(rival, bins)

    ax.bar(np.arange(len(bins)) + 0.4, buckets_base, color="green", alpha=1, width=0.6, label='baseline', hatch='/')
    ax.bar(np.arange(len(bins)) + 0.6, buckets_rival, color="red", alpha=0.7, width=0.6, label='rival')

    temp = np.zeros_like(buckets_rival)
    temp[-1] = adjust_time
    ax.bar(np.arange(len(bins)), temp, color="red", alpha=0.7, width=0.6)
    ax.yaxis.grid(True, linestyle='-', which='major', color='grey', alpha=0.3)

    ax.set_xticks(np.arange(len(bins)), bins)
    ax.set_xticklabels(["$2^{" + str(i+6) + "}$" if i != 10 else "tuning\noverheads" for i, x in enumerate(bins)])

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
parser.add_argument('-t', '--timeline-url', dest='timeline', default="https://nightly.cs.washington.edu/reports/herbie/1720478924:nightly:artem-popl-eval-histograms:e2004ab7a9/timeline.json")
args = parser.parse_args()
plot_histogram(args)