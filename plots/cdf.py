import numpy as np
import requests
import matplotlib.pyplot as plt
from matplotlib.ticker import *
import pandas as pd
import json
import argparse

def load_mixsample(url, tool):
    timeline_json = requests.get(url).json()
    outcomes = timeline_json[0]["mixsample-" + str(tool)]
    outcomes = pd.DataFrame(outcomes, columns=['time', 'op', 'precision'])
    return outcomes


def plot_cdf(args):
    baseline = load_mixsample(args.timeline, "base").sort_values(by=['precision']).groupby(by='precision').sum().cumsum().reset_index()
    rival = load_mixsample(args.timeline, "rival").sort_values(by=['precision']).groupby(by='precision').sum().cumsum().reset_index()

    fig, ax = plt.subplots(figsize=(4, 3.5))
    ax.plot(baseline['precision'], np.array(baseline['time'])/1000, linewidth=2, color='g', label='baseline')
    ax.plot(rival['precision'], np.array(rival['time'])/1000, linewidth=2, color='red', label='rival')
    ax.set_xscale('log', base=2)
    ax.yaxis.grid(True, linestyle='-', which='major', color='grey', alpha=0.3)
    ax.set_xlabel("Precision (number of bits)")
    ax.set_ylabel("Seconds spent")
    plt.tight_layout()
    plt.legend()
    plt.savefig("cdf.pdf", format="pdf")


parser = argparse.ArgumentParser(prog='histograms.py', description='Script outputs mixed precision histograms for a Herbie run')
parser.add_argument('-t', '--timeline-url', dest='timeline', default="https://nightly.cs.washington.edu/reports/herbie/1720270631:nightly:artem-popl-eval-histograms:6f0787f921/timeline.json")
args = parser.parse_args()
plot_cdf(args)
