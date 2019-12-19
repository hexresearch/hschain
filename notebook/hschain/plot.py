#!/usr/bin/python
"""
"""

# ----
import json
import tempfile
import itertools
import subprocess
import matplotlib.pyplot as plt
import scipy.stats       as stats
from   matplotlib.font_manager import FontProperties
# ----
import numpy as np
import pandas as pd
import statsmodels.api as sm


## ----------------------------------------------------------------
## Commit time related plots
## ----------------------------------------------------------------

class CommitData(object):
    "Prepare commit data into ready to plot form"
    def __init__(self, logs):
        # Clean data
        dfs = {k : d.commit for k,d in logs.items() if not d.commit.empty}
        # Get summary stats
        pts = pd.concat([d[['H','at']] for _,d in dfs.items()])
        t0  = np.min(pts['at'])
        ts  = (pts['at'].dt.tz_localize(None) - t0).astype('timedelta64') / 1e9
        hs  = pts['H']
        # Calculate relative time
        for k, df in dfs.items() :
            df['dt'] = (df['at'].dt.tz_localize(None) - t0).astype('timedelta64') / 1e9
        self.fit = logs.fitTvsH
        self.TPS = logs.TPS
        # Store precalculated data
        self.tMin = np.min(ts)
        self.tMax = np.max(ts)
        self.hMin = np.min(hs)
        self.hMax = np.max(hs)
        self.dfs  = dfs

    def plot_points(self, ax, reltime=False):
        "Simply plot points"
        for k,df in self.dfs.items() :
            xs = df['dt' if reltime else 'at']
            ys = df['H']
            ax.plot(xs, ys, '+', label=k)
        self.add_title_h("Height vs time")
        p  = self.fit.params
        hs = np.asarray([ np.min(df['H']), np.max(df['H'])])
        ax.plot( hs * p[1] + p[0], hs, '-', color='gray', lw=0.5)
        plt.xlabel("time")
        plt.ylabel("H")

    def plot_residuals_HvsT(self, ax):
        for k,df in self.dfs.items() :
            p  = self.fit.params
            xs = df['dt']
            ys = df['H'] - (xs / p[1] - p[0]/p[1])
            ax.plot(xs, ys, '+', label=k)
        self.add_title_h("Height residuals")
        plt.xlabel("time")
        plt.ylabel("ΔH")

    def plot_residuals_TvsH(self, ax):
        for k,df in self.dfs.items() :
            p  = self.fit.params
            xs = df['H']
            ys = df['dt'] - (xs * p[1] + p[0])
            ax.plot(xs, ys, '+', label=k)
        self.add_title_h("Time residuals")
        plt.xlabel("H")
        plt.ylabel("Δt")

    def plot_ntx(self,ax):
        for k in self.dfs:
            df = self.dfs[k]
            break
        tot = np.sum(df['Ntx'])
        avg = np.average(df['Ntx'])
        plt.title("Block size (μ=%.2f, tot=%i)" % (avg,tot))
        plt.xlabel("Height")
        plt.ylabel("N of transactions")
        plt.axhline(y=0,   color='k')
        plt.axhline(y=avg, color='k')
        plt.plot(df['H'], df['Ntx'],'+')

    def plot_ntx_distr(self,ax):
        for k in self.dfs:
            df = self.dfs[k]
            break
        ntx = df['Ntx']
        mu  = np.average(ntx)
        sig = np.std(ntx)
        n1  = np.min(ntx)
        n2  = np.max(ntx)
        #
        plt.title("Block size (μ=%.2f)" % (mu))
        plt.xlabel("N of transactions")
        plt.ylabel("prob. density")
        #
        plt.hist(ntx, n2-n1+1, density=True)
        x   = np.linspace(n1, n2, 200)
        plt.plot(x, stats.norm.pdf(x, mu, sig), color='r')
        plt.axvline(mu, color='r')

    def plot_n_signatures(self,ax):
        for k in self.dfs:
            df = self.dfs[k]
            break
        df = df[df['H']>1]
        avg = np.average(df['nsign'])
        plt.title("N signatures for block (avg = %.2f)" % avg)
        plt.plot(df['H'] - 1, df['nsign'],'+')

    def add_title_h(self,s ):
        plt.title("%s (%.03f s/block, %.f tps)" %
                  (s,
                   float(self.fit.params[1]),
                   self.TPS
                  ))

# ----------------------------------------------------------------
# Plotting routines
# ----------------------------------------------------------------

class SimplePlot(object):
    "Simple plotter"
    def __init__(self):
        self.fig = plt.figure()
        self.ax  = plt.subplot(111)
        plt.grid()
    def __enter__(self):
        return self.ax

    def __exit__(self, type, value, trace):
        pass

class LegendPlot(object):
    "Plotter with legend"
    def __init__(self):
        fig,ax = figure_with_legend()
        self.fig = fig
        self.ax  = ax
        plt.grid()
    def __enter__(self):
        return self.ax

    def __exit__(self, type, value, trace):
        add_legend(self.ax)


def figure_with_legend():
    fig = plt.figure(figsize=[9, 4.8])
    ax  = plt.subplot(111)
    box = ax.get_position()
    ax.set_position([box.x0, box.y0, box.width * 0.8, box.height])
    return fig,ax

def add_legend(ax) :
    fontP = FontProperties()
    fontP.set_size('small')
    ax.legend(loc='center left', bbox_to_anchor=(1, 0.5), prop=fontP)


# ----------------------------------------------------------------
# Plotting routines
# ----------------------------------------------------------------

def plot_round(logs):
    """
    Plot round growth
    """
    dfs = {k : v.round for k,v in logs.items() if not v.round.empty}
    #
    fig,ax = figure_with_legend()
    plt.title("Round vs T")
    plt.grid()
    plt.xlabel("Time")
    plt.ylabel("Round")
    for k,v in dfs.items():
        plt.plot(v['at'], v['R'], lw=0.5, marker='x', markersize=2, label=k)
    add_legend(ax)
    return fig

def plot_round_distr(logs):
    "Plot distribution of round numbers per figure"
    dfs = {k : v.roundDistr for k,v in logs.items() if not v.round.empty}
    n   = len(dfs)
    fig,ax = figure_with_legend()
    for i,(nm,rs) in enumerate(dfs.items()):
        xs = np.asarray(range(len(rs)))
        plt.bar(xs + i / n, rs, width=1/n, align='edge', label=nm)
        print("%12s: <R> = %.2f" % (nm, np.average(xs, weights=rs)))
    add_legend(ax)
    return fig

def plot_mempool_size(dfs):
    "Plot mempool size over time"
    fig,ax = figure_with_legend()
    plt.grid()
    plt.title("Mempool size")
    colors = plt.rcParams['axes.prop_cycle'].by_key()['color']
    def pickColor(i):
        return colors[i % len(colors)]
    for i,k in enumerate(dfs) :
        df = dfs[k].mempool
        dB = df[df['msg'] == "Mempool before filtering"]
        dA = df[df['msg'] == "Mempool after filtering"]
        plt.plot(dB['at'], dB['size'], '+', color=pickColor(i), ms=3, label=k+' before')
        plt.plot(dA['at'], dA['size'], 'x', color=pickColor(i), ms=3, label=k+' after')
    add_legend(ax)
    return fig


def plot_mempool_added(dfs):
    "Plot N of tx added to mempool over time"
    fig = plt.figure()
    plt.grid()
    plt.title("Number of transaction added to mempool")
    colors = plt.rcParams['axes.prop_cycle'].by_key()['color']
    for i,df in enumerate(dfs) :
        df = dfs[df].mempool
        d  = df[df['msg'] == "Mempool after filtering"]
        plt.plot(d['at'], d['added'], '+', color=colors[i], ms=3)
        plt.plot(d['at'], d['discarded'], 'v', color=colors[i], ms=3)
        plt.plot(d['at'], d['filtered'], 'x', color=colors[i], ms=3)
    return fig

def plot_gossip(logs, key):
    """
    Plot statistics about gossip"
    """
    fig,ax = figure_with_legend()
    plt.grid()
    plt.title("Gossip statistics for: "+key)
    for i,(k,d) in enumerate(logs.items()):
        tx = d[key]
        tx = tx - tx.values[0]
        ax.plot(d['at'], tx, '+', label=k, markersize=1.5)
    add_legend(ax)
    return fig

def plot_gossip_rxtx_ratio(dfs, key):
    "Plot statistics about gossip"
    dfs    = [d.gossip() for d in dfs]
    fig    = plt.figure()
    plt.grid()
    plt.title("Rx/Tx ratio for: "+key)
    for d in dfs:
        plt.plot(d['at'], d['Rx' + key]/d['Tx'+key], '+')
    return fig
