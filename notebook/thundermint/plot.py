#!/usr/bin/python
"""
"""

# ----
import json
import tempfile
import subprocess
import matplotlib.pyplot as plt
# ----
import numpy as np
import pandas as pd
import statsmodels.api as sm



def plot_commit_time(logs) :
    "Plot commit time and prime average block time"
    # Subtract leading time
    dfs = [d for d in logs]
    #
    t0  = np.min([df['at'].values[0] for df in dfs])
    for df in dfs :
        df['at'] = (df['at'] - t0).astype('timedelta64[s]')
    # Fit averaged commit times with straign line
    n  = np.min([df['H'].shape[0] for df in dfs])
    hs = dfs[0]['H'][0:n]
    ts = np.average( [df['at'][0:n] for df in dfs], axis=0 )
    r  = sm.OLS(ts, sm.add_constant(hs), missing='drop').fit()
    # Do plot
    figA = plt.figure()
    plt.grid()
    plt.title('Commit time')
    plt.xlabel('Time (s)')
    plt.ylabel('Height')
    plt.plot(hs*r.params[1] + r.params[0], hs, color='grey', linewidth=0.5)
    for df in dfs :
        plt.plot(df['at'] , df['H'], '+')
    print("Time for commit of singe block %.3f s" % float(r.params[1]))
    #
    figB = plt.figure()
    plt.grid()
    plt.title('Commit time residuals')
    plt.xlabel('Time (s)')
    plt.ylabel('Delta H')
    for df in dfs :
        plt.plot(df['at'] , df['H'] - (df['at'] - r.params[0]) / r.params[1])
    return [figA,figB]


def plot_n_tx_in_block(logs):
    "Plot number of transactions in block for every height"
    dfs = [d for d in logs]
    tot = np.sum(dfs[0]['Ntx'])
    avg = np.average(dfs[0]['Ntx'])
    #
    fig = plt.figure()
    plt.grid()
    plt.title("Block size")
    plt.xlabel("Height")
    plt.ylabel("N of transactions")
    plt.axhline(y=0,   color='k')
    plt.axhline(y=avg, color='k')
    for df in dfs:
        plt.plot(df['H'], df['Ntx'])
    print( "Total transactions commited: ", tot )
    print( "Transactions per block:      ", avg )
    return fig

def plot_mempool_size(dfs):
    "Plot mempool size over time"
    fig = plt.figure()
    plt.grid()
    plt.title("Mempool size")
    colors = plt.rcParams['axes.prop_cycle'].by_key()['color']
    for i,df in enumerate(dfs) :
        dB = df[df['msg'] == "Mempool before filtering"]
        dA = df[df['msg'] == "Mempool after filtering"]
        plt.plot(dB['at'], dB['size'], '+', color=colors[i], ms=3)
        plt.plot(dA['at'], dA['size'], 'x', color=colors[i], ms=3)
    return fig

def plot_mempool_added(dfs):
    "Plot N of tx added to mempool over time"
    fig = plt.figure()
    plt.grid()
    plt.title("Number of transaction added to mempool")
    colors = plt.rcParams['axes.prop_cycle'].by_key()['color']
    for i,df in enumerate(dfs) :
        df = df.mempool
        d  = df[df['msg'] == "Mempool after filtering"]
        plt.plot(d['at'], d['added'], '+', color=colors[i], ms=3)
        plt.plot(d['at'], d['discarded'], 'v', color=colors[i], ms=3)
        plt.plot(d['at'], d['filtered'], 'x', color=colors[i], ms=3)
    return fig

def plot_gossip(dfs, key):
    "Plot statistics about gossip"
    dfs    = [d.gossip() for d in dfs]
    fig    = plt.figure()
    colors = plt.rcParams['axes.prop_cycle'].by_key()['color']
    plt.grid()
    plt.title("Gossip statistics for: "+key)
    for i,d in enumerate(dfs):
        plt.plot(d['at'], d['Rx' + key], '+', color=colors[i])
        plt.plot(d['at'], d['Tx' + key], 'x', color=colors[i])
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
