#!/usr/bin/python
"""
"""

# ----
import json
import tempfile
import subprocess
import matplotlib.pyplot as plt
from   matplotlib.font_manager import FontProperties
# ----
import numpy as np
import pandas as pd
import statsmodels.api as sm

# ----------------------------------------------------------------
# Extracting data from full data frame
# ----------------------------------------------------------------

def to_commit(d) :
    r = d[d['msg'] == "Actual commit"].copy()
    r['H'] = r['data'].apply(lambda x : x['H'])
    return r.drop(['data','msg','host'], axis=1)

def to_commit_n_tx(df):
    r = df[df['msg'] == "Actual commit"]
    return pd.DataFrame({'H'  : r['data'].apply(lambda x : x['H']),
                         'Ntx': r['data'].apply(lambda x : x['Ntx']),
                        })

def extract_round(df) :
    """
    Extract round information from data frame
    """
    df = df[df['msg'] == 'Entering propose'].reset_index(drop=True)
    df['H'] = df['data'].apply(lambda x: x['H'])
    df['R'] = df['data'].apply(lambda x: x['R'])
    return df.drop(['data', 'host'], axis=1)

def extract_commit(df) :
    """
    Extract commit information from data frame
    """
    df = df[df['msg'] == 'Actual commit'].reset_index(drop=True)
    df['H'] = df['data'].apply(lambda x: x['H'])
    return df.drop(['data', 'host'], axis=1)


# ----------------------------------------------------------------
# Plotting routines
# ----------------------------------------------------------------

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

def plot_commit_time(logs) :
    "Plot commit time and prime average block time"
    # Subtract leading time
    dfs = {k : to_commit(d) for k,d in logs.items()}
    #
    t0  = np.min([df['at'].values[0] for df in dfs.values() if len(df) > 0])
    for k, df in dfs.items() :
        df['at'] = (df['at'] - t0).astype('timedelta64[s]')
    # Height plot
    fig,ax = figure_with_legend()
    plt.title('Commit time')
    plt.grid()
    plt.xlabel('Time (s)')
    plt.ylabel('Height')
    for k,df in dfs.items() :
        ax.plot(df['at'] , df['H'], '+', label=k)
    add_legend(ax)
    return fig

def plot_n_tx_in_block(logs):
    """
    Plot number of transactions in block for every height
    """
    dfs = [to_commit_n_tx(d) for d in logs.values()]
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
        plt.plot(df['H'], df['Ntx'],'+')
    print( "Total transactions commited: ", tot )
    print( "Transactions per block:      ", avg )
    return fig

def plot_round(logs):
    """
    Plot round growth
    """
    dfs = {k : extract_round(v) for k,v in logs.items()}
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


def plot_mempool_size(dfs):
    "Plot mempool size over time"
    fig = plt.figure()
    plt.grid()
    plt.title("Mempool size")    
    colors = plt.rcParams['axes.prop_cycle'].by_key()['color']
    def pickColor(i):
        return colors[i % len(colors)]
    for i,df in enumerate(dfs) :
        dB = df[df['msg'] == "Mempool before filtering"]
        dA = df[df['msg'] == "Mempool after filtering"]
        plt.plot(dB['at'], dB['size'], '+', color=pickColor(i), ms=3)
        plt.plot(dA['at'], dA['size'], 'x', color=pickColor(i), ms=3)
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
