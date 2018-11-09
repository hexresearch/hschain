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

def to_commit(d) :
    r = d[d['msg'] == "Entering new height ----------------"].copy()
    r['H'] = r['data'].apply(lambda x : x['H'])
    return r.drop(['data','msg','host'], axis=1)

def to_commit_n_tx(df):
    r = df[df['msg'] == "Actual commit"]
    return pd.DataFrame({'H'  : r['data'].apply(lambda x : x['H']),
                         'Ntx': r['data'].apply(lambda x : x['Ntx']),
                        })

# ----------------------------------------------------------------

def plot_commit_time(logs) :
    "Plot commit time and prime average block time"
    # Subtract leading time
    dfs = {k:to_commit(d) for k,d in logs.items()}
    #
    t0  = np.min([df['at'].values[0] for df in dfs.values() if len(df) > 0])
    for k, df in dfs.items() :
        df['at'] = (df['at'] - t0).astype('timedelta64[s]')
    # Fit averaged commit times with straign line
    #
    # FIXME: we need to join over H
    #        This is crucial when some node is stuck
#    n  = np.min([df['H'].shape[0] for df in dfs.values()])
#    hs = list(dfs.values())[0]['H'][0:n]
#    ts = np.average( [df['at'][0:n] for df in dfs.values()], axis=0 )
#    r  = sm.OLS(ts, sm.add_constant(hs), missing='drop').fit()
    # Plotting
    fontP = FontProperties()
    fontP.set_size('small')
    # Height plot
    figA = plt.figure()
    axA  = plt.subplot(111)
    boxA = axA.get_position()
    axA.set_position([boxA.x0, boxA.y0, boxA.width * 0.8, boxA.height])
    axA.grid()
    plt.title('Commit time')
    plt.xlabel('Time (s)')
    plt.ylabel('Height')
#    axA.plot(hs*r.params[1] + r.params[0], hs, color='grey', linewidth=0.5)
    for k,df in dfs.items() :
        axA.plot(df['at'] , df['H'], '+', label=k)
    axA.legend(loc='center left', bbox_to_anchor=(1, 0.5), prop=fontP)
#    print("Time for commit of singe block %.3f s" % float(r.params[1]))
    # Residuals plot
    # figB = plt.figure()
    # axB  = plt.subplot(111)
    # boxB = axB.get_position()
    # axB.set_position([boxB.x0, boxB.y0, boxB.width * 0.8, boxB.height])
    # axB.grid()
    # plt.title('Commit time residuals')
    # plt.xlabel('Time (s)')
    # plt.ylabel('Delta H')
    # for k,df in dfs.items() :
    #     axB.plot(df['at'] , df['H'] - (df['at'] - r.params[0]) / r.params[1], label=k)
    # axB.legend(loc='center left', bbox_to_anchor=(1, 0.5), prop=fontP)
    return [figA]

def plot_n_tx_in_block(logs):
    "Plot number of transactions in block for every height"
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
