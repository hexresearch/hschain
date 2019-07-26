#!/usr/bin/python
"""
"""
import functools
import glob
import json
import os
import numpy  as np
import pandas as pd

# ================================================================
# Loading logs
# ================================================================

def lazy(fun):
    "Decorator for lazy fields in class"
    return property(functools.lru_cache(maxsize=None)(fun))

keys = set(['at','ns','data','msg','sev'])

def gen_dicts(itr):
    """
    Helper function which generates list of dictionaries to be made in
    data frames
    """
    for s in itr:
        s = s.strip()
        if s == "":
            continue
        try:
            r = json.loads(s)
        except json.JSONDecodeError as e:
            print("Parse error %s" % format(e))
            print(s)
            raise e
        r = json.loads(s)
        r = {k:r[k] for k in keys}
        r['ns'] = r['ns'][1:]
        if not r['ns']:
            continue
        yield(r)


class Log(object):
    """
    Import logs as data frames. Data is read line-by-line from some
    iterable. Most likely file
    """

    def __init__(self, iterable) :
        df       = pd.DataFrame.from_records(gen_dicts(iterable), columns=keys)
        df['at'] = pd.to_datetime(df['at'])
        self.df  = df

    @lazy
    def cons(self):
        "Consensus related logs"
        df = self.df
        df = df[df['ns'].apply(lambda ns: ns==["consensus"])].drop(['ns'], axis=1)
        return df

    @lazy
    def mempool(self):
        "Raw mempool stats"
        df = self.df
        mempool = df[df['ns'].apply(lambda ns: ns == ['consensus','mempool'])].drop(['ns'], axis=1).copy()
        mempool['size']      = mempool['data'].apply(lambda x: x['size'])
        mempool['filtered']  = mempool['data'].apply(lambda x: x['filtered'])
        mempool['added']     = mempool['data'].apply(lambda x: x['added'])
        mempool['discarded'] = mempool['data'].apply(lambda x: x['discarded'])
        return mempool

    @lazy
    def commit(self):
        "Information about commit"
        r     = self.df[self.df['msg'] == "Actual commit"]
        at    = r['at']
        h     = r['data'].apply(lambda x : x['H'])
        ntx   = r['data'].apply(lambda x : x['Ntx'])
        nsign = r['data'].apply(lambda x : x['nsign'])
        return pd.DataFrame(data={'at':at,'H':h, 'Ntx':ntx, 'nsign':nsign})

    @lazy
    def round(self):
        "Extract round information from data frame"
        df = self.cons
        df = df[df['msg'] == 'Entering propose']
        H  = df['data'].apply(lambda x: x['H'])
        R  = df['data'].apply(lambda x: x['R'])
        return pd.DataFrame(data={'at':df['at'],'H':H, 'R':R})


def load_logs_files(prefix, names=None):
    dct = {}
    if names is None:
        files = glob.glob(prefix+"/*")
    else:
        files = [ prefix+"/"+nm for nm in names ]
    for nm in sorted(files):
        with open(nm) as f:
            dct[os.path.basename(nm)] = Log(f)
    return dct
