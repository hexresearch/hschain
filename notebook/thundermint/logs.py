#!/usr/bin/python
"""
"""
import functools
import glob
import json
import os
import enum
import numpy  as np
import pandas as pd

# ================================================================
# Loading logs
# ================================================================

def lazy(fun):
    "Decorator for lazy fields in class"
    return property(functools.lru_cache(maxsize=None)(fun))

keys = set(['at','ns','data','msg','sev'])
# Log keys

@enum.unique
class Step(enum.Enum):
    "Steps of algorithm"
    NewH      = "Entering new height ----------------"
    Propose   = "Entering propose"
    Prevote   = "Entering prevote"
    Precommit = "Entering precommit"
    Commit    = "Decision to commit"
    Done      = "Actual commit"

keyOrdSet = [k.value for k in Step]
keySet    = frozenset(keyOrdSet)


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
    def consClean(self):
        "Cleaned up consensus related logs"
        df = self.cons.copy()
        df['H'] = df['data'].apply(lambda x: x.get('H'))
        df['R'] = df['data'].apply(lambda x: x.get('R'))
        df      = df[df['msg'].isin(keySet)]
        return df

    @functools.lru_cache(maxsize=32)
    def consStep(self, step):
        """
        Only returns entries for individuals steps (only R=0 is
        returned where applicable
        """
        df = self.consClean
        df = df[df['msg'] == step.value]
        if step in [Step.Propose, Step.Prevote, Step.Precommit] :
            df = df[df['R'] == 0]
        return pd.DataFrame(index=df['H'].values, data={'at':df['at'].values })

    @lazy
    def stepsTime(self):
        "Compute duration of steps"
        df = self.cons.copy()
        df['H']  = df['data'].apply(lambda x: x.get('H'))
        df       = df[df['H']>0]
        df       = df[df['msg'].isin(keySet)]
        deltaT   = (df['at'].values[1:] - df['at'].values[:-1]).astype('timedelta64[ms]')
        df       = df[:-1]
        df['dt'] = deltaT.copy()
        return {k:v for k,v in df.groupby(['msg'])}

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
    def mempoolStat(self):
        "Cleaned up mempool stats"
        df   = self.mempool
        dfA  = df[df['msg'] == 'Mempool after filtering']
        dfB  = df[df['msg'] == 'Mempool before filtering']
        datA = dfA['data'].apply(lambda x: x['filtered']).values
        datB = dfB['data'].apply(lambda x: x['filtered']).values
        #
        fltT = (dfA['at'].values - dfB['at'].values).astype('timedelta64[ms]').astype(float)
        return pd.DataFrame(data={
            'fltT'    : fltT,
            'filtered': datA-datB,
            'sizeB'   : dfB['data'].apply(lambda x: x['size']).values,
            'sizeA'   : dfA['data'].apply(lambda x: x['size']).values,
        })

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

    @lazy
    def gossip(self):
        df = self.df
        df = df[df['msg'] == 'Gossip stats']
        r = pd.DataFrame.from_records(
            df['data'].values,
            columns=['TxP','RxPV','RxP','TxPV','TxTx','TxB',
                     'TxPC','RxPex','RxB','RxTx','RxPC','TxPex'],)
        at = df['at']
        if len(at) > 0:
            at = (at.values - at.values[0]).astype('timedelta64[ms]').astype('float')
            at /= 1000
        r['dt'] = at
        return r


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
