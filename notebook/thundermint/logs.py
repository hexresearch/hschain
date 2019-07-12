#!/usr/bin/python
"""
"""

import json
import numpy  as np
import pandas as pd

# ================================================================
# Loading logs
# ================================================================

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
        """
        """
        # Load data
        df       = pd.DataFrame.from_records(gen_dicts(iterable), columns=keys)
        df['at'] = pd.to_datetime(df['at'])
        #
        self.df      = df
        # self.net     = df[df['ns'] == 'net'].drop(['ns'], axis=1)
        self.cons = df[df['ns'].apply(lambda ns: ns==["consensus"])].drop(['ns'], axis=1)
        self.mempool = df[df['ns'].apply(lambda ns: ns == ['consensus','mempool'])].drop(['ns'], axis=1).copy()
        self.mempool['size']      = self.mempool['data'].apply(lambda x: x['size'])
        self.mempool['filtered']  = self.mempool['data'].apply(lambda x: x['filtered'])
        self.mempool['added']     = self.mempool['data'].apply(lambda x: x['added'])
        self.mempool['discarded'] = self.mempool['data'].apply(lambda x: x['discarded'])

    def commit_times(self):
        df      = self.cons[self.cons['msg'] == "Entering new height ----------------"].copy()
        df['H'] = df['data'].apply(lambda x : x['H'])
        return df.drop(['data','sev','msg'], axis=1)

    def commit_n_tx(self):
        df = self.cons[self.cons['msg'] == "Actual commit"]
        return pd.DataFrame({'H'  : df['data'].apply(lambda x : x['H']),
                             'Ntx': df['data'].apply(lambda x : x['Ntx']),
                             })

    def gossip(self) :
        net = self.net.copy()
        net = net[net['msg'] == 'Gossip stats'].reset_index()
        df  = pd.DataFrame.from_records(net['data'].values)
        df['at'] = net['at']
        return df


def load_logs_files(prefix, names):
    dct = {}
    for nm in names:
        with open(prefix+"/"+nm) as f:
            dct[nm] = Log(f)
    return dct
