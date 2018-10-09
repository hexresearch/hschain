#!/usr/bin/python
"""
"""

import json
import numpy  as np
import pandas as pd

# ================================================================
# Loading logs
# ================================================================

class Log(object):
    "Log file decoded as data frame"

    def __init__(self, fname = None, lines = None) :
        # Dispatch on arguments
        def load_line(s) :
            try:
                return json.loads(s)
            except json.JSONDecodeError as e:
                print("Parse error %s" % format(e))
                print(s)
                raise e
        #
        if lines is not None:
            rows = lines
        elif fname is not None:
            with open(fname) as f :
                rows = [ load_line(s) for s in f.readlines() if s.strip() != ""]
        else:
            raise Exception("Nor lines nor file is set")
        # Load data
        df       = pd.DataFrame.from_records(rows, columns=['at','data','msg','ns','sev'])
        df['at'] = pd.to_datetime(df['at'])
        df['ns'] = df['ns'].apply(lambda x : x[-1])
        #
        self.all     = df
        self.net     = df[df['ns'] == 'net'].drop(['ns'], axis=1)
        self.cons    = df[df['ns'] == 'consensus'].drop(['ns'], axis=1)
        self.mempool = df[df['ns'] == 'mempool'].drop(['ns'], axis=1).copy()
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
