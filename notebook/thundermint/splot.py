#!/usr/bin/python
"""
"""
import pandas as pd
import tempfile
import subprocess
import IPython.display



def read_trace(log,i) :
    # Decision to commit
    rowsD = pd.DataFrame(
        { 'at'   : log.cons[log.cons['msg'] == "Decision to commit"]['at'],
          'node' : 1000+i,
          'msg'  : "#FF9900",
        })
    rowsA = pd.DataFrame(
        { 'at'   : log.cons[log.cons['msg'] == "Actual commit"]['at'],
          'node' : 1000+i,
          'msg'  : "#99cc00",
        })
    # Commit trace
    rowsC = log.cons[log.cons['msg'] == 'Entering new height ----------------'].copy()
    rowsC['msg'] = rowsC['data'].apply(lambda x : str(x['H'] % 2))
    rowsC = pd.DataFrame({
        'at'  : rowsC['at'],
        'node': i,
        'msg' : rowsC['data'].apply(lambda x : "white" if x['H'] % 2 == 0 else "black")
    })
    # Steps trace
    msgMap = \
        { 'Entering new height ---------------- ' : "#eeeeff",
          'Entering propose Timeout'   : "#bbffbb",
          'Entering propose PC_Nil'    : "#ff5050",
          'Entering prevote Timeout'   : "#ffff66",
          'Entering precommit PV_Maj'  : "#0066ff",
          'Entering precommit Timeout' : "#ff33cc",
        }
    steps    = log.cons[log.cons['msg'].apply(lambda s: s.startswith('Entering'))].copy().reset_index()
    reasons  = steps['data'].apply(lambda x: x.get('reason')).fillna("")
    messages = (steps['msg'] + ' ' + reasons).apply(lambda x: msgMap.get(x,x))
    rowsS = pd.DataFrame(
        {'at'  : steps['at'],
         'node': 1000 + i,
         'msg' : messages,
        })
    return pd.concat([rowsC,rowsS,rowsD,rowsA])

def read_traces(logs):
    return pd.concat([read_trace(log,i) for i,log in enumerate(logs)]).sort_values(by='at').reset_index()

def splot(trace, w=1900) :
    with tempfile.NamedTemporaryFile(mode='w', suffix='.trace', delete=False) as f :
        for i in [0,1,2,3] :
            print( "%s <NODE%i XXX" % (trace['at'][0], i), file=f.file)
            print( "%s <NODE%i XXX" % (trace['at'][0], 1000+i), file=f.file)
        for _,q in trace.iterrows() :
            print( "%s >NODE%i %s" % (q['at'], q['node'], q['msg']), file=f.file)
        with tempfile.NamedTemporaryFile(suffix='.png') as out :
            subprocess.run(["splot", "-w", str(w), "-h", str(4*2*30), "-bh 18", "-if", f.name, "-o", out.name],
                           check=True)
            return IPython.display.Image(out.name, unconfined=True)
