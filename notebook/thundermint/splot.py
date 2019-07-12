"""
"""
import pandas as pd
import tempfile
import subprocess
import IPython.display



def read_trace(log,i) :
    # Decision to commit
    rowsD = pd.DataFrame(
        { 'at'   : log[log['msg'] == "Decision to commit"]['at'],
          'node' : 1000+i,
          'msg'  : "#FF9900", # orange
        })
    rowsA = pd.DataFrame(
        { 'at'   : log[log['msg'] == "Actual commit"]['at'],
          'node' : 1000+i,
          'msg'  : "#99cc00", # жёлто-зелёный
        })
    # Commit trace
    rowsC = log[log['msg'] == 'Entering new height ----------------'].copy()
    rowsC['msg'] = rowsC['data'].apply(lambda x : str(x['H'] % 2))
    rowsC = pd.DataFrame({
        'at'  : rowsC['at'],
        'node': i,
        'msg' : rowsC['data'].apply(lambda x : "white" if x['H'] % 2 == 0 else "black")
    })
    # Steps trace
    msgMap = \
        { 'Entering new height ---------------- ' : "#eeeeff", # Светло-серый
          'Entering propose Timeout'   : "#bbffbb", # Салатовый 
          'Entering propose PC_Nil'    : "#ff5050", # Красный
          'Entering prevote Timeout'   : "#ffff66", # Желтый
          'Entering precommit PV_Maj'  : "#0066ff", # Синий
          'Entering precommit Timeout' : "#ff33cc", # Розовый
        }
    steps    = log[log['msg'].apply(lambda s: s.startswith('Entering'))].copy().reset_index()
    reasons  = steps['data'].apply(lambda x: x.get('reason')).fillna("")
    messages = (steps['msg'] + ' ' + reasons).apply(lambda x: msgMap.get(x,x))
    rowsS = pd.DataFrame(
        {'at'  : steps['at'],
         'node': 1000 + i,
         'msg' : messages,
        })
    return pd.concat([rowsC,rowsS,rowsD,rowsA])

def splot(logs, w=1900) :
    nLogs = len(logs)
    trace = pd.concat([read_trace(log.cons,i)
                       for i,log
                       in enumerate(logs.values())]).sort_values(by='at').reset_index()
    with tempfile.NamedTemporaryFile(mode='w', suffix='.trace', delete=False) as f :
        for i in range(nLogs) :
            print( "%s <NODE%i XXX" % (trace['at'][0], i), file=f.file)
            print( "%s <NODE%i XXX" % (trace['at'][0], 1000+i), file=f.file)
        for _,q in trace.iterrows() :
            print( "%s >NODE%i %s" % (q['at'], q['node'], q['msg']), file=f.file)
        with tempfile.NamedTemporaryFile(suffix='.png') as out :
            subprocess.run(["splot", "-w", str(w), "-h", str(nLogs*2*30), "-bh 18", "-if", f.name, "-o", out.name],
                           check=True)
            return IPython.display.Image(out.name, unconfined=True)
