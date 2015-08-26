#!/usr/bin/python3

import glob
import os
import re

# Extract planning and execution times for all iterations. Write
# individual and total times to files
def extract_timings(logfile):
    print("Analysing logfile " + logfile)
    m = re.compile("(.*).log").match(logfile)
    query = m.group(1)

    dbtimeFile = query + ".dbtime"
    planningFile = query + ".planning"
    execFile = query + ".exec"

    planningTimes = []
    execTimes = []

    with open(logfile) as f:
        for l in f:
            if "Execution time" in l:
                time = l.split()[2]
                execTimes.append(float(time))

    with open(logfile) as f:
        for l in f:
            if "Planning time" in l:
                time = l.split()[2]
                planningTimes.append(float(time))

    with open(planningFile, "w") as pf:
        for t in planningTimes:
            pf.write(str(t) + "\n")

    with open(execFile, "w") as pf:
        for t in execTimes:
            pf.write(str(t) + "\n")

    dbTimes = [ p + e for (p, e) in zip(planningTimes, execTimes) ]

    with open(dbtimeFile, mode="w") as tf:
        tf.write("\n".join(str(t) for t in dbTimes))

def transpose_matrix(matrix):
    return list(map(list, zip(*matrix)))

# For all iterations, add the total execution time for all subqueries
def add_subquery_times():
    allTimings = []
    for dbtime in glob.glob("*.dbtime"):
        with open(dbtime) as f:
            allTimings.append([float(l) for l in f.read().split()])

    perExec = transpose_matrix(allTimings)
    totalFile = os.path.basename(os.getcwd()) + ".total"
    with open(totalFile, mode="w") as tf:
        for e in perExec:
            tf.write(str(sum(e)) + "\n")

for d in glob.glob("*"):
    print("Extract timings from " + d)
    os.chdir(d)

    for f in glob.glob("*.log"):
        extract_timings(f)

    add_subquery_times()

    os.chdir("..")
