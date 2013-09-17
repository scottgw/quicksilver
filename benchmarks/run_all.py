#!/usr/bin/env python
import os
import string
import time
import csv
from subprocess import Popen, call, PIPE

langs=['cxx', 'haskell', 'go', 'qs' , 'erlang']

tasks={'concurrent': ['condition', 'mutex', 'prodcons', 'threadring', 'chameneos'],
       'parallel': ['randmat', 'thresh', 'winnow', 'outer', 'product']}

inputs= {'mutex': '20000 32',
         'condition': '20000 32',
         'prodcons': '20000 32',
         'threadring': '5000000',
         'chameneos': '600000',
         'randmat': '10000 0',
         'thresh': '10000 1',
         'winnow': '10000 1000',
         'outer': '10000',
         'product': '10000'}

def make_command(lang, task, num_workers):
    return './run.sh ' + inputs[task] + ' ' + str(num_workers)

def run(results, sort, task, lang, num_workers):
    t1 = time.time ()
    
    os.chdir (os.path.join(sort, lang, task))

    proc = Popen (make_command (lang, task, num_workers), 
                  stdout=PIPE, stderr=PIPE, 
                  shell=True)
    (out,err) = proc.communicate ()
    os.chdir (os.path.join ('..', '..', '..'))
    t2 = time.time ()

    if lang == 'erlang':
        # print(err)
        lastline = string.split(err.strip(),'\n')[-1]
        tdiff = float(lastline)
    else:
        tdiff = t2 - t1

    data = [task, lang, num_workers, tdiff]
    results.append(data)
    print (data)

def main():
    for sort in ['concurrent', 'parallel']:
        results = []

        for task in tasks[sort]:
            for lang in langs:
                for workers in [4]:
                    for i in range(1):
                        run(results, sort, task, lang, workers)

        with open(sort + '_results.csv', 'wb') as csv_file:
            perfwriter = csv.writer(csv_file, quoting=csv.QUOTE_MINIMAL)
            perfwriter.writerow(['Task', 'Language', 'Threads', 'Time'])
            for result in results:
                perfwriter.writerow(result)

if __name__ == "__main__":
    main()
