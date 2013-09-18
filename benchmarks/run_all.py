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
        lines = string.split(err.strip(),'\n')

        total_time_str = lines[-1]
        tdiff = float(total_time_str)

        if len(lines) > 1:
            computation_time_str = lines[-2]
            compdiff = float(computation_time_str)            
        else:
            compdiff = tdiff


    else:
        tdiff = t2 - t1
        compdiff = tdiff

    data = [task, lang, num_workers, tdiff, compdiff]
    results.append(data)
    print (data)

def main():
    headings = ['Task', 'Language', 'Threads', 'Total Time', 'Computation Time']
    for sort in ['concurrent', 'parallel']:
        results = []

        for task in tasks[sort]:
            for lang in langs:
                for workers in [4]:
                    for i in range(1):
                        run(results, sort, task, lang, workers)

        with open(sort + '_results.csv', 'wb') as csv_file:
            perfwriter = csv.writer(csv_file, quoting=csv.QUOTE_MINIMAL)
            perfwriter.writerow(headings)
            for result in results:
                perfwriter.writerow(result)

if __name__ == "__main__":
    main()
