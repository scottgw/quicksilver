#!/usr/bin/env python
import os
import string
import time
import csv
import multiprocessing
from subprocess import Popen, call, PIPE

langs=['cxx', 'haskell', 'go', 'qs', 'erlang']
# langs=['erlang']
tasks={'concurrent': ['condition', 'mutex', 'prodcons', 'threadring', 'chameneos'],
       # 'parallel': ['randmat', 'thresh', 'winnow', 'outer', 'product', 'chain']
       'parallel': ['randmat', 'thresh', 'winnow', 'outer', 'product', 'chain']
       }

inputs= {'mutex': '20000 32',
         'condition': '20000 32',
         'prodcons': '20000 32',
         'threadring': '5000000',
         'chameneos': '600000',
         'randmat': '10000 0',
         'thresh': '10000 1',
         'winnow': '10000 10000',
         'outer': '10000',
         'product': '10000',
         'chain': '10000 0 1 10000'}

def core_list():
    i = 1
    cores = []

    while i <= multiprocessing.cpu_count():
        cores.append(i)
        i = i * 2

    return cores


def make_command(lang, task, num_workers):
    
    return './run.sh ' + inputs[task] + ' ' + str(num_workers)

def make_variant_command(variant, num_workers):
    flags = {'all': '',
             'qoq': '-q ',
             'dyn': '-d ',
             'stat': '-s ',
             'none': '-n '}

    return './run.sh ' + flags[variant] + str(num_workers)

# Run the qs benchmarks with/without the different optimizations
def run_variant (results, sort, task, workers):
    variants = ['all', 'qoq', 'dyn', 'stat', 'none']
    lang = 'qs'

    os.chdir (os.path.join(sort, lang, task))

    for variant in variants:

        t1 = time.time ()
        cmd = make_variant_command (variant, workers)
        proc = Popen (cmd, stdout=PIPE, stderr=PIPE, shell=True)
        (out,err) = proc.communicate ()
        t2 = time.time ()

        tdiff = t2 - t1
        if len(err) > 0:
            lines = string.split(err.strip(),'\n')
            computation_time_str = lines[-1]
            compdiff = float(computation_time_str)          
        else:
            compdiff = tdiff

        data = [task, variant, tdiff, compdiff]
        results.append(data)
        print (data)

    os.chdir (os.path.join ('..', '..', '..'))

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
    elif lang == 'qs':
        tdiff = t2 - t1
        if len(err) > 0:
            lines = string.split(err.strip(),'\n')
            computation_time_str = lines[-1]
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
    headings = ['Task', 'Language', 'Threads', 'TotalTime', 'CompTime']
    variant_headings = ['Task', 'Variant', 'TotalTime', 'CompTime']
    variant_results = []
    for sort in ['concurrent', 'parallel']:
        results = []
        
        if sort == 'concurrent':
            worker_range = [4]
        else:
            worker_range = core_list()

        for task in tasks[sort]:
            for lang in langs:
                for workers in worker_range:
                    for i in range(0, 1):
                        if lang == 'qs':
                            run_variant (variant_results, sort, task, workers)
                        run(results, sort, task, lang, workers)

        with open(sort + '_results.csv', 'wb') as csv_file:
            perfwriter = csv.writer(csv_file, quoting=csv.QUOTE_MINIMAL)
            perfwriter.writerow(headings)
            for result in results:
                perfwriter.writerow(result)


    with open('variant_results.csv', 'wb') as csv_file:
        perfwriter = csv.writer(csv_file, quoting=csv.QUOTE_MINIMAL)
        perfwriter.writerow(variant_headings)
        for variant_result in variant_results:
            perfwriter.writerow(variant_result)


if __name__ == "__main__":
    main()
