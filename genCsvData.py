#! /usr/bin/python

import csv
import random
import sys

if __name__ == "__main__":
    if len(sys.argv) < 4:
        print ("usage:\n"
               "genCsvData.py output.csv numEx features")
        sys.exit(1)
    numEx = int(sys.argv[2])
    features = int(sys.argv[3])
    outputcsv = open(sys.argv[1], 'w')
    writer = csv.writer(outputcsv, delimiter=",")

    for i in range(numEx):
        row = [False, False, random.randint(0,1)] + [2*random.random() - 1 for j in range(features)]
        writer.writerow(row)
        
    outputcsv.close()
