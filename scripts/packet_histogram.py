import csv
import numpy
import matplotlib.pyplot as plt
import sys

with open(sys.argv[1], "r") as f:
    latencies = [float(row['latency']) for row in csv.DictReader(f)]
    plt.hist(latencies, bins='auto')
    plt.title("NoC Packet Latencies")
    plt.show()
