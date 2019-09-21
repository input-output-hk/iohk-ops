from prometheus_client import Gauge
from prometheus_client import Summary
from prometheus_client import start_http_server
from numbers import Number
import netifaces as ni
import requests
import json
import random
import time
import urllib.request
import sys
import warnings

#Create a metric to track time spent and requests made.
EXPORTER_PORT = 8000
SLEEP_TIME = 10

JORMUNGANDR_REQUEST_TIME = Summary('jormungandr_process_time', 'Time spent processing jormungandr metrics')
jormungandr_blockRecvCnt = Gauge('jormungandr_blockRecvCnt', 'Jormungandr blockRecvCnt')
jormungandr_lastBlockDate = Gauge('jormungandr_lastBlockDate', 'Jormungandr lastBlockDate')
jormungandr_lastBlockFees = Gauge('jormungandr_lastBlockFees', 'Jormungandr lastBlockFees')
jormungandr_lastBlockHeight = Gauge('jormungandr_lastBlockHeight', 'Jormungandr lastBlockHeight')
jormungandr_lastBlockSum = Gauge('jormungandr_lastBlockSum', 'Jormungandr lastBlockSum')
jormungandr_lastBlockTime = Gauge('jormungandr_lastBlockTime', 'Jormungandr lastBlockTime')
jormungandr_lastBlockTx = Gauge('jormungandr_lastBlockTx', 'Jormungandr lastBlockTx')
jormungandr_txRecvCnt = Gauge('jormungandr_txRecvCnt', 'Jormungandr txRecvCnt')
jormungandr_uptime = Gauge('jormungandr_uptime', 'Jormungandr uptime')

# Not a numeric metric, therefore excluded:
jormungandr_lastBlockHash = Gauge('jormungandr_lastBlockHash', 'Jormungandr lastBlockHash')

# Decorate function with metric.
@JORMUNGANDR_REQUEST_TIME.time()
def process_jormungandr_metrics():

    ifList = ni.interfaces()
    if len(ifList) == 0:
        raise Exception('There are no network interfaces available.')
    elif len(ifList) == 1 and ifList[0] == "lo":
        raise Exception('Only the loopback interface is available.')
    elif len(ifList) >= 2:
        ifList.remove("lo")
        if len(ifList) == 1:
            iface = ifList[0]
        elif len(ifList) > 1:
            if "eth0" in ifList:
                iface = "eth0"
            else:
                iface = ifList[0]
            warnings.warn(f'More than one non-loopback interface is available: {ifList}.  Using {iface}.')
    ip = ni.ifaddresses(iface)[ni.AF_INET][0]['addr']
    url = f'http://{ip}:3001/api/v0/node/stats'
    json_obj = urllib.request.urlopen(url)
    metrics = json.loads(json_obj.read().decode('utf-8'))
    print(f'processing jormungandr metrics from {url} via {iface}')
    # print(f'metrics = {metrics}')
    for metric in metrics:
        if isinstance(metrics[metric], str):
            try:
                metrics[metric] = float(metrics[metric])
            except ValueError:
                metrics[metric] = False
        elif not isinstance(metrics[metric], (float, int)):
            metrics[metric] = False
    # print(f'Santized metrics = {metrics}')
    # print(f'Metric = {metric} val = {metrics[metric]}')
    jormungandr_blockRecvCnt.set(metrics['blockRecvCnt'])
    jormungandr_lastBlockDate.set(metrics['lastBlockDate'])
    jormungandr_lastBlockFees.set(metrics['lastBlockFees'])
    jormungandr_lastBlockHeight.set(metrics['lastBlockHeight'])
    jormungandr_lastBlockSum.set(metrics['lastBlockSum'])
    jormungandr_lastBlockTime.set(metrics['lastBlockTime'])
    jormungandr_lastBlockTx.set(metrics['lastBlockTx'])
    jormungandr_txRecvCnt.set(metrics['txRecvCnt'])
    jormungandr_uptime.set(metrics['uptime'])
    sys.stdout.flush()

if __name__ == '__main__':
    # Start up the server to expose the metrics.
    start_http_server(EXPORTER_PORT)
    # Main Loop: Process all API's and sleep for a certain amount of time
    while True:
        try:
            process_jormungandr_metrics()
        except:
            print("failed to process jormungandr metrics")
            jormungandr_blockRecvCnt.set(False)
            jormungandr_lastBlockDate.set(False)
            jormungandr_lastBlockFees.set(False)
            jormungandr_lastBlockHeight.set(False)
            jormungandr_lastBlockSum.set(False)
            jormungandr_lastBlockTime.set(False)
            jormungandr_lastBlockTx.set(False)
            jormungandr_txRecvCnt.set(False)
            jormungandr_uptime.set(False)
        time.sleep(SLEEP_TIME)
