from prometheus_client import Gauge
from prometheus_client import Summary
from prometheus_client import start_http_server
from numbers import Number
from dateutil.parser import *
from dateutil.tz import *
from datetime import *
import os
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
FAUCET_ADDRESS = os.getenv("FAUCET_ADDRESS")

JORMUNGANDR_REQUEST_TIME = Summary('jormungandr_process_time', 'Time spent processing jormungandr metrics')
jormungandr_blockRecvCnt = Gauge('jormungandr_blockRecvCnt', 'Jormungandr blockRecvCnt')
jormungandr_lastBlockDate = Gauge('jormungandr_lastBlockDate', 'Jormungandr lastBlockDate')
jormungandr_lastBlockFees = Gauge('jormungandr_lastBlockFees', 'Jormungandr lastBlockFees')
jormungandr_lastBlockHash = Gauge('jormungandr_lastBlockHash', 'Jormungandr lastBlockHash')
jormungandr_lastBlockHeight = Gauge('jormungandr_lastBlockHeight', 'Jormungandr lastBlockHeight')
jormungandr_lastBlockSum = Gauge('jormungandr_lastBlockSum', 'Jormungandr lastBlockSum')
jormungandr_lastBlockTime = Gauge('jormungandr_lastBlockTime', 'Jormungandr lastBlockTime')
jormungandr_lastBlockTx = Gauge('jormungandr_lastBlockTx', 'Jormungandr lastBlockTx')
jormungandr_txRecvCnt = Gauge('jormungandr_txRecvCnt', 'Jormungandr txRecvCnt')
jormungandr_uptime = Gauge('jormungandr_uptime', 'Jormungandr uptime')
if FAUCET_ADDRESS is not None:
    jormungandr_faucetFunds = Gauge('jormungandr_faucetFunds', 'Jormungandr Faucet Fund Level in Lovelace')
    jormungandr_faucetCounter = Gauge('jormungandr_faucetCounter', 'Jormungandr Faucet Counter')

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
    metrics['lastBlockTime'] = parse(metrics['lastBlockTime']).timestamp()
    #print(f'metrics = {metrics}')
    for metric in metrics:
        metrics[metric] = sanitize(metrics[metric])
    #print(f'Santized metrics = {metrics}')
    jormungandr_blockRecvCnt.set(metrics['blockRecvCnt'])
    jormungandr_lastBlockDate.set(metrics['lastBlockDate'])
    jormungandr_lastBlockFees.set(metrics['lastBlockFees'])
    jormungandr_lastBlockHash.set(metrics['lastBlockHash'])
    jormungandr_lastBlockHeight.set(metrics['lastBlockHeight'])
    jormungandr_lastBlockSum.set(metrics['lastBlockSum'])
    jormungandr_lastBlockTime.set(metrics['lastBlockTime'])
    jormungandr_lastBlockTx.set(metrics['lastBlockTx'])
    jormungandr_txRecvCnt.set(metrics['txRecvCnt'])
    jormungandr_uptime.set(metrics['uptime'])
    if FAUCET_ADDRESS is not None:
        url = f'http://{ip}:3001/api/v0/account/{FAUCET_ADDRESS}'
        json_obj = urllib.request.urlopen(url)
        metrics = json.loads(json_obj.read().decode('utf-8'))
        #print(f'metrics = {metrics}')
        jormungandr_faucetFunds.set(sanitize(metrics['value']))
        jormungandr_faucetCounter.set(sanitize(metrics['counter']))
    sys.stdout.flush()

def sanitize(metric):
    if isinstance(metric, str):
        try:
            metric = float(metric)
        except ValueError:
            try:
                metric = int(metric, 16)
            except ValueError:
                metric = False
    elif not isinstance(metric, (float, int)):
        metric = False
    return metric

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
            jormungandr_lastBlockHash.set(False)
            jormungandr_lastBlockHeight.set(False)
            jormungandr_lastBlockSum.set(False)
            jormungandr_lastBlockTime.set(False)
            jormungandr_lastBlockTx.set(False)
            jormungandr_txRecvCnt.set(False)
            jormungandr_uptime.set(False)
            if FAUCET_ADDRESS is not None:
                jormungandr_faucetFunds.set(False)
                jormungandr_faucetCounter.set(False)
        time.sleep(SLEEP_TIME)
