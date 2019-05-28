from prometheus_client import Gauge
from prometheus_client import Summary
from prometheus_client import start_http_server
import requests
import json
import random
import time
import urllib.request
import sys

#Create a metric to track time spent and requests made.
EXPORTER_PORT = 8000
SLEEP_TIME = 300 # 5 minutes
REQUEST_TIME = Summary('binance_process_time', 'Time spent processing binance assets')
binance_deposits = Gauge('binance_deposits', 'Deposits enabled')
binance_withdraws = Gauge('binance_withdraws', 'Withdraws enabled')

# Decorate function with metric.
@REQUEST_TIME.time()
def process_binance_assets():
    url = "https://www.binance.com/assetWithdraw/getAllAsset.html"
    json_obj = urllib.request.urlopen(url)
    crypto_assets = json.loads(json_obj.read().decode('utf-8'))
    print("processing binance assets")
    for crypto_asset in crypto_assets:
        if crypto_asset['assetCode']== 'ADA':
            binance_deposits.set(crypto_asset['enableCharge'])
            binance_withdraws.set(crypto_asset['enableWithdraw'])
    sys.stdout.flush()

if __name__ == '__main__':
    # Start up the server to expose the metrics.
    start_http_server(EXPORTER_PORT)
    # Main Loop: Process all API's and sleep for a certain amount of time
    while True:
        process_binance_assets()
        time.sleep(SLEEP_TIME)
