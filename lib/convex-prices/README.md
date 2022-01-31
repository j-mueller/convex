# convex-prices

HTTP API for asset prices

## Usage:


Build everything: `cabal build cardano-node convex-prices`

Then start the cardano node (adjust the paths if necessary):

```bash
cabal exec -- cardano-node run --topology ~/convex/node-config/mainnet/mainnet-topology.json --database-path ~/convex/node-config/mainnet/db2 --socket-path ~/convex/node-config/mainnet/socket --config ~/convex/node-config/mainnet/mainnet-config.json +RTS -N4
```

When the node has fully caught up, you can start the server with

```bash
cabal run -j convex-prices -- \
  --node-config ~/convex/node-config/mainnet/mainnet-config.json \
  --node-socket ~/convex/node-config/mainnet/socket \
```

```bash
$ curl localhost:8098/prices | jq .

{
  "0c78f619e54a5d00e143f66181a2c500d0c394b38a10e86cd1a23c5f.ADAX": {
    "variance": 1478367385494.0454,
    "lastPrice": 565000,
    "stdDev": 1215881.3204807637,
    "lastTradeSlot": 50009882,
    "meanPrice": 635613.6889739574,
    "lastTradeTimestamp": "2022-01-07 17:22:53 UTC",
    "ewMeanPrice": 533399.9673095059
  },
  "0c92aabef5a8f91a36470d0762806c165c0d04aa992541e25d55486a.BCG": {
    "variance": 4.402906269202632,
    "lastPrice": 1,
    "stdDev": 2.0983103367239635,
    "lastTradeSlot": 49999727,
    "meanPrice": 1.2929399899142713,
    "lastTradeTimestamp": "2022-01-07 14:33:38 UTC",
    "ewMeanPrice": 1.2073262894644219
  },
  "14a3455f71c435a04ea1fdb50a3ef4c1cab0e79fb1565627ac66a575.RAVE": {
    "variance": 18418430.23217109,
    "lastPrice": 6900,
    "stdDev": 4291.669865235569,
    "lastTradeSlot": 50009952,
    "meanPrice": 4932.628393925238,
    "lastTradeTimestamp": "2022-01-07 17:24:03 UTC",
    "ewMeanPrice": 5958.643670880204
  },
  (...)
}
```