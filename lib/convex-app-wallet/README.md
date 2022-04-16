# convex-app-wallet

An app wallet for high throughput apps

## Transaction lifecycle

1. Receive transaction through API
2. Balance transaction with inputs and outputs
3. Compute fees
4. Send to network
5. Wait for txn to appear on blockchain OR input spent by something else

## UTXO lifecycle

1. Available
2. Spent tentatively (by a transaction that has been sent to the network)
3. Spent definitively (by a transaction that has been applied to the ledger)

## Requirements

* Wallet should recognise if one of the inputs of its own pending transactions was spent -> Mark txn as failed
* Wallet should retry partial transactions
* Statistics should be available on
  * Number of txns in each state
  * Txns submitted
  * Fees paid
  * Balance
* Should deal with rollbacks