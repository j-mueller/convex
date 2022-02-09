# convex-options

Plutus implementation of [option](https://en.wikipedia.org/wiki/Option_(finance)) contracts. Put and call options are supported, but the call option requires the issuer to lock the asset in the output. This is because the option is for physical delivery, that is, the buyer of the option receives the asset when exercising the option. This is done to avoid having to use an oracle.

Buyer and seller of the option are represented by two tokens (NFTs). These tokens can be traded independently.

There are two situations in which a position of options tokens is market risk neutral:

1. If you hold both the buyer and the seller token of an option
2. If you hold the buyer token of a PUT option and the buyer token of the corresponding CALL option

This fact can be used to build an AMM style liquidity pool.

TODO:

[ ] tests
[ ] pricing
[ ] node client
[ ] think about AMM / liquidity (planned for v2)
