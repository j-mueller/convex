# convex-node-client

Conveniences for cardano-node clients. Modules of note are

* [`Convex.NodeClient.Types`](src/Convex/NodeClient/Types.hs) with an easy-to-use `runNodeClient` that runs a node client using just the node's config file, no additional configuration needed
* [`Convex.NodeClient.Fold`](src/Convex/NodeClient/Fold.hs) for writing chain followers
* [`Convex.Event`](src/Convex/Event.hs) with some convenience types for extracting Plutus script events from the blockchain

See `convex-muesli-data-export` for how these can be used together.