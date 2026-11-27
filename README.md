# streamly-network-udp

UDP sockets for [Streamly](https://streamly.composewell.com/), over the
[network](https://hackage.haskell.org/package/network) package. The
datagrams received are a stream, each with the address of its sender;
datagrams are sent one by one or through a fold.

```haskell
import Network.Socket (SockAddr (SockAddrInet), tupleToHostAddress)
import Streamly.Data.Array (fromList)
import Streamly.Data.Stream (toList)
import Streamly.External.Network.UDP (anyAddress, datagramsWithin, sendTo,
                                      socketAt, withSocket)

-- Ask, and collect the answers that come within a second.
main :: IO ()
main = withSocket (socketAt anyAddress) $ \socket -> do
  sendTo socket (SockAddrInet 9003 (tupleToHostAddress (192, 0, 2, 1))) (fromList [1, 2, 3])
  answers <- toList (datagramsWithin 1000000 socket)
  print (map fst answers)
```

## What it gives

- `withSocket`: a socket bound to an address, with `reuseAddress` and
  `broadcast` as options.
- `datagrams`: the datagrams received, without end; `datagramsWith` to
  give each less room; `datagramsWithin` for those that come within a
  time.
- `sendTo`, and the folds `write` (one address) and `writeTo` (an address
  with each datagram).
- For IPv4 multicast: `joinGroup`, `leaveGroup`, `setMulticastTTL`,
  `setMulticastLoopback`.

## Limits of this version

- Unix only: the multicast options are set as Unix systems expect them.
- Multicast groups are IPv4 only. Sockets can be IPv6.
- No connected sockets: every datagram names its destination.
