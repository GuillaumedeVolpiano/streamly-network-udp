# Revision history for streamly-network-udp

## 0.1.0.0 -- 2026-11-28

* First release on Hackage.
* UDP sockets bound to an address (`withSocket`), with the address reuse
  and broadcast options.
* Datagrams received as a stream with their senders (`datagrams`,
  `datagramsWith`, `datagramsWithin`), and sent one by one (`sendTo`) or
  through a fold (`write`, `writeTo`).
* IPv4 multicast: joining and leaving a group, hop limit and loopback.
