# Revision history for streamly-network-udp

## 0.1.0.1 -- 2026-11-27

* Builds with GHC 9.8 and 9.10 as well as 9.12: the language is GHC2021
  with the extensions GHC2024 adds, and the bounds on `base` and on the
  libraries that come with the compiler are wider.

## 0.1.0.0 -- 2026-11-21

* UDP sockets bound to an address (`withSocket`), with the address reuse
  and broadcast options.
* Datagrams received as a stream with their senders (`datagrams`,
  `datagramsWith`, `datagramsWithin`), and sent one by one (`sendTo`) or
  through a fold (`write`, `writeTo`).
* IPv4 multicast: joining and leaving a group, hop limit and loopback.
