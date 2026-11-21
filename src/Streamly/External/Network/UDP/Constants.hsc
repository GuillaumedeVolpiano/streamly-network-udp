-- |
-- Module      : Streamly.External.Network.UDP.Constants
-- Description : The numbers of the IPv4 multicast socket options
-- License     : BSD-3-Clause
--
-- These numbers differ from one system to another, and the network package
-- does not give them: they are read from the headers of the system.
module Streamly.External.Network.UDP.Constants
  ( ipProtocol
  , addMembership
  , dropMembership
  , multicastTtl
  , multicastLoop
  ) where

#include <netinet/in.h>

import Foreign.C.Types (CInt)

ipProtocol, addMembership, dropMembership, multicastTtl, multicastLoop :: CInt
ipProtocol = #const IPPROTO_IP
addMembership = #const IP_ADD_MEMBERSHIP
dropMembership = #const IP_DROP_MEMBERSHIP
multicastTtl = #const IP_MULTICAST_TTL
multicastLoop = #const IP_MULTICAST_LOOP
