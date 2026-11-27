-- |
-- Module      : Streamly.External.Network.UDP
-- Description : UDP datagrams as streams
-- License     : BSD-3-Clause
--
-- UDP sockets for Streamly, over the @network@ package. The datagrams
-- received are a stream, each with the address of its sender; datagrams
-- are sent one by one or through a fold.
--
-- > import Network.Socket (SockAddr (SockAddrInet), tupleToHostAddress)
-- > import Streamly.Data.Array (fromList)
-- > import Streamly.Data.Stream (toList)
-- > import Streamly.External.Network.UDP (anyAddress, datagramsWithin, sendTo,
-- >                                       socketAt, withSocket)
-- >
-- > -- Ask, and collect the answers that come within a second.
-- > main :: IO ()
-- > main = withSocket (socketAt anyAddress) $ \socket -> do
-- >   sendTo socket (SockAddrInet 9003 (tupleToHostAddress (192, 0, 2, 1))) (fromList [1, 2, 3])
-- >   answers <- toList (datagramsWithin 1000000 socket)
-- >   print (map fst answers)
--
-- Unix only: the multicast options are set as Unix systems expect them.
module Streamly.External.Network.UDP
  ( -- * Sockets
    SocketConfig (..)
  , socketAt
  , anyAddress
  , withSocket

    -- * Receiving
  , datagrams
  , datagramsWith
  , datagramsWithin
  , maxDatagramSize

    -- * Sending
  , sendTo
  , write
  , writeTo

    -- * Broadcast and multicast
  , setBroadcast
  , joinGroup
  , leaveGroup
  , setMulticastTTL
  , setMulticastLoopback
  ) where

import Control.Exception (bracket)
import Control.Monad (void, when)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Word (Word8)
import Foreign.C.Types (CInt)
import Foreign.Ptr (castPtr, plusPtr)
import Foreign.Storable (Storable (..))
import GHC.Clock (getMonotonicTimeNSec)
import Network.Socket (Family (AF_INET, AF_INET6, AF_UNIX), HostAddress,
                       SockAddr (..), Socket,
                       SocketOption (Broadcast, ReuseAddr, SockOpt),
                       SocketType (Datagram), bind, close, defaultProtocol,
                       setSockOpt, setSocketOption)
import Network.Socket qualified as N (socket)
import Network.Socket.ByteString (recvFrom)
import Network.Socket.ByteString qualified as NB (sendTo)
import Streamly.Data.Array (Array)
import Streamly.Data.Fold (Fold, drainMapM)
import Streamly.Data.Stream (Stream, repeatM, unfoldrM)
import Streamly.External.ByteString (fromArray, toArray)
import Streamly.External.Network.UDP.Constants (addMembership, dropMembership,
                                                ipProtocol, multicastLoop,
                                                multicastTtl)
import System.Timeout (timeout)

-- | How to open a socket.
data SocketConfig = SocketConfig
  { address :: SockAddr
  -- ^ The address and the port to bind to. Its family is that of the
  -- socket.
  , reuseAddress :: Bool
  -- ^ Let several sockets bind to this address, as the listeners of a
  -- multicast group do.
  , broadcast :: Bool
  -- ^ Allow sending to broadcast addresses.
  }
  deriving stock (Eq, Show)

-- | A socket bound to an address, alone on it and without broadcast.
socketAt :: SockAddr -> SocketConfig
socketAt address = SocketConfig {address, reuseAddress = False, broadcast = False}

-- | Every IPv4 interface, on a port chosen by the system: what a socket
-- that asks and listens to the answers needs.
anyAddress :: SockAddr
anyAddress = SockAddrInet 0 0

-- | Open a UDP socket, bind it, use it and close it.
withSocket :: SocketConfig -> (Socket -> IO a) -> IO a
withSocket settings use =
  bracket (N.socket family Datagram defaultProtocol) close $ \socket -> do
    when settings.reuseAddress (setSocketOption socket ReuseAddr 1)
    when settings.broadcast (setBroadcast socket True)
    bind socket settings.address
    use socket
  where
    family = case settings.address of
      SockAddrInet {} -> AF_INET
      SockAddrInet6 {} -> AF_INET6
      SockAddrUnix {} -> AF_UNIX

-- | The largest datagram UDP can carry over IPv4, headers included: no
-- datagram is cut when this much room is given to each.
maxDatagramSize :: Int
maxDatagramSize = 65535

-- | The datagrams received, each with the address of its sender, without
-- end. An empty datagram is an empty array.
datagrams :: MonadIO m => Socket -> Stream m (SockAddr, Array Word8)
datagrams = datagramsWith maxDatagramSize

-- | Like 'datagrams', with the room given to each datagram, in bytes: a
-- longer one is cut to it.
datagramsWith :: MonadIO m => Int -> Socket -> Stream m (SockAddr, Array Word8)
datagramsWith size socket = repeatM (liftIO (receiveFrom size socket))

-- | The datagrams received within a number of microseconds, counted from
-- the moment the stream starts.
datagramsWithin :: MonadIO m => Int -> Socket -> Stream m (SockAddr, Array Word8)
datagramsWithin microseconds socket = unfoldrM step Nothing
  where
    step started = liftIO $ do
      now <- getMonotonicTimeNSec
      let deadline = maybe (now + fromIntegral (max 0 microseconds) * 1000) id started
      if now >= deadline
        then pure Nothing
        else fmap (,Just deadline) <$> timeout (fromIntegral ((deadline - now + 999) `div` 1000)) (receiveFrom maxDatagramSize socket)

receiveFrom :: Int -> Socket -> IO (SockAddr, Array Word8)
receiveFrom size socket = do
  (bytes, sender) <- recvFrom socket size
  pure (sender, toArray bytes)

-- | Send one datagram to an address. An empty array is an empty datagram.
sendTo :: MonadIO m => Socket -> SockAddr -> Array Word8 -> m ()
sendTo socket destination datagram =
  -- One call: a datagram goes whole or not at all, and the sendAllTo of
  -- network sends nothing for an empty one.
  liftIO (void (NB.sendTo socket (fromArray datagram) destination))

-- | Send each array as a datagram to one address.
write :: MonadIO m => Socket -> SockAddr -> Fold m (Array Word8) ()
write socket destination = drainMapM (sendTo socket destination)

-- | Send each array as a datagram to the address that comes with it.
writeTo :: MonadIO m => Socket -> Fold m (SockAddr, Array Word8) ()
writeTo socket = drainMapM (uncurry (sendTo socket))

-- | Allow or forbid sending to broadcast addresses.
setBroadcast :: Socket -> Bool -> IO ()
setBroadcast socket allowed = setSocketOption socket Broadcast (if allowed then 1 else 0)

-- | A multicast group and the interface to reach it by, as the system
-- wants them (@struct ip_mreq@).
data Membership = Membership HostAddress HostAddress

instance Storable Membership where
  sizeOf _ = 8
  alignment _ = 4
  peek pointer = Membership <$> peek (castPtr pointer) <*> peek (castPtr pointer `plusPtr` 4)
  poke pointer (Membership group interface) = poke (castPtr pointer) group >> poke (castPtr pointer `plusPtr` 4) interface

-- | Join an IPv4 multicast group, on the interface that has an address, or
-- on the one the system chooses when the address is 0. The socket then
-- receives the datagrams sent to the group on the port it is bound to.
joinGroup :: Socket -> HostAddress -> HostAddress -> IO ()
joinGroup socket group interface = setSockOpt socket (SockOpt ipProtocol addMembership) (Membership group interface)

-- | Leave an IPv4 multicast group joined with 'joinGroup'.
leaveGroup :: Socket -> HostAddress -> HostAddress -> IO ()
leaveGroup socket group interface = setSockOpt socket (SockOpt ipProtocol dropMembership) (Membership group interface)

-- | How many routers the IPv4 multicast datagrams sent may cross. The
-- systems start at 1: the local network.
setMulticastTTL :: Socket -> Int -> IO ()
setMulticastTTL socket hops = setSockOpt socket (SockOpt ipProtocol multicastTtl) (fromIntegral hops :: CInt)

-- | Whether the IPv4 multicast datagrams sent come back to the sockets of
-- this machine that joined the group. The systems start with it on.
setMulticastLoopback :: Socket -> Bool -> IO ()
setMulticastLoopback socket enabled = setSockOpt socket (SockOpt ipProtocol multicastLoop) (if enabled then 1 else 0 :: CInt)
