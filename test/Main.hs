-- | Datagrams between sockets of this machine.
module Main (main) where

import Control.Exception (try)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import GHC.Clock (getMonotonicTime)
import Network.Socket qualified as Net
import Streamly.Data.Stream qualified as Stream
import Streamly.External.ByteString qualified as StreamlyBS
import Streamly.External.Network.UDP
import Test.Tasty (TestTree, defaultMain, localOption, mkTimeout, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))

main :: IO ()
main = defaultMain tests

loopback :: Net.SockAddr
loopback = Net.SockAddrInet 0 (Net.tupleToHostAddress (127, 0, 0, 1))

-- | Two sockets on the loopback interface, each with its address.
withPair :: ((Net.Socket, Net.SockAddr) -> (Net.Socket, Net.SockAddr) -> IO a) -> IO a
withPair use =
  withSocket (socketAt loopback) $ \one ->
    withSocket (socketAt loopback) $ \two -> do
      oneAddress <- Net.getSocketName one
      twoAddress <- Net.getSocketName two
      use (one, oneAddress) (two, twoAddress)

bytesOf :: [(Net.SockAddr, a)] -> (a -> ByteString) -> [ByteString]
bytesOf received convert = map (convert . snd) received

tests :: TestTree
tests =
  localOption (mkTimeout 20_000_000) $
    testGroup
      "streamly-network-udp"
      [ testCase "datagrams arrive whole, in order, with their sender" $
          withPair $ \(one, oneAddress) (two, twoAddress) -> do
            mapM_ (sendTo one twoAddress . StreamlyBS.toArray) ["first", "", "third"]
            received <- Stream.toList (Stream.take 3 (datagrams two))
            map fst received @?= replicate 3 oneAddress
            bytesOf received StreamlyBS.fromArray @?= ["first", "", "third"]
      , testCase "a large datagram is not cut, a small room cuts it" $
          withPair $ \(one, _) (two, twoAddress) -> do
            let large = BS.pack (take 60_000 (cycle [0 .. 255]))
            sendTo one twoAddress (StreamlyBS.toArray large)
            whole <- Stream.toList (Stream.take 1 (datagrams two))
            bytesOf whole StreamlyBS.fromArray @?= [large]
            sendTo one twoAddress (StreamlyBS.toArray "0123456789")
            cut <- Stream.toList (Stream.take 1 (datagramsWith 4 two))
            bytesOf cut StreamlyBS.fromArray @?= ["0123"]
      , testCase "write sends each array to one address" $
          withPair $ \(one, _) (two, twoAddress) -> do
            Stream.fold (write one twoAddress) (Stream.fromList (map StreamlyBS.toArray ["a", "b"]))
            received <- Stream.toList (Stream.take 2 (datagrams two))
            bytesOf received StreamlyBS.fromArray @?= ["a", "b"]
      , testCase "writeTo sends each array to its own address" $
          withPair $ \(one, oneAddress) (two, twoAddress) -> do
            Stream.fold (writeTo one) (Stream.fromList [(twoAddress, StreamlyBS.toArray "to two"), (oneAddress, StreamlyBS.toArray "to one")])
            atTwo <- Stream.toList (Stream.take 1 (datagrams two))
            atOne <- Stream.toList (Stream.take 1 (datagrams one))
            (bytesOf atTwo StreamlyBS.fromArray, bytesOf atOne StreamlyBS.fromArray) @?= (["to two"], ["to one"])
      , testCase "datagramsWithin gives what came, then ends in time" $
          withPair $ \(one, _) (two, twoAddress) -> do
            mapM_ (sendTo one twoAddress . StreamlyBS.toArray) ["a", "b"]
            before <- getMonotonicTime
            received <- Stream.toList (datagramsWithin 200_000 two)
            after <- getMonotonicTime
            bytesOf received StreamlyBS.fromArray @?= ["a", "b"]
            assertBool ("waited " <> show (after - before) <> " s") (after - before >= 0.2 && after - before < 1)
      , testCase "datagramsWithin with no time left gives nothing" $
          withPair $ \(one, _) (two, twoAddress) -> do
            sendTo one twoAddress (StreamlyBS.toArray "ignored")
            received <- Stream.toList (datagramsWithin 0 two)
            length received @?= 0
      , testCase "broadcast needs the option" $ do
          let everyone = Net.SockAddrInet 9 (Net.tupleToHostAddress (255, 255, 255, 255))
          refused <- try (withSocket (socketAt anyAddress) (\socket -> sendTo socket everyone (StreamlyBS.toArray "x")))
          case refused of
            Left (_ :: IOError) -> pure ()
            Right () -> assertFailure "sent a broadcast without the option"
          withSocket (socketAt anyAddress) {broadcast = True} (\socket -> sendTo socket everyone (StreamlyBS.toArray "x"))
      , testCase "a socket that joined a group receives what is sent to it, until it leaves" $ do
          let group = Net.tupleToHostAddress (239, 255, 77, 77)
              everywhere port = Net.SockAddrInet port 0
          withSocket (socketAt (everywhere 0)) {reuseAddress = True} $ \listener -> do
            port <- Net.socketPort listener
            joinGroup listener group 0
            withSocket (socketAt anyAddress) $ \sender -> do
              setMulticastTTL sender 1
              setMulticastLoopback sender True
              sendTo sender (Net.SockAddrInet port group) (StreamlyBS.toArray "to the group")
              received <- Stream.toList (Stream.take 1 (datagramsWithin 2_000_000 listener))
              bytesOf received StreamlyBS.fromArray @?= ["to the group"]
              leaveGroup listener group 0
              sendTo sender (Net.SockAddrInet port group) (StreamlyBS.toArray "to nobody")
              after <- Stream.toList (datagramsWithin 200_000 listener)
              length after @?= 0
      , testCase "two sockets share an address with reuseAddress, not without" $ do
          withSocket (socketAt loopback) {reuseAddress = True} $ \first -> do
            address <- Net.getSocketName first
            withSocket (socketAt address) {reuseAddress = True} (\_ -> pure ())
          withSocket (socketAt loopback) $ \first -> do
            address <- Net.getSocketName first
            taken <- try (withSocket (socketAt address) (\_ -> pure ()))
            case taken of
              Left (_ :: IOError) -> pure ()
              Right () -> assertFailure "bound twice to the same address"
      ]
