-- | Datagrams between sockets of this machine.
module Main (main) where

import Control.Exception (try)
import Data.ByteString (ByteString, pack)
import GHC.Clock (getMonotonicTime)
import Network.Socket (SockAddr (SockAddrInet), Socket, getSocketName,
                       socketPort, tupleToHostAddress)
import Streamly.Data.Stream (fold, fromList, toList)
import Streamly.Data.Stream qualified as S (take)
import Streamly.External.ByteString (fromArray, toArray)
import Streamly.External.Network.UDP (SocketConfig (broadcast, reuseAddress),
                                      anyAddress, datagrams, datagramsWith,
                                      datagramsWithin, joinGroup, leaveGroup,
                                      sendTo, setMulticastLoopback,
                                      setMulticastTTL, socketAt, withSocket,
                                      write, writeTo)
import Test.Tasty (TestTree, defaultMain, localOption, mkTimeout, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))

main :: IO ()
main = defaultMain tests

loopback :: SockAddr
loopback = SockAddrInet 0 (tupleToHostAddress (127, 0, 0, 1))

-- | Two sockets on the loopback interface, each with its address.
withPair :: ((Socket, SockAddr) -> (Socket, SockAddr) -> IO a) -> IO a
withPair use =
  withSocket (socketAt loopback) $ \one ->
    withSocket (socketAt loopback) $ \two -> do
      oneAddress <- getSocketName one
      twoAddress <- getSocketName two
      use (one, oneAddress) (two, twoAddress)

bytesOf :: [(SockAddr, a)] -> (a -> ByteString) -> [ByteString]
bytesOf received convert = map (convert . snd) received

tests :: TestTree
tests =
  localOption (mkTimeout 20_000_000) $
    testGroup
      "streamly-network-udp"
      [ testCase "datagrams arrive whole, in order, with their sender" $
          withPair $ \(one, oneAddress) (two, twoAddress) -> do
            mapM_ (sendTo one twoAddress . toArray) ["first", "", "third"]
            received <- toList (S.take 3 (datagrams two))
            map fst received @?= replicate 3 oneAddress
            bytesOf received fromArray @?= ["first", "", "third"]
      , testCase "a large datagram is not cut, a small room cuts it" $
          withPair $ \(one, _) (two, twoAddress) -> do
            let large = pack (take 60_000 (cycle [0 .. 255]))
            sendTo one twoAddress (toArray large)
            whole <- toList (S.take 1 (datagrams two))
            bytesOf whole fromArray @?= [large]
            sendTo one twoAddress (toArray "0123456789")
            cut <- toList (S.take 1 (datagramsWith 4 two))
            bytesOf cut fromArray @?= ["0123"]
      , testCase "write sends each array to one address" $
          withPair $ \(one, _) (two, twoAddress) -> do
            fold (write one twoAddress) (fromList (map toArray ["a", "b"]))
            received <- toList (S.take 2 (datagrams two))
            bytesOf received fromArray @?= ["a", "b"]
      , testCase "writeTo sends each array to its own address" $
          withPair $ \(one, oneAddress) (two, twoAddress) -> do
            fold (writeTo one) (fromList [(twoAddress, toArray "to two"), (oneAddress, toArray "to one")])
            atTwo <- toList (S.take 1 (datagrams two))
            atOne <- toList (S.take 1 (datagrams one))
            (bytesOf atTwo fromArray, bytesOf atOne fromArray) @?= (["to two"], ["to one"])
      , testCase "datagramsWithin gives what came, then ends in time" $
          withPair $ \(one, _) (two, twoAddress) -> do
            mapM_ (sendTo one twoAddress . toArray) ["a", "b"]
            before <- getMonotonicTime
            received <- toList (datagramsWithin 200_000 two)
            after <- getMonotonicTime
            bytesOf received fromArray @?= ["a", "b"]
            assertBool ("waited " <> show (after - before) <> " s") (after - before >= 0.2 && after - before < 1)
      , testCase "datagramsWithin with no time left gives nothing" $
          withPair $ \(one, _) (two, twoAddress) -> do
            sendTo one twoAddress (toArray "ignored")
            received <- toList (datagramsWithin 0 two)
            length received @?= 0
      , testCase "broadcast needs the option" $ do
          let everyone = SockAddrInet 9 (tupleToHostAddress (255, 255, 255, 255))
          refused <- try (withSocket (socketAt anyAddress) (\socket -> sendTo socket everyone (toArray "x")))
          case refused of
            Left (_ :: IOError) -> pure ()
            Right () -> assertFailure "sent a broadcast without the option"
          withSocket (socketAt anyAddress) {broadcast = True} (\socket -> sendTo socket everyone (toArray "x"))
      , testCase "a socket that joined a group receives what is sent to it, until it leaves" $ do
          let group = tupleToHostAddress (239, 255, 77, 77)
              everywhere port = SockAddrInet port 0
          withSocket (socketAt (everywhere 0)) {reuseAddress = True} $ \listener -> do
            port <- socketPort listener
            joinGroup listener group 0
            withSocket (socketAt anyAddress) $ \sender -> do
              setMulticastTTL sender 1
              setMulticastLoopback sender True
              sendTo sender (SockAddrInet port group) (toArray "to the group")
              received <- toList (S.take 1 (datagramsWithin 2_000_000 listener))
              bytesOf received fromArray @?= ["to the group"]
              leaveGroup listener group 0
              sendTo sender (SockAddrInet port group) (toArray "to nobody")
              after <- toList (datagramsWithin 200_000 listener)
              length after @?= 0
      , testCase "two sockets share an address with reuseAddress, not without" $ do
          withSocket (socketAt loopback) {reuseAddress = True} $ \first -> do
            address <- getSocketName first
            withSocket (socketAt address) {reuseAddress = True} (\_ -> pure ())
          withSocket (socketAt loopback) $ \first -> do
            address <- getSocketName first
            taken <- try (withSocket (socketAt address) (\_ -> pure ()))
            case taken of
              Left (_ :: IOError) -> pure ()
              Right () -> assertFailure "bound twice to the same address"
      ]
