module Dsp where

import System.Exit (ExitCode)
import Sound.Sox.Play
import Sound.Sox.Frame (C(..))
import Sound.Sox.Option.Format
import Sound.File.Sndfile
import Sound.File.Sndfile.Buffer.StorableVector
import qualified Data.StorableVector as V
import qualified Data.StorableVector.Lazy as LazyV
import Synthesizer.Generic.Cut
import Synthesizer.Generic.Filter.NonRecursive
import Synthesizer.Generic.Filter.Recursive.Comb
import Synthesizer.Generic.Filter.Delay
import Synthesizer.Generic.Signal
import Synthesizer.Generic.Analysis
import Synthesizer.Generic.Noise
import qualified Synthesizer.Generic.Displacement as Distort
import Synthesizer.Basic.Distortion
import qualified Synthesizer.Generic.Loop as Loop
import Foreign.Storable
import qualified Control.Concurrent.Thread.Delay as Delay
import Data.List (sort)
import Text.Printf
import Control.Monad
import Prelude hiding (readFile, take, negate, reverse)

foo :: IO (LazyV.Vector Double)
foo = do
    (info, Just test) <- readFile "test2.flac"
    let snippet = take (numSeconds info 11) (toLazy $ fromBuffer test)
    return snippet

simplePlay :: (Storable a, Sound.Sox.Frame.C a) => LazyV.Vector a -> IO ExitCode
simplePlay = simple LazyV.hPut (numberOfChannels 2) 88200

lazySort :: (Ord a, Storable a) => LazyV.Vector a -> LazyV.Vector a
lazySort file = LazyV.pack LazyV.defaultChunkSize . sort $ LazyV.unpack file

testlol :: (Fractional y, Ord y, Storable y) => LazyV.Vector y -> LazyV.Vector y
testlol = LazyV.map (\x -> if x < 0.02 then 0 else x)

testlol2 :: (Fractional y, Ord y, Storable y) => LazyV.Vector y -> LazyV.Vector y
testlol2 = LazyV.map (\x -> if x > 0.05 then x+0.05 else x)

double2float :: Double -> Float
double2float = realToFrac

range :: (Ord a, Storable a, Num a) => LazyV.Vector a -> a
range x = LazyV.maximum x - LazyV.minimum x

eightbit :: LazyV.Vector Double -> LazyV.Vector Double
eightbit file = LazyV.map (\x -> (fromIntegral x / 16) - min') ints
    where ints = LazyV.map (round . (*16)) normalized :: LazyV.Vector Int
          normalized = LazyV.map (+ min') file
          min' = LazyV.minimum file

uniqueValues' :: Eq a => [a] -> [a] -> [a]
uniqueValues' [] acc = acc
uniqueValues' (x:xs) acc
    | x `elem` acc = uniqueValues' xs acc
    | otherwise = uniqueValues' xs (x:acc)

uniqueValues :: (Eq a, Storable a) => LazyV.Vector a -> LazyV.Vector a
uniqueValues xs = LazyV.pack LazyV.defaultChunkSize $ uniqueValues' (LazyV.unpack xs) []

lowerSampleRate :: (Num t1, Eq t1) => t1 -> t1 -> [t] -> [t]
lowerSampleRate _ _ [] = []
lowerSampleRate n initial (x:xs)
    | n == 0 = x : lowerSampleRate initial initial xs
    | otherwise = lowerSampleRate (n-1) initial xs

playAtLowerSampleRate :: (RealFrac a1, Storable a, C a) => a1 -> LazyV.Vector a -> IO ExitCode
playAtLowerSampleRate n sample = do
    let fixedSample = LazyV.pack LazyV.defaultChunkSize $ lowerSampleRate (n - 1) (n - 1) (LazyV.unpack sample)
    simple LazyV.hPut (numberOfChannels 2) (round $ 88200 * (1 / n)) fixedSample

main :: IO ()
main = do
    (info, Just test1) <- readFile "test3.flac"
    (_, Just test2) <- readFile "test2.flac"
    (_, Just test3) <- readFile "test.flac"
    (_, Just test4) <- readFile "test4.flac"

    printf "The file has %d frames, a sample rate of %d, %d channels, \
            \%d sections, and lasts for %f seconds\n" (frames info)
            (samplerate info) (channels info) (sections info) (duration info)

    let snippet = take (numSeconds info 11) (toLazy $ fromBuffer test1)
        snippet2 = take (numSeconds info 11) (toLazy $ fromBuffer test2)
        snippet3 = take (numSeconds info 11) (toLazy $ fromBuffer test3)
        snippet4 = take (numSeconds info 11) (toLazy $ fromBuffer test4)

    let playEffect' f msg = playEffect snippet f msg info
    let playSnippet snippet' = play snippet' info

    let wooLong = LazyV.take (LazyV.length snippet4 * 20) $ LazyV.cycle snippet4

    let wooplusWhite = LazyV.zipWith (+) wooLong (white $ LazySize 8)

    let minisnippet = take 88200 $ LazyV.drop 88200 snippet2

    print minisnippet

    playSnippet minisnippet

    playEffect' (LazyV.map clip') "Noisy..."

    playEffect wooLong id "woo long" info

    playEffect wooplusWhite id "woo + white noise" info

    playEffect wooLong (LazyV.map (powerSigned 4)) "Clipped woo!" info

    playEffect' (\x -> amplify 10 $ envelopeVector x wooLong) "Envelope..."

    playEffect' (\x -> Distort.mixMulti [x, snippet2, snippet3]) "Mix multi songs..."

    playEffect' (Distort.distort powerSigned snippet2) "Distortion..."

    playEffect' (take (numSeconds info 11) . Loop.fade (0.5 :: Double) (numSeconds info 2) (numSeconds info 2)) "Fade loop..."

    playEffect' id "Original file..."

    -- fade in time, normal volume time, fade out time
    playEffect' (fadeInOut (numSeconds info 3) (numSeconds info 5) 
                (numSeconds info 3)) "Fade..."

    playEffect' (downsample2 (LazySize 8)) "This is apparently a downsample, I\
                                           \ must be doing it wrong. It sounds\
                                           \ funny though..."

    playEffect' (amplify 0.1) "Negative amplify..."

    playEffect' (envelope (white $ LazySize 8)) 
                "Splicing with some disgusting white noise..."

    playEffect' (take (numSeconds info 11) . Loop.simple (numSeconds info 1)
                (numSeconds info 3)) "Loop..."

    playEffect' reverse "Reversed..."

    playEffect' (mix snippet2) "Mixed..."

    playEffect' (run (numSeconds info 1) (1 :: Double)) "Echoes..."

    playEffect' (reverse' info) "Reversed sorta"

    playEffect' (LazyV.zipWith (+) snippet2) "Double...?"

    let avg = average snippet
    playEffect' (LazyV.map (\x -> if x > avg then x + 0.1 else x - 0.1)) "Noisy..."

    let newSnippet = static (numSeconds info 0.5) snippet
    playEffect' (mix newSnippet) "Delay..."
        

{- this is VERY bad - it's inefficent as hell, and there's got to be a better
way! fromBuffer returns a Data.StorableVector, but play wants a
Data.StorableVector.Lazy. For now this is a quick hack, but if performance
starts being needed, you should definitely fix this -}
toLazy :: (Storable a) => V.Vector a -> LazyV.Vector a
toLazy xs = LazyV.pack LazyV.defaultChunkSize $ V.unpack xs

{- need to set sample rate to sample rate * numchannels - plays in half speed
otherwise. I guess sample rate is per channel? -}
play :: LazyV.Vector Double -> Info -> IO ExitCode
play file info = simple LazyV.hPut (numberOfChannels numChannels)
                                   (samplerate info * numChannels) file
    where numChannels = channels info

numSeconds :: Info -> Double -> Int
numSeconds info n = round $ n * fromIntegral (samplerate info) * fromIntegral (channels info)

playEffect :: t -> (t -> LazyV.Vector Double) -> String -> Info -> IO ()
playEffect signal f msg info = do
    putStrLn msg
    void $ play (f signal) info
    Delay.delay oneSecond
    where oneSecond = 1000000

reverse' :: Synthesizer.Generic.Cut.Transform t => Info -> t -> t
reverse' info sig = append half (reverse half)
    where half = take (numSeconds info 5) sig

clip' :: (Fractional a, RealFrac a1) => a1 -> a
clip' a = round' a 1
    where round' num digits = (fromInteger . round $ num * (10^digits)) / (10.0^^digits)
