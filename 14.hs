{-
MIT License

Copyright (c) 2018,2019 OleksandrZhabenko

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
-}

import System.CPUTime
import Control.Monad 
import System.Process (callCommand)

additionalF :: Num a => [a] -> a
additionalF [] = 0
additionalF [x] = x
additionalF (x:y:ys) = additionalF ((10*x + y):ys)
additionalF [x,y] = 10*x + y

stringToInteger :: String -> Integer
stringToInteger [] = 0
stringToInteger y = additionalF (stringToInteger' y)
     where stringToInteger' [] = [0]
           stringToInteger' (z:zs) = case z of
                   '0' -> 0:(stringToInteger' zs)
                   '1' -> 1:(stringToInteger' zs)
                   '2' -> 2:(stringToInteger' zs)
                   '3' -> 3:(stringToInteger' zs)
                   '4' -> 4:(stringToInteger' zs)
                   '5' -> 5:(stringToInteger' zs)
                   '6' -> 6:(stringToInteger' zs)
                   '7' -> 7:(stringToInteger' zs)
                   '8' -> 8:(stringToInteger' zs)
                   '9' -> 9:(stringToInteger' zs)
                   _ -> [0]
                   
basicNoteDuration :: IO Integer
basicNoteDuration = do 
               base <- readFile "base"
               let baseI = stringToInteger base in return baseI

durationF :: IO String -> IO Double
durationF y = do 
      z <- y
      basicNoteDurationS <- basicNoteDuration      
      let bn = case z of
              "03" -> (15*fromInteger basicNoteDurationS)/800
              "02" -> (7*fromInteger basicNoteDurationS)/400
              "01" -> (3*fromInteger basicNoteDurationS)/200
              "0" -> (fromInteger basicNoteDurationS)/100
              "13" -> (15*fromInteger basicNoteDurationS)/1600
              "12" -> (7*fromInteger basicNoteDurationS)/800              
              "11" -> (3*fromInteger basicNoteDurationS)/400
              "1" -> (fromInteger basicNoteDurationS)/200
              "23" -> (15*fromInteger basicNoteDurationS)/3200
              "22" -> (7*fromInteger basicNoteDurationS)/1600
              "21" -> (3*fromInteger basicNoteDurationS)/800              
              "2" -> (fromInteger basicNoteDurationS)/400
              "33" -> (15*fromInteger basicNoteDurationS)/6400
              "32" -> (7*fromInteger basicNoteDurationS)/3200
              "31" -> (3*fromInteger basicNoteDurationS)/1600
              "3" -> (fromInteger basicNoteDurationS)/800
              "43" -> (15*fromInteger basicNoteDurationS)/12800
              "42" -> (7*fromInteger basicNoteDurationS)/6400
              "41" -> (3*fromInteger basicNoteDurationS)/3200
              "4" -> (fromInteger basicNoteDurationS)/1600
              "53" -> (15*fromInteger basicNoteDurationS)/25600
              "52" -> (7*fromInteger basicNoteDurationS)/12800
              "51" -> (3*fromInteger basicNoteDurationS)/6400
              "5" -> (fromInteger basicNoteDurationS)/3200
              "63" -> (15*fromInteger basicNoteDurationS)/51200
              "62" -> (7*fromInteger basicNoteDurationS)/25600
              "61" -> (3*fromInteger basicNoteDurationS)/12800
              "6" -> (fromInteger basicNoteDurationS)/6400
              "73" -> (15*fromInteger basicNoteDurationS)/102400
              "72" -> (7*fromInteger basicNoteDurationS)/51200
              "71" -> (3*fromInteger basicNoteDurationS)/25600
              "7" -> (fromInteger basicNoteDurationS)/12800
              _ -> error "No_note\n"
                  in return bn
                  
ggg :: Double -> Integer -> Double
ggg a b = (fromInteger $! round (4400*a**(fromInteger b)))/10

pitchFrequency :: String -> String
pitchFrequency x = case x of
              "21" -> "440"
              "22" -> "466.2"
              "30" -> "523.3"
              "31" -> "554.4"
              "3" -> "493.9"
              "40" -> "622.3"
              "41" -> "659.3"
              "4" -> "587.3"
              "51" -> "740"
              "52" -> "784"
              "53" -> "830.6"
              "5" -> "698.5"
              "60" -> "932.3"
              "61" -> "987.8"
              "6" -> "880"
              "70" -> "1108.7"
              "71" -> "1174.7"
              "72" -> "1244.5"
              "7" -> "1046.5"
              "80" -> "1396.9"
              "81" -> "1480"
              "8" -> "1318.5"
              "90" -> "1661.2"
              "91" -> "1760"
              "9" -> "1568"
              "-41" -> "110"
              "-40" -> "116.5"
              "-4" -> "123.5"
              "-31" -> "130.8"
              "-30" -> "138.6"
              "-3" -> "146.8"
              "-21" -> "155.6"
              "-20" -> "164.8"
              "-2" -> "174.6"
              "-12" -> "185"
              "-11" -> "196"
              "-10" -> "207.7"
              "-1" -> "220"
              "-01" -> "233.1"
              "-00" -> "246.9"
              "00" -> "277.2"
              "01" -> "293.7"
              "02" -> "311.1"
              "0" -> "261.6"
              "10" -> "349.2"
              "11" -> "370"
              "1" -> "329.6"
              "20" -> "415.3"
              "2" -> "392"
              otherwise -> error "No_note\n"

-- let a = exp (1/12*log 2) in show (dropWhile (< 110) (takeWhile (<= 1760) (map (ggg a) [-32..32])))

tripleString :: IO String -> IO String
tripleString noteInfo = do
                nI <- noteInfo
                let k = words nI in 
                 thirdElem k
                  where
                      thirdElem (x:y:z:zs) = if y == "t" 
                               then do 
                                   duration2 <- durationF $! return x
                                   let s1 = " synth " ++ (show duration2) ++ " triangle " ++ (pitchFrequency z) ++ " gain -n -10\n" in return s1
                               else do
                                   duration2 <- durationF $! return x
                                   let s2 = " synth " ++ (show duration2) ++ " sine " ++ (pitchFrequency z) ++ " gain -n -10\n" in return s2
                      thirdElem (x:xs) = do
                                   duration2 <- durationF $! return x
                                   let s3 = " synth " ++ (show duration2) ++ " sine 440 bandreject 440 440\n" in return s3
                      thirdElem _ = return ("No_note\n")     

zFunction :: IO String -> IO Integer -> IO String
zFunction noteInfo time = do
                nI <- tripleString noteInfo
                t <- time
                let t1 = 10000000000 + (t `div` 10000000) in
                     return ("sox -n " ++ (show t1) ++ ".flac" ++ nI)

main = replicateM_ 20000 (zFunction getLine getCPUTime >>= callCommand) 
