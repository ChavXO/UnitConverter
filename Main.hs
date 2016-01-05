module Main where

import Data.Maybe
import Data.List
import Control.Monad

import Graphics.UI.WX hiding (Event)
import Reactive.Banana
import Reactive.Banana.WX
import Text.Printf


main :: IO ()
main = start converter

lengthUnits = ["miles", "kilometers"]
weightUnits = ["pounds", "kilograms"]
units = ["Length", "Weight"]

converter :: IO ()
converter = do
    f <- frame [text := "Unit Converter"
                , clientSize := sz 500 150]
    p <- panel f []
    unitCon   <- comboBox p [items := units, selection := 0]
    conBox1   <- comboBox p [items := lengthUnits, selection := 0]
    conBox2   <- comboBox p [items := lengthUnits, selection := 1]
    input1    <- textCtrl p []
    input2    <- textCtrl p []
    set unitCon [ clientSize := sz 460 50
                , position := pt 20 10]
    set input1  [ clientSize := sz 220 30
                , position := pt 20 70]
    set input2  [ clientSize := sz 220 30
                , position := pt 260 70]
    set conBox1 [ clientSize := sz 220 30 
                , position := pt 20 110]
    set conBox2 [ clientSize := sz 220 30 
                , position := pt 260 110]    
    let networkDescription :: MomentIO ()
        networkDescription = do
        
        binput1    <- behaviorText input1 "0"
        binput2    <- behaviorText input2 "0"
        unit1      <- behavior conBox1 selection
        unit2      <- behavior conBox2 selection
        conversion <- behavior unitCon selection
        
        let rate = 0.7 :: Double
            withString f input firstUnit secondUnit quantity
                = maybe "--" (printf "%.2f") . fmap 
                (\x -> f (x, firstUnit, secondUnit, quantity)) 
                $ listToMaybe [x | (x,"") <- reads input]

            -- define output values in terms of input values
            output1, output2 :: Behavior String
            output1 = withString (convert) <$> 
                            binput2 <*> unit1 <*> unit2 <*> conversion
            output2 = withString (convert) <$> 
                            binput1 <*> unit1 <*> unit2 <*> conversion
    
        sink input1   [text :== output1]
        sink input2   [text :== output2] 

    network <- compile networkDescription    
    actuate network

readNumber s = listToMaybe [x | (x,"") <- reads s]  

convert :: (Double, Int, Int, Int) -> Double
convert (n, box1, box2, quant) = convert' n to from
    where quantity = units !! quant
          unitList = case quantity of
                         "Length" -> lengthUnits
                         "Weight" -> weightUnits
          to = unitList !! box1
          from = unitList !! box2
          
convert' amount "kilometers" "miles" = amount / 1.60934
convert' amount "miles" "kilometers" = amount / 1.60934
convert' amount _ _ = amount
