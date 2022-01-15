module Main where

main :: IO ()
main = putStrLn "Hello, Cardano: Ivy token!"

type Ada = Double
type Ivy = Double

data Pool = Pool {
    ada :: Ada,
    ivy :: Ivy
} deriving (Show)

data Wallet = Wallet{
    wAda :: Ada,
    wIvy :: Ivy
} deriving (Show)

def :: (Pool, Wallet)
def = 
    (
        (Pool 1000 1000),
        (Wallet 1000 1000)
    )

k = 1000000
ada * ivy = k

sellIvy :: (Pool, Wallet) -> Ivy -> (Pool, Wallet)
sellIvy (p, w) a 
    | a > 0 && wIvy w >= a 
          = let 
                newIvy = ivy p + a
                -- targetAda * newIvy ?= k => targetAda = k / newIvy, targetAda will be less
                targetAda = k / newIvy
                -- targetAda * newIvy = k
                receiveAda = abs (ada p - targetAda)
            in 
                (
                    p {
                        ada = targetAda,
                        ivy = newIvy
                    },
                    w {
                        wAda = wAda w + receiveAda,
                        wIvy = wIvy w - a
                    }
                )
    | otherwise             = (p, w)

sellAda :: (Pool, Wallet) -> Ivy -> (Pool, Wallet)
sellAda (p, w) a 
    | a > 0 && wAda w >= a 
          = let 
                newAda = ada p + a
                targetIvy = k / newAda
                receiveIvy = abs (ivy p - targetIvy)
            in 
                (
                    p {
                        ada = newAda,
                        ivy = targetIvy
                    },
                    w {
                        wAda = wAda w - a,
                        wIvy = wIvy w + receiveIvy
                    }
                )
    | otherwise             = (p, w)

priceIvy :: Pool -> Double
priceIvy p = ada p / ivy p