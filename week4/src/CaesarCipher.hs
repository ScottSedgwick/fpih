module CaesarCipher where

-- Caesar's Cipher 
-- Jeremy.Singer@glasgow.ac.uk
-- great example for QuickCheck

import Data.Char

-- is this a letter to be ciphered?
shouldcipher :: Char -> Bool
shouldcipher c = isLetter c && isAscii c

-- enciphers single char at a time - NO WRAPPING
cipherchar :: Int -> Char -> Char
cipherchar shift c
 | shouldcipher c = chr $ ord c + shift
 | otherwise      = c

-- encipher a whole string
cipher :: Int -> [Char] -> [Char]
cipher shift = map (bettercipherchar shift) 

-- inverse of cipher function
decipher :: Int -> [Char] -> [Char]
decipher shift = cipher (-shift) 

-- should we wrap around the alphabet, if we shift past Z?
wraparound :: Int -> Char -> Bool
wraparound shift c 
 | isLower c && ord c + shift > ord 'z' = True
 | isUpper c && ord c + shift > ord 'Z' = True
 | otherwise = False

-- implementation of character substitution with wrapping
bettercipherchar :: Int -> Char -> Char
bettercipherchar shift c
 | shouldcipher c =  chr(ord c + adjustedshift)
 | otherwise      = c
 where adjustedshift = let shift' = shift `mod` 26
                       in if (wraparound shift' c)
                          then shift'-26
                          else shift'