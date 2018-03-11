--For holding the various types we'll need to use.
--Storing them here lets all our files use them.

--Add the other data types to the list once made
module Types (Token) where

--Template taken from the coursys. We can change if needed.
data Token = Comment [Char] |
             NumToken Int |
             AlphaNumToken [Char] |
             SpecialToken [Char] |
             Lparen | Rparen | Lbrak | Rbrak | LBrace | RBrace
             Equal | Semicolon | Arrow | Quote | Colon deriving Show

