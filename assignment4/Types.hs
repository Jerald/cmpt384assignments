-- For storing our types

module Types where

-- <Letter> is any letter, as one would expect, except lowercase v's
-- <Arrow>  is '->' and is for implications
-- <Hat>    is the caret character, '^', used for conjunctions
-- <Tilde>  is the tilde character, '~', and is for negations
-- <Vee>    is a lowercase 'v', used for disjunctions
-- <Equal>  is the double equals sign, '==', and is for equivlances
-- <LBrack> is a left bracket, "("
-- <RBrack> is a right bracket, ")"
-- <Turnstile> is the "|-" characters. It's used to separate hypothesis and goals.
-- <Comma> is a comma, ",". It can be used to deliniate formulas. Optional to implement. See notes at top of Prover.hs
data Token = Letter Char | Arrow | Hat | Tilde | Vee | Equal | LBrack | RBrack | Turnstile | Comma deriving Show

-- Propositional Formula data type
data PF = Prop Char | Neg PF | Conj PF PF | Disj PF PF | Imp PF PF | Equiv PF PF deriving Show

-- Represents a proof conjecture
-- The first list is the hypotheses, the second is the goals
data ProofConjecture = Conjecture [PF] [PF] deriving Show

