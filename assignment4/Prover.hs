-- For applying the wang algorithm to our parsed tokens

-- Explanation of wang algorithm notation and purpose:
-- What we have is a list of hypotheses and a list of goals.
-- These are notated in the form H |- G, where H is the hypotheses and G is the goals.
-- For both cases, they are a list of porpositional formulas. Often they are comma separated,
-- But in the case of H, that's the equivalent of them all being a conjunction.
-- For the case of G, they're equivalent of them all being a disjunction.
-- Because of this, we can simply use disjunctions and conjunctions to deliniate the different formulas.
-- That said, the comma has been added as a valid token. Though its usage would require some context aware parsing we may not want to deal with.

module Prover where

import Types
import Data.List
-- COVER EDGE CASE FOR EMPTY HYPOTHESES LIST AT EACH STEP
-- (Cover it for goals too, but less angrily)

-- Rule 1a: if the goal list has a proposition that is also in the hypothesis list, the conjecture is true.
-- Rule 1b: if both the goal list and hypothesis list contain propositions only, and no common propositions, the conjecture is disproven.
-- True indicates success, false indicates failure. Nothing indicates unknown.

isJustProp :: [PF] -> Bool
isJustProp([]) = True
isJustProp ((Prop cur):rest) = isJustProp(rest)
isJustProp (rest) = False

wang1 :: ProofConjecture -> Maybe Bool
wang1 proof@(Conjecture hypos goals)
   | (length (intersect hypos goals)) == 0 = Just True
   | isJustProp(hypos) && isJustProp(goals)= Just False
   | otherwise                             = Nothing

-- Rule 2a: H, ~f |- G reduces to H |- G,f
-- Rule 2b: H |- G, ~f reduces to H, f |- G
wang2 :: ProofConjecture -> [ProofConjecture]

wang2 proof@(Conjecture hypos@((Neg pf):hx) goals@(g:gx)) =
    (Conjecture hx (goals ++ (pf:[]))) : []

wang2 proof@(Conjecture hypos@(h:hx) goals@((Neg pf):gx)) =
    (Conjecture (hypos ++ (pf:[])) gx) : []

wang2 proof = error("Prover logic error in wang2 :(")

-- Rule 3a: H, f1 ^ f2 |- G reduces to H, f1, f2 |- G
-- Rule 3b: H |- G, f1 ^ f2 reduces to H |- G, f1 and H |- G, f2
wang3 :: ProofConjecture -> [ProofConjecture]

wang3 proof@(Conjecture hypos@((Conj pf1 pf2):hx) goals@(g:gx)) =
    (Conjecture (hypos ++ (pf1:pf2:[])) goals) : []

wang3 proof@(Conjecture hypos@(h:hx) goals@((Conj pf1 pf2):gx)) =
    (Conjecture hypos (gx ++ (pf1:[]))) :
    (Conjecture hypos (gx ++ (pf1:[]))) : []
wang3 proof = error("Prover logic error in wang3 :(")

-- Rule 4a: H, f1 v f2 |- G reduces to H, f1 |- G and H, f2 |- G
-- Rule 4b: H |- G, f1 v f2 reduces to H |- G, f1, f2
wang4 :: ProofConjecture -> [ProofConjecture]
wang4 proof@(Conjecture hypos@((Disj pf1 pf2):hx) goals@(g:gx)) =
    (Conjecture (hx ++ (pf1:[])) goals) :
    (Conjecture (hx ++ (pf2:[])) goals) : []

wang4 proof@(Conjecture hypos@(h:hx) goals@((Disj pf1 pf2):gx)) =
    (Conjecture hypos (gx ++ (pf1:pf2:[]))) : []

wang4 proof = error("Prover logic error in wang 4 :(")

-- Rule 5a: H, f1 -> f2 |- G reduces to H, f2 |- G and H |- G, f1
-- Rule 5b: H |- G, f1 -> f2 reduces to H, f1 |- G, f2
wang5 :: ProofConjecture -> [ProofConjecture]
wang5 proof@(Conjecture hypos@((Imp pf1 pf2):hx) goals@(g:gx)) =
    (Conjecture (hx ++ (pf2:[])) goals) :
    (Conjecture hx (goals ++ (pf1:[]))) : []

wang5 proof@(Conjecture hypos@(h:hx) goals@((Imp pf1 pf2):gx)) =
    (Conjecture (hypos ++ (pf1:[])) (gx ++ (pf2:[]))) : []

wang5 proof = error("Prover logic error in wang 5 :(")

-- Rule 6a: H, f1 == f2 |- G reduces to H, f1, f2 |- G and H |- G, f1, f2
-- Rule 6b: H |- G, f1 == f2 reduces to H, f1 |- G, f2 and H, f2 |- G, f1
wang6 :: ProofConjecture -> [ProofConjecture]
wang6 proof@(Conjecture hypos@((Equiv pf1 pf2):hx) goals@(g:gx)) =
    (Conjecture (hx ++ (pf1:pf2:[])) goals) :
    (Conjecture hx (goals ++ (pf1:pf2:[]))) : []

wang6 proof@(Conjecture hypos@(h:hx) goals@((Equiv pf1 pf2):gx)) =
    (Conjecture (hypos ++ (pf1:[])) (gx ++ (pf2:[]))) :
    (Conjecture (hypos ++ (pf2:[])) (gx ++ (pf1:[]))) : []

wang6 proof = error("Prover logic error in wang 6 :(")

{-
    A proof conjecture is proven or disproven as follows.

    1. We form a list of conjectures to be proven, initially consisting of just the one given conjecture.

    2. In each step, we take one conjecture from the list and proceed as follows:

    - If the conjecture is proven by Rule 1a, we remove it from the list of conjectures. If there are no more conjectures, then the process is complete and the proof succeeds.
    - If the conjecture is disproven by Rule 1b, then the process is complete and the proof fails.

    - Otherwise, we choose a composite formula from either the hypothesis or goal lists to apply a reduction step.

    - The reduction step uses one of the rules 2a through 6b to generate one or two new conjectures.
    - The new conjectures are added to the conjecture list and we repeat the overall process.
-}

-- Performs one iteration of step 2 of the above process
-- Returns either the 1 or 2 new conjectures to add, no conjectures to show a success, or Nothing to show a fail.
wangStep :: ProofConjecture -> Maybe [ProofConjecture]
wangStep proof
  | wang1(proof) == Just True = Just []
  | wang1(proof) == Just False= Nothing
  | otherwise                 = Just (wangReduce(proof))
-- Selects and performs a reduction step on a conjecture
wangReduce :: ProofConjecture -> [ProofConjecture]

wangReduce proof@(Conjecture hypos@((Neg   pf):hx) goals@(g:gx)) =
    wang2 proof
wangReduce proof@(Conjecture hypos@((Conj  pf1 pf2):hx) goals@(g:gx)) =
    wang3 proof
wangReduce proof@(Conjecture hypos@((Disj  pf1 pf2):hx) goals@(g:gx)) =
    wang4 proof
wangReduce proof@(Conjecture hypos@((Imp   pf1 pf2):hx) goals@(g:gx)) =
    wang5 proof
wangReduce proof@(Conjecture hypos@((Equiv pf1 pf2):hx) goals@(g:gx)) =
    wang6 proof

wangReduce proof@(Conjecture hypos@(h:hx) goals@((Neg   pf):gx)) =
    wang2 proof
wangReduce proof@(Conjecture hypos@(h:hx) goals@((Conj  pf1 pf2):gx)) =
    wang3 proof
wangReduce proof@(Conjecture hypos@(h:hx) goals@((Disj  pf1 pf2):gx)) =
    wang4 proof
wangReduce proof@(Conjecture hypos@(h:hx) goals@((Imp   pf1 pf2):gx)) =
    wang5 proof
wangReduce proof@(Conjecture hypos@(h:hx) goals@((Equiv pf1 pf2):gx)) =
    wang6 proof

-- No more goals, we're dead somehow. Failure state.
wangReduce proof@(Conjecture hypos@(hx) goals@([])) = []
-- No more hypotheses, we're dead somehow. Failure state.
wangReduce proof@(Conjecture hypos@([]) goals@(gx)) = []
