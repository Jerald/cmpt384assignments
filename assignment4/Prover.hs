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

-- Rule 1a: if the goal list has a proposition that is also in the hypothesis list, the conjecture is true.
-- Rule 1b: if both the goal list and hypothesis list contain propositions only, and no common propositions, the conjecture is disproven.

-- Rule 2a: H, ~f |- G reduces to H |- G,f
-- Rule 2b: H |- G, ~f reduces to H, f |- G

-- Rule 3a: H, f1 ^ f2 |- G reduces to H, f1, f2 |- G
-- Rule 3b: H |- G, f1 ^ f2 reduces to H |- G, f1 and H |- G, f2

-- Rule 4a: H, f1 v f2 |- G reduces to H, f1 |- G and H, f2 |- G
-- Rule 4b: H |- G, f1 v f2 reduces to H |- G, f1, f2

-- Rule 5a: H, f1 -> f2 |- G reduces to H, f2 |- G and H |- G, f1
-- Rule 5b: H |- G, f1 -> f2 reduces to H, f1 |- G, f2

-- Rule 6a: H, f1 == f2 |- G reduces to H, f1, f2 |- G and H |- G, f1, f2
-- Rule 6b: H |- G, f1 == f2 reduces to H, f1 |- G, f2 and H, f2 |- G, f1

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