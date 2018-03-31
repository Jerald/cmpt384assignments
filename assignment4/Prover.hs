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