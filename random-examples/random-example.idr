
-- ##################
-- Random Example
-- ##################

data ATMState = Ready | CardInserted | InSession

data AL : String -> ATMState -> Type where
        FOO: AL "a" Ready
        BAR: Nat -> AL s CardInserted
        ZZUP: AL "b" InSession

foo : AL "a" Ready
foo = FOO

zzup : AL "b" InSession
zzup = ZZUP

paco : AL "" CardInserted
paco = BAR 0

pacoMultiVerse : AL "" CardInserted
pacoMultiVerse = BAR 1

juan : AL "k kiere" CardInserted
juan = BAR 0

