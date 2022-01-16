
-- ##################
-- Random Example
-- ##################

data State = Hungry | Coding | Chill

data MOOD : State -> String -> Type where
        ZZUP: MOOD Coding "idris"
        FOO: MOOD Hungry s
        BAR: Nat -> MOOD Chill s
        
-- ##################################
-- ##################################

-- Singleton
zzup : MOOD Coding "idris"
zzup = ZZUP

-- Different dependent types, same constructor
foo : MOOD Hungry "pizza"
foo = FOO

foo' : MOOD Hungry "apple"
foo' = FOO

-- ##################################
-- Same type but not same constructor

paco : MOOD Chill "netflix"
paco = BAR 0

pacoMultiVerse : MOOD Chill "netflix"
pacoMultiVerse = BAR 1

-- Different dependent types, same constructor
juan : MOOD Chill "music"
juan = BAR 0

