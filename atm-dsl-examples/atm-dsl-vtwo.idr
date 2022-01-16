module Dsl

import Data.Vect

PIN : Type
PIN = Vect 4 Int

secretPIN : PIN
secretPIN = [1,2,3,4]



-- ##################
-- Utils
-- ##################
toInt : String -> Int
toInt s = the Int (cast s)

intToString: Int -> String
intToString m = the String (cast m)

charToString : Char -> String
charToString c = the String (cast c)

toVect : (n: Nat) -> List Int -> Vect n Int
toVect Z xs = []
toVect (S k) (x::xs) = x :: toVect k xs

toPIN : List Int -> PIN
toPIN xs = toVect 4 xs

parsePIN : String -> PIN
parsePIN s = toPIN (map toInt (map charToString (unpack s)))



-- ##################
-- Datatypes
-- ##################

data ATMState = Ready | CardInserted | InSession
data PINCheck = CorrectPIN | IncorrectPIN

chkPIN : PINCheck -> ATMState
chkPIN CorrectPIN = InSession
chkPIN IncorrectPIN = CardInserted

isCorrect : PIN -> PINCheck
isCorrect m = if m == secretPIN then CorrectPIN else IncorrectPIN


data ATMcmd: (ty: Type) -> ATMState -> (ty -> ATMState) -> Type where
    InsertCard : ATMcmd () Ready (const CardInserted)
    EjectCard: ATMcmd () st (const Ready)
    GetPIN: ATMcmd PIN CardInserted (const CardInserted)
    CheckPIN: PIN -> ATMcmd PINCheck CardInserted Dsl.chkPIN
    GetAmount: ATMcmd Nat st (const st)
    Dispense: (amount: Nat) -> ATMcmd () InSession (const InSession)
    Message : String -> ATMcmd () st (const st)

    -- Monad
    Pure : (res: ty) -> ATMcmd ty (st_final res) st_final
    (>>=) : ATMcmd a st1 st2 -> ((res:a) -> ATMcmd b (st2 res) stf) -> ATMcmd b st1 stf



-- ##################
-- Interpreter
-- ##################

runATM : ATMcmd res sti stf -> IO res
runATM InsertCard = do putStrLn "Please insert your card (press enter)"
                       x <- getLine
                       pure ()

runATM EjectCard = putStrLn "Eject card.."
runATM GetPIN = do putStrLn "Insert PIN: "
                   s <- getLine
                   pure (parsePIN s)

runATM (CheckPIN pin) = if pin == secretPIN
                        then pure CorrectPIN
                        else pure IncorrectPIN

runATM GetAmount = do putStrLn "How much would you like?  "
                      amount <- getLine
                      pure (cast amount)

runATM (Dispense amount) = putStrLn ("Here is $" ++ show amount ++ ". Bye!")
runATM (Message msg) = putStrLn msg
runATM (Pure res) = pure res
runATM (x >>= f) = do res <- runATM x
                      runATM (f res)



-- ##################
-- ATM
-- ##################
--
-- To execute program type: ' :exec runATM atm '

atm : ATMcmd () Ready (const Ready)
atm = do InsertCard
         pin <- GetPIN
         pinOK <- CheckPIN pin
         Message "Checking Card"
         case pinOK of
              CorrectPIN => do cash <- GetAmount
                               Dispense cash
                               EjectCard
              IncorrectPIN => do Message "Incorrect PIN"
                                 EjectCard
