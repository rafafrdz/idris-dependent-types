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


data ATMcmd: (ty: Type) -> ATMState -> ATMState -> Type where
    InsertCard : ATMcmd () Ready CardInserted
    EjectCard: ATMcmd () st Ready
    GetPIN: ATMcmd PIN CardInserted CardInserted
    CheckPIN: (p:PIN) -> ATMcmd PINCheck CardInserted (Main.chkPIN (Main.isCorrect p))
    GetAmount: ATMcmd Nat st st
    Dispense: (amount: Nat) -> ATMcmd () InSession InSession
    

readPIN : IO PIN
readPIN = do s <- getLine
             pure (parsePIN s)


-- ##################
-- Examples
-- ##################

setPIN : (m:Int) -> ATMcmd PINCheck CardInserted (chkPIN (isCorrect (parsePIN (intToString m))))
setPIN m = CheckPIN (parsePIN (intToString m))

atmOK : ATMcmd PINCheck CardInserted InSession
atmOK = CheckPIN [1,2,3,4]

atmKO : ATMcmd PINCheck CardInserted CardInserted
atmKO = CheckPIN [7,7,0,9]