module FormM.Token where

data Token a -- == AlexPn
  = TokenInt {fooI::Int,    apn :: a}
  | TokenTop               {apn :: a}
  | TokenBot               {apn :: a}
  | TokenPrp               {apn :: a}
  | TokenP                 {apn :: a}
  | TokenNeg               {apn :: a}
  | TokenOB                {apn :: a}
  | TokenCB                {apn :: a}
  | TokenCon               {apn :: a}
  | TokenDis               {apn :: a}
  | TokenImpl              {apn :: a}
  | TokenEqui              {apn :: a}
  | TokenDia               {apn :: a}
  | TokenBox               {apn :: a}

  deriving (Eq,Show)
