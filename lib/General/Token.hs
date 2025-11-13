module General.Token where

data Token a -- == AlexPn
  = TokenInt    {i::Int,    apn :: a}
  | TokenString {s::String, apn :: a}
  | TokenTop               {apn :: a}
  | TokenBot               {apn :: a}
  | TokenPrp               {apn :: a}
  | TokenNeg               {apn :: a}
  | TokenOB                {apn :: a}
  | TokenCB                {apn :: a}
  | TokenCon               {apn :: a}
  | TokenDis               {apn :: a}
  | TokenImpl              {apn :: a}
  | TokenEqui              {apn :: a}
  | TokenDiaL              {apn :: a}
  | TokenDiaR              {apn :: a}
  | TokenBoxL              {apn :: a}
  | TokenBoxR              {apn :: a}

  deriving (Eq,Show)
