{-# LANGUAGE DeriveGeneric #-}
module Payment where

-- This example simply shows that you can derive the catamorphism even for a
-- completely ad hoc sum type like PaymentService, originally described in
-- http://blog.ploeh.dk/2016/11/28/easy-domain-modelling-with-types
-- http://blog.ploeh.dk/2018/06/18/church-encoded-payment-types
import Text.Read
import Control.Applicative
import Data.Functor.Classes
import Data.Aeson
import GHC.Generics
import Fix

data PaymentService = PaymentService {
    paymentServiceName :: String
  , paymentServiceAction :: String
  } deriving (Show, Eq, Read)

data ChildPaymentService = ChildPaymentService {
    originalTransactionKey :: String
  , parentPaymentService :: PaymentService
  } deriving (Show, Eq, Read)

data PaymentTypeF c =
    IndividualF PaymentService
  | ParentF PaymentService
  | ChildF ChildPaymentService
  deriving (Show, Eq, Read)

instance Functor PaymentTypeF where
  fmap _ (IndividualF ps) = IndividualF ps
  fmap _     (ParentF ps) = ParentF ps
  fmap _     (ChildF cps) = ChildF cps

instance Eq1 PaymentTypeF where
  liftEq _ (IndividualF x) (IndividualF y) = x == y
  liftEq _     (ParentF x)     (ParentF y) = x == y
  liftEq _      (ChildF x)      (ChildF y) = x == y
  liftEq _             _              _  = False

instance Show1 PaymentTypeF where
  liftShowsPrec _ _ = showsPrec

instance Read1 PaymentTypeF where
  liftReadPrec _ _ = readIndividual <|> readParent <|> readChild
    where
      readIndividual = readData (readUnaryWith readPrec "IndividualF" IndividualF)
      readParent = readData (readUnaryWith readPrec "ParentF" ParentF)
      readChild = readData (readUnaryWith readPrec "ChildF" ChildF)

individualF :: PaymentService -> Fix PaymentTypeF
individualF = Fix . IndividualF

parentF :: PaymentService -> Fix PaymentTypeF
parentF = Fix . ParentF

childF :: ChildPaymentService -> Fix PaymentTypeF
childF = Fix . ChildF

-- This is the catamorphism for Fix PaymentTypeF
paymentF :: (PaymentService -> c) ->
            (PaymentService -> c) ->
            (ChildPaymentService -> c) ->
            Fix PaymentTypeF -> c
paymentF fi fp fc = cata alg
  where alg (IndividualF ps) = fi ps
        alg     (ParentF ps) = fp ps
        alg     (ChildF cps) = fc cps

data PaymentJson = PaymentJson {
    name :: String
  , action :: String
  , startRecurrent :: Bool
  , transactionKey :: Maybe String
  } deriving (Show, Eq, Generic)

instance ToJSON PaymentJson

toJson :: Fix PaymentTypeF -> PaymentJson
toJson =
  paymentF
    (\(PaymentService n a) -> PaymentJson n a False Nothing)
    (\(PaymentService n a) -> PaymentJson n a True Nothing)
    (\(ChildPaymentService k (PaymentService n a)) ->
        PaymentJson n a False $ Just k)