module PaymentTests where

import Control.Monad
import Test.Framework (testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.Framework.Providers.HUnit
import Test.HUnit.Base
import Test.QuickCheck
import Fix
import Payment

instance Arbitrary PaymentService where
  arbitrary = liftM2 PaymentService arbitrary arbitrary

instance Arbitrary ChildPaymentService where
  arbitrary = liftM2 ChildPaymentService arbitrary arbitrary

newtype PaymentTypeFArb =
  PaymentTypeFArb (Fix PaymentTypeF) deriving (Show, Eq, Read)

instance Arbitrary PaymentTypeFArb where
  arbitrary = PaymentTypeFArb <$> oneof [genIndividual, genParent, genChild]
    where
      genIndividual = individualF <$> arbitrary
      genParent = parentF <$> arbitrary
      genChild = childF <$> arbitrary

paymentTests =
  [
    testGroup "Payment properties" [
      testProperty "Equals self" $ do
        PaymentTypeFArb p <- arbitrary
        return $ p === p
      ,
      testProperty "Show and read round-trips" $ do
        PaymentTypeFArb p <- arbitrary
        return $ p === read (show p)
    ],
    testGroup "Payment examples" $ hUnitTestToTests $ TestList [
      "Individual to JSON" ~:
        let i = individualF $ PaymentService "MasterCard" "Pay"

            actual = toJson i

            expected = PaymentJson "MasterCard" "Pay" False Nothing
        in  expected ~=? actual
      ,
      "Parent to JSON" ~:
        let p = parentF $ PaymentService "MasterCard" "Pay"

            actual = toJson p

            expected = PaymentJson "MasterCard" "Pay" True Nothing
        in  expected ~=? actual
      ,
      "Child to JSON" ~:
        let c =
              childF
              $ ChildPaymentService "12345"
              $ PaymentService "MasterCard" "Pay"
        
            actual = toJson c

            expected = PaymentJson "MasterCard" "Pay" False $ Just "12345"
        in  expected ~=? actual
    ]
  ]