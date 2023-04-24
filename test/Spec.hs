import Data.DeBruijn (SomeDB (SomeDB), SomeSNat (SomeSNat), fromDB, inject, raise)
import GHC.TypeNats (fromSNat)
import Test.QuickCheck (quickCheck)

prop_injectIsIdentity :: SomeSNat -> SomeDB -> Bool
prop_injectIsIdentity (SomeSNat m) (SomeDB _bound index) =
  fromDB (inject m index) == fromDB index

prop_raiseIsAdd :: SomeSNat -> SomeDB -> Bool
prop_raiseIsAdd (SomeSNat m) (SomeDB _bound index) =
  fromDB (raise m index) == fromSNat m + fromDB index

main :: IO ()
main = do
  quickCheck prop_injectIsIdentity
  quickCheck prop_raiseIsAdd
