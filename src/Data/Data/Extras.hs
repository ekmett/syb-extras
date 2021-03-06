{-# LANGUAGE Rank2Types, GeneralizedNewtypeDeriving, MultiParamTypeClasses, FlexibleInstances, TypeOperators #-}
module Data.Data.Extras
  (
  -- * Data.Data
  -- ** Kind: *
    module Data.Data
  -- ** Kind: * -> *
  , Data1(..)
  , fromConstr1
  , fromConstrB1
  , fromConstrM1
  -- ** Kind: * -> * -> *
  , Data2(..)
  , fromConstr2
  , fromConstrB2
  , fromConstrM2
  -- * Combinators
  -- * Lifting @k@ in @'gfoldl'{n} k@
  , liftK
  , liftK2
  -- * Lifting @f@ in @'gunfold'{n} f@
  , liftF
  , liftF2
  ) where

import Data.Data
import Data.Maybe
import Control.Monad
import Data.Eq.Type

newtype ID x = ID { unID :: x }
newtype CONST c a = CONST { unCONST :: c }
data    Qi q a = Qi { _qiCount :: Int , unQi :: Maybe q }
newtype Qr r a = Qr { unQr  :: r -> r }
newtype Mp m x = Mp { unMp :: m (x, Bool) }

class Typeable1 f => Data1 f where
  gfoldl1 :: Data a => (forall d b. Data d => c (d -> b) -> d -> c b) -> (forall g. g -> c g) -> f a -> c (f a)
  gfoldl1 _ z = z

  gunfold1 :: Data a => (forall b r. Data b => c (b -> r) -> c r) -> (forall r. r -> c r) -> Constr -> c (f a)

  toConstr1 :: Data a => f a -> Constr

  dataTypeOf1 :: Data a => f a -> DataType

  dataCast1_1 :: (Typeable1 t, Data a) => (forall d. Data d => c (t d)) -> Maybe (c (f a))
  dataCast1_1 _ = Nothing

  dataCast2_1 :: (Typeable2 t, Data a) => (forall d e. (Data d, Data e) => c (t d e)) -> Maybe (c (f a))
  dataCast2_1 _ = Nothing

  gmapT1 :: Data a => (forall b. Data b => b -> b) -> f a -> f a
  gmapT1 f x0 = unID (gfoldl1 (\(ID c) x -> ID (c (f x))) ID x0)

  gmapQl1 :: Data a => (r -> r' -> r) -> r -> (forall d. Data d => d -> r') -> f a -> r
  gmapQl1 o r f = unCONST . gfoldl1 (\c x -> CONST $ unCONST c `o` f x) (\_ -> CONST r)

  gmapQr1 :: Data a => (r' -> r -> r) -> r -> (forall d. Data d => d -> r') -> f a -> r
  gmapQr1 o r0 f x0 = unQr (gfoldl1 (\(Qr c) x -> Qr (\r -> c (f x `o` r))) (const (Qr id)) x0) r0

  gmapQ1 :: Data a => (forall d. Data d => d -> u) -> f a -> [u]
  gmapQ1 f = gmapQr1 (:) [] f

  gmapQi1 :: Data a => Int -> (forall d. Data d => d -> u) -> f a -> u
  gmapQi1 i f x = fromJust $ unQi $ gfoldl1 (\(Qi i' q) a -> Qi (i'+1) (if i==i' then Just (f a) else q))
                                            (\_ -> Qi 0 Nothing)
                                            x

  gmapM1 :: (Monad m, Data a) => (forall d. Data d => d -> m d) -> f a -> m (f a)
  gmapM1 f = gfoldl1 (\c x -> do c' <- c; liftM c' (f x)) return

  gmapMp1 :: (MonadPlus m, Data a) => (forall d. Data d => d -> m d) -> f a -> m (f a)
  gmapMp1 f x = unMp (gfoldl1 k z x) >>= \(x',b) ->
                if b then return x' else mzero
    where
      z g = Mp (return (g,False))
      k (Mp c) y
        = Mp ( c >>= \(h, b) ->
                 (f y >>= \y' -> return (h y', True))
                 `mplus` return (h y, b)
             )

  gmapMo1 :: (MonadPlus m, Data a) => (forall d. Data d => d -> m d) -> f a -> m (f a)
  gmapMo1 f x = unMp (gfoldl1 k z x) >>= \(x',b) ->
                if b then return x' else mzero
    where
      z g = Mp (return (g,False))
      k (Mp c) y
        = Mp ( c >>= \(h,b) -> if b
                        then return (h y, b)
                        else (f y >>= \y' -> return (h y',True))
                             `mplus` return (h y, b)
             )


fromConstr1 :: (Data1 f, Data a) => Constr -> f a
fromConstr1 = fromConstrB1 undefined

fromConstrB1 :: (Data1 f, Data a) => (forall d. Data d => d) -> Constr -> f a
fromConstrB1 f = unID . gunfold1 (\c -> ID (unID c f)) ID

fromConstrM1 :: (Monad m, Data1 f, Data a) => (forall d. Data d => m d) -> Constr -> m (f a)
fromConstrM1 f = gunfold1 (\c -> ap c f) return

instance Data1 [] where
  gfoldl1 = gfoldl
  toConstr1 = toConstr
  gunfold1 = gunfold
  dataTypeOf1 = dataTypeOf
  dataCast1_1 f = gcast1 f

instance Data1 Maybe where
  gfoldl1 = gfoldl
  toConstr1 = toConstr
  gunfold1 = gunfold
  dataTypeOf1 = dataTypeOf
  dataCast1_1 f = gcast1 f
instance Data a => Data1 (Either a) where
  gfoldl1 = gfoldl
  toConstr1 = toConstr
  gunfold1 = gunfold
  dataTypeOf1 = dataTypeOf
  dataCast1_1 f = gcast1 f
instance Data a => Data1 ((,) a) where
  gfoldl1 = gfoldl
  toConstr1 = toConstr
  gunfold1 = gunfold
  dataTypeOf1 = dataTypeOf
  dataCast1_1 f = gcast1 f


class Typeable2 f => Data2 f where
  gfoldl2 :: (Data a, Data x) => (forall d b. Data d => c (d -> b) -> d -> c b) -> (forall g. g -> c g) -> f a x -> c (f a x)
  gfoldl2 _ z = z

  gunfold2 :: (Data a, Data x) => (forall b r. Data b => c (b -> r) -> c r) -> (forall r. r -> c r) -> Constr -> c (f a x)

  toConstr2 :: (Data a, Data x) => f a x -> Constr

  dataTypeOf2 :: (Data a, Data x) => f a x -> DataType

  dataCast1_2 :: (Typeable1 t, Data a, Data x) => (forall d. Data d => c (t d)) -> Maybe (c (f a x))
  dataCast1_2 _ = Nothing

  dataCast2_2 :: (Typeable2 t, Data a, Data x) => (forall d e. (Data d, Data e) => c (t d e)) -> Maybe (c (f a x))
  dataCast2_2 _ = Nothing

  gmapT2 :: (Data a, Data x) => (forall b. Data b => b -> b) -> f a x -> f a x
  gmapT2 f x0 = unID (gfoldl2 (\(ID c) x -> ID (c (f x))) ID x0)

  gmapQl2 :: (Data a, Data x) => (r -> r' -> r) -> r -> (forall d. Data d => d -> r') -> f a x -> r
  gmapQl2 o r f = unCONST . gfoldl2 (\c x -> CONST $ unCONST c `o` f x) (\_ -> CONST r)

  gmapQr2 :: (Data a, Data x) => (r' -> r -> r) -> r -> (forall d. Data d => d -> r') -> f a x -> r
  gmapQr2 o r0 f x0 = unQr (gfoldl2 (\(Qr c) x -> Qr (\r -> c (f x `o` r))) (const (Qr id)) x0) r0

  gmapQ2 :: (Data a, Data x) => (forall d. Data d => d -> u) -> f a x -> [u]
  gmapQ2 f = gmapQr2 (:) [] f

  gmapQi2 :: (Data a, Data x) => Int -> (forall d. Data d => d -> u) -> f a x -> u
  gmapQi2 i f x = fromJust $ unQi $ gfoldl2 (\(Qi i' q) a -> Qi (i'+2) (if i==i' then Just (f a) else q))
                                            (\_ -> Qi 0 Nothing)
                                            x

  gmapM2 :: (Monad m, Data a, Data x) => (forall d. Data d => d -> m d) -> f a x -> m (f a x)
  gmapM2 f = gfoldl2 (\c x -> do c' <- c; liftM c' (f x)) return

  gmapMp2 :: (MonadPlus m, Data a, Data x) => (forall d. Data d => d -> m d) -> f a x -> m (f a x)
  gmapMp2 f x = unMp (gfoldl2 k z x) >>= \(x',b) ->
                if b then return x' else mzero
    where
      z g = Mp (return (g,False))
      k (Mp c) y
        = Mp ( c >>= \(h, b) ->
                 (f y >>= \y' -> return (h y', True))
                 `mplus` return (h y, b)
             )

  gmapMo2 :: (MonadPlus m, Data a, Data x) => (forall d. Data d => d -> m d) -> f a x -> m (f a x)
  gmapMo2 f x = unMp (gfoldl2 k z x) >>= \(x',b) ->
                if b then return x' else mzero
    where
      z g = Mp (return (g,False))
      k (Mp c) y
        = Mp ( c >>= \(h,b) -> if b
                        then return (h y, b)
                        else (f y >>= \y' -> return (h y',True))
                             `mplus` return (h y, b)
             )

fromConstr2 :: (Data2 f, Data a, Data x) => Constr -> f a x
fromConstr2 = fromConstrB2 undefined

fromConstrB2 :: (Data2 f, Data a, Data x) => (forall d. Data d => d) -> Constr -> f a x
fromConstrB2 f = unID . gunfold2 (\c -> ID (unID c f)) ID

fromConstrM2 :: (Monad m, Data2 f, Data a, Data x) => (forall d. Data d => m d) -> Constr -> m (f a x)
fromConstrM2 f = gunfold2 (\c -> ap c f) return

instance Data2 Either where
  gfoldl2 = gfoldl
  toConstr2 = toConstr
  gunfold2 = gunfold
  dataTypeOf2 = dataTypeOf
  dataCast2_2 f = gcast2 f

instance Data2 (,) where
  gfoldl2 = gfoldl
  toConstr2 = toConstr
  gunfold2 = gunfold
  dataTypeOf2 = dataTypeOf
  dataCast2_2 f = gcast2 f


-- horrible hackery
newtype WrappedData1 f a = WrapData1 { _unwrapData1 :: f a } deriving (Iso (f a))
class Iso a b where
  iso :: f a -> f b
  osi :: f b -> f a

instance Iso a a where
  iso = id
  osi = id

iso1 :: f a := WrappedData1 f a
iso1 = Refl iso

liftK :: (Data1 d1, Data a) => (forall d b. Data d => c (d -> b) -> d -> c b) -> c (d1 a -> b') -> d1 a -> c b'
liftK k cf d = k (subst (lift2 iso1) cf) (WrapData1 d)

liftF :: (Data1 b1, Data a) => (forall b r. Data b => c (b -> r) -> c r) -> c (b1 a -> r') -> c r'
liftF f cf = f (subst (lift2 iso1) cf)

data1 :: c (f a) -> c (WrappedData1 f a)
data1 = iso

undata1 :: c (WrappedData1 f a) -> c (f a)
undata1 = osi

instance Typeable1 f => Typeable1 (WrappedData1 f) where
  typeOf1 (WrapData1 a) = typeOf1 a

instance Data1 f => Data1 (WrappedData1 f) where
  gfoldl1 k z (WrapData1 a) = data1 $ gfoldl1 k z a
  gunfold1 k z c = undata1 $ gunfold1 k z c
  toConstr1 (WrapData1 a) = toConstr1 a
  dataTypeOf1 (WrapData1 a) = dataTypeOf1 a

-- these lie and let us convert instances
instance (Data1 f, Data a) => Data (WrappedData1 f a) where
  gfoldl k z (WrapData1 a) = data1 $ gfoldl1 k z a
  gunfold k z c = undata1 $ gunfold1 k z c
  toConstr (WrapData1 a) = toConstr1 a
  dataTypeOf (WrapData1 a) = dataTypeOf1 a

newtype WrappedData2 f a b = WrapData2 { _unwrapData2 :: f a b } deriving (Iso (f a b))

iso2 :: f a b := WrappedData2 f a b
iso2 = Refl iso

liftK2 :: (Data2 d2, Data a, Data x) => (forall d b. Data d => c (d -> b) -> d -> c b) -> c (d2 a x -> b') -> d2 a x -> c b'
liftK2 k cf d = k (subst (lift2 iso2) cf) (WrapData2 d)

liftF2 :: (Data2 b2, Data a, Data x) => (forall b r. Data b => c (b -> r) -> c r) -> c (b2 a x -> r') -> c r'
liftF2 f cf = f (subst (lift2 iso2) cf)

data2 :: c (f a b) -> c (WrappedData2 f a b)
data2 = iso

undata2 :: c (WrappedData2 f a b) -> c (f a b)
undata2 = osi

instance Typeable2 f => Typeable2 (WrappedData2 f) where
  typeOf2 (WrapData2 a) = typeOf2 a

instance Data2 f => Data2 (WrappedData2 f) where
  gfoldl2 k z (WrapData2 a) = data2 $ gfoldl2 k z a
  gunfold2 k z c = undata2 $ gunfold1 k z c
  toConstr2 (WrapData2 a) = toConstr2 a
  dataTypeOf2 (WrapData2 a) = dataTypeOf2 a

instance (Data2 f, Data a) => Data1 (WrappedData2 f a) where
  gfoldl1 k z (WrapData2 a) = data2 $ gfoldl2 k z a
  gunfold1 k z c = undata2 $ gunfold2 k z c
  toConstr1 (WrapData2 a) = toConstr2 a
  dataTypeOf1 (WrapData2 a) = dataTypeOf2 a

-- these lie and let us convert instances
instance (Data2 f, Data a, Data b) => Data (WrappedData2 f a b) where
  gfoldl k z (WrapData2 a) = data2 $ gfoldl2 k z a
  gunfold k z c = undata2 $ gunfold2 k z c
  toConstr (WrapData2 a) = toConstr2 a
  dataTypeOf (WrapData2 a) = dataTypeOf2 a
