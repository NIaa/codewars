 module ISO where
  
  import Data.Void
  -- A type of `Void` have no value.
  -- So it is impossible to construct `Void`,
  -- unless using undefined, error, unsafeCoerce, infinite recursion, etc
  -- And there is a function
  -- absurd :: Void -> a
  -- That get any value out of `Void`
  -- We can do this becuase we can never have void in the zeroth place.
  
  -- so, when are two type, `a` and `b`, considered equal?
  -- a definition might be, it is possible to go from `a` to `b`,
  -- and from `b` to `a`.
  -- Going a roundway trip should leave you the same value.
  -- Unfortunately it is virtually impossible to test this in Haskell.
  -- This is called Isomorphism.
  
  type ISO a b = (a -> b, b -> a)
  
  -- given ISO a b, we can go from a to b
  substL :: ISO a b -> (a -> b)
  substL = fst
  
  -- and vice versa
  substR :: ISO a b -> (b -> a)
  substR = snd
  
  -- There can be more than one ISO a b
  isoBool :: ISO Bool Bool
  isoBool = (id, id)
  
  isoBoolNot :: ISO Bool Bool
  isoBoolNot = (not, not)
  
  -- isomorphism is reflexive
  refl :: ISO a a
  refl = (id, id)
  
  -- isomorphism is symmetric
  symm :: ISO a b -> ISO b a
  symm (a, b) = (b, a)
  
  -- isomorphism is transitive
  trans :: ISO a b -> ISO b c -> ISO a c
  trans (ab, ba) (bc, cb) = (bc . ab, ba . cb) 
  
  -- We can combine isomorphism:
  isoTuple :: ISO a b -> ISO c d -> ISO (a, c) (b, d)
  isoTuple (ab, ba) (cd, dc) = 
    (\(a, c) -> (ab a, cd c), \(b, d) -> (ba b, dc d))
  
  isoList :: ISO a b -> ISO [a] [b]
  isoList (a, b) = ((a <$>), (b <$>))
  
  isoMaybe :: ISO a b -> ISO (Maybe a) (Maybe b)
  isoMaybe (a, b) = ((a <$>), (b <$>))
  
  isoEither :: ISO a b -> ISO c d -> ISO (Either a c) (Either b d)
  isoEither (ab, ba) (cd, dc) = (fun ab cd, fun ba dc)
    where fun f g ex = case ex of
            (Left  x) -> Left  $ f x
            (Right x) -> Right $ g x
  
  isoFunc :: ISO a b -> ISO c d -> ISO (a -> c) (b -> d)
  isoFunc (ab, ba) (cd, dc) = ((\ac -> cd . ac . ba), (\bd -> dc . bd . ab))

  -- Going another way is hard (and is generally impossible)
  isoUnMaybe :: ISO (Maybe a) (Maybe b) -> ISO a b
  isoUnMaybe (mamb, mbma) = (\a -> unJust $ mamb $ Just a, substL $ isoUnMaybe $ symm (mamb, mbma)) where
    unJust (Just a) = a
    unJust Nothing = unVar (mamb Nothing)
    -- determines whether the var is Nothing or not.
    unVar (Just a) = a
    unVar Nothing = undefined

  -- Remember, for all valid ISO, converting and converting back
  -- Is the same as the original value.
  -- You need this to prove some case are impossible.
  
  -- We cannot have
  -- isoUnEither :: ISO (Either a b) (Either c d) -> ISO a c -> ISO b d.
  -- Note that we have
  
  isoEU :: ISO (Either [()] ()) (Either [()] Void)
  --isoEU = (fun id undefined, fun id (\b -> ()))
  --  where fun f g ex = case ex of
  --          (Left  x) -> Left  $ f x
  --          (Right x) -> Right $ g x    


  isoEU = (f, g) where
    f (Right ()) = Left []
    f (Left x ) = Left $ (): x
    g (Left []) = Right ()
    g( Left (_:xs)) = Left xs

  -- where (), the empty tuple, has 1 value, and Void has 0 value
  -- If we have isoUnEither,
  -- We have ISO () Void by calling isoUnEither isoEU
  -- That is impossible, since we can get a Void by substL on ISO () Void
  -- So it is impossible to have isoUnEither
  
  -- And we have isomorphism on isomorphism!
  isoSymm :: ISO (ISO a b) (ISO b a)
  isoSymm = (symm, symm)