-- | Utilities for n-eithers: sums types with more than two terms built from nested eithers.
-- |
-- | Nested eithers arise naturally in sum combinators. You shouldn't 
-- | represent sum data using nested eithers, but if combinators you're working with
-- | create them, utilities in this module will allow to to more easily work
-- | with them, including translating to and from more traditional sum types.
-- | 
-- | ```purescript
-- | data Color = Red Number | Green Number | Blue Number
-- |
-- | toEither3 :: Color -> Either3 Number Number Number
-- | toEither3 = either3 Red Green Blue
-- |
-- | fromEither3 :: Either3 Number Number Number -> Color
-- | fromEither3 (Red   v) = either1of3
-- | fromEither3 (Green v) = either2of3
-- | fromEither3 (Blue  v) = either3of3
-- | ```
module Data.Either.Nested where
  import Data.Either

  type Either2 a z = Either a z
  type Either3 a b z = Either (Either2 a b) z
  type Either4 a b c z = Either (Either3 a b c) z
  type Either5 a b c d z = Either (Either4 a b c d) z
  type Either6 a b c d e z = Either (Either5 a b c d e) z
  type Either7 a b c d e f z = Either (Either6 a b c d e f) z
  type Either8 a b c d e f g z = Either (Either7 a b c d e f g) z
  type Either9 a b c d e f g h z = Either (Either8 a b c d e f g h) z
  type Either10 a b c d e f g h i z = Either (Either9 a b c d e f g h i) z

  -- Either2
  either1of2 :: forall a b. a -> Either2 a b
  either1of2 = Left

  either2of2 :: forall a b. b -> Either2 a b
  either2of2 = Right

  -- Either3
  either1of3 :: forall a b c. a -> Either3 a b c
  either1of3 v = (Left (Left v))

  either2of3 :: forall a b c. b -> Either3 a b c
  either2of3 v = (Left (Right v))

  either3of3 :: forall a b c. c -> Either3 a b c
  either3of3 v = Right v

  -- Either4
  either1of4 :: forall a b c d. a -> Either4 a b c d
  either1of4 v = (Left (Left (Left v)))

  either2of4 :: forall a b c d. b -> Either4 a b c d
  either2of4 v = (Left (Left (Right v)))

  either3of4 :: forall a b c d. c -> Either4 a b c d
  either3of4 v = (Left (Right v))

  either4of4 :: forall a b c d. d -> Either4 a b c d
  either4of4 v = Right v

  -- Either5
  either1of5 :: forall a b c d e. a -> Either5 a b c d e
  either1of5 v = (Left (Left (Left (Left v))))

  either2of5 :: forall a b c d e. b -> Either5 a b c d e
  either2of5 v = (Left (Left (Left (Right v))))

  either3of5 :: forall a b c d e. c -> Either5 a b c d e
  either3of5 v = (Left (Left (Right v)))

  either4of5 :: forall a b c d e. d -> Either5 a b c d e
  either4of5 v = (Left (Right v))

  either5of5 :: forall a b c d e. e -> Either5 a b c d e
  either5of5 v = Right v

  -- Either6
  either1of6 :: forall a b c d e f. a -> Either6 a b c d e f
  either1of6 v = (Left (Left (Left (Left (Left v)))))

  either2of6 :: forall a b c d e f. b -> Either6 a b c d e f
  either2of6 v = (Left (Left (Left (Left (Right v)))))

  either3of6 :: forall a b c d e f. c -> Either6 a b c d e f
  either3of6 v = (Left (Left (Left (Right v))))

  either4of6 :: forall a b c d e f. d -> Either6 a b c d e f
  either4of6 v = (Left (Left (Right v)))

  either5of6 :: forall a b c d e f. e -> Either6 a b c d e f
  either5of6 v = (Left (Right v))

  either6of6 :: forall a b c d e f. f -> Either6 a b c d e f
  either6of6 v = Right v

  -- Either7
  either1of7 :: forall a b c d e f g. a -> Either7 a b c d e f g
  either1of7 v = (Left (Left (Left (Left (Left (Left v))))))

  either2of7 :: forall a b c d e f g. b -> Either7 a b c d e f g
  either2of7 v = (Left (Left (Left (Left (Left (Right v))))))

  either3of7 :: forall a b c d e f g. c -> Either7 a b c d e f g
  either3of7 v = (Left (Left (Left (Left (Right v)))))

  either4of7 :: forall a b c d e f g. d -> Either7 a b c d e f g
  either4of7 v = (Left (Left (Left (Right v))))

  either5of7 :: forall a b c d e f g. e -> Either7 a b c d e f g
  either5of7 v = (Left (Left (Right v)))

  either6of7 :: forall a b c d e f g. f -> Either7 a b c d e f g
  either6of7 v = (Left (Right v))

  either7of7 :: forall a b c d e f g. g -> Either7 a b c d e f g
  either7of7 v = Right v

  -- Either8
  either1of8 :: forall a b c d e f g h. a -> Either8 a b c d e f g h
  either1of8 v = (Left (Left (Left (Left (Left (Left (Left v)))))))

  either2of8 :: forall a b c d e f g h. b -> Either8 a b c d e f g h
  either2of8 v = (Left (Left (Left (Left (Left (Left (Right v)))))))

  either3of8 :: forall a b c d e f g h. c -> Either8 a b c d e f g h
  either3of8 v = (Left (Left (Left (Left (Left (Right v))))))

  either4of8 :: forall a b c d e f g h. d -> Either8 a b c d e f g h
  either4of8 v = (Left (Left (Left (Left (Right v)))))

  either5of8 :: forall a b c d e f g h. e -> Either8 a b c d e f g h
  either5of8 v = (Left (Left (Left (Right v))))

  either6of8 :: forall a b c d e f g h. f -> Either8 a b c d e f g h
  either6of8 v = (Left (Left (Right v)))

  either7of8 :: forall a b c d e f g h. g -> Either8 a b c d e f g h
  either7of8 v = (Left (Right v))

  either8of8 :: forall a b c d e f g h. h -> Either8 a b c d e f g h
  either8of8 v = Right v

  -- Either9
  either1of9 :: forall a b c d e f g h i. a -> Either9 a b c d e f g h i
  either1of9 v = (Left (Left (Left (Left (Left (Left (Left (Left v))))))))

  either2of9 :: forall a b c d e f g h i. b -> Either9 a b c d e f g h i
  either2of9 v = (Left (Left (Left (Left (Left (Left (Left (Right v))))))))

  either3of9 :: forall a b c d e f g h i. c -> Either9 a b c d e f g h i
  either3of9 v = (Left (Left (Left (Left (Left (Left (Right v)))))))

  either4of9 :: forall a b c d e f g h i. d -> Either9 a b c d e f g h i
  either4of9 v = (Left (Left (Left (Left (Left (Right v))))))

  either5of9 :: forall a b c d e f g h i. e -> Either9 a b c d e f g h i
  either5of9 v = (Left (Left (Left (Left (Right v)))))

  either6of9 :: forall a b c d e f g h i. f -> Either9 a b c d e f g h i
  either6of9 v = (Left (Left (Left (Right v))))

  either7of9 :: forall a b c d e f g h i. g -> Either9 a b c d e f g h i
  either7of9 v = (Left (Left (Right v)))

  either8of9 :: forall a b c d e f g h i. h -> Either9 a b c d e f g h i
  either8of9 v = (Left (Right v))

  either9of9 :: forall a b c d e f g h i. i -> Either9 a b c d e f g h i
  either9of9 v = Right v

  -- Either10
  either1of10 :: forall a b c d e f g h i j. a -> Either10 a b c d e f g h i j
  either1of10 v = (Left (Left (Left (Left (Left (Left (Left (Left (Left v)))))))))

  either2of10 :: forall a b c d e f g h i j. b -> Either10 a b c d e f g h i j
  either2of10 v = (Left (Left (Left (Left (Left (Left (Left (Left (Right v)))))))))

  either3of10 :: forall a b c d e f g h i j. c -> Either10 a b c d e f g h i j
  either3of10 v = (Left (Left (Left (Left (Left (Left (Left (Right v))))))))

  either4of10 :: forall a b c d e f g h i j. d -> Either10 a b c d e f g h i j
  either4of10 v = (Left (Left (Left (Left (Left (Left (Right v)))))))

  either5of10 :: forall a b c d e f g h i j. e -> Either10 a b c d e f g h i j
  either5of10 v = (Left (Left (Left (Left (Left (Right v))))))

  either6of10 :: forall a b c d e f g h i j. f -> Either10 a b c d e f g h i j
  either6of10 v = (Left (Left (Left (Left (Right v)))))

  either7of10 :: forall a b c d e f g h i j. g -> Either10 a b c d e f g h i j
  either7of10 v = (Left (Left (Left (Right v))))

  either8of10 :: forall a b c d e f g h i j. h -> Either10 a b c d e f g h i j
  either8of10 v = (Left (Left (Right v)))

  either9of10 :: forall a b c d e f g h i j. i -> Either10 a b c d e f g h i j
  either9of10 v = (Left (Right v))

  either10of10 :: forall a b c d e f g h i j. j -> Either10 a b c d e f g h i j
  either10of10 v = Right v

  either2 :: forall a b z. (a -> z) -> (b -> z) -> Either2 a b -> z
  either2 = either

  either3 :: forall a b c z. (a -> z) -> (b -> z) -> (c -> z) -> Either3 a b c -> z
  either3 a b z = either (either2 a b) z

  either4 :: forall a b c d z. (a -> z) -> (b -> z) -> (c -> z) -> (d -> z) -> Either4 a b c d -> z
  either4 a b c z = either (either3 a b c) z 

  either5 :: forall a b c d e z. (a -> z) -> (b -> z) -> (c -> z) -> (d -> z) -> (e -> z) -> Either5 a b c d e -> z
  either5 a b c d z = either (either4 a b c d) z 

  either6 :: forall a b c d e f z. (a -> z) -> (b -> z) -> (c -> z) -> (d -> z) -> (e -> z) -> (f -> z) -> Either6 a b c d e f -> z
  either6 a b c d e z = either (either5 a b c d e) z 

  either7 :: forall a b c d e f g z. (a -> z) -> (b -> z) -> (c -> z) -> (d -> z) -> (e -> z) -> (f -> z) -> (g -> z) -> Either7 a b c d e f g -> z
  either7 a b c d e f z = either (either6 a b c d e f) z 

  either8 :: forall a b c d e f g h z. (a -> z) -> (b -> z) -> (c -> z) -> (d -> z) -> (e -> z) -> (f -> z) -> (g -> z) -> (h -> z) -> Either8 a b c d e f g h -> z
  either8 a b c d e f g z = either (either7 a b c d e f g) z

  either9 :: forall a b c d e f g h i z. (a -> z) -> (b -> z) -> (c -> z) -> (d -> z) -> (e -> z) -> (f -> z) -> (g -> z) -> (h -> z) -> (i -> z) -> Either9 a b c d e f g h i -> z
  either9 a b c d e f g h z = either (either8 a b c d e f g h) z

  either10 :: forall a b c d e f g h i j z. (a -> z) -> (b -> z) -> (c -> z) -> (d -> z) -> (e -> z) -> (f -> z) -> (g -> z) -> (h -> z) -> (i -> z) -> (j -> z) -> Either10 a b c d e f g h i j -> z
  either10 a b c d e f g h i z = either (either9 a b c d e f g h i) z