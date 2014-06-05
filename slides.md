% `ec`: A Haskell Elliptic-Curve Cryptography Library 
% Created by Brandon Azad and Nikhil Desai

# Intro
* A new library for ECC.
* Advantages:
    * Modular
    * Cleanly implemented
    * Readily exensible
* Provides performant implementations while retaining flexibility

# Motivation
* Cryptography research: creating new _schemes_ for encryption/authentication
    * Innovative mixing and matching of different primitives
    * Making incremental improvements to existing ones
    * Collaboration/extensibility necessary
* Don't want researchers to reinvent the wheel
    * Want a clean, easily-utilized framework for testing
    * Insipired by Cryptol
* Existing libraries kind of suck

# Why Haskell?
* Clean specifications possible without sacrificing performance
* Good for mathematical abstraction
* We're in this class

# Language of ECC
* Elliptic curves 
    * Equation of the form $y^2=x^3+ax^2+b$ 
* Group structure possible (insert image)

# Language of ECC
* Elliptic curves can be defined over any field: $\mathbb{R},$ $\mathbb{Z}_p,$ $\mathbb{Z}_{2^k},$ etc.
* Typically work with elliptic curves over *finite* fields - interesting case
    * Finite representations possible
    * Discrete log problem hard

$$x_R=\left(\frac{y_P-y_Q}{x_P-x_Q}\right)^2-x_P-x_Q;$$ $$y_R=\left(\frac{y_P-y_Q}{x_P-x_Q}\right)(x_P-x_R)-y_P.$$ 

# ElGamal encryption 
* ECC provides *fast* exponentiation, but discrete log is hard!
* Take $g$ public, $\alpha$ private, $m$ message:
    * Pick $r\leftarrow\mathbb{Z}_p$
    * Send $(g^r, g^{\alpha r}g^m)$ 

# Library design 
* Key typeclasses
    * `Field`
    * `EllipticCurvePoint` (multi-parameter)
    * `EllipticCurve`  (multi-parameter)

# The `Field` typeclass
~~~~ {#mycode .haskell }
class (Eq f) => Field f where
  type FieldParameter f
  zero :: f
  one :: f
  add :: FieldParameter f -> f -> f -> f
  neg :: FieldParameter f -> f -> f
  rep :: FieldParameter f -> Integer -> f -> f
  mul :: FieldParameter f -> f -> f -> f
  sqr :: FieldParameter f -> f -> f 
  inv :: FieldParameter f -> f -> f
  pow :: FieldParameter f -> f -> Integer -> f
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# `FieldOperations f`
~~~~ {#mycode .haskell} 
data FieldOperations f = FieldOperations
  { (+) :: f -> f -> f       
  , (.-) :: f -> f          
  , (-) :: f -> f -> f     
  , (#) :: Integer -> f -> f
  , (*) :: f -> f -> f
  , (^.) :: f -> f
  , (./) :: f -> f
  , (/) :: f -> f -> f
  , (^) :: f -> Integer -> f
  }

ops :: (Field f) => FieldParameter f -> FieldOperations f
ops p = FieldOperations (add p) (neg p) (sub p) (rep p)
                        (mul p) (sqr p) (inv p) (div p) 
                        (pow p)
~~~~~~~~~~~

# Field of integers mod $p$ 
~~~~ {#mycode .haskell}
instance (KnownNat n) => Field (Integer `Mod` n) where

  type FieldParameter (Integer `Mod` n) = ()

  zero = 0
  one = 1
  add _ = (+)
  neg _ = negate
  sub _ = (-)
  rep _ n a = fromInteger n * a
  mul _ = (*)
  inv _ a = toMod $ 
      invm' (natVal (Proxy :: Proxy n)) (unMod a)
~~~~~~~

# The `EllipticCurvePoint` type relation
~~~~ {#mycode .haskell}
class EllipticCurvePoint c p where
  toAffine :: (Field f) => EC c f -> p c f -> Affine c f
  fromAffine :: (Field f) => EC c f -> Affine c f -> p c f
  zeroPoint :: (Field f) => EC c f -> p c f
  isZeroPoint :: (Field f) => EC c f -> p c f -> Bool
  affineXY :: (Field f) => EC c f -> p c f -> Maybe (f, f)

data Affine (c :: * -> *) f = 
     Affine { affineX, affineY :: f } 
     | AffinePointAtInfinity
~~~~~~ 

# The `EllipticCurve` type relation
~~~~ {#mycode .haskell}
class (EllipticCurvePoint c p) => EllipticCurve c p where
  onCurve :: (Field f) => EC c f -> p c f -> Bool
  add :: (Field f) => EC c f -> p c f -> p c f -> p c f
  double :: (Field f) => EC c f -> p c f -> p c f
  negate :: (Field f) => EC c f -> p c f -> p c f
  multiply :: (Field f) => EC c f -> Int -> Integer 
                           -> p c f -> p c f
~~~~~~ 

# Implementing Weierstrass 
~~~~ {#mycode .haskell}
data Weierstrass f = Weierstrass { weierstrassA, weierstrassB :: f }

instance EllipticCurvePoint Weierstrass Jacobian where
  toAffine (EC _ FieldOperations {..}) (Jacobian x y z)
    | z == zero = AffinePointAtInfinity
    | otherwise =
      let iz3 = (./) (z ^ 3)
      in Affine (x * iz3 * z) (y * iz3)

  fromAffine _ (Affine !x !y)        = Jacobian x   y   one
  fromAffine _ AffinePointAtInfinity = Jacobian one one zero

~~~~~~~
-----------------
~~~~ {#mycode .haskell}
instance EllipticCurve Weierstrass Jacobian where
  onCurve (EC (Weierstrass a b) FieldOperations {..}) (Jacobian !x !y !z) =
    let z2 = z^2
        z4 = z2^2
        z6 = z2 * z4
    in y^2 == (x^3) + (a * x * z4) + (b * z6)

  add c@(EC _ FieldOperations {..})
      p1@(Jacobian !x1 !y1 !z1) p2@(Jacobian !x2 !y2 !z2)
    | z1 == zero = p3 `seq` p2
    | z2 == zero = p3 `seq` p1
    | otherwise  = p3 `seq` p3
    where
      p3 = jacobianAdd c x1 y1 z1 x2 y2 z2

  double c@(EC _ FieldOperations {..}) p@(Jacobian !x1 !y1 !z1) =
    jacobianDouble' c x1 y1 z1 (z1^2)

  negate c@(EC _ FieldOperations {..}) p@(Jacobian !x1 !y1 !z1) =
    Jacobian x1 ((.-) y1) z1
~~~~~~~~

# Optimization/efficiency 
* "Pluggable" library - insert your own code for timing resistance/efficiency

# "Interfacing" with OpenSSL

# Future work 
--> 
