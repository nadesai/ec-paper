Intro
=====
Cryptography is a field that thrives on diversity. 
New developments in the field often rely upon creating innovative new _schemes_ for encryption or authentication -
developing such schemes requires innovatively mixing and matching different primitives,
or making incremental improvements to existing ones. 
However, cryptography research often depends on efficient 
profiling and testing of these new schemes in real-world cryptographic situations. Developing a scheme
is not the same as implementing it, and the quality of first-pass research-quality code leaves much to be desired.
In particular, such code tends not to be resistant to various cryptographic attacks, and is not
the best possible presentation of such code. 

Motivation
====
Our library is designed to fill in this gap. It is targeted at cryptographic researchers, and aims to
give them a framework in which implementing crytographic protocols is simple, but performant code is still
output. In this way, they can build new techniques in a modular framework. 

For the purposes of this project, our library focuses on various techniques in _elliptic-curve cryptography_ (ECC).  
We chose to focus on ECC for a number of reasons. First, the existing library for ECC in Haskell, Marcel Fourne's `hecc`,
appears to have a few defects with it - it has not been updated in a long time, and it appears to not be timing-attack resistant,
according to various profiles we have run.

Second is that elliptic curve cryptography depends on a great deal of interesting mathematical operations, and thus
building an elliptic-curve library provides an exercise in mathematical modeling via Haskell. Mathematical programming
in Haskell is especially straightforward because of its purely-functional approach and its expressive type system,
making the process of building new curves literally just "doing the math." Moreover, thanks to the Haskell type system,
we can make sure that defining new curves or switching to a new field is as simple as changing a type signature.

We hope that in making the definition of new crytographic and elliptic-curve techniques easier, we will allow profiling
and testing of these new techniques to be performed in a consistent, safe, and practical manner, to give researchers
a sense not just of the mathematics of their protocols, but also of their real-world usability. 

Language of elliptic curve cryptography [TODO: by Thursday]
====
Elliptic-curve cryptography is a major subfield of cryptography that relies heavily on the language of group
and field theory, as well as affine and projective geometry. 

* Explain the math behind ECC at a high level
* Define elliptic curves and group operation over them
* Define some standard curves (Weierstrass) - note computations involving field inversions, etc. etc.

Use/Library Design
===
The library is structured around a series of *core typeclasses* that model the behavior of the mathematics of 
elliptic-curve cryptography. These typeclasses include `Field`, `Curve`, and `Point`, which model the obvious 

# Implementation
The implementation of this library was mostly an exercise in mathematical modelling with Haskell.
In particular, defining the notions of underlying fields, curve groups, and points within them 
were all easily done using the system of typeclasses and type relations available to us. 
For efficiency, we utilize some code specific to the GHC system, including interactions with
unboxed types and foreign calls out to bit-manipulating C code. However, the core abstractions
are pure Haskell, and fairly easy to understand.

## Key typeclasses
The key typeclasses of our code are the `Field`, `Curve`, and `Point` typeclasses, and the  
relations of `EllipticCurvePoint` and `EllipticCurve` between the typeclasses of this form.
Let's go through each of them in turn.

### The `Field` typeclass
The `Field` typeclass is a very standard one, defining the key operations possible on a field.
This includes the real or complex numbers, but more importantly the class of finite fields of prime
order. Our typeclass is defined by the minimum specifications required for operations on fields 
- addition, negation, multiplication, inversion, and the 0 and 1 elements. 
However, computing specific calculations directly from the definition can be tedious - for example,
squaring an element can often be done much faster in a specific field than simply multiplying the element
by itself. Thus, we allow the researcher to provide "shortcuts" for subtraction, division, repeated addition, 
squaring, and exponentiation (repeated squaring).

One of the important requirements of the `Field` typeclass is the necessity to define a type called `FieldParameter` - 
which holds auxiliary information about the field. For example, a field over a prime $p$ would have to store 
the value of $p$ as auxiliary information, since the definitions of every operation depend on its value. 

### The `EllipticCurve` type relation
The EllipticCurve type relation is one between curves and points alone, but with a constraint that the curve
must be over a field. In practice, it defines the group operations necessary to work over an elliptic curve - 
namely point addition, repeated point addition, and point negation - it then builds up a timing-attack-resistant
system for point multiplication using the Montgomery ladder from these definitions.  

### The 

## "Interfacing" with OpenSSL
* Cool fields e.g. P521
* Writing up an actual curve in our library
* Experience of writing up curves using this library
* Ease of profiling 

# Future work
