Sections:

* Intro [TODO: by Thursday]
* Motivation [TODO: by Thursday]
  * Interesting
  * Targeted from crypto research perspective
  * Can build new techniques in a modular framework
  * Building new curves is just defining the math.
  * Switching out new curves is just changing type signatures (inputs to PCF)
  * Ease of profiling 
* Language of elliptic curve cryptography [TODO: by Thursday]
  * Explain the math behind ECC at a high level
  * Define elliptic curves and group operation over them
  * Define some standard curves (Weierstrass) - note computations involving field inversions, etc. etc.
* Use/Library Design
  * Typeclasses
  * Field, Curve, Point
* Implementation
  * "Interfacing" with OpenSSL
    * Cool fields e.g. P521
  * Writing up an actual curve in our library
  * Experience of writing up curves using this library
  * Ease of profiling 
* Future work
