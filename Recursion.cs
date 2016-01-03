using System;
using System.Collections;
using System.Collections.Generic;

namespace Recursion
{
    /*
     * relation : * -> *
     * relation a = (a, a) -> *
     * 
     * (==) : (a:*) -> relation a
     * (==) a (x, y) = {
     *   transport : (F:a -> *) -> F x -> F y
     * }
     * 
     * category : *
     * category = {
     *   object : *
     *   (~>) : relation object
     *   
     *   null : (x:object) -> (x ~> x)
     *   (>>) : (x:object, y:object, z:object) -> (x ~> y, y ~> z) -> (x ~> z)
     *   
     *   identityLeft : (x:object, y:object) -> (f:x ~> y) -> (null x >> f) == f
     *   identityRight : (x:object, y:object) -> (f:x ~> y) -> (f >> null y) == f
     *   associativity : (w:object, x:object, y:object, z:object) -> (f:w ~> x, g:x ~> y, h:y ~> z) -> ((f >> g) >> h) == (f >> (g >> h))
     * }
     * 
     * functor : relation category
     * functor (a, b) = {
     *   map : object a -> object b
     *   fmap : (x:object a, y:object a) -> (x ~> y) -> (map x ~> map y)
     *   
     *   identity : (x:object a) -> fmap (x, x) (null a x) == null b (map x)
     *   compose : (x:object a, y:object a, z:object a) -> (f:x ~> y, g:y ~> z) -> fmap (x, z) (f >> g) == (fmap (x, y) f >> fmap (y, z) g)
     * }
     * 
     * natural : (a:category, b:category) -> relation (functor (a, b))
     * natural (a, b) (F, G) = {
     *   transform : (x:object a) -> map F x ~> map G x
     *   
     *   commute : (x:object a, y:object a) -> (f:x ~> y) -> (fmap F (x, y) f >> transform y) == (transform x >> fmap G (x, y) f)
     * }
     * 
     * algebra : (a:category) -> functor (a, a) -> *
     * algebra a F = {
     *   carrier : object a
     *   reduce : map F carrier ~> carrier
     * }
     * 
     * homomorphism : (a:category) -> (F:functor (a, a)) -> relation (algebra a F)
     * homomorphism a F (r, s) = {
     *   morph : carrier r ~> carrier s
     *   
     *   preserving : (reduce r >> morph) == (fmap F (carrier r, carrier s) morph >> reduce s)
     * }
     * 
     * initial : category -> *
     * initial a = {
     *   bottom : object a
     *   project : (x:object a) -> (bottom ~> x)
     *   
     *   factor : (x:object a, y:object) -> (f:x ~> y) -> (project x >> f) == project y
     * }
     * 
     * algebraic : (a:category) -> functor (a, a) -> category
     * algebraic a F = {
     *   object = algebra a F
     *   (~>) = homomorphism a F
     *   
     *   null r = { morph = null a (carrier r) }
     *   (>>) (r, s, t) (f, g) = { morph = morph f >> morph g }
     * }
     * 
     * leastfix : (a:category) -> functor (a, a) -> *
     * leastfix a F = initial (algebraic a F)
     * 
     * recursion : (a:category) -> (F:functor (a, a)) -> leastfix a F
     * 
     * fix : (a:category) -> (F:functor (a, a)) -> a
     * fix a F = carrier (bottom (recursion a F))
     * 
     * iso : (a:category) -> (F:functor (a, a)) -> map F (fix a F) ~> fix a F
     * iso a F = reduce (bottom (recursion a F))
     * 
     * catamorphism : (a:category) -> (F:functor (a, a)) -> (r:object a) -> (map F r ~> r) -> (fix a F ~> r)
     * catamorphism a F r step = morph (project (recursion a F) { carrier = r, reduce = step })
     * 
     * 
     * 
     * 
     * fixpoint : (a:category) -> functor (a, a) -> *
     * fixpoint a F = algebra (map isomorphism a) (fmap isomorphism (a, a) F)
     * 
     * 
     * 
     * 
     * 
     * 
     * 
     * 
     * 
     * 
     * 
     * 
     * 
     * opposite : category -> category
     * opposite a = {
     *   object = object a
     *   (~>) (x, y) = y ~> x
     *   
     *   null = null a
     *   (>>) (x, y, z) (f, g) = g >> f
     * }
     * 
     * (<~) : (a:category) -> relation a
     * (<~) a = (~>) (opposite a)
     * 
     * (<<) : (a:category) -> (x:object a, y:object a, z:object a) -> (x <~ y, y <~ z) -> (x <~ z)
     * (<<) a = (>>) (opposite a)
     * 
     * flip : (a:category, b:category) -> functor (a, b) -> functor (opposite a, opposite b)
     * flip (a, b) F = {
     *   map = map F
     *   fmap (x, y) = fmap F (y, x)
     * }
     * 
     * coalgebra : (a:category) -> functor (a, a) -> *
     * coalgebra a F = algebra (opposite a) (flip (a, a) F)
     * 
     * expand : (a:category) -> (F:functor (a, a)) -> (r:coalgebra a F) -> (carrier r ~> map F (carrier r))
     * expand a F r = reduce r
     * 
     * cohomomorphism : (a:category) -> (F:functor (a, a)) -> relation (coalgebra a F)
     * cohomomorphism a F (r, s) = homomorphism (opposite a) (flip (a, a) F) (s, r)
     * 
     * coalgebraic : (a:category) -> functor (a, a) -> category
     * coalgebraic a F = {
     *   object = coalgebra a F
     *   (~>) = cohomomorphism a F
     * }
     * 
     * terminal : category -> *
     * terminal a = initial (opposite a)
     * 
     * top : (a:category) -> terminal a -> object a
     * top a init = bottom init
     * 
     * greatfix : (a:category) -> functor (a, a) -> *
     * greatfix a F = terminal (coalgebraic a F)
     * 
     * corecurion : (a:category) -> (F:functor (a, a)) -> greatfix a F
     * corecursion a = recursion (opposite a) (flip (a, a) F)
     * 
     * cofix : (a:category) -> (F:functor (a, a)) -> a
     * cofix a F = fix (opposite a) (flip (a, a) F)
     * 
     * anamorhism : (a:category) -> (F:functor (a, a)) -> (r:object a) -> (r ~> map F r) -> (r ~> cofix a F)
     * anamorphism a F r step = catamorphism (opposite a) (flip (a, a) F)
     * 
     * 
     * 
     * 
     * 
     * 
     * 
     * 
     
     * 
     * 
     * 
     * 
     * 
     * 
     * 
     * image : (a:category, b:category) -> functor (a, b) -> category
     * image (a, b) F = {
     *   object = object a
     *   (~>) (x, y) = (~>) b (map F x, map F y)
     * 
     *   null x = null b (map F x)
     *   (>>) (x, y, z) = (>>) b (map F x, map F y, map F z)
     * }
     * 
     * chain : category -> *
     * chain a = functor (nat, a)
     * 
     * limit : (a:category) -> chain a -> *
     * limit a C = {
     *   omega : object a
     *   project : (n:nat) -> map F n ~> omega
     * }
     * 
     * lub : category -> *
     * lub a = (c:chain a) -> (x:limit a c, (y:limit a c) -> top x ~> top y)
     * 
     * iterated : (a:category) -> (F:functor (a, a)) -> initial a -> chain a
     * iterated a F init 0 = bottom init
     * iterated a F init (1+n) = map F (iterated a F init n)
     * 
     * recursion : (a:category) -> (F:functor (a, a)) -> initial a -> lub a -> leastfix a F
     * recursion a F init lub = {
     *   bottom : algebra a F
     *   bottom = {
     *     carrier : object a
     *     carrier = lub (iterated a F init)
     *     carrier = map F carrier
     *     
     *     reduce : map F carrier ~> carrier
     *     reduce = null a carrier
     *   }
     *   
     *   project : (r:algebra a F) -> (bottom ~> r)
     *   project r = {
     *     morph : carrier bottom ~> carrier r
     *     morph = fmap F (carrier bottom, carrier r) morph >> reduce r
     *   }
     * }
     * 
     * 
     * 
     * 
     * 
     * 
     * 
     * 
     * 
     * 
     * 
     * 
     * 
     * diagram : category -> *
     * diagram = (i:category, functor (i, a))
     * 
     * limit : (a:category) -> diagram a -> *
     * limit a D = terminal { object = cone a D, (~>) = ... }
     * 
     * 
     * 
     * (<~>) : (a:category) -> relation (object a)
     * (<~>) a (x, y) = {
     *   forward : x ~> y
     *   backward : x <~ y
     *   
     *   idForward : null a x == (forward >> backward)
     *   idBackward : (forward << backward) == null a y
     * }
     * 
     * 
     * limit : (I:category, a:category) -> (D:functor (I, a)) -> object a -> *
     * limit (I, a) D apex = (\c. c ~> apex) <~> (\c. delta c ~> D)
     * 
     * hom : (a:category) -> functor (cross (opposite a, a), set)
     * hom a = {
     *   map : relation (object a)
     *   map = (~>)
     *   
     *   fmap : ((x, y):(object a, object a), (p, q):(object a, object a)) -> (p ~> x, y ~> q) -> ((x ~> y) -> (p ~> q))
     *   fmap ((x, y), (p, q)) (f, g) h = f >> h >> g
     * }
     * 
     * presheaf : category -> *
     * presheaf a = functor (a, *)
     * 
     * representation : (a:category) -> presheaf a -> presheaf a
     * representation a F = {
     *   map x = natural hom(x, -) F
     *   fmap (x, y) (f:x ~> y) (t:map x) = {
     *     transform z (g:y ~> z) = t z (f >> g)
     *   }
     * }
     * 
     * yoneda : (a:category) -> (F:presheaf a) -> representation a F <~> F
     * yoneda a F = {
     *   forward = {
     *     transform x t = t x (null x)
     *   }
     *   
     *   backward = {
     *     transform x (xs:map F x) (y:object a) (f:x ~> y) = fmap F (x, y) f xs
     *   }
     * }
     * 
     * 
     * discrete : category -> *
     * discrete a = {
     *   univalence : (x:object a, y:object a) -> (x <~> y) -> (x == y)
     * }
     * 
     * 
     * 
     * 
     * kleili M = {
     *   object = object M
     *   (~>) (a, b) = a ~> map M b
     *   
     *   identity = return M
     *   (>>) (a, b, c) (f, g) = f >> fmap M (b, map M c) g >> join M c
     * }
     * 
     * yoneda (kleisli M) : (F:functor (kleisli M, *)) -> 
     * yoneda (kleisli M) F = {
     *   forward = {
     *     transform x = t x (return M x)
     *   }
     *   
     *   backward = {
     *     transform x (xs:map F x) (y:object a) (f:x ~> map M y) = fmap F (x, y) f xs : map M (map F y)
     *   }
     * }
     * 
        async M a = (r:*) -> (a -> M r) -> M r

        bind : (M <: monad) -> M <-> async M
        bind M .forward : (a:*) -> M a -> async M a
        bind M .forward a xs r k = join M r (fmap M (r, M r) k xs)
        bind M .forward a xs r k = join M r (fmap M (r, M r) k xs)
        bind M .backward : (a:*) -> async M a -> M a
        bind M .backward a ms = ms a (return M a)
        bind M .backward a ms = ms a (identity M a)

        yoneda F a = (r:*) -> (a -> r) -> F r

        embed : (F <: functor) -> F <-> yoneda F
        embed F .forward : (a:*) -> F a -> yoneda F a
        embed F .forward a xs r k = fmap F (a, r) k xs
        embed F .backward : (a:*) -> yoneda F a -> F a
        embed F .backward a fs = fs a (identity a)

     * 
     * 
     * 
     * 
     * 
     * 
     * 
     * 
     * 
     * 
     * 
     * 
     * set : category
     * set = {
     *   object = *
     *   (~>) = (->)
     *   
     *   null a x = x
     *   (>>) (a, b, c) (f, g) x = g (f x)
     * }
     * 
     * cat : category
     * cat = {
     *   object = category
     *   (~>) = functor
     *   
     *   null a = {
     *     map = null set (object a) 
     *     fmap (x, y) = null set (x ~> y)
     *   }
     *   
     *   (>>) (a, b, c) (F, G) = {
     *     map = map F >> map G
     *     fmap (x, y) = fmap F (x, y) >> fmap G (map F x, map F y)
     *   }
     * }
     * 
     * objective : functor (cat, set)
     * objective = {
     *   map : category -> *
     *   map = object
     *   
     *   fmap : (a:category, b:category) -> functor (a, b) -> (object a -> object b)
     *   fmap (a, b) F = map F
     * }
     * 
     * opposite : category -> category
     * opposite = {
     *   object = object a
     *   (~>) (x, y) = y ~> x
     *   
     *   null = null a
     *   (>>) (x, y, z) (f, g) = g >> f
     * }
     * 
     * opp : functor (cat, cat)
     * opp = {
     *   map : category -> category
     *   map = opposite
     *   
     *   fmap : (a:category, b:category) -> functor (a, b) -> functor (opposite a, opposite b)
     *   fmap (a, b) F = {
     *     map = map F
     *     fmap (x, y) = fmap F (y, x)
     *   }
     * }
     * 
     * cross : (category, category) -> category
     * cross (a, b) = {
     *   object = (object a, object b)
     *   (x, r) ~> (y, s) = (x ~> y, r ~> s)
     * }
     * 
     * crossing = functor (cross (cat, cat), cat)
     * crossing = {
     *   map : (category, category) -> category
     *   map = cross
     *   
     *   fmap : ((a:category, r:category), (b:category, s:category)) -> (functor (a, b), functor (r, s)) -> functor (cross (a, r), cross (b, s))
     *   fmap ((a, r), (b, s)) (F, G) = {
     *     map : (object a, object r) -> (object b, object s)
     *     map (x, m) = (map F x, map G m)
     *     
     *     fmap : ((x:object a, m:object r), (y:object a, n:object r)) -> (x ~> y, m ~> n) -> (map F x ~> map F y, map G m ~> map G n)
     *     fmap ((x, m), (y, n)) (f, g) = (fmap F (x, y) f, fmap G (m, n) g)
     *   }
     * }
     * 
     * morphism : (a:category) -> functor (cross (opposite a, a), set)
     * morphism a = {
     *   map : relation (object a)
     *   map = (~>)
     *   
     *   fmap : ((x, y):(object a, object a), (p, q):(object a, object a)) -> (p ~> x, y ~> q) -> ((x ~> y) -> (p ~> q))
     *   fmap ((x, y), (p, q)) (f, g) h = f >> h >> g
     * }
     * 
     * naturally : (category, category) -> category
     * naturally (a, b) = {
     *   object = functor (a, b)
     *   (~>) = natural (a, b)
     *   
     *   null F = { transform x = null b (map F x) }
     *   (>>) (F, G, H) (N, M) = { transform x = N x >> M x }
     * }
     * 
     * nat : functor (cross (opposite cat, cat), cat)
     * nat = {
     *   map : (category, category) -> category
     *   map = naturally
     * 
     *   fmap : ((a:category, b:category), (p:category, q:category)) -> (functor (p, a), functor (b, q)) -> functor (naturally (a, b), naturally (p, q))
     *   fmap ((a, b), (p, q)) (F, G) = {
     *     map : functor (a, b) -> functor (p, q)
     *     map H = F >> H >> G
     *   
     *     fmap : (N:functor (a, b), M:functor (a, b)) -> natural (a, b) (N, M) -> natural (p, q) (map N, map M)
     *     fmap (N, M) t = {
     *       transform : (x:object p) -> functor (map (F >> N >> G) x, map (F >> M >> G) x)
     *       transform x = fmap G (map (F >> N) x, map (F >> M) x) (transform t (map F x))
     *     }
     *   }
     * }
     * 
     * functonatural : (a:category, b:category) -> functor (cross (opposite (naturally (a, b)), naturally (a, b)), set)
     * functonatural (a, b) = {
     *   map : relation (functor (a, b))
     *   map = natural (a, b)
     *   
     *   fmap : ((F:functor (a, b), G:functor (a, b)), (P:functor (a, b), Q:functor (a, b))) -> (natural (a, b) (P, F), natural (a, b) (G, Q)) -> natural (a, b) (F, G) -> natural (a, b) (P, Q)
     *   fmap ((F, G), (P, Q)) (N, M) T = {
     *     transform : (x:object a) -> map P x ~> map Q x
     *     transform x = transform N x >> transform T x >> tranform M x
     *   }
     * }
     * 
     * algebraically : (a:category) -> functor (opposite (naturally (a, a)), cat)
     * algebraically a = {
     *   map : functor (a, a) -> *
     *   map = algebraic a
     *   
     *   fmap : (F:functor (a, a), G:functor (a, a)) -> natural (a, a) (G, F) -> functor (algebraic a F, algebraic a G)
     *   fmap (F, G) N = {
     *     map : algebra a F -> algebra a G
     *     map r = {
     *       carrier : object a
     *       carrier = carrier r
     *     
     *       reduce : map G carrier ~> carrier
     *       reduce = transform N carrier >> reduce r
     *     }
     *     
     *     fmap : (r:algebra a F, s:algebra a F) -> homomorphism a F (a, s) -> homomorphism a G (map r, map s)
     *     fmap (r, s) H = {
     *       morph : carrier r ~> carrier s
     *       morph = morph H
     *       
     *       preserving : (reduce (map r) >> morph) == (fmap G (carrier (map r), carrier (map s)) morph >> reduce (map s))
     *       preserving : (transform N carrier >> reduce r >> morph H) == (fmap G (carrier r, carrier s) (morph H) >> transform N carrier >> reduce s)
     *       preserving : (transform N carrier >> reduce r >> morph H) == (transform N carrier >> fmap F (carrier r, carrier s) (morph H) >> reduce s)
     *       preserving : (reduce r >> morph H) == (fmap F (carrier r, carrier s) (morph H) >> reduce s)
     *       preserving = preserving H
     *     }
     *   }
     * }
     * 
     * 
     * isomorphism : functor (cat, cat)
     * isomorphism = {
     *   map : category -> category
     *   map a = { object = object a, (~>) = (<~>) }
     *   
     *   fmap : (a:category, b:category) -> functor (a, b) -> functor (map a, map b)
     *   fmap (a, b) F = {
     *     map : object a -> object b
     *     map = map F
     *     
     *     fmap : (x:object a, y:object a) -> (x <~> y) -> (map F x <~> map F y)
     *     fmap (x, y) { forward, backward } = {
     *       forward = fmap F (x, y)
     *       backward = fmap F (y, x)
     *     }
     *   }
     * }
     * 
     * 
     * 
     * 
     * 
     * 
     * 
     * 
     * 
     * 
     * 
     * 
     * 
     * crossProduct : (a:category, b:category) -> product cat (a, b)
     * crossProduct (a, b) {
     *   (a * b) = cross (a, b)
     *   
     *   left : functor (a * b, a)
     *   left = {
     *     map (x, y) = x
     *     fmap ((x, y), (r, s)) (f, g) = f
     *   }
     *   
     *   right : functor (a * b, b)
     *   right = {
     *     map (x, y) = y
     *     fmap ((x, y), (r, s)) (f, g) = g
     *   }
     *   
     *   join : (c:category) -> (functor (c, a), functor (c, b)) -> functor (c, cross (a, b))
     *   join c (F, G) = {
     *     map p = (map F p, map G p)
     *     fmap (p, q) f = (fmap F f, fmap G f)
     *   }
     * }
     * 
     * natExponent : (a:category, b:category) -> exponent cat (a, b)
     * natExponent (a, b) = {
     *   (*) = crossProduct
     *   
     *   (b ^ a) = naturally (a, b)
     *   
     *   apply : functor (cross (naturally (b, a), a), b)
     *   apply = {
     *     map (F, x) = map F x
     *     fmap ((F, x), (G, y)) (N, f) = fmap F (x, y) f >> transform N y
     *   }
     *   
     *   curry : (c:category) -> (F:functor (cross (c, a), b)) -> functor (c, naturally (a, b))
     *   curry c F = {
     *     map p = {
     *       map x = map F (p, x)
     *       fmap (x, y) f = fmap F ((p, x), (p, y)) (null c p, f)
     *     }
     *     
     *     fmap : (p:object c, q:object c) -> (f:p ~> q) -> natural (map p, map q)
     *     fmap (p, q) f = {
     *       transform : (x:object a) -> map F (p, x) ~> map F (q, x)
     *       transfom x = fmap F ((p, x), (q, x)) (f, null a x)
     *     }
     *   }
     * }
     * 
     * 
     * product : (a:category) -> relation (object a)
     * product a (x, y) = {
     *   (x * y) : object a
     *   
     *   left : (x * y) ~> x
     *   right : (x * y) ~> y
     *   join : (r:object a) -> (r ~> x, r ~> y) -> (r ~> (x * y))
     *   
     *   betaLeft : (r:object a, f:r ~> x, g:r ~> y) -> (join r (f, g) >> left) == f
     *   betaRight : (r:object a, f:r ~> x, g:r ~> y) -> (join r (f, g) >> right) == g
     *   eta : join (x * y) (left, right) == null (x * y)
     * }
     * 
     * coproduct : (a:category) -> relation (object a)
     * coproduct a = product (opposite a)
     * 
     * exponential : (a:category) -> relation (object a)
     * exponential a (x, y) = {
     *   (*) : (x:object a, y:object a) -> product a (x, y)
     *   
     *   (y ^ x) : object a
     * 
     *   apply : ((y ^ x) * x) ~> y  
     *   curry : (r:object a) -> (f:(r * x) ~> y) -> (r ~> (y ^ x))
     *   
     *   eta : (r:object a) -> (f:(r * x) ~> y) -> join (curry f, null x) >> apply == f
     * }
     * 
     * logarithm? : (a:category) -> relation (object a)
     * logarithm? a (x, y) = {
     *   (+) : (x:object a, y:object a) -> coproduct a (x, y)
     *   
     *   log : object a
     * 
     *   apply : y ~> (log + x)
     *   curry : (r:object a) -> (f:y ~> (r + x)) -> (log ~> r)
     *   
     *   eta : (r:object a) -> (f:y ~> (r + x)) -> apply >> join (curry f, null x) == f
     * }
     * 
     * 
     * 
     * 
     * 
     * 
     * 
     * 
     * 
     * 
     * 
     * 
     * 
     * 
     * 
     * 
     * ---------------------
     * (type : type)
     * 
     * object : type -> type
     * (a:*) -> object a == { invoke : object a -> a }
     * ---------------------
     * 
     * object : * -> *
     * object a = {
     *   invoke : object a -> a
     * }
     * 
     * null : (a:*) -> object a
     * null a .invoke self = self .invoke self
     * 
     * new : (a:*) -> object a -> a
     * new a = null a .invoke
     * 
     * loop : (a:*) -> (a -> a) -> object a
     * loop a f .invoke self = f (new a self)
     * 
     * bottom : (a:*) -> a
     * bottom a = new a (null a)
     * 
     * recur : (a:*) -> (a -> a) -> a
     * recur a f = new a (loop a f)
     * 
     * 
     * 
     * 
     * recur : (a:category) -> (F:functor (a, a)) -> initial { object = algebra a F, (~>) = homomorphism a F }
     * 
     * 
     * 
     * 
     * 
     * 
     * 
     * 
     * 
     * 
     * 
     * order : *
     * order = Lesser | Equal | Greater
     * 
     * flip : order -> order
     * flip Lesser = Greater
     * flip Equal = Equal
     * flip Greater = Lesser
     * 
     * ordering : * -> *
     * ordering a = {
     *   (<>) : order -> relation a
     *   
     *   reflexive : (x:a) -> (<>) Equal (x, x)
     *   symmetric : (x:a, y:a) -> (o:order) -> (<>) o (x, y) -> (<>) (flip o) (y, x)
     *   transitive : (x:a, y:a, z:a) -> (o:order) -> ((<>) o (x, y), (<>) o (y, z)) -> (<>) o (x, z)
     * }
     * 
     * equivalence : * -> *
     * equivalence a = {
     *   (~=) : relation a
     *   
     *   reflexive : (x:a) -> (x ~= x)
     *   symmetric : (x:a, y:a) -> (x ~= y) -> (y ~= x)
     *   transitive : (x:a, y:a, z:a) -> (x ~= y, y ~= z) -> (x ~= z)
     * }
     * 
     * decision : * -> *
     * decision a = Proof a | Refutation (not a)
     * 
     * comparison : (a <: ordering) -> *
     * comparison a = {
     *   (<?>) : (x:a, y:a) -> (o:order, (<>) o (x, y))
     *   
     *   consistency : (x:a, y:a) -> ((<>) o (x, y)) <-> ((x <?> y) == o)
     * }
     * 
     * discrete : (a <: equivalence) -> *
     * discrete a = {
     *   (=?) : (x:a, y:a) -> decision (x ~= y)
     * }
     * 
     * 
     * 
     * 
     * 
     * 
     * 
     * 
     * 
     * 
     * 
     * ((x:a) -> F x <~> G x) -> ((x:a) -> F x) <~> ((x:a) -> G x))
     * 
     * natural (a, b) (F, G) -> functor (fiber a F, fiber b G)
     * 
     * (F <~> G) -> (fiber F <~> fiber G)
     * 
     * adjunction : (a:category, b:category) -> (functor (a, b), functor (b, a)) -> *
     * adjunction (a, b) (F, G) = {
     *   unit : null a ~> (F >> G)
     *   counit : (G >> F) ~> null b
     *   
     *   leftIdentity : null F == (natmap (a, b) F unit >> conatmap (b, a) F counit)   
     *   rightIdentity : (conatmap (a, b) G unit >> natmap (b, a) F counit) == null G
     * }
     * 
     * 
     * natmap C n : natural (G >> C, H >> C)
     * natmap C n = {
     *   transform x = fmap (map G x, map H x) (transform n x)
     * }
     * 
     * conatmap C n : natural (C >> G, C >> H)
     * conatmap C n = {
     *   transform x = transform n (map C x)
     * }
     * 
     * 
     * adjunction : (a:category, b:category) -> *
     * adjunction = {
     *   F : functor (a, b)
     *   G : functor (b, a)
     *   
     *   natural : (x:object a, y:object b) -> (map F x ~> y) <~> (x ~> map G y)
     *   
     *   
     *   preF x = map F x ~> y
     *   preG x = x ~> map G y
     *   
     *   prenatural : (y:object b) -> (\x. map F x ~> y) <~> (\x. x ~> map G y)
     *   prenatural : (x:object a) -> (\y. map F x ~> y) <~> (\y. x ~> map G y)
     *   
     *   
     * }
     * 
     * 
     * 
     * 
     * 
     * 
     * 
     * F : * -> *
     * F a = {
     *   ...
     * }
     * 
     * fmap : (a:*, b:*) -> (a -> b) -> (F a -> F b)
     * 
     * ---
     * 
     * F : * -> *
     * F a = {
     *   ...
     * }
     * 
     * fmap : (a:*) -> F a -> (b:*) -> (a -> b) -> F b
     * 
     * ---
     * 
     * F : * -> *
     * F = fix \Self a. {
     *   ...
     *   fmap : (b:*) -> (a -> b) -> Self b
     * }
     * 
     * ---
     * 
     * F : * -> *
     * F = fix \Self a. {
     *   ...
     *   fmap : (b:*) -> (a -> b) -> Self b
     * }
     * 
     * fmap : (a:*, b:*) -> (a -> b) -> (F a -> F b)
     * fmap (a, b) f xs = {
     *   ...
     *   fmap : (r:*) -> (b -> r) -> F r
     *   fmap r g = xs .fmap r (f >> g)
     * }
     * 
     * ---
     * ---
     * ---
     * 
     * functor : (* -> *) -> *
     * functor F = {
     *   fmap : (a:*, b:*) -> (a -> b) -> (F a -> F b)
     * 
     *   identity : (a:*) -> fmap (a, a) (null a) == null (F a)
     *   compose : (a:*, b:*, c:*) -> (f:a -> b, g:b -> c) -> (fmap (a, b) f >> fmap (b, c) g) == fmap (a, c) (f >> g)
     * }
     * 
     * ---
     * 
     * functor : (* -> *) -> *
     * functor F = {
     *   fmap : (a:*) -> F a -> (b:*) -> (a -> b) -> F b
     * 
     *   identity : (a:*) -> (xs:F a) -> fmap a xs a (null a) == xs
     *   compose : (a:*) -> (xs:F a) -> (b:*, c:*) -> (f:a -> b, g:b -> c) -> fmap b (fmap a xs b f) c g  == fmap a xs c (f >> g)
     * }
     * 
     * ---
     * 
     * functor : (* -> *) -> *
     * functor F = (a:*) -> (xs:F a) -> functorial a xs
     * 
     * functorial : (F:* -> *) -> (a:*) -> F a -> *
     * functorial F a xs = {
     *   fmap : (b:*) -> (a -> b) -> F b
     * 
     *   identity : fmap a (null a) == xs                    -- oops --
     *   compose : (b:*, c:*) -> (f:a -> b, g:b -> c) -> fmap b (fmap b f) c g  == fmap c (f >> g)
     * }
     * 
     * ---
     * 
     * object : * -> *
     * object a = {
     *   invoke : object a -> a
     * }
     * 
     * new : (a:*) -> object a -> a
     * new a o = o .invoke o
     * 
     * recursor : (a:*) -> (a -> a) -> object a
     * recursor a f = {
     *   invoke : object a -> a
     *   invoke self = f (self .invoke self)
     * }
     * 
     * loop : (a:*) -> (a -> a) -> a
     * loop a f = new a (recursor a f)
     * 
     * ---
     * 
     * trait : (F:trait F -> *) -> *
     * trait F = {
     *   invoke : (self:trait F) -> F self
     * }
     * 
     * ---
     * 
     * loop : loop loop
     * loop F = (x:F x) -> *
     * 
     * mixin : loop mixin
     * mixin M = {
     *   mix : trait M -> *
     * }
     * 
     * trait : loop mixin
     * trait M = {
     *   invoke : (self:trait M) -> M .mix self
     * }
     * 
     * mixer : (F:loop F) -> mixin (mixer F)
     * mixer F = {
     *   mix : trait (mixer F) -> *
     *   mix self = F (self .invoke self) 
     * }
     * 
     * functorial : (a:*) -> mixin (functorial a)
     * functorial a = mixer \self. {
     *   fmap : (b:*) -> (a -> b) -> trait (functorial b)
     *   
     *   identity : self .fmap a (null a) == self
     *   compose : (b:*, c:*) -> (f:a -> b, g:b -> c) -> new (self .fmap b f) .fmap c g == self .fmap c (f >> g)
     * }
     * 
     * 
     * F : * -> *
     * F a = trait \self. {
     *   content : ...
     *   functor : functorial a .mix self
     * }
     * 
     * 
     * 
     * 
     * 
     * 
     * 
     * 
     * 
     * 
     * 
     * response : * -> *
     * response a = {
     *   type : Failure | Success
     *   content : choose type { exception, a }
     * }
     * 
     * isOk : (a:*, response a) -> bool
     * isOk (a, r) = (r.type =? Success)
     * 
     * ---
     * 
     * response : * -> *
     * response a = {
     *   type : Failure | Success
     *   content : choose type { exception, a }
     *   isOk : bool
     * }
     * 
     * isOk : (a:*, response a) -> bool
     * isOk (a, r) = (r.type =? Success) => r.isOk ???
     * 
     * ---
     * 
     * response : * -> *
     * response a = {
     *   type : Failure | Success
     *   content : choose type { exception, a }
     * }
     * 
     * isOk : (a:*, response a) -> *
     * isOk (a, r) = (r.type == Success)
     * 
     * ---
     * 
     * response : * -> *
     * response a = {
     *   type : Failure | Success
     *   content : choose type { exception, a }
     *   isOk : *
     * }
     * 
     * isOk : (a:*, response a) -> *
     * isOk (a, r) = (r.type == Success) -> (x:r.isOk) -> x == x ???
     * 

     * 
     * 
     * 
     * 
     * 
     * 
     * 
     * 
     * 
     * 
     * 
     * 
     * 
     * 
     * 
     * 
     * 
     * 
     * 
     * 
     * circle = least \C:*. {
     *   point : C
     *   loop : point == point
     * }
     * 
     * circle = (r:*) -> (x:r, x == x) -> r
     * (r:*) -> (F r -> r) -> (fix F -> r)
     * 
     * 
     * 
     * circle r = (x:r, x == x)
     * 
     * (r:*) -> circle r -> (fix circle -> r)
     * 
     * 
     * interval = least \I:*. <
     *   zero : I
     *   one : I
     *   segment : zero == one
     * >
     * 
     * least { object = bool, (~>) = (==) } not
     * 
     * 
     * (r:bool) -> (not r == r) -> r
     * 
     * 
     * identity : (a:category) -> relation a
     * identity a = least \I (x, y). x <~> y
     * 
     * algebra { object = a, (~>) = (==) } (\x. x) = {
     *   carrier : a
     *   loop : carrier == carrier
     * }
     * 
     * 
     * 
     * 
     * 
     * 
     * 
     * 
     * 
     * 
     * 
     * 
     * listF : * -> * -> *
     * listF a r = Nil | Cons (a, r)
     *   implicit : functor (*, *) 
     * 
     * lists : * -> initial (algebra (*, ->) (listF a))
     * lists a = recursion (*, ->) (listF a)
     * 
     * list : * -> *
     * list a = lists a .bottom .carrier
     * 
     * 
     * xs : list a
     * xs = [1, 3, 42, 23]
     * 
     * lists a .project (nat, 0, +) .morph xs : nat
     * 
     * lists a .project (listF a .fmap (list a .bottom)) : list a .bottom -> listF a (list a .bottom)
     * 
     * 
     * 
     * monad M = {
     *   (1 + M x M) -> M
     * } = {
     *   (a:*) -> a -> M a
     *   (a:*) -> M (M a) -> M a
     * }
     * 
     * comonad M = {
     *   M -> (1 * M x M)
     * } = {
     *   (a:*) -> M a -> a
     *   (a:*) -> M a -> M (M a)
     * }
     * 
     * 
     * hom : (category, category) -> *
     * hom (a, b) = functor (a, internal ^ opposite b)
     * 
     * yoneda : (a:category) -> hom (a, a)
     * yoneda = curry for exponential from natural transformations 
     * 
     * lemma : (a:category) -> (F:hom (a, a)) -> hom (yoneda a, F) <~> F a
     * 
     * 
     * list a .project 
     * 
     * 
     * 
     * isomorphisms : category -> category
     * isomorphisms a = {
     *   object = object a
     *   (~>) = (<~>) a
     * }
     * 
     * 
     * 
     * 
     * monoid : (a:category) -> *
     * monoid a = {
     *   concrete (tensor a)
     *   
     *   m : object a
     *   unit : 1 ~> m
     *   join : (m x m) ~> m
     *   
     *   (unit, null m) >> join == identityLeft
     *   (null m, unit) >> join == identityRight
     *   (join, null m) >> join == associative >> (null m, join) >> join
     * }
     * 
     * 
     * exponential : category -> *
     * exponential a = {
     *   (*) : (object a, object a) -> product a
     *   (^) : (object a, object a) -> object a
     *   
     * 
     *   apply : ((y ^ x) * x) ~> y
     * }
     * 
     * fibers = {
     *   object = nat -> *
     *   (F ~> G) = (x:nat) -> F x -> G x
     * }
     * 
     * T : functor (fibers, fibers)
     * T = {
     *   map : (nat -> *) -> (nat -> *)
     *   map P 0 = Top
     *   map P (1+n) = P n
     *   
     *   fmap : (F:nat -> *, G:nat -> *) -> ((x:nat) -> F x -> G x) -> (y:nat) -> map F x -> map G x)
     *   fmap (F, G) f 0 = id
     *   fmap (F, G) f (1+n) = f n
     * }
     * 
     * rule : algebra fibers T
     * rule = {
     *   carrier : nat -> *
     *   carrier n = 
     *   reduce : (x:nat) -> map T carrier x -> carrier n
     * 
     * }
     * 
     * 
     * recurse fibers T rule : (x:nat) -> fix (map T) x -> carrier rule x
     * 
     * 
     * 
     * 
     * 
     * 
     * 
     * 
     * coproduct a = (x:object a, y:object a) -> {
     *   (+) : object a
     *   
     *   left : x ~> (x + y)
     *   right : y ~> (x + y)
     *   split : (r:object a) -> (x ~> r, y ~> r) -> ((x + y) ~> r)
     *   
     *   etaLeft : (r:object a, f:x ~> r, g:y ~> r) -> (left >> split r (f, g)) == f
     *   etaRight : (r:object a, f:r ~> x, g:r ~> y) -> (right >> split r (f, g)) == g 
     *   beta : split (x + y) (left, right) == null (x + y)
     * }
     * 
     * coproduct a == product (opposite a)
     * coproduct a = (x:object a, y:object a) -> {
     *   (*) : object a
     *   
     *   left : (x * y) <~ x
     *   right : (x * y) <~ y
     *   join : (r:object a) -> (r <~ x, r <~ y) -> (r <~ (x * y))
     *   
     *   betaLeft : (r:object a, f:r <~ x, g:r <~ y) -> (join r (f, g) >> left) == f
     *   betaRight : (r:object a, f:r <~ x, g:r <~ y) -> (join r (f, g) >> right) == g   
     *   eta : join (x * y) (left, right) == null (x * y)
     * }
     * 
     *
     * 
     * 
     * 
     * I : initial (algebra a F)
     * I .project { carrier = map F (carrier I), reduce = fmap F (map F (carrier I), carrier I) (reduce I) } : homomorphism a F (I, apply F I)
     * 
     * 
     * 
     * 
     * recurse : (a:*) -> (a -> a) -> a
     * 
     * recursion : (a:category) -> (F:functor (a, a)) -> leastfix a F
     * 
     * 
     * free F = fix \r. 1 + F * r
     *        = fix \r a. a + F (r a)
     * 
     * 
     * recursion (opposite a) (flip F) : initial { object = algebra (opposite a) (flip F), (~>) = homomorphism (opposite a) (flip F) }
     * 
     * {
     *   bottom : coalgebra a F
     *   project : (x:object a) -> unique (homomorphism (opposite a) F (bottom, x))
     * }
     * 
     * 
     * fold : (a:category) -> (F:functor (a, a)) -> (fix:initial (algebra a F)) -> (r:algebra a F) -> carrier (bottom fix) ~> carrier r
     * 
     * unfold : (a:category) -> (F:functor (a, a)) -> (fix:terminal (coalgebra a F)) -> (r:algebra (opposite a) F) -> carrier r ~> carrier (bottom fix)
     * 
     * 
     * 
     * 
     * 
     * adjoint : (C:category, D:category) -> functor (C, D) -> *
     * adjoint (C, D) = {
     *   U : functor (U, C)
     *   F : functor (D, C)
     *   
     *   match : (x:object D, y:object C) -> (map F x ~> y) <-> (x ~> map U y)
     * }
     * 
     * 
     * 
     * 
     * 
     * 
     * 
     * 
     * homomorphism a F (r, s) = {
     *   morph : carrier r ~> carrier s
     *   
     *   preserving : (reduce r >> morph) == (fmap F (carrier r, carrier s) morph >> reduce s)
     * }
     * homomorphism (opposite a) F (s, r) = {
     *   morph : carrier r ~> carrier s
     *   
     *   preserving : (morph >> expand s) == (expand r >> fmap F (carrier r, carrier s) morph)
     * }
     * {
     *   morph : carrier s ~> carrier r
     *   
     *   preserving : (morph >> reduce r) == (reduce s >> fmap F (carrier r, carrier s) morph)
     * }
     * 
     * {
     *   map : object a -> object a
     *   fmap : (x:object a, y:object a) -> (y ~> x) -> (map y ~> map x)
     *   
     *   identity : (x:object a) -> fmap (x, x) (null x) == null (map x)
     *   compose : (x:object a, y:object a, z:object a) -> (f:y ~> x, g:z ~> y) -> fmap (x, z) (g >> f) == (fmap (y, z) g >> fmap (x, y) f)
     * }
     * 
     * 
     * 
     * 
     * 
     * loop : (a:*) -> (a -> a) -> a
     * loop : (a:category) -> (F:functor (a, a)) -> initial (algebra a F)
     * 
     * fixed : (a:category) -> (object a -> object a) -> *
     * fixed a f = {
     *   point : object a,
     *   invariant : f point ~> point
     * }
     * 
     * project I r : bottom I ~> r : >> fmap F (carrier r) : bottom ~> 
     * 
     * 
     * 1 ~> x  f:x ~> x
     * 
     * 
     * fixed : (a:category) -> (I:initial a) -> (x:object x) -> (f:x ~> x) -> *
     * fixed a I x f = project I x >> f == project I x
     * 
     * 
     * hom : category -> *
     * hom a = functor (product (opposite a, a), functional)
     * 
     * 
     * 
     * boolean : *
     * boolean = False | True
     * 
     * proof : boolean -> *
     * proof False = (a:*) -> a
     * proof True = (a:*, a)
     * 
     * set : * -> *
     * set a = {
     *   contains : a -> boolean
     * }
     * 
     * element : (a:*, set a) -> *
     * element (a, s) = (x:a, proof (s .contains x))
     * 
     * sets : * -> category
     * sets = {
     *   object : (a:*, set a)
     *   (s ~> r) = element s -> element r 
     *   
     * }
     * 
     * 
     * power : (a:*) -> (a -> *) -> *
     * power a f = (x:a, f x)
     * 
     * 
     * 
     * 
     * 
     * tensor : category -> *
     * tensor a = {
     *   1 : object a
     *   (*) : (object a, object a) -> object a
     *   
     *   split : (w, x, y, z:object a) -> ((a ~> b) & (c ~> d)) -> (a * c) ~> (b * d)
     *   
     *   identityLeft : (m:object a) -> (1 * m) ~> m
     *   identityRight : (m:object a) -> (m * 1) ~> m
     *   associative : (m, n, o:object a) -> ((m * n) * o) ~> (m * (n * o))
     * } 
     * 
     * 
     * monoid : category -> *
     * monoid a = {
     *   _ : tensor a
     *   m : object a
     *   
     *   unit : 1 ~> m
     *   join : (m * m) ~> m
     *   
     *   unitorLeft : split (unit, null m) >> join == identityLeft
     *   unitorRight : split (null m, unit) >> join == identityRight
     *   pentagon : split (join, null m) >> join == associative >> split (null m, join) >> join
     * }
     *
     * monad : *
     * monad = monoid (functor (*, *)) with { tensor = functorComposition }
     * 
     * 
     * _ : monad
     * _ = {
     *   1 a = a
     *   (F * G) a = F (G a)
     *   (,) (F, G, H, J) (f:F => G, g:H => J) a = transport f (H a) >> fmap G (H a, J a) (transport g a)
     * 
     *   identityLeft F a = id (F a)
     *   identityRight F a = id (F a)
     *   associative (F, G, H) a = id (F (G (H a)))
     *   
     *   m = option
     *   
     *   unit : (a:*) -> a -> option a
     *   unit a x = Just x
     *   
     *   join : (a:*) -> option (option a) -> option a
     *   join a None = None
     *   join a (Some x) = x
     *   
     *   unitorLeft : (unit, null option) >> join == identityLeft
     * }
     * 
     * 
     * _ : monoid *
     * _ = {
     * 
     *   1 = ()
     *   (x * y) = (x & y)
     *   (f, g) (x, y) = (f x, g y)
     *   
     *   identityLeft : (() & int) -> int
     *   identityLeft ((), x) = x
     *   identityRight (x, ()) = x
     *   associative ((x, y), z) = (x, (y, z))
     *   
     *   m = int
     *      
     *   unit : () -> int
     *   unit () = 0
     *   join : (int & int) -> int
     *   join (x, y) = x + y
     *   
     *   identityLeft : (x:int) -> (0 + x) == x
     *   identityRight : (x:int) -> (x + 0) == x
     *   associative : (x:int, y:int, z:int) -> ((x + y) + z) == (x + (y + z))
     * }
     * 
     * 
     * 
     * dist : ((a ~> b), (c ~> d)) -> (a * c) ~> (a * d)
     * 
     * monoid : * -> *
     * monoid a = {
     *   unit : a
     *   join : (a * a) -> a
     *   
     *   left : (x:a) -> join (unit, x) == x
     *   right : (x:a) -> join (x, unit) == x
     *   associative : (x:a, y:a, z:a) -> join (join (x, y), z) == join (x, join (y, z))
     * }
     * 
     * 
     * monoid : (a:category) -> (a, (a, a) -> a, (a, a) -> a) -> *
     * 
     * monoid a (1, *, +) =
     * {
     *   x : object a
     *   ops : (1 + (x * x)) ~> x
     * }
     * 
     * monoid (functor (a, a)) (1, *, +) =
     * {
     *   f : functor (a, a)
     *   ops : (x:object a) -> (x + f (f x)) ~> f x
     * }
     * 
     * 
     * 
     * 
     * functional : category
     * functional = {
     *   object = *
     *   (~>) (a, b) = (x:a) -> b
     * 
     * }
     * 
     * functorial : category
     * functorial = {
     *   object = category
     *   (~>) = functor
     *   
     *   null a = {
     *     map x = x
     *     fmap (x, y) f = f
     *   }
     *   
     *   (>>) (a, b, c) (F, G) = {
     *     map x = map G (map F x)
     *     fmap (x, y) f = fmap G (F x, F y) (fmap F (x, y) f)
     *   }
     * }
     * 
     * naturally : (category, category) -> category
     * naturally (a, b) = {
     *   object = functor (a, b)
     *   (~>) = natural (a, b)
     *   
     *   null F = {
     *     transform x = null (map F x)
     *   }
     *   
     *   (>>) (F, G, H) (n, m) = {
     *     transform x = transform n x >> transform m x
     *   }
     * }
     * 
     * naturally ((->), (->)) = {
     *   object = { map : * -> *, fmap : (a:*, b:*) -> (a -> b) -> (map a -> map b)), ... }
     *   (~>) (F, G) = (a:*) -> map F a -> map G a 
     * }
     * 
     * naturally ((==) a, (->)) = {
     *   object = { map : a -> *, fmap : (x:a) -> (map x -> map x), ... }
     *   (~>) (F, G) = (a:*) -> map F a -> map G a 
     * }
     * 
     * 
     * 
     * natural = {
     *   object = category -> category
     *   (~>) (F, G) = (a:category) -> functor (F a, G a)
     * 
     * }
     * 
     * endo : (a:category) -> a -> algebra * monoid
     * endo a x = {
     *   carrier : type
     *   carrier = x ~> x
     *   reduce : monoid (x ~> x) -> (x ~> x)
     *   reduce Null = null x
     *   reduce (Compose (f, g)) = f >> g
     * }
     *  
     * homomorphic : (a:category) -> (f:functor (a, a)) -> category
     * homomorphic a f (
     *   objet = algebra a f
     *   (~>) = homomorphism a f
     * 
     *   null F = {
     *     map = null carrier
     *   }
     * 
     * }
     * 
     * 
     * 
     * recurse : (a:category) -> (f:functor (a, a)) -> (r:algebra a map) -> (fix map ~> carrier)
     * recurse a f r = (self:fix f ~> r) <~> fmap (fix f, r) self >> reduce
     * 
     * 
     * recurse naturally (f:functor (naturally, naturally)) -> (r:algebra functiorial (map:category -> category))
     * 
     *
     * 
     * 
     * 
     * 
     * 
     * 
     * 
     * 
     * 
     * 
     * 
     * 
     * 
     * 
     * 
     * 
     * relation : * -> *
     * relation a = (a, a) -> *
     * 
     * (==) : (a:*) -> relation a
     * (==) a (x, y) = (f:a -> *) -> f x -> f y
     * 
     * category : * -> *
     * category a = {
     *   (~>) : relation a
     *   
     *   null : (x:a) -> (x ~> x)
     *   (>>) : (x:a, y:a, z:a) -> (x ~> y, y ~> z) -> (x ~> z)
     *   
     *   identityLeft : (x:a, y:a) -> (p:x ~> y) -> (null x >> p) == p
     *   identityRight : (x:a, y:a) -> (p:x ~> y) -> (p >> null y) == p
     *   associative : (w:a, x:a, y:a, z:a) -> (r:w ~> x, s:x ~> y, t:y ~> z) -> ((r >> s) >> t) == (r >> (s >> t))
     * }
     * 
     * functor : (a <: category) -> (a -> a) -> *
     * functor a f = {
     *   fmap : (x:a, y:a) -> (x ~> y) -> (f x ~> f y)
     *   
     *   identity : (x:a) -> fmap (x, x) (null x) == null (f x)
     *   compose : (x:a, y:a, z:a) -> (p:x ~> y, q:y ~> z) -> fmap (x, z) (p >> q) == (fmap (x, y) p >> fmap (y, z) q)
     * }
     * 
     * algebra : (a <: category) -> (a -> a) -> a -> *
     * algebra a f r = {
     *   reduce : f r ~> r
     * }
     * 
     * homomorphism : (a <: category) -> (f <: functor a) -> (r <: algebra a f) -> (s <: algebra a f) -> *
     * homomorphism a f r s = {
     *   map : r ~> s
     *   
     *   preserving : (xs:f r) -> (reduce xs >> map) == reduce (fmap (r, s) map xs)
     * }
     * 
     * (a <: category) -> (f <: functor a) -> category (algebra a f)
     * 
     * 
     * 
     * proof True = top
     * proof False = bottom
     * 
     * instance category boolean where
     *   (~>) (x, y) = proof (not x || y)
     * 
     * 
     * 
     * 
     * object : * -> *
     * object a = {
     *   invoke : object a -> a
     * }
     * 
     * loop : (a:*) -> object a
     * loop a = {
     *   invoke self = self .invoke self
     * }
     * 
     * recursor : (a:*) -> (a -> a) -> object a
     * recursor a f = {
     *   invoke self = f (loop a .invoke self)
     * }
     * 
     * (<~>) : (a:*) -> (a -> a) -> a
     * (<~>) = loop a .invoke (recursor a f)
     * 
     * recurse : (a <: category) -> (f <: functor a) -> (r <: algebra a f) -> (fix f ~> r)
     * recurse a f r = (self:fix f ~> r) <~> fmap (fix f, r) self >> reduce
     * 
     * 
     * 
     * 
     * 
     * 
     * 
     * instance (category *) (->) where
     *   null a x = x
     *   (>>) (a, b, c) (f, g) x = g (f x)
     * 
     * monoid : * -> *
     * monoid a = Null | Join (a, a)
     * 
     * instance (functor (->)) monoid where
     *   fmap (a, b) f Null = Null
     *   fmap (a, b) f (Compose (x, y)) = Compose (f x, f y)
     * 
     * instance (algebra (->) monoid) (vector a) where
     *   reduce Null = []
     *   reduce (Compose (u, v)) = u ++ v
     *   
     * instance (algebra (->) monoid) number where
     *   reduce Null = 0
     *   reduce (Compose (x, y)) = x + y
     *   
     * length : homomorphism (->) monoid ([], ++) (0, +) 
     * 
     */
    /*
     * boolean : *
     * boolean = True | False
     * 
     * equatable : * -> *
     * equatable a = {
     *   (=?) : (a, a) -> boolean
     * }
     * 
     */

    /*
     * absurd : *
     * absurd : [a] -> a
     * 
     * relation : * -> *
     * relation a = (a, a) -> *
     * 
     * (==) : [a] -> relation a
     * (==) a (x, y) = [f:a -> *] -> f x -> f y
     * 
     * decision : * -> *
     * decision a = Proof a | Refutation (a -> absurd)
     * 
     * equatable : * -> *
     * equatable a = {
     *   (=?) : [x:a, y:a] -> decision (x == y)
     * }
     * 
     */

    /*
     * algebra : (a <: category) -> (a -> a) -> a -> *
     * algebra a f r = {
     *   reduce : f r ~> r
     * }
     * 
     * homomorphism : (a <: category) -> (f <: functor a) -> (r <: algebra a f) -> (s <: algebra a f) -> *
     * homomorphism f r s = {
     *   convert : r ~> s
     *   preserving : (xs:f a) -> convert (reduce xs) == reduce (fmap (r, s) convert xs)
     * }
     * 
     * over : (a:*) -> (a -> a) -> relation a -> relation a
     * over f (~>) (x, y) = f x ~> f y
     * 
     * functor : (a <: category) -> (a -> a) -> *
     * functor (a, (~>)) f = homomorphism (relation a) (CategoryFunctor a) ((~>), compose) (over f (~>), compose)
     * functor a f = {
     *   fmap : (~>) => over f (~>)
     *   
     *   identity : (x:a) -> fmap (x, x) (null x) == null (f x)
     *   compose : (x:a, y:a, z:a) -> (p:x ~> y, q:y ~> z) -> (fmap (x, z) (p >> q) == (fmap (x, y) p >> fmap (y, z) q))
     * }
     * 
     * class CategoryFunctor : (a:*) -> (self:relation a) -> relation a
     *   Null : (x:a) -> CategoryFunctor a (x, x)
     *   Compose : (x:a, y:a, z:a) -> (self (x, y), self (y, z)) -> CategoryFunctor (x, z)
     *   
     * homomorphism (relation a) (CategoryFunctor a) ((~>), compose) (over f (~>), compose)
     * 
     * monoid : * -> *
     * monoid a = Null | Compose (a, a)
     * 
     * _ : (a:*) -> homomorphism * monoid (list a, append) (int, sum)
     * _ = {
     *   convert = length
     * }
     * 
     */
    /*
     * relation : * -> *
     * relation a = (a, a) -> *
     * 
     * (==) : (a:*) -> relation a
     * (==) a (x, y) = (f:a -> *) -> f x -> f y
     * 
     * category : * -> *
     * category a = {
     *   (~>) : relation a
     *   null : (x:a) -> (x ~> x)
     *   (>>) : (x:a, y:a, z:a) -> (x ~> y, y ~> z) -> (x ~> z)
     *   
     *   identityLeft : (x:a, y:a) -> (p:x ~> y) -> (null x >> p) == p
     *   identityRight : (x:a, y:a) -> (p:x ~> y) -> (p >> null y) == p
     *   associative : (w:a, x:a, y:a, z:a) -> (r:w ~> x, s:x ~> y, t:y ~> z) -> ((r >> s) >> t) == (r >> (s >> t))
     * }
     * 
     * functor : (a <: category) -> (a -> a) -> *
     * functor a f = {
     *   fmap : (x:a, y:a) -> (x ~> y) -> (f x ~> f y)
     *   
     *   identity : (x:a) -> fmap (x, x) (null x) == null (f x)
     *   compose : (x:a, y:a, z:a) -> (p:x ~> y, q:y ~> z) -> (fmap (x, z) (p >> q) == (fmap (x, y) p >> fmap (y, z) q))
     * }
     * 
     * object : * -> *
     * object a = {
     *   invoke : object a -> a
     * }
     * 
     * loop : (a:*) -> object a
     * loop a .invoke self = self .invoke self
     * 
     * recursor : (a:*) -> (a -> a) -> object a
     * recursor a f .invoke self = f (loop a self)
     * 
     * fixpoint : (a:*) -> (a -> a) -> a
     * fixpoint a f = loop a (recursor a f)
     * 
     * recurse : (a <: category) -> (f <: functor a) -> (r:a) -> (r ~> f r) -> (r ~> fixpoint a f)
     * recurse a f r step = fixpoint (r ~> fixpoint a f) \self. step >> fmap (r, fix f) self
     * 
     * 
     * 
     * listF : * -> * -> *
     * listF a r = Nil | Cons (a, r)
     * 
     * list a : * -> *
     * list a = fix (listF a)
     * 
     * reduce : (a:*) -> list a -> (r:*) -> (a -> r -> r) -> (r -> r)
     * reduce a xs r cons nil = recurse (dual *) (listF a) step xs
     *   where step : listF a r -> r
     *         step Nil = nil
     *         step (Cons (x, z)) = cons x z
     *         
     * reduce : (a:*) -> list a -> (r:*) -> (a -> r -> r) -> (r -> r)
     * reduce a xs r cons nil = recurse xs
     *   where recurse : list a -> r
     *         recurse Nil = nil
     *         recurse (Cons (x, xs)) = cons x (recurse xs)
     * 
     * iterate : (a:*) -> a -> (a -> a) -> list a
     * iterate a x f = recurse * (listF a) step x
     *   where step : a -> listF a a
     *         step s = Cons (s, s)
     * 
     * 
     * 
     * 
     * groupoid : * -> *
     * groupoid a = {
     *   (~) : relation a
     *   null : (x:a) -> (x ~ x)
     *   inverse : (x:a, y:a) -> (x ~ y) -> (y ~ x)
     *   (>>) : (x:a, y:a, z:a) -> (x ~ y, y ~ z) -> (x ~ z)
     *   
     *   identitySelf : (x:a) -> inverse (x, x) (null x) == null x
     *   involution : (x:a, y:a) -> (p:x ~ y) -> inverse (y, x) (inverse (x, y) p) == p
     *   cancelInverse : (x:a, y:a) -> (p:x ~ y) -> (p >> inverse p) == null x
     *   
     *   identityLeft : (x:a, y:a) -> (p:x ~ y) -> (null x >> p) == p
     *   identityRight : (x:a, y:a) -> (p:x ~ y) -> (p >> null y) == p
     *   associative : (w:a, x:a, y:a, z:a) -> (r:w ~ x, s:x ~ y, t:y ~ z) -> ((r >> s) >> t) == (r >> (s >> t))
     * }
     * 
     * functoid : (a <: groupoid) -> (a -> a) -> *
     * functoid a f = {
     *   fmap : (x:a, y:a) -> (x ~ y) -> (f x ~ f y)
     *   
     *   identity : (x:a) -> fmap (x, x) (null x) == null (f x)
     *   mirrored : (x:a, y:a) -> (p:x ~ y) -> fmap (y, x) (inverse (x, y) p) == inverse (fmap (x, y) p)
     *   composed : (x:a, y:a, z:a) -> (p:x ~ y, q:y ~ z) -> (fmap (x, z) (p >> q) == (fmap (x, y) p >> fmap (y, z) q))
     * }
     * 
     */
}