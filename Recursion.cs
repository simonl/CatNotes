using System;

namespace Recursion
{
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
     *   reduce : f r ~ r
     * }
     * 
     * homomorphism : (a <: category) -> (f <: functor a) -> (r <: algebra a f) -> (s <: algebra a f) -> *
     * homomorphism f r s = {
     *   convert : r ~ s
     *   preserving : (xs:f a) -> convert (reduce xs) == reduce (fmap (r, s) convert xs)
     * }
     * 
     * over : (a:*) -> (a -> a) -> relation a -> relation a
     * over f (~) (x, y) = f x ~ f y
     * 
     * functor : (a <: category) -> (a -> a) -> *
     * functor (a, (~)) f = homomorphism (relation a) (CategoryFunctor a) ((~), compose) (over f (~), compose)
     * functor a f = {
     *   fmap : (~) => over f (~)
     *   
     *   identity : (x:a) -> fmap (x, x) (null x) == null (f x)
     *   compose : (x:a, y:a, z:a) -> (p:x ~ y, q:y ~ z) -> (fmap (x, z) (p <> q) == (fmap (x, y) p <> fmap (y, z) q))
     * }
     * 
     * class CategoryFunctor : (a:*) -> (self:relation a) -> relation a
     *   Null : (x:a) -> CategoryFunctor a (x, x)
     *   Compose : (x:a, y:a, z:a) -> (self (x, y), self (y, z)) -> CategoryFunctor (x, z)
     *   
     * homomorphism (relation a) (CategoryFunctor a) ((~), compose) (over f (~), compose)
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
     *   (~) : relation a
     *   null : (x:a) -> (x ~ x)
     *   (<>) : (x:a, y:a, z:a) -> (x ~ y, y ~ z) -> (x ~ z)
     *   
     *   identityLeft : (x:a, y:a) -> (p:x ~ y) -> (null x <> p) == p
     *   identityRight : (x:a, y:a) -> (p:x ~ y) -> (p <> null y) == p
     *   associative : (w:a, x:a, y:a, z:a) -> (r:w ~ x, s:x ~ y, t:y ~ z) -> ((r <> s) <> t) == (r <> (s <> t))
     * }
     * 
     * functor : (a <: category) -> (a -> a) -> *
     * functor a f = {
     *   fmap : (x:a, y:a) -> (x ~ y) -> (f x ~ f y)
     *   
     *   identity : (x:a) -> fmap (x, x) (null x) == null (f x)
     *   compose : (x:a, y:a, z:a) -> (p:x ~ y, q:y ~ z) -> (fmap (x, z) (p <> q) == (fmap (x, y) p <> fmap (y, z) q))
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
     * recurse : (a <: category) -> (f <: functor a) -> (r:a) -> (r ~ f r) -> (r ~ fixpoint a f)
     * recurse a f r step = fixpoint (r ~ fixpoint a f) \self. step <> fmap (r, f r) self
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
     *   (<>) : (x:a, y:a, z:a) -> (x ~ y, y ~ z) -> (x ~ z)
     *   
     *   identitySelf : (x:a) -> inverse (x, x) (null x) == null x
     *   involution : (x:a, y:a) -> (p:x ~ y) -> inverse (y, x) (inverse (x, y) p) == p
     *   cancelInverse : (x:a, y:a) -> (p:x ~ y) -> (p <> inverse p) == null x
     *   
     *   identityLeft : (x:a, y:a) -> (p:x ~ y) -> (null x <> p) == p
     *   identityRight : (x:a, y:a) -> (p:x ~ y) -> (p <> null y) == p
     *   associative : (w:a, x:a, y:a, z:a) -> (r:w ~ x, s:x ~ y, t:y ~ z) -> ((r <> s) <> t) == (r <> (s <> t))
     * }
     * 
     * functoid : (a <: groupoid) -> (a -> a) -> *
     * functoid a f = {
     *   fmap : (x:a, y:a) -> (x ~ y) -> (f x ~ f y)
     *   
     *   identity : (x:a) -> fmap (x, x) (null x) == null (f x)
     *   mirrored : (x:a, y:a) -> (p:x ~ y) -> fmap (y, x) (inverse (x, y) p) == inverse (fmap (x, y) p)
     *   composed : (x:a, y:a, z:a) -> (p:x ~ y, q:y ~ z) -> (fmap (x, z) (p <> q) == (fmap (x, y) p <> fmap (y, z) q))
     * }
     * 
     */
}