using System;

namespace Recursion
{
    /*
     * 
     * relation : * -> *
     * relation a = (a, a) -> *
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
     * fix : (a:*) -> (a -> a) -> a
     * fix a f = loop a (recursor a f)
     * 
     * recurse : (a <: category) -> (f <: functor a) -> (r:a) -> (r ~ f r) -> (r ~ fix a f)
     * recurse a f r step = fix (r ~ fix a f) \self. step <> fmap (r, f r) self
     * 
     */
}