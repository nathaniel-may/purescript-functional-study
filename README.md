![ci](https://github.com/nathaniel-may/purescript-zipperm/actions/workflows/main.yml/badge.svg?branch=main)

# purescript-functional-study

When an artist draws a particular real person for the first time, sometimes they'll do a study first and make dozens of sketches of the person with references to capture their expressions and posture in different perspectives before starting the final piece. This repository is a similar study- a series of sketches of different datatypes to see where their category theory instances land and to deepen my understanding of functional data structures with effects. I probably should have implemented this in Haskell instead of Purescript, but it started out as an attempt to build a library for a front end project so here we are. The code is not particularly elegant but it all mostly works. Below is a little bit about each datatype I sketched out:

## Zipper
I find the `Zipper` datatype so cute. There's nothing special about this one, I just wanted the practice defining the instances for it.

## ZipperM
`ZipperM` is an effectful zipper where the focus is "unwrapped." Unlike a `Zipper (m a)` as you unwrap items in a `ZipperM m a`, they will be put back with `pure` so the effects are only ever executed once.

## BufferedZipper
`BufferedZipper` behaves exactly like `ZipperM` but you can set a limit to how many values it will continue to store, allowing the zipper to evict values rather than keep them around forever. This relies on the `CacheM` type which was pretty fun to implement. Overall this ends up being far more complex than its worth and drove me to think of better ways to do this kind of automatic cache eviction.

I also implemented this in Scala a few years back which was a fun project that I'm quite happy with.

## CacheM
You can see from the definition that `CacheM` stores both the monadic action and the value itself, allowing for execution and eviction of a single value. Interestingly, this datatype is an Applicative but not a Monad. It's also so close to being a proper Comonad, but the caching itself clearly breaks the left identity law so it's not.

## Necklace
`Necklace` is a type most often seen in math. It's like a zipper, but it wraps around. It's not nearly as cute though because it's not an algebraic data type. I implemented this to practice reasoning about Comonads and to practice implementing their instances. In the Comonad instance for `NonEmptyList` you could implement `duplicate` as tails or permutations but tails is a little more natural. In the Comand instance for a `Necklace` permutations is the only remotely reasonable option. It's also exactly one of the common usecases for necklaces in combinatorial species.

## NecklaceM
Thought about it, but didn't because it would have been messy for very little gain.

## MonadCache, CacheT
After all this thought about specialized cache eviction for effectful zippers I wanted to implement the caching outside the datatype so you could easily swap your zipper for a list or a hashmap and not have to rewrite all your effect handling. This lead to what I think is a fairly elegant monad transformer to store and retrieve elements from the cache. It's mostly a specialized instance of StateT, but it has the convenience function `MonadCache.fetch :: forall m k v. MonadCache m k v => (k -> m v) -> k -> m v`. So you can easily turn an effectul expression like `traverse fetchOverNetwork inputs` into `traverse (MonadCache.fetch fetchOverNetwork) inputs` and your cache will be utilized to only perform the effect for each input exactly once even if some of the inputs are repeated. But this still doesn't have the convenience of a fixed-size cache that will evict elements automatically like `BufferedZipper` had.

## RollingCache
`RollingCache` is basically a fixed-size `HashMap` that deletes the elements that were least-recently retrieved. It was implemented with the intent for it to be used as the state in `StateT`.

## MonadRollingCache, RollingCacheT
These types are basically the same as `MonadCache` and `CacheT` except that they use `RollingCache` instead of a `HashMap` for their state. This allows the cache to be pinned to a finite size, and for eviction of the least-recently used elements to be automatically evicted as you use the cache. An example of using this can be found in test/Examples/RollingCache.purs, and that example can be run with `make example`. This seems particularly practical to me, and I'll probably use it in a future project. Since it's just a specialized `StateT`, I'm probably not going to bother making a library out of it, but feel free to copy paste it into your project to make it your own if you like the idea.

Now in the real world, this kind of caching is generally done in your database, and on caching layers with something like Redis so that your client could connect to different stateless servers on each request and not suffer performance hits due to cold caches. But this could still be very useful client-side to avoid using cookies as caches, or server-side for low footprint applications that only operate with one server.
