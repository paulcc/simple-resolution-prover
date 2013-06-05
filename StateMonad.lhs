> module StateMonad where

A Monad for handling Mutable (update-able) state

(NOT ENTIRELY HAPPY ABOUT THE NOTES HERE - lectures will contain more info)

%----------------------------------------------------------------------------
One way of handling (mutable) state is to model a state-using computation
as a FUNCTION which takes an input state and produces an output state plus 
some value.

We can use the framework of MONADS to make this easier to use.

Remember how the IO monad helps us get IO actions in sequence? Sequencing is
also important for state manipulation too - reads and updates need to be 
done in a predictable order.

The following wraps the state-changing function in an algebraic type, 
polymorphic over the type of state (s) and of value (a).

> data ST s a = MkST (s -> (a,s))
>       --            |     | |
>       --            |     | +-- output state
>       --            |     +---- result of computation
>       --            +---------- input state

(We need to wrap it up, in order to make instance declarations.)

Terminology: the name ST is a TYPE CONSTRUCTOR, taking two types.
That is, given two types, it gives us a new type. Compare this 
to Maybe, which takes one type. You can think of such type 
constructors as `functions' at the type level of the language.

Haskell does check that such type constructors are applied to 
the correct number of `arguments', and that the arguments are
the right type. In effect, it does a kind of type checking 
at the type level. To avoid confusion, the `types' of type 
expressions are called KINDS. 

The kind '*' represents actual types, and the kind (a -> b) represents
a type constructor which takes a type of kind 'a' and returns a type of
kind 'b'. (Uses of the arrow can be nested, for complex types - and 
the arrow is right-associative.)

The kind of 'ST' is (* -> * -> *), because it takes two types and returns
another type.

Are these all acceptable type expressions? (justify)
 - ST Int Bool
 - ST (ST Int Bool) Int
 - ST ST Int
 - ST (Int -> Int) String



%---------------------------------------
Before saying how ST becomes a monad, we need to say how it is a FUNCTOR.
  (it probably isn't used in the code, but it is part of the technical 
   conditions of being a monad, as explained when I discuss laws, below.)

<> class Functor f where                     -- contained in Prelude
<>    fmap :: (a -> b) -> (f a -> f b)       -- hence commented out.

The Functor "class" gives a single function `fmap' (for "functor map"), 
which provides an overloaded name for the concept of mapping.

Mapping is about changing values of some type 'a' in a structure of type 
(f a) to values of type 'b', via some function of type (a -> b), without
modifying the shape of the structure. The way the mapping works depends 
ONLY on the structure of the `container' type, not on the types of the
values inside the container. So, once we know which form of container
is being used, we can select a class instance which tells us how to do 
the mapping for that container.

This use of classes is an extension of the simple class system, which only 
deals with types of kind '*'. "Functor" is an example of a "constructor 
class" (usually we just say 'class'), which deals with type constructors.
Functor is only usable with 1-ary type constructors (whose kind is * -> *).
Here's how instances for lists and Maybe are declared:

eg
<> instance Functor [] where         -- [] is the type constr. for lists
<>    fmap = map                     -- fmap is the standard map.
eg
<> instance Functor Maybe where
<>    fmap f Nothing  = Nothing      -- nothing to change
<>    fmap f (Just x) = Just (f x)   -- unpack the x, change, re-pack


Below, we want to apply function `g` to the non-state value, and leave 
the state unchanged. The value we map over is a function, so we need to 
modify that function so it returns the changed `a' value.

> instance Functor (ST s) where
>   fmap g (MkST sf)      -- unpack the function
>    = MkST               -- re-wrap (later)
>    $ \s ->              -- get input state
>      let (a,s2) = sf s  -- `run' the function, get val + new state
>      in  (g a, s2)      -- change the val. with g, don't change state


Note that the instance mentioned (ST s). We can't say just ST here, because 
ST is 2-ary whereas Functor expects a 1-ary constructor. But the 's' doesn't 
play any part in the functor-ness, so it is ok to add it in.  ST monads will
work with ANY state type (and any value type).


%----------------------------------------------------------------------------
Next, Monads. 

Like Functors, Monads are also a form of type class for type constructors,
but they overload two functions, whose names you may recognise from IO.
IO is just an instance of this class. Note that do-notation is based on 
(>>=), so any instance of the Monad class can use do-notation!

<> class Monad m where                   -- NB 'm' has kind (* -> *)
<>    return :: a -> m a
<>    (>>=)  :: m a -> (a -> m b) -> m b


First, we'll see the monad instance for Maybe, as it is simpler. 
We need to give two definitions:

<> return_Maybe :: a -> Maybe a
<> bind_Maybe   :: Maybe a -> (a -> Maybe b) -> Maybe b
<> ---- (>>=)   :: m     a -> (a -> m     b) -> m     b


With suitable definitions of these, Maybe can be used to simulate simple 
exception handling. Think of Maybe as a simple container - either empty 
(Nothing) or full (Just x).  `return' is easy - it should `inject' the 
'a' into a monadic setting, ie wrap it with Just.

<> return_Maybe a = Just a


The bind definition does the real work. Exception-generating computations
must be done in SEQUENCE, and a series of computations stopped as soon as 
any one fails. What we want is a way to build sequences, which has this 
property. The following does that - when the first computation gives Nothing,
then we don't look at the second computation. Otherwise, take the value and
feed it to the next computation.

<> bind_Maybe Nothing  k = Nothing
<> bind_Maybe (Just a) k = k a


The intuitive argument about these definitions being `right' will be further
justified when we look at monad laws, below. 


Finally, we can make Maybe a monad with an instance declaration.
    (The Prelude already contains this code.)

<> instance Monad Maybe where
<>    return = return_Maybe
<>    (>>=)  = bind_Maybe



---
An Example: (integer) division by zero
 - div0 `fails' if the divisor is 0 (result of division is undefined)
 - otherwise, returns the answer (wrapped up in Just)
 - div_some does some of these error-prone divisions
 - if all of them succeed, then the final value is returned to the caller
 - if one fails, they all fail

<> div0 :: Int -> Int -> Maybe Int
<> div0 x 0 = Nothing
<> div0 x y = return $ x `div` y

<> div_some 
<>  = do
<>       x <- div0 100 2
<>       y <- div0 100 0
<>       z <- div0 100 3
<>       return (x + y + z)

EXERCISE: write the explicit form of div_some without do-notation (ie, using
 (>>=)) - this should make the stop-on-first-failure point clearer.






%----------------------------------------------------------------------------
Next, the ST monad. Again, we need to give two definitions:

> return_ST :: a -> ST s a
> bind_ST   :: ST s a -> (a -> ST s b) -> ST s b

State-based computations must be done in sequence, so the updates happen in 
the right order. In ST, the notion of sequencing is to combine two state-
handling computations by tie-ing the state together - the output state of 
one is the input state of the next.

Consider what `return' should do: we want it to create a monad value which
does nothing except return the given value. How's this? 

> return_ST a 
>  = MkST    -- wrapper
>  $ \s ->   -- input state
>    (a, s)  -- given value, with input state

The bind operation is harder. We want to join two state-handling computations
together. Essentially, we take an input state, run the first computation to
get a value `a' and a new state, get the second monad by passing the 'a' 
to the function 'k', then feed the new state in to the second monad. The
overall result is the result from the second monad. 

Try drawing this as a wiring diagram! It is very much like plumbing.

> (MkST sf) `bind_ST` k 
>  = MkST                  -- re-wrap (later)
>  $ \s ->                 -- input state
>    let (a,s2) = sf s     -- run first monad, get value + new state
>        (MkST sf2) = k a  -- get (+unpack) second monad by applying k to a
>    in sf2 s2             -- pass new state to second monad
>                          -- output value & state come from second monad


Finally, we can make (ST s) a monad with an instance declaration.

> instance Monad (ST s) where
>   return = return_ST
>   (>>=)  = bind_ST



%----------------------------------------------------------------------------
Auxiliary operations
 - these are extra operations we'd want for a state-handling monad.
 
 - running a state computation - take an input state, return the final state
   and result
 - get the current state
 - set the current state
 - change the current state by applying some function
 - note how the last two have () as the value - no other value is sensible

> runST :: s -> ST s a -> (a,s)
> runST s (MkST sf) = sf s

> getST :: ST s s 
> getST = MkST $ \s -> (s,s)

> setST :: s -> ST s ()
> setST s = MkST $ \_ -> ((), s)

> chgST :: (s -> s) -> ST s ()
> chgST f = MkST $ \s -> ((), f s)




%----------------------------------------------------------------------------
Monad Laws

There are FOUR `laws' which an implementation of a Monad should obey.
   (===) means "is equivalent to".

(*) return is a (left/right) identity of >>=
    1) return a >>= k === k a    -- left identity
    2) m >>= return === m        -- right identity

    These say: return really does do nothing, except return its arg!

(*) associativity of >>=
    3) m >>= (\x -> k x >>= h) === (m >>= k) >>= h

    This says: it doesn't matter how you bracket the binds - the result 
    is identical.

(*) action of fmap
    4) fmap f m === m >>= (return . f)

    This says: mapping a function over a monad value m is the same as 
    doing the computation and applying f to the result. Ie, 

Here's the laws written as do-notation

1) do { x <- return a; k x} === k a
2) do { a <- m; return a }  === m
3) do { x <- m; do { y <- k x; h y } } 
                  === do { z <- do { x <- m; k x } }; h z }
4) fmap f m === do { x <- m ; return (f x) }


How do we prove these? Basically, by expanding or contracting definitions
(these represent equalities, don't forget), rearranging terms to allow 
definitions to be used, and by using basic facts about Haskell. The 
following example should give you an idea of the process.

For this course, I don't expect formal proofs, but I do expect you to show 
signs of knowing how to justify that a law holds for a definition. 


Example: law (2) for ST.

*) m >>= return 
   -- unpack m - suppose it is (MkST sf)

*) (MkST sf) >>= return 
   -- expand >>=

*) MkST $ \s -> let (a,s2) = sf s 
                    (MkST sf2) = return a
                in sf2 s2
   -- expand return

*) MkST $ \s -> let (a,s2) = sf s 
                    (MkST sf2) = MkST (\q -> (a,q))
                in sf2 s2
   -- infer what sf2 must be, and substitute

*) MkST $ \s -> let (a,s2) = sf s 
                    (MkST sf2) = MkST (\q -> (a,q))
                in (\q -> (a,q)) s2
   -- reduce end expression, remove the used let

*) MkST $ \s -> let (a,s2) = sf s 
                in (a,s2)
   -- remove used let

*) MkST $ \s -> sf s
   -- (\s -> f s) and (f) mean the same.

*) MkST $ sf
   -- this is what we supposed 'm' to be, hence proved.





%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
Example: Summing a list, by adding each value to the current state

sum_list steps through the list
 - At the end, it stops and returns ()
 - otherwise, uses chgST to increment the state and processes remaining items
do_sum
 - runs the state computation with initial value zero, and collects
   the final state.

<> sum_list :: [Int] -> ST Int ()
<> sum_list [] 
<>  = return ()
<> sum_list (x:xs) 
<>  = do
<>        chgST (s -> s + x)
<>        sum_list xs

<> do_sum :: [Int] -> Int
<> do_sum xs
<>  = final_state
<>    where
<>        init_value = 0
<>        (_, final_state) = runST init_value $ sum_list xs


