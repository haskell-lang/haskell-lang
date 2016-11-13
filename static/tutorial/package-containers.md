# Containers

In order to solve almost any problem, that requires manipulation of data, the
very first question should be: What is the most suitable data structure that we
can use for the problem at hand?

## Preface

Haskell is a pure functional language, thus, by it's nature, most of the
available containers are immutable and, without a doubt, the most common one is
a list `[a]`. Certainly, it is not efficiency that made it so popular, but
rather its simplicity, consequently, it is also the first data structure, that
you get introduced to while learning Haskell. Although, lists are perfectly
suitable for some problems, more often than not, we need something that is
tailored to how we are trying to use our data.

Here are some situations
that [containers](https://www.stackage.org/lts/package/containers) package can
be of help. It provides efficient implementation of some of the most commonly
used containers used in programming:
* `Data.Set` - you care about uniqueness and possibly the order of elements.
* `Data.Map` - you need a mapping from unique keys to values and operations you
  perform can take advantage of ordering of keys.
* `Data.IntSet` and `Data.IntMap` - just as above, but when elements and keys
  respectively are `Int`s.
* `Data.Sequence` - can be of use when a linear structure is required for a
  finite number of elements with fast access from both of its sides and a fast
  concatenation with other sequences.
* `Data.Tree` and `Data.Graph` - for describing more complicated relations of
  elements.

## Map

`Map` is one of the most interesting and commonly used abstractions from the
package, so most of the examples will be based on it. Moreover, interface
provided for `Map`, `IntMap`, `Set` and `IntSet` is very similar, so
analogous examples can be easily derived for all of the above.

### Setup a problem.

One of the common mappings in real life, that we encounter, is a person's
identification number, that maps a unique number to an actual human
being. Social Security Number (SSN) is what normally used for that purpose in the
USA and despite that it is not totally unique, for demonstration purpose, we can
assume it actually is. Although it is a 9 digit number and using `IntMap` would
be more efficient, it does have some structure to it and we will take advantage
of it, so we will use a custom data type `SSN` as a key.

```haskell
#!/usr/bin/env stack
-- stack --install-ghc --resolver lts-7.7 runghc

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List as List
import Data.Monoid
import Text.Printf

-- | Social Security Number. Commonly used as a unique identification number of a
-- person.
data SSN = SSN
  { ssnPrefix :: Int
  , ssnInfix :: Int
  , ssnSuffix :: Int
  } deriving (Eq, Ord)

instance Show SSN where
  show (SSN p i s) = printf "%03d-%02d-%04d" p i s

data Gender = Male | Female deriving (Eq, Show)

data Person = Person
  { firstName :: String
  , lastName :: String
  , gender :: Gender
  } deriving (Eq)

instance Show Person where
  show (Person fName lName g) = fName ++ ' ':lName ++ " (" ++ show g ++ ")"
  
type Employees = Map.Map SSN Person
```

I would like to stress how important `Eq` and `Ord` instances of a data type
used as a key actually are. Because they are essential for underlying representation
of a `Map`, providing incomplete or incorrect instances for these classes will
lead to some strange behavior of your data mappings, therefore, either make sure
you know what you are doing when creating custom instances, or use derived
instances, as they are always safe.

Because Social Security Numbers have a specific structure we would like to
enforce it by providing a constructor function that performs certain validations.

```haskell
mkSSN :: Int -> Int -> Int -> SSN
mkSSN p i s
  | p <= 0 || p == 666 || p >= 900 = error $ "Invalid SSN prefix: " ++ show p
  | i <= 0 || i > 99 = error $ "Invalid SSN infix: " ++ show i
  | s <= 0 || s > 9999 = error $ "Invalid SSN suffix: " ++ show s
  | otherwise = SSN p i s
```

### Converting maps

Let's go ahead and create our mapping of employees using `Map.fromList`:

```haskell
employees :: Employees
employees =
  Map.fromList
    [ (mkSSN 525 21 5423, Person "John" "Doe" Male)
    , (mkSSN 521 01 8756, Person "Mary" "Jones" Female)
    , (mkSSN 585 11 1234, Person "William" "Smith" Male)
    , (mkSSN 525 15 5673, Person "Maria" "Gonzalez" Female)
    , (mkSSN 524 34 1234, Person "Bob" "Jones" Male)
    , (mkSSN 522 43 9862, Person "John" "Doe" Male)
    , (mkSSN 527 75 1035, Person "Julia" "Bloom" Female)
    ]
```

As you can see above, there is no particular order to our data as we defined it,
which results in creation of a `Map` in _O(n*log n)_ time complexity, but, if we
were sure ahead of time, that our list was sorted and unique with respect to the
first element of a tuple, it would be more efficient to use `Map.fromAscList`,
which would run in _O(n)_ complexity instead.

### Operate on data.

Now that we have our program properly set up, most of available functions will
correspond directly to functions that we might try to use on our data,
e.g.:

```haskell
lookupEmployee :: SSN -> Employees -> Maybe Person
lookupEmployee = Map.lookup
```

which does exactly what is expected of it:

```haskell
λ> lookupEmployee (mkSSN 524 34 1234) employees
Just Bob Jones (Male)
λ> lookupEmployee (mkSSN 555 12 3456) employees
Nothing
```

In order to refrain from redefining functions which trivially correspond to existing
ones, let's go through some of them:

* Checking presence of an employee by the social security number:
```haskell
λ> mkSSN 585 11 1234 `Map.member` employees
True
λ> mkSSN 621 24 8736 `Map.member` employees
False
```
* Looking up an employee with a default name:
```haskell
λ> Map.findWithDefault (Person "Bill" "Smith" Male) (mkSSN 585 11 1234) employees
William Smith (Male)
λ> Map.findWithDefault (Person "Anthony" "Richardson" Male) (mkSSN 621 24 8736) employees
Anthony Richardson (Male)
```
* Getting total number of employees.
```haskell
λ> Map.size employees
7
```
* Deleting an employee:
```haskell
λ> Map.size $ Map.delete (mkSSN 585 11 1234) employees
6
λ> Map.size $ Map.delete (mkSSN 621 24 8736) employees
7
```
* Adding an employee:
```haskell
λ> Map.size $ Map.insert (mkSSN 621 24 8736) (Person "Anthony" "Richardson" Male) employees
8
```

### Folding

It would be useful to present our `Employees` in a user friendly format, so
let's define a `showMap` function by converting our `Map` to a printable string:

```haskell
showMap :: (Show k, Show v) => Map.Map k v -> String
showMap = List.intercalate "\n" . map show . Map.toList
```

Let's give it a try:

```haskell
λ> putStrLn $ showMap employees
(521-01-8756,Mary Jones (Female))
(522-43-9862,John Doe (Male))
(524-34-1234,Bob Jones (Male))
(525-15-5673,Maria Gonzalez (Female))
(525-21-5423,John Doe (Male))
(527-75-1035,Julia Bloom (Female))
(585-11-1234,William Smith (Male))
```

As you can see, all employees are sorted by their SSN, so conversion to a list is
done in ascending order, but it is worth noting, that if there is a desire to
guarantee this behavior, `Map.toAscList` should be used instead, or
`Map.toDescList` to get it in a reverse order.

Conversion is nice an simple, but how about using folding in a way that is
native to a `Map`? While we are at it, we should probably improve formatting as
well.

```haskell
showEmployee :: (SSN, Person) -> String
showEmployee (social, person) =
  concat [show social, ": ", show person]

showEmployees :: Employees -> String
showEmployees es
  | Map.null es = ""
  | otherwise = showE ssn0 person0 ++ Map.foldrWithKey prepender "" rest
  where
    showE = curry showEmployee
    ((ssn0, person0), rest) = Map.deleteFindMin es
    prepender key person acc = '\n' : showE key person ++ acc

printEmployees :: Employees -> IO ()
printEmployees = putStrLn . showEmployees
```

Now that looks slightly better:

```haskell
λ> printEmployees employees
521-01-8756: Mary Jones (Female)
522-43-9862: John Doe (Male)
524-34-1234: Bob Jones (Male)
525-15-5673: Maria Gonzalez (Female)
525-21-5423: John Doe (Male)
527-75-1035: Julia Bloom (Female)
585-11-1234: William Smith (Male)
```

__Exercise__: Using the fact that List is an instance of `Monoid` and implement
`showEmployees` with the help of `Map.foldMapWithKey`.

__Exercise__: Implement `showEmployeesReversed` using `Map.foldlWithKey`
(_hint_: use `Map.deleteFindMax`).


### Mapping

There is a collection of mapping functions available, which range from a simple
`Map.map` to a more complex `Map.mapAccumRWithKey`.

Because `Map` is an instance of `Functor` we can use `fmap` for mapping a
function over it's values, but for this example we will use it's native equivalent
`Map.map` to  retain only last names of employees and `Map.elems` to retrieve a list of
new elements:

```haskell
λ> Map.elems $ Map.map lastName employees
["Jones","Doe","Jones","Gonzalez","Doe","Bloom","Smith"]
```

If for some reason, we would like to map a function over keys, we can use
`Map.mapKeys` for this purpose, for instance getting a list of all SSN prefixes:

```haskell:
λ> Map.keys $ Map.mapKeys (show . ssnPrefix) employees
["521","522","524","525","527","585"]
```

We need to pay some extra attention to usage of `Map.mapKeys`, because whenever
a function that is being mapped over is not 1-to-1, it is possible that some
values will be lost. Although, sometimes discarding some elements maybe desired
or simply irrelevant, here is an example of how we can mistakenly lose an
employee if we assume that last four numbers of a social are unique:

```haskell
λ> putStrLn $ showMap $ Map.mapKeys ssnSuffix employees
(1035,Julia Bloom)
(1234,William Smith)
(5423,John Doe)
(5673,Maria Gonzalez)
(8756,Mary Jones)
(9862,John Doe)
```

If we are sure that our function is not only 1-to-1, but it is also monotonic
(i.e. it doesn't change the order of the resulting keys) we could use a more
efficient mapping function `Map.mapKeysMonotonic`. Say a `show` function on
`SSN` would be safe to use, since ordering would be preserved. One of the
simplest examples of a non-monotonic function would be `negate` and a
strictly-monotonic one: `succ`.

## Filtering

Let's start with a couple of simple examples:

```haskell
λ> printEmployees (Map.filter (("Jones"==) . lastName) employees)
521-01-8756: Mary Jones (Female)
524-34-1234: Bob Jones (Male)
```

Partitioning by gender:
```haskell
λ> let (men, women) = Map.partition ((Male==) . gender) employees
λ> printEmployees men
522-43-9862: John Doe (Male)
524-34-1234: Bob Jones (Male)
525-21-5423: John Doe (Male)
585-11-1234: William Smith (Male)
λ> printEmployees women
521-01-8756: Mary Jones (Female)
525-15-5673: Maria Gonzalez (Female)
527-75-1035: Julia Bloom (Female)
```


[Prior to June 25th, 2011](<https://www.ssa.gov/employer/stateweb.htm>), Social
Security prefixes, were restricted to states where they were issued in. Let's
assume this is still the case and use this information to figure out which
states our employees received their Social Security Cards in.

First, we need to define a function, that retrieves employees within a
prefix range, so a naïve approach would be to use `Map.filterWithKey`:

```haskell
withinPrefixRangeNaive :: Int -> Int -> Employees -> Employees
withinPrefixRangeNaive prefixLow prefixHigh = Map.filterWithKey ssnInRange where
  ssnInRange (SSN prefix _ _) _ = prefix >= prefixLow && prefix <= prefixHigh
```

which runs in _O(n)_, but we can do better than that, simply by taking
advantage of ordering of keys:

```haskell
withinPrefixRange :: Int -> Int -> Employees -> Employees
withinPrefixRange prefixLow prefixHigh =
  fst . Map.split (SSN (prefixHigh + 1) 0 0) . snd . Map.split (SSN prefixLow 0 0)

employeesFromColorado :: Employees -> Employees
employeesFromColorado = withinPrefixRange 521 524
```

Naturally, this function will give us all employees that got their Social
Security Card in Colorado:

```haskell
λ> printEmployees $ employeesFromColorado employees
521-01-8756: Mary Jones
522-43-9862: John Doe
524-34-1234: Bob Jones
```

That worked well for Colorado state, but some states have noncontiguous groups
of area numbers, which means we need to join together results from a couple of
ranges:

```haskell
employeesFromNewMexico :: Employees -> Employees
employeesFromNewMexico es =
  withinPrefixRange 525 525 es `Map.union` withinPrefixRange 585 585 es
```


## Sets and Maps

Until previous example, we've looked at functions that deal only with a single
`Map`, but most of the familiar functions from set theory are also made
available to us, so we can easily operate on more than one `Map`/`Set`. In order
to provide some meaningful examples let's define geographic regions of the USA
and their SSN prefix ranges:


### Creation

```haskell
data State =
  Arizona | California | Colorado | NewMexico | Nevada | Oklahoma | Texas | Utah | ...
  deriving (Show, Eq, Ord, Enum)

statePrefixRangeMap :: Map.Map State [(Int, Int)]
statePrefixRangeMap =
  Map.fromList
    [ (Arizona, [(526, 527)])
    , (California, [(545, 573)])
    , (Colorado, [(521, 524)])
    , (NewMexico, [(525, 525), (585, 585)])
    , (Nevada, [(530, 530), (680, 680)])
    , (Oklahoma, [(440, 448)])
    , (Texas, [(449, 467)])
    , (Utah, [(528, 529)])
      ...
    ]

allStates :: Set.Set State
allStates = Set.fromDistinctAscList [toEnum 0 ..]
```

For compactness, only states that are considered the South West are included,
complete source code can be found as
a [gist](https://gist.github.com/lehins/8003b8e7cd2e56d4da1c2d75c43f4135).

Note, that because we are using `toEnum` and `enumFrom` - (`..`), we are guaranteed
that all States will be unique and in a proper ascending order, thus we are safe
to use `Set.fromDistinctAscList` instead of a less efficient `Set.fromList` or even
`Set.fromAscList`.

### Binary operations

Exclude employees from New Mexico:

```haskell
λ> printEmployees (employees Map.\\ employeesFromNewMexico employees)
521-01-8756: Mary Jones
522-43-9862: John Doe
524-34-1234: Bob Jones
527-75-1035: Julia Bloom
```

Some common manipulations on `Set`s:

```haskell
λ> let fourCorners = Set.fromList [Arizona, NewMexico, Colorado, Utah]
λ> let borderStates = Set.fromList [California, Arizona, NewMexico, Texas]
λ> Set.union fourCorners borderStates
fromList [Arizona,California,Colorado,NewMexico,Texas,Utah]
λ> Set.intersection fourCorners borderStates
fromList [Arizona,NewMexico]
λ> Set.difference fourCorners borderStates
fromList [Colorado,Utah]
λ> Set.difference borderStates fourCorners
fromList [California,Texas]
λ> let symmetricDifference a b = Set.union a b Set.\\ Set.intersection a b
λ> symmetricDifference fourCorners borderStates
fromList [California,Colorado,Texas,Utah]
```


### Conversion

Map a State to a set of SSN prefixes that correspond to it:

```haskell
statePrefixMap :: Map.Map State (Set.Set Int)
statePrefixMap =
  Map.fromSet (Set.fromList . concatMap (uncurry enumFromTo) . (statePrefixRangeMap Map.!)) allStates
```

Inverse of what we have above: Map from prefix to `State`:

```haskell
prefixStateMap :: Map.Map Int State
prefixStateMap = Map.foldlWithKey addPrefixes Map.empty statePrefixMap where
  addPrefixes spm state = Map.union spm . Map.fromSet (const state)
```

### Transformation

Using above Map we can list all employees we have per state:

```haskell
-- | Transform `Map` to another `Map`
statePersonsMap :: Employees -> Map.Map State [Person]
statePersonsMap = Map.foldlWithKey updateState Map.empty
  where updateState sm ssn p =
          case Map.lookup (ssnPrefix ssn) prefixStateMap of
            Nothing    -> sm
            Just state -> Map.alter (consPerson p) state sm
        consPerson p Nothing = Just [p]
        consPerson p (Just ps) = Just (p:ps)
```

And create a `Set` of all Social Security Numbers of our employees per State:

```haskell
-- Transform `Map` to `Set` to `Map`
stateSocialsMap :: Employees -> Map.Map State (Set.Set SSN)
stateSocialsMap = Set.foldl updateState Map.empty . Map.keysSet
  where updateState sm ssn =
          case Map.lookup (ssnPrefix ssn) prefixStateMap of
            Nothing    -> sm
            Just state -> Map.alter (addSSN ssn) state sm
        addSSN ssn Nothing = Just $ Set.singleton ssn
        addSSN ssn (Just ssnSet) = Just $ Set.insert ssn ssnSet
```

Here is a trial run on our `employees` data base:

```haskell
λ> putStrLn $ showMap $ statePersonsMap employees
(Arizona,[Julia Bloom (Female)])
(Colorado,[Mary Jones (Female),John Doe (Male),Bob Jones (Male)])
(NewMexico,[Maria Gonzalez (Female),John Doe (Male),William Smith (Male)])
λ> putStrLn $ showMap $ stateSocialsMap employees
(Arizona,fromList [527-75-1035])
(Colorado,fromList [521-01-8756,522-43-9862,524-34-1234])
(NewMexico,fromList [525-15-5673,525-21-5423,585-11-1234])
```

__Alternative approach__

Although desired result is achieved, we can do better in terms of performance,
since above implementation does not take advantage of the ordering of Social
Security Numbers that is made available to us by `Map` interface.

First, we want to generalize our `employeesFrom*` functions and then use it to
partition our `Employees`:

```haskell
employeesFrom :: State -> Employees -> Employees
employeesFrom state es = Map.unions $ map fromRange (statePrefixRangeMap Map.! state)
  where fromRange (low, high) = withinPrefixRange low high es

allStateEmployeesMap :: Employees -> Map.Map State Employees
allStateEmployeesMap es = Map.fromSet (`employeesFrom` es) allStates
```

Calling `allStateEmployeesMap` will produce undesired empty Maps of `Employees`
for some States, but there is a reason behind it, it will help us demonstrate
similarities between `Map.filter` and `Map.mapMaybe`:

```haskell
statePersonsMap' :: Employees -> Map.Map State [Person]
statePersonsMap' = Map.map Map.elems . Map.filter (not . Map.null) . allStateEmployeesMap

stateSocialsMap' :: Employees -> Map.Map State (Set.Set SSN)
stateSocialsMap' = Map.mapMaybe nonEmptyElems . allStateEmployeesMap
  where nonEmptyElems sem | Map.null sem = Nothing
                          | otherwise = Just $ Map.keysSet sem
```

### Subset and Submap.

Here are two equivalent approaches of how we would check if we are missing a
`State` from our `Map`:

```haskell
λ> Set.isProperSubsetOf (Map.keysSet $ allStateEmployeesMap employees) allStates
False
λ> Set.isProperSubsetOf (Map.keysSet $ statePersonsMap employees) allStates
True
λ> Map.isProperSubmapOfBy (const . const True) (allStateEmployeesMap employees) statePrefixMap
False
λ> Map.isProperSubmapOfBy (const . const True) (statePersonsMap employees) statePrefixMap
True
```


## Conclusion

In this tutorial we looked at very useful `Map` and `Set` containers, but there
are plenty of other packages that provide implementations of all sorts of data
structures that might be a better match for your needs. In particular, if keys and
elements for `Map` and `Set` respectively are hashable and their ordering is of
no importance, it is recommended to use `HashMap` and `HashSet`
from
[unordered-containers](https://www.stackage.org/lts/package/unordered-containers) instead.
That package provides very similar interface to one from [containers](https://www.stackage.org/lts/package/containers) library, but it has
[much smaller memory impact](https://www.fpcomplete.com/blog/2016/05/weigh-package#containers-vs-unordered-containers) and
better performance.

Here is a list of some packages and corresponding tutorials, that can help you choose and
get started with an appropriate data structure for your problem:

* [vector](https://www.stackage.org/lts/package/vector): [Efficient Packed-Memory Data Representations - vector library](https://haskell-lang.org/library/vector)
* [bytestring](https://www.stackage.org/lts/package/bytestring) and [text](https://www.stackage.org/lts/package/text): [String Types](https://haskell-lang.org/tutorial/string-types)
