# Module Documentation

## Module Data.Char


A type and functions for single characters.

#### `Char`

``` purescript
newtype Char
```

A unicode character.

#### `charString`

``` purescript
charString :: Char -> String
```

Returns the string of length `1` containing only the given character.

#### `toCharCode`

``` purescript
toCharCode :: Char -> Number
```

Returns the numeric Unicode value of the character.

#### `fromCharCode`

``` purescript
fromCharCode :: Number -> Char
```

Constructs a character from the given Unicode numeric value.

#### `eqChar`

``` purescript
instance eqChar :: Eq Char
```

Characters can be compared for equality with `==` and `/=`.

#### `ordChar`

``` purescript
instance ordChar :: Ord Char
```

Characters can be compared with `compare`, `>`, `>=`, `<` and `<=`.

#### `showChar`

``` purescript
instance showChar :: Show Char
```

Characters can be rendered as a string with `show`.


## Module Data.String


Wraps the functions of Javascript's `String` object.
A String represents a sequence of characters.
For details of the underlying implementation, see [String Reference at MDN](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String).

#### `charAt`

``` purescript
charAt :: Number -> String -> Maybe Char
```

Returns the character at the given index, if the index is within bounds.

#### `fromChar`

``` purescript
fromChar :: Char -> String
```

Returns a string of length `1` containing the given character.

#### `singleton`

``` purescript
singleton :: Char -> String
```

Returns a string of length `1` containing the given character.
Same as `fromChar`.

#### `charCodeAt`

``` purescript
charCodeAt :: Number -> String -> Maybe Number
```

Returns the numeric Unicode value of the character at the given index,
if the index is within bounds.

#### `null`

``` purescript
null :: String -> Boolean
```

Returns `true` if the given string is empty.

#### `uncons`

``` purescript
uncons :: String -> Maybe { tail :: String, head :: Char }
```

Returns the first character and the rest of the string,
if the string is not empty.

#### `takeWhile`

``` purescript
takeWhile :: (Char -> Boolean) -> String -> String
```

Returns the longest prefix (possibly empty) of characters that satisfy
the predicate:

#### `dropWhile`

``` purescript
dropWhile :: (Char -> Boolean) -> String -> String
```

Returns the suffix remaining after `takeWhile`.

#### `fromCharArray`

``` purescript
fromCharArray :: [Char] -> String
```

Converts an array of characters into a string.

#### `indexOf`

``` purescript
indexOf :: String -> String -> Number
```

Returns the index of the first occurrence of the first string in the
second string. Returns `-1` if there is no match.

#### `indexOf'`

``` purescript
indexOf' :: String -> Number -> String -> Number
```

Returns the index of the first occurrence of the first string in the
second string, starting at the given index. Returns `-1` if there is
no match.

#### `lastIndexOf`

``` purescript
lastIndexOf :: String -> String -> Number
```

Returns the index of the last occurrence of the first string in the
second string. Returns `-1` if there is no match.

#### `lastIndexOf'`

``` purescript
lastIndexOf' :: String -> Number -> String -> Number
```

Returns the index of the first occurrence of the last string in the
second string, starting at the given index. Returns `-1` if there is
no match.

#### `length`

``` purescript
length :: String -> Number
```

Returns the number of characters the string is composed of.

#### `localeCompare`

``` purescript
localeCompare :: String -> String -> Number
```

Locale-aware sort order comparison. Returns a negative number if the
first string occurs before the second in a sort, a positive number
if the first string occurs after the second, and `0` if their sort order
is equal.

#### `replace`

``` purescript
replace :: String -> String -> String -> String
```

Replaces the first occurence of the first argument with the second argument.

#### `take`

``` purescript
take :: Number -> String -> String
```

Returns the first `n` characters of the string.

#### `drop`

``` purescript
drop :: Number -> String -> String
```

Returns the string without the first `n` characters.

#### `count`

``` purescript
count :: (Char -> Boolean) -> String -> Number
```

Returns the number of characters in the string for which the predicate holds.

#### `split`

``` purescript
split :: String -> String -> [String]
```

Returns the substrings of the first string separated along occurences
of the second string.

#### `toCharArray`

``` purescript
toCharArray :: String -> [Char]
```

Converts the string into an array of characters.

#### `toLower`

``` purescript
toLower :: String -> String
```

Returns the argument converted to lowercase.

#### `toUpper`

``` purescript
toUpper :: String -> String
```

Returns the argument converted to uppercase.

#### `trim`

``` purescript
trim :: String -> String
```

Removes whitespace from the beginning and end of a string, including
[whitespace characters](http://www.ecma-international.org/ecma-262/5.1/#sec-7.2)
and [line terminators](http://www.ecma-international.org/ecma-262/5.1/#sec-7.3).

#### `joinWith`

``` purescript
joinWith :: String -> [String] -> String
```

Joins the strings in the array together, inserting the first argument
as separator between them.


## Module Data.String.Regex


Wraps Javascript's `RegExp` object that enables matching strings with
patternes defined by regular expressions.
For details of the underlying implementation, see [RegExp Reference at MDN](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/RegExp).

#### `Regex`

``` purescript
data Regex :: *
```

Wraps Javascript `RegExp` objects.

#### `showRegex`

``` purescript
instance showRegex :: Show Regex
```


#### `RegexFlags`

``` purescript
type RegexFlags = { unicode :: Boolean, sticky :: Boolean, multiline :: Boolean, ignoreCase :: Boolean, global :: Boolean }
```

Flags that control matching.

#### `noFlags`

``` purescript
noFlags :: RegexFlags
```

All flags set to false.

#### `regex`

``` purescript
regex :: String -> RegexFlags -> Regex
```

Constructs a `Regex` from a pattern string and flags.

#### `source`

``` purescript
source :: Regex -> String
```

Returns the pattern string used to construct the given `Regex`.

#### `flags`

``` purescript
flags :: Regex -> RegexFlags
```

Returns the `RegexFlags` used to construct the given `Regex`.

#### `renderFlags`

``` purescript
renderFlags :: RegexFlags -> String
```

Returns the string representation of the given `RegexFlags`.

#### `parseFlags`

``` purescript
parseFlags :: String -> RegexFlags
```

Parses the string representation of `RegexFlags`.

#### `test`

``` purescript
test :: Regex -> String -> Boolean
```

Returns `true` if the `Regex` matches the string.

#### `match`

``` purescript
match :: Regex -> String -> Maybe [String]
```

Matches the string against the `Regex` and returns an array of matches
if there were any.
See [reference](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/match).

#### `replace`

``` purescript
replace :: Regex -> String -> String -> String
```

Replaces occurences of the `Regex` with the first string. The replacement
string can include special replacement patterns escaped with `"$"`.
See [reference](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/replace).

#### `replace'`

``` purescript
replace' :: Regex -> (String -> [String] -> String) -> String -> String
```

Transforms occurences of the `Regex` using a function of the matched
substring and a list of submatch strings.
See the [reference](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/replace#Specifying_a_function_as_a_parameter).

#### `search`

``` purescript
search :: Regex -> String -> Number
```

Returns the index of the first match of the `Regex` in the string, or
`-1` if there is no match.

#### `split`

``` purescript
split :: Regex -> String -> [String]
```

Split the string into an array of substrings along occurences of the `Regex`.


## Module Data.String.Unsafe


Unsafe string and character functions.

#### `charCodeAt`

``` purescript
charCodeAt :: Number -> String -> Number
```

Returns the numeric Unicode value of the character at the given index.

**Unsafe:** throws runtime exception if the index is out of bounds.

#### `charAt`

``` purescript
charAt :: Number -> String -> Char
```

Returns the character at the given index.

**Unsafe:** throws runtime exception if the index is out of bounds.

#### `char`

``` purescript
char :: String -> Char
```

Converts a string of length `1` to a character.

**Unsafe:** throws runtime exception if length is not `1`.



