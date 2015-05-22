-- | Wraps Javascript's `RegExp` object that enables matching strings with
-- | patternes defined by regular expressions.
-- | For details of the underlying implementation, see [RegExp Reference at MDN](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/RegExp).
module Data.String.Regex
  ( Regex(..)
  , RegexFlags(..)
  , regex
  , source
  , flags
  , renderFlags
  , parseFlags
  , test
  , match
  , replace
  , replace'
  , search
  , split
  , noFlags
  ) where

import Data.Function
import Data.Maybe
import Data.String (indexOf)

-- | Wraps Javascript `RegExp` objects.
foreign import data Regex :: *

foreign import showRegex'
  """
  function showRegex$prime(r) {
    return '' + r;
  }
  """ :: Regex -> String

instance showRegex :: Show Regex where
  show = showRegex'

-- | Flags that control matching.
type RegexFlags =
  { global :: Boolean
  , ignoreCase :: Boolean
  , multiline :: Boolean
  , sticky :: Boolean
  , unicode :: Boolean
  }

-- | All flags set to false.
noFlags :: RegexFlags
noFlags = { global     : false
          , ignoreCase : false
          , multiline  : false
          , sticky     : false
          , unicode    : false }

foreign import regex'
  """
  function regex$prime(s1) {
    return function(s2) {
      return new RegExp(s1, s2);
    };
  }
  """ :: String -> String -> Regex

-- | Constructs a `Regex` from a pattern string and flags.
regex :: String -> RegexFlags -> Regex
regex source flags = regex' source $ renderFlags flags

-- | Returns the pattern string used to construct the given `Regex`.
foreign import source
  """
  function source(r) {
    return r.source;
  }
  """ :: Regex -> String

-- | Returns the `RegexFlags` used to construct the given `Regex`.
foreign import flags
  """
  function flags(r) {
    return {
      multiline: r.multiline,
      ignoreCase: r.ignoreCase,
      global: r.global,
      sticky: !!r.sticky,
      unicode: !!r.unicode
    };
  }
  """ :: Regex -> RegexFlags

-- | Returns the string representation of the given `RegexFlags`.
renderFlags :: RegexFlags -> String
renderFlags flags =
  (if flags.global then "g" else "") ++
  (if flags.ignoreCase then "i" else "") ++
  (if flags.multiline then "m" else "") ++
  (if flags.sticky then "y" else "") ++
  (if flags.unicode then "u" else "")

-- | Parses the string representation of `RegexFlags`.
parseFlags :: String -> RegexFlags
parseFlags s =
  { global: indexOf "g" s >= 0
  , ignoreCase: indexOf "i" s >= 0
  , multiline: indexOf "m" s >= 0
  , sticky: indexOf "y" s >= 0
  , unicode: indexOf "u" s >= 0
  }

-- | Returns `true` if the `Regex` matches the string.
foreign import test
  """
  function test(r) {
    return function(s) {
      return r.test(s);
    };
  }
  """ :: Regex -> String -> Boolean

foreign import _match
  """
  function _match(r, s, Just, Nothing) {
    var m = s.match(r);
    return m == null ? Nothing : Just(m);
  }
  """ :: forall r. Fn4 Regex String ([String] -> r) r r

-- | Matches the string against the `Regex` and returns an array of matches
-- | if there were any.
-- | See [reference](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/match).
match :: Regex -> String -> Maybe [String]
match r s = runFn4 _match r s Just Nothing

-- | Replaces occurences of the `Regex` with the first string. The replacement
-- | string can include special replacement patterns escaped with `"$"`.
-- | See [reference](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/replace).
foreign import replace
  """
  function replace(r) {
    return function(s1) {
      return function(s2) {
        return s2.replace(r, s1);
      };
    };
  }
  """ :: Regex -> String -> String -> String

-- | Transforms occurences of the `Regex` using a function of the matched
-- | substring and a list of submatch strings.
-- | See the [reference](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/replace#Specifying_a_function_as_a_parameter).
foreign import replace'
  """
  function replace$prime(r) {
    return function(f) {
      return function(s2) {
        return s2.replace(r, function(match) {
          return f(match)(Array.prototype.splice.call(arguments, 1, arguments.length - 3));
        });
      };
    };
  }
  """ :: Regex -> (String -> [String] -> String) -> String -> String

-- | Returns the index of the first match of the `Regex` in the string, or
-- | `-1` if there is no match.
foreign import search
  """
  function search(r) {
    return function(s) {
      return s.search(r);
    };
  }
  """ :: Regex -> String -> Number

-- | Split the string into an array of substrings along occurences of the `Regex`.
foreign import split
  """
  function split(r) {
    return function(s) {
      return s.split(r);
    };
  }
  """ :: Regex -> String -> [String]
