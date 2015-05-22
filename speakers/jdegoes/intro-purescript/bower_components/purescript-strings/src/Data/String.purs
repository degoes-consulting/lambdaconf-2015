-- | Wraps the functions of Javascript's `String` object.
-- | A String represents a sequence of characters.
-- | For details of the underlying implementation, see [String Reference at MDN](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String).
module Data.String
  (
    charAt,
    charCodeAt,
    fromCharArray,
    fromChar,
    indexOf,
    indexOf',
    lastIndexOf,
    lastIndexOf',
    null,
    uncons,
    length,
    singleton,
    localeCompare,
    replace,
    count,
    take,
    takeWhile,
    drop,
    dropWhile,
    split,
    toCharArray,
    toLower,
    toUpper,
    trim,
    joinWith
  ) where

  import Data.Maybe
  import Data.Char
  import Data.Function
  import qualified Data.String.Unsafe as U

  foreign import _charAt
    """
    function _charAt(i, s, Just, Nothing) {
      return i >= 0 && i < s.length ? Just(s.charAt(i)) : Nothing;
    }
    """ :: forall a. Fn4 Number String (a -> Maybe a) (Maybe a) (Maybe Char)

  -- | Returns the character at the given index, if the index is within bounds.
  charAt :: Number -> String -> Maybe Char
  charAt n s = runFn4 _charAt n s Just Nothing

  -- | Returns a string of length `1` containing the given character.
  fromChar :: Char -> String
  fromChar = charString

  -- | Returns a string of length `1` containing the given character.
  -- | Same as `fromChar`.
  singleton :: Char -> String
  singleton = fromChar

  foreign import _charCodeAt
    """
    function _charCodeAt(i, s, Just, Nothing) {
      return i >= 0 && i < s.length ? Just(s.charCodeAt(i)) : Nothing;
    }
    """ :: forall a. Fn4 Number String (a -> Maybe a) (Maybe a) (Maybe Number)

  -- | Returns the numeric Unicode value of the character at the given index,
  -- | if the index is within bounds.
  charCodeAt :: Number -> String -> Maybe Number
  charCodeAt n s = runFn4 _charCodeAt n s Just Nothing

  -- | Returns `true` if the given string is empty.
  null :: String -> Boolean
  null s = length s == 0

  -- | Returns the first character and the rest of the string,
  -- | if the string is not empty.
  uncons :: String -> Maybe {head :: Char, tail :: String}
  uncons s | null s = Nothing
  uncons s = Just {head : U.charAt 0 s, tail : drop 1 s}

  -- | Returns the longest prefix (possibly empty) of characters that satisfy
  -- | the predicate:
  takeWhile :: (Char -> Boolean) -> String -> String
  takeWhile p s = take (count p s) s

  -- | Returns the suffix remaining after `takeWhile`.
  dropWhile :: (Char -> Boolean) -> String -> String
  dropWhile p s = drop (count p s) s

  -- | Converts an array of characters into a string.
  foreign import fromCharArray
    """
    function fromCharArray(a) {
      return a.join('');
    }
    """ :: [Char] -> String

  -- | Returns the index of the first occurrence of the first string in the
  -- | second string. Returns `-1` if there is no match.
  foreign import indexOf
    """
    function indexOf(x) {
      return function(s) {
        return s.indexOf(x);
      };
    }
    """ :: String -> String -> Number

  -- | Returns the index of the first occurrence of the first string in the
  -- | second string, starting at the given index. Returns `-1` if there is
  -- | no match.
  foreign import indexOf'
    """
    function indexOf$prime(x) {
      return function(startAt) {
        return function(s) {
          return s.indexOf(x, startAt);
        };
      };
    }
    """ :: String -> Number -> String -> Number

  -- | Returns the index of the last occurrence of the first string in the
  -- | second string. Returns `-1` if there is no match.
  foreign import lastIndexOf
    """
    function lastIndexOf(x) {
      return function(s) {
        return s.lastIndexOf(x);
      };
    }
    """ :: String -> String -> Number

  -- | Returns the index of the first occurrence of the last string in the
  -- | second string, starting at the given index. Returns `-1` if there is
  -- | no match.
  foreign import lastIndexOf'
    """
    function lastIndexOf$prime(x) {
      return function(startAt) {
        return function(s) {
          return s.lastIndexOf(x, startAt);
        };
      };
    }
    """ :: String -> Number -> String -> Number

  -- | Returns the number of characters the string is composed of.
  foreign import length
    """
    function length(s) {
      return s.length;
    }
    """ :: String -> Number

  -- | Locale-aware sort order comparison. Returns a negative number if the
  -- | first string occurs before the second in a sort, a positive number
  -- | if the first string occurs after the second, and `0` if their sort order
  -- | is equal.
  foreign import localeCompare
    """
    function localeCompare(s1) {
      return function(s2) {
        return s1.localeCompare(s2);
      };
    }
    """ :: String -> String -> Number

  -- | Replaces the first occurence of the first argument with the second argument.
  foreign import replace
    """
    function replace(s1) {
      return function(s2) {
        return function(s3) {
          return s3.replace(s1, s2);
        };
      };
    }
    """ :: String -> String -> String -> String

  -- | Returns the first `n` characters of the string.
  foreign import take
    """
    function take(n) {
      return function(s) {
        return s.substr(0, n);
      };
    }
    """ :: Number -> String -> String

  -- | Returns the string without the first `n` characters.
  foreign import drop
    """
    function drop(n) {
      return function(s) {
        return s.substr(n);
      };
    }
    """ :: Number -> String -> String

  -- | Returns the number of characters in the string for which the predicate holds.
  foreign import count
    """
    function count(p){
      return function(s){
        var i;
        for(i = 0; i < s.length && p(s.charAt(i)); i++){};
        return i;
      };
    }
    """ :: (Char -> Boolean) -> String -> Number

  -- | Returns the substrings of the first string separated along occurences
  -- | of the second string.
  foreign import split
    """
    function split(sep) {
      return function(s) {
        return s.split(sep);
      };
    }
    """ :: String -> String -> [String]

  -- | Converts the string into an array of characters.
  foreign import toCharArray
    """
    function toCharArray(s) {
      return s.split('');
    }
    """ :: String -> [Char]

  -- | Returns the argument converted to lowercase.
  foreign import toLower
    """
    function toLower(s) {
      return s.toLowerCase();
    }
    """ :: String -> String

  -- | Returns the argument converted to uppercase.
  foreign import toUpper
    """
    function toUpper(s) {
      return s.toUpperCase();
    }
    """ :: String -> String

  -- | Removes whitespace from the beginning and end of a string, including
  -- | [whitespace characters](http://www.ecma-international.org/ecma-262/5.1/#sec-7.2)
  -- | and [line terminators](http://www.ecma-international.org/ecma-262/5.1/#sec-7.3).
  foreign import trim
    """
    function trim(s) {
      return s.trim();
    }
    """ :: String -> String

  -- | Joins the strings in the array together, inserting the first argument
  -- | as separator between them.
  foreign import joinWith
    """
    function joinWith(s) {
      return function(xs) {
        return xs.join(s);
      };
    }
    """ :: String -> [String] -> String
