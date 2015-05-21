/*******************************************************************************/
/************** Configuration Parameters For Server ****************************/
/*******************************************************************************/

'use strict';

/************** Titles *********************************************************/

var demoTitle     = "LiquidHaskell";
var demoSubtitle  = "Refinement Types via SMT and Predicate Abstraction";

/************** Header Links 8**************************************************/

var allLinks = [ { url: "https://github.com/ucsd-progsys/liquidhaskell"          , name: "Code" } 
               , { url: "http://goto.ucsd.edu/~rjhala/liquid/haskell/blog/about/", name: "About"} 
               , { url: "http://goto.ucsd.edu/~rjhala/liquid/haskell/blog/"      , name: "Blog" }
               ];

/************** Editor Modes ***************************************************/

var toolName       = "liquidhaskell";
var editorTheme    = "ace/theme/xcode";
var editorMode     = "ace/mode/haskell";
var defaultErrText = "Liquid Type Error";
var queryServerURL = "http://localhost:8090/" 

/************** List of Demos **************************************************/

var allCategories = [ { type : "basic"    , name: "Basics" }
                    , { type : "measure"  , name: "Measures" }
                    , { type : "absref"   , name: "Abstract Refinements" }
                    , { type : "tutorial" , name: "HOPA Tutorial" }
                    ];

var allDemos =
  { // Basic Demos
    "blank.hs"              : { name : "Blank"            , type : "basic"  },
    "evens.hs"              : { name : "Even numbers"     , type : "basic"  },
    "refinements101.hs"     : { name : "Refinements 101"  , type : "basic"  },
    "refinements101reax.hs" : { name : "Refinements 102"  , type : "basic"  },
    "vectorbounds.hs"       : { name : "Vector Bounds"    , type : "basic"  },
    // Measure Demos
    "lenMapReduce.hs"       : { name : "Safe List"        , type : "measure"},
    "Csv.hs"                : { name : "CSV Lists"        , type : "measure"},
    "KMeansHelper.hs"       : { name : "K-Means Lib"      , type : "measure"},
    "KMeans.hs"             : { name : "K-Means"          , type : "measure"}, 
    "TalkingAboutSets.hs"   : { name : "Talk About Sets"  , type : "measure"},
    "UniqueZipper.hs"       : { name : "Unique Zippers"   , type : "measure"},
    "LambdaEval.hs"         : { name : "Lambda Eval"      , type : "measure"}, 
    "treesum.hs"            : { name : "List-Tree Sum"    , type : "measure"},
    // Abstract Refinement Demos
    "absref101.hs"          : { name : "Parametric Invariants", type : "absref" },
    "filter.hs"             : { name : "A Fine Filter"        , type : "absref" },
    "Order.hs"              : { name : "Ordered Lists"        , type : "absref" },
    "Map.hs"                : { name : "BinSearch Tree"       , type : "absref" },
    "Foldr.hs"              : { name : "Induction"            , type : "absref" },
    "IMaps.hs"              : { name : "Indexed Maps"         , type : "absref" },
    // HOPA Tutorial Demos
    "SimpleRefinements.hs" : { name : "Simple Refinements", type : "tutorial" },  
    "Loop.hs"              : { name : "HO Loop"           , type : "tutorial" },
    "Composition.hs"       : { name : "Composition"       , type : "tutorial" },
    "Array.hs"             : { name : "Finite Maps"       , type : "tutorial" }
  };
