
'use strict';

/*@ numLines :: (string) => int */
function numLines(str){
    return str.split("\n").length;
}
/********************************************************************************/
/******** "Global" Annotation Table *********************************************/
/********************************************************************************/

/*@ type Annot1 = { ident : string
                  , ann   : string
                  , row   : int
                  , col   : int  
                  , size  : int
                  } */ 

/*@ type Annot  = array [array [annotJ]] */

var curAnnot = "";

/*@ annotTable :: Annot */
var annotTable 
   = { 5 : { 14 : { ident : "foo"
                  , ann   : "int -> int"
                  , row   : 5
                  , col   : 14
                  }
           }
     , 9 : { 22 : { ident : "map" 
                  , ann   : "(a -> b) -> [a] -> [b]"
                  , row   : 9
                  , col   : 22
                  }
           , 28 : { ident : "xs"
                  , ann   : "[b]" 
                  , row   : 9 
                  , col   : 28
                  }
           } 
     }

/*@ codeBlocks :: array[number] */
var codeBlocks = [];

/********************************************************************************/
/******** Function Returning Annot for A Row/Column *****************************/
/********************************************************************************/

var zooper     = "   Int\n-> Bool\n-> IO String";

function getAnnotText(row, col, annT) {
  var rowA = annT[row];
  
  if (!rowA){
    // No annotations defined for this row...
    return null;
  }

  for (var c in rowA){
    if (c == col) {
      // Found annotation beginning at exact row, col
      return rowA[c].ann;
    }
  }
  return null;
}

/*@ getRealRow :: (int, int, array[int]) => int */
function getRealRow(block, row, codeS){
    var off = row;
    for(var i = 0; i < block; i++){
        off += codeS[i];
    }
    return off;
}

function getAnnotTextBlock(i, row, col, annT, codeS){
    var realRow = getRealRow(i, row, codeS);
    return getAnnotText(realRow, col, annT);
}


/******************************************************************/
/****** PUBLIC API ************************************************/
/******************************************************************/

/*@ getAnnot :: (int) => ((int, int) => string?) */
function getAnnot(i){
    var get = function(row, col){
        var r = getAnnotTextBlock(i,
                                  row + 1,
                                  col + 1,
                                  annotTable,
                                  codeBlocks);
        if (r) { curAnnot = r;}
        return r;
    }
    return get;
}

/*@ setAnnots :: (array[number], Annot) => void */
function setAnnots(blocks, t) {
    codeBlocks = blocks;
    annotTable = t;
}


