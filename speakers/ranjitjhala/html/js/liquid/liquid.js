'use strict';

function compare(x, y){
    if (x < y)
        return -1;
    if (x > y)
        return 1;
    return 0;
}

function replicate(n, x) {
    var a = [];
    for (var i = 0; i < n; i++){
        a.push(x);
    }
    return a;
}

function replicatei(n, f) {
    var a = [];
    for (var i = 0; i < n; i++){
        a.push(f(i));
    }
    return a;
}

/*******************************************************************************/
/************** Setting Up Editor **********************************************/
/*******************************************************************************/

var hidden = true;

function toggleHidden(){
    if (hidden) {
      $(".hidden").removeClass("hidden").addClass("unhidden");
      hidden = false;
    } else {
      $(".unhidden").removeClass("unhidden").addClass("hidden");
      hidden = false;
    }
}


/*******************************************************************************/
/************** Setting Up Editor **********************************************/
/*******************************************************************************/

var SrcMode     = require(editorMode).Mode; // var SrcMode     = require("ace/mode/haskell").Mode;
var numEditors  = $('.welleditor').length;
var progEditor  = [];
progEditor.typeTooltip = [];

function programId(i)     { return "program-"      + i; }
function programPaneId(i) { return "program-pane-" + i; }

// Create Editors
for (var i = 0; i < numEditors; i++){
    var pi = ace.edit(programId(i));

    pi.renderer.setShowGutter(true);        // keep gutter for error-message hook
    pi.setOption("showLineNumbers", false); // hide line numbers

    pi.setShowPrintMargin(false);
    pi.setOptions({ maxLines: Infinity});
    pi.setTheme(editorTheme);    // progEditor.setTheme("ace/theme/xcode");
    pi.setOptions({
        fontFamily: "Source Code Pro",
        fontSize: "13pt"
    });
    pi.getSession().setMode(new SrcMode());
    progEditor[i]  = pi;
    progEditor.typeTooltip[i] = new TokenTooltip(pi, getAnnot(i));
}

// lift `on` to work for collection of editors
progEditor.on = function(event, fn){
    for (var i = 0; i < numEditors; i++){
        progEditor[i].on(event, fn);
    }
}

// lift `setKeyboardHandler` to work for collection of editors
progEditor.setKeyboardHandler = function(h){
    for (var i = 0; i < numEditors; i++){
        progEditor[i].setKeyboardHandler(h);
    }
}

progEditor.getSourceBlocks = function (){
    return progEditor.map(function (p){ return p.getSession().getValue();});
}

progEditor.getSourceCode = function (){
    return progEditor.getSourceBlocks().join("\n");
}

// Globals
progEditor.errorMarkers = replicate(numEditors, []);


/*******************************************************************************/
/** Markers For Errors *********************************************************/
/*******************************************************************************/

function errorRange(err){

  var row0 = err.start.line - 1;
  var col0 = err.start.column - 1;
  var row1 = err.stop.line - 1;
  var col1 = err.stop.column - 1;

  if (row0 == row1 && col0 == col1){
    return new Range(row0, col0, row0, col0 + 1);
  } else {
    return new Range(row0, col0, row1, col1);
  }
}

function errorMarker(editor, err){
  var r = errorRange(err);
  return editor.session.addMarker(r, "ace_step", "error");
}

function errorAceAnnot(err){
    var etext = defaultErrText;
    if (err.message) {
        etext = err.message;
    }
    var ann = { row   : err.start.line - 1
              , column: err.start.column
              , text  : etext
              , type  : "error"
              };
    return ann;
}

function setBlockErrors(i, errs){
    var editor = progEditor[i];
    // Add Error Markers
    progEditor.errorMarkers[i].forEach(function(m){ editor.session.removeMarker(m); });
    progEditor.errorMarkers[i] = errs.map(function(e){ return errorMarker(editor, e);});

    // Add Gutter Annotations
    editor.session.clearAnnotations();
    var annotations  = errs.map(errorAceAnnot);
    editor.session.setAnnotations(annotations);
}

function compareErr(e1, e2){
    return compare(e1.start.line, e2.start.line);
}

/*@ blockEnds :: (array[int]) => array[int] */
function blockEnds(blocks) {
    var n    = blocks.length;
    var ends = new Array(n);
    var off  = 0;
    for (var i = 0; i < blocks.length; i++){
        ends[i] = off + blocks[i];
        off     = ends[i];
    }
    return ends;
}


/*@ shiftPos :: (pos, off) => pos */
function shiftPos(pos, off){
    return { line   : pos.line - off,
             column : pos.column
           };
}

/*@ shiftError :: (error, off) => error */
function shiftError(e, off){
    return { start   : shiftPos(e.start, off),
             stop    : shiftPos(e.stop , off),
             message : e.message
           };
}

/*@ errorBlock :: (array[int], error) => {block:int, error: error} */
function errorBlock(ends, err){
    var l = err.start.line;
    var i = 0;
    while (i < ends.length){
        if (l <= ends[i])
            break;
        i++;
    }
    var off  = (0 < i) ? ends[i-1] : 0;
    return {block: i, error: shiftError(err, off)};
}

/*@ blockErrors :: (array[int], array[error]) => array[array[error]] */
function blockErrors(blocks, errs){
    var ends  = blockEnds(blocks);
    var res   = blocks.map(function(){return []});
    errs.forEach(function(err){
        var eb = errorBlock(ends, err);
        res[eb.block].push(eb.error);
    });
    return res;
}

/*@ setErrors :: (array[int], array[error]) => array[int] */
function setErrors(blocks, errs){
    var errors  = blockErrors(blocks, errs);
    var res     = [];
    for (var i = 0; i < numEditors; i++){
        var es = errors[i];
        setBlockErrors(i, es);
        if (es.length > 0) res.push(i);
    }
    return res.map(function(x, i){ return { index : i, data : x }; });
}

/*******************************************************************************/
/************** URLS ***********************************************************/
/*******************************************************************************/

function isPrefix(p, q) {
  return (p == q.slice(0, p.length))
}

function getQueryURL(){
  return queryServerURL + 'query';
}

function getSrcURL(file){
  if (file.match("/")){
    return file;
  } else {
    return ('demos/' + file);
  }
}



/*******************************************************************************/
/************** Queries ********************************************************/
/*******************************************************************************/

function getCheckQuery($scope){
  return { type    : "check",
           program : progEditor.getSourceCode()
         };
}

function getRecheckQuery($scope){
  var p = "";
  if ($scope.filePath) p = $scope.filePath;

  return { type    : "recheck",
           program : progEditor.getSourceCode(),
           path    : p
         };
}

function getLoadQuery($scope){
  return { type    : "load",
           path    : $scope.localFilePath
         };
}

function getSaveQuery($scope){
  return { type    : "save"
         , program : progEditor.getSourceCode()
         , path    : $scope.localFilePath
         };
}

function getPermaQuery($scope){
  return { type    : "perma"
         , program : progEditor.getSourceCode()
         };
}

/*******************************************************************************/
/************** Tracking Status and Source *************************************/
/*******************************************************************************/

function clearStatus($scope){
  $scope.isSafe       = false;
  $scope.isUnsafe     = false;
  $scope.isError      = false;
  $scope.isCrash      = false;
  $scope.isChecking   = false;
  $scope.isBad        = false;
  $scope.isUnknown    = true ;
  $scope.errorBlocks  = [];
}

function setStatusChecking($scope){
  clearStatus($scope);
  $scope.isChecking = true;
  $scope.isUnknown  = false;
}

function setStatusResult($scope, data){
  var result          = getResult(data);
  debugResult         = result;
  clearStatus($scope);
  $scope.isChecking   = false;
  $scope.isSafe       = (result == "safe"  );
  $scope.isUnsafe     = (result == "unsafe");
  $scope.isCrash      = (result == "crash" );
  $scope.isError      = (result == "error" );
  $scope.isBad        = ($scope.isError || $scope.isUnsafe);
  debugBad            = $scope.isBad;
  $scope.isUnknown    = !($scope.isSafe || $scope.isError || $scope.isUnsafe || $scope.isCrash);
  $scope.filePath     = data.path;
  return result;
}

/*******************************************************************************/
/************** Loading Files **************************************************/
/*******************************************************************************/
// DEAD CODE. All loading happens via server.

/*@ fileText :: (file, (string) => void) => void */
function fileText(file, k){
  var reader = new FileReader();
  reader.addEventListener("load", function(e){
    k(e.target.result);
  });
  reader.readAsText(file);
}

/*******************************************************************************/
/** Extracting JSON Results ****************************************************/
/*******************************************************************************/

function getResult(d) {
  var res = "crash";
  if (d) {
    res = d.status;
  }
  return res;
}

function getWarns(d){
  var ws = [];
  if (d && d.errors){
    var ws = d.errors.map(function(x){
               return x.message;
             });
  }
  return ws;
}

/*******************************************************************************/
/************** Top-Level Demo Controller **************************************/
/*******************************************************************************/

var debugBad    = false;
var debugErrors = null;
var debugQuery  = null;
var debugData   = null;
var debugResult = null;
var debugResp   = 0;
var debugFiles  = null;
var debugDemo   = null;
var debugSrcURL = null;
var debugZ      = null;

function LiquidDemoCtrl($scope, $http, $location) {

  // Start in non-fullscreen
  // NUKE $scope.isFullScreen  = false;
  // NUKE $scope.embiggen      = "FullScreen";
  // NUKE $scope.demoTitle     = demoTitle;
  // NUKE $scope.demoSubtitle  = demoSubtitle;
  // NUKE $scope.links         = allLinks;
  // NUKE $scope.categories    = getCategories();
  // NUKE $scope.isLocalServer = (document.location.hostname == "localhost");
  // NUKE $scope.localFilePath = "";

  clearStatus($scope);

  // For debugging
  $scope.gong =  function(s) { alert(s); };

  // Clear Status when editor is changed
  progEditor.on("change", function(e){
    $scope.$apply(function(){
      clearStatus($scope);
    });
  });

  // Change editor keybindings
  $scope.keyBindingsNone  = function (){ progEditor.setKeyboardHandler(null); };
  $scope.keyBindingsVim   = function (){ progEditor.setKeyboardHandler("ace/keyboard/vim"); };
  $scope.keyBindingsEmacs = function (){ progEditor.setKeyboardHandler("ace/keyboard/emacs"); };

  // http://www.cleverweb.nl/javascript/a-simple-search-with-angularjs-and-php/
  function verifyQuery(query){
    debugQuery = query;
    setStatusChecking($scope);
    $http.post(getQueryURL(), query)
         .success(function(data, status) {
            debugResp        = debugResp + 1;
            $scope.status    = status;
            debugData        = data;
            $scope.warns     = getWarns(data);
            $scope.annotHtml = data.annotHtml;
            $scope.result    = setStatusResult($scope, data);

            // This may be "null" if liquid crashed...
            if (data) {
                var blocks = progEditor.getSourceBlocks()
                                       .map(function(str){ return numLines(str); });

                setAnnots(blocks, data.types);
                $scope.errorBlocks = setErrors(blocks, data.errors);
                debugErrors        = $scope.errorBlocks;
            };

        })
         .error(function(data, status) {
            var msg = (data || "Request failed") + status;
            alert(msg);
         });
  };

  $scope.verifySource   = function(){ verifyQuery(getCheckQuery($scope));   };

  // $scope.reVerifySource = function(){ verifyQuery(getRecheckQuery($scope)); };
}

/************************************************************************/
/***** Initialize Angular ***********************************************/
/************************************************************************/

var demo = angular.module("liquidDemo", []);
demo.controller('LiquidDemoCtrl', LiquidDemoCtrl);
// toggleEditorSize({isFullScreen : false });
