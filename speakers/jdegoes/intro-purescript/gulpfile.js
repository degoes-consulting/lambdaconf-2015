var gulp = require('gulp')
  , browserify  = require('gulp-browserify')
  , purescript  = require('gulp-purescript');

gulp.task('default', function() {
  return gulp.src(['src/**/*.purs', 'bower_components/purescript-*/src/**/*.purs']).pipe(purescript.psc({
    main: 'Game',
    modules: ['Game']
  })).pipe(browserify({})).pipe(gulp.dest('output'));
});