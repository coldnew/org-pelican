
// Load plugins
var gulp = require('gulp'),
    del = require('del'),
    ghpages = require('gulp-gh-pages'),
    browserSync = require('browser-sync').create(),
    runSequence = require('run-sequence'),
    run = require('gulp-run');

// publish release version
gulp.task('publish', function(cb) {
    // NOTE: I use `publishconf.py` instad of `pelicanconf.py` here for building
    // deploy release
    return run("pelican -s publishconf.py").exec();
});

// deploy
// ref: https://github.com/kud/kud.github.io
gulp.task('gh-pages', function(){
    return gulp.src('output/**/*')
        .pipe(ghpages({
            branch: "master",
            cacheDir: '.deploy'
        }));
});

gulp.task('deploy', function() {
    runSequence(
        // 'clean',  // no need to clean all files
        // copy theme file should triggger after publish
        'publish', 
        'gh-pages'
    );
});

// Static server
// http://www.browsersync.io/docs/options/
gulp.task('server', function() {
    browserSync.init({
        server: {
            baseDir: "./output"
        },
        ui: {
            port: 8080
        }
    });
});

// Clean
gulp.task('clean', function(cb) {
    del(['output/**'], cb)
});

// Default task
gulp.task('default', function() {
    runSequence('clean');
});

// Watch
gulp.task('watch', ['server'], function() {
	// task for watch files change
});
