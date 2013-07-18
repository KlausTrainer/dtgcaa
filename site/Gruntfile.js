// Generated on<%= (new Date).toISOString().split('T')[0] %> using <%= pkg.name %> <%= pkg.version %>
'use strict';
var lrSnippet = require('grunt-contrib-livereload/lib/utils').livereloadSnippet;
var mountFolder = function (connect, dir) {
    return connect.static(require('path').resolve(dir));
};

// # Globbing
// for performance reasons we're only matching one level down:
// 'test/spec/{,*/}*.js'
// use this if you want to match all subfolders:
// 'test/spec/**/*.js'

module.exports = function (grunt) {
    // load all grunt tasks
    require('matchdep').filterDev('grunt-*').forEach(grunt.loadNpmTasks);

    // configurable paths
    var folders = {
        app: 'app',
        dist: 'dist',
        tmp: '.tmp'
    };

    grunt.initConfig({
        folders: folders,
        watch: {
            compass: {
                files: ['<%= folders.app %>/styles/{,*/}*.{scss,sass}'],
                tasks: ['compass:server']
            },
            server: {
                options: {
                    livereload: true
                },
                files: [
                    '<%= folders.tmp %>/*.html',
                    '<%= folders.tmp %>/styles/{,*/}*.css',
                    '<%= folders.app %>/scripts/{,*/}*.js',
                    '<%= folders.app %>/images/{,*/}*.{png,jpg,jpeg,gif,webp,svg}'
                ]
            },
            jade: {
                files: '<%= folders.app %>/jade/**/*.jade',
                tasks: ['jade']
            }
        },
        connect: {
            options: {
                port: 9000,
                // change this to '0.0.0.0' to access the server from outside
                hostname: '0.0.0.0'
            },
            server: {
                options: {
                    middleware: function (connect) {
                        return [
                            lrSnippet,
                            mountFolder(connect, folders.tmp),
                            mountFolder(connect, folders.app)
                        ];
                    }
                }
            },
            test: {
                options: {
                    middleware: function (connect) {
                        return [
                            mountFolder(connect, folders.tmp),
                            mountFolder(connect, 'test')
                        ];
                    }
                }
            },
            dist: {
                options: {
                    middleware: function (connect) {
                        return [
                            mountFolder(connect, folders.dist)
                        ];
                    }
                }
            }
        },
        open: {
            server: {
                path: 'http://localhost:<%= connect.options.port %>'
            }
        },
        clean: {
            dist: {
                files: [{
                    dot: true,
                    src: [
                        '<%= folders.tmp %>',
                        '<%= folders.dist %>/*',
                        '!<%= folders.dist %>/.git*'
                    ]
                }]
            },
            server: '<%= folders.tmp %>'
        },
        mocha: {
            all: {
                options: {
                    run: true,
                    urls: ['http://localhost:<%= connect.options.port %>/index.html']
                }
            }
        },
        compass: {
            options: {
                sassDir: '<%= folders.app %>/styles',
                cssDir: '<%= folders.tmp %>/styles',
                imagesDir: '<%= folders.app %>/images',
                javascriptsDir: '<%= folders.app %>/scripts',
                fontsDir: '<%= folders.app %>/styles/fonts',
                importPath: '<%= folders.app %>/bower_components',
                relativeAssets: true
            },
            dist: {
                options: {
                    noLineComments: true
                }
            },
            server: {
                options: {
                    debugInfo: true
                }
            }
        },
        jade: {
            html: {
                files: grunt.file.expandMapping(['{,*/}*.jade', '!**/_*'], 'dest', {
                    cwd: 'app/jade',
                    rename: function (dest, src) {

                        if (/i18n/.test(src)) {
                            return '<%= folders.tmp %>/' + src.replace(/index.i18n-(.*).jade/, '$1.html');;
                        }

                        return '<%= folders.tmp %>/' + src.replace(/\.jade$/, '.html');
                    }
                }),
                options: {
                    client: false,
                    pretty: true,
                    basedir: '<%= folders.app %>/jade',
                    data: function(dest, src) {

                        var page = src[0].replace(/app\/jade\/(.*)\/index.jade/, '$1');

                        return {
                            page: page
                        };
                    }
                }
            }
        },
        rev: {
            dist: {
                files: {
                    src: [
                        '<%= folders.dist %>/scripts/{,*/}*.js',
                        '<%= folders.dist %>/styles/{,*/}*.css',
                        '<%= folders.dist %>/images/{,*/}*.{png,jpg,jpeg,gif,webp}',
                        '<%= folders.dist %>/styles/fonts/*'
                    ]
                }
            }
        },
        useminPrepare: {
            html: '<%= folders.tmp %>/index.html',
            options: {
                dest: '<%= folders.dist %>'
            }
        },
        usemin: {
            html: ['<%= folders.dist %>/{,*/}*.html'],
            css: ['<%= folders.dist %>/styles/{,*/}*.css'],
            options: {
                dirs: ['<%= folders.dist %>']
            }
        },
        imagemin: {
            dist: {
                files: [{
                    expand: true,
                    cwd: '<%= folders.app %>/images',
                    src: '{,*/}*.{png,jpg,jpeg}',
                    dest: '<%= folders.dist %>/images'
                }]
            }
        },
        svgmin: {
            dist: {
                files: [{
                    expand: true,
                    cwd: '<%= folders.app %>/images',
                    src: '{,*/}*.svg',
                    dest: '<%= folders.dist %>/images'
                }]
            }
        },
        cssmin: {
            dist: {
                files: {
                    '<%= folders.dist %>/styles/main.css': [
                        '<%= folders.tmp %>/styles/{,*/}*.css'
                    ]
                }
            }
        },
        htmlmin: {
            dist: {
                options: {
                    /*removeCommentsFromCDATA: true,
                    // https://github.com/folders/grunt-usemin/issues/44
                    //collapseWhitespace: true,
                    collapseBooleanAttributes: true,
                    removeAttributeQuotes: true,
                    removeRedundantAttributes: true,
                    useShortDoctype: true,
                    removeEmptyAttributes: true,
                    removeOptionalTags: true*/
                },
                files: [{
                    expand: true,
                    cwd: '<%= folders.tmp %>',
                    src: '{,*/}*.html',
                    dest: '<%= folders.dist %>'
                }]
            }
        },
        // Put files not handled in other tasks here
        copy: {
            dist: {
                files: [{
                    expand: true,
                    dot: true,
                    cwd: '<%= folders.app %>',
                    dest: '<%= folders.dist %>',
                    src: [
                        '*.{ico,txt}',
                        '.htaccess',
                        'images/{,*/}*.{webp,gif}',
                        'styles/fonts/*'
                    ]
                }]
            },
            js: {
                files: [{
                    expand: true,
                    cwd: '<%= folders.app %>',
                    dest: '<%= folders.tmp %>',
                    src: [
                        'scripts/{,*/}*js'
                    ]
                }]
            },
            css: {
                files: [{
                    expand: true,
                    cwd: '<%= folders.app %>',
                    dest: '<%= folders.tmp %>',
                    src: [
                        'styles/{,*/}*css'
                    ]
                }]
            },
            assets: {
                files: [{
                    expand: true,
                    cwd: '<%= folders.app %>',
                    dest: '<%= folders.dist %>',
                    src: [
                        'assets/{,*/}*.*'
                    ]
                }]
            }
        },
        concurrent: {
            server: [
                'compass:server'
            ],
            test: [
                'compass'
            ],
            dist: [
                'compass:dist'
            ]
        },
        htmlbuild: {
            dist: {
                src: '<%= folders.tmp %>/index.html',
                dest: 'dist/',
                options: {
                    beautify: true,
                    styles: {
                        test: '<%= folders.tmp %>/styles/main.css'
                    }
                }
            }
        }
    });

    grunt.registerTask('server', function (target) {
        if (target === 'dist') {
            return grunt.task.run(['build', 'open', 'connect:dist:keepalive']);
        }

        grunt.task.run([
            'clean:server',
            'jade',
            'concurrent:server',
            'connect:server',
            'open',
            'watch'
        ]);
    });

    grunt.registerTask('test', [
        'clean:server',
        'concurrent:test',
        'connect:test',
        'mocha'
    ]);

    grunt.registerTask('build', [
        'clean:dist',
        'jade',
        'copy:js',
        'copy:css',
        'concurrent:dist',
        'copy:dist',
        'copy:assets',
        'htmlbuild:dist'
    ]);

    grunt.registerTask('default', [
        'jshint',
        'test',
        'build'
    ]);
};
