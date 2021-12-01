/**
 * Load a given external library, as a javascript file
 * to run in the global scope, by adding it to the DOM
 */
function dynamicallyLoadScript(url) {
  var script = document.createElement('script')
  script.src = url
  /** Forces scripts to be loaded in order. */
  script.async = false
  script.defer = true
  // make sure document.body exists, since the scripts we load
  // assume that it does
  if (document.body) {
    document.body.appendChild(script)
  } else {
    var observer = new MutationObserver(function() {
      if (document.body) {
        document.body.appendChild(script)
        observer.disconnect();
      }
    });
    observer.observe(document.documentElement, { childList: true });
  }
}

/**
 * Loads all libraries, including sound and graphics.
 */
function loadAllLibs() {
  const files = [
    // list library
    '/externalLibs/list.js',
    // sound
    '/externalLibs/sound/sounds.js',
    '/externalLibs/sound/soundToneMatrix.js',
    '/externalLibs/sound/riffwave.js',
    '/externalLibs/sound/microphone.js',
    // graphics
    '/externalLibs/graphics/gl-matrix.js',
    '/externalLibs/graphics/webGLhi_graph.js',
    '/externalLibs/graphics/webGLhi_graph_ce.js',
    '/externalLibs/graphics/webGLgraphics.js',
    '/externalLibs/graphics/webGLcurve.js',
    '/externalLibs/graphics/webGLrune.js',
    // list visualizer
    '/externalLibs/visualizer/KineticJS.js',
    '/externalLibs/visualizer/visualizer.js',
    // binary tree library
    '/externalLibs/tree.js',
    // support for Practical Assessments (presently none)
    // video
    '/externalLibs/video/video_lib.js',
    // inspector
    '/externalLibs/inspector/inspector.js',
    // env visualizer
    '/externalLibs/env_visualizer/ConcreteJs.js',
    '/externalLibs/env_visualizer/visualizer.js'
  ]

  for (var i = 0; i < files.length; i++) {
    dynamicallyLoadScript(files[i])
  }
}

loadAllLibs()
