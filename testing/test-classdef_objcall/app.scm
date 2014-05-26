(module app mzscheme
  (require (lib "class.ss")
           (lib "list.ss")
           (lib "etc.ss")
           "class_def.scm"
           "render.scm"
           "render_sun.scm"
           "render_earth.scm")
  
  (define sun-renderer (make-object render_sun%))
  (define earth-renderer (make-object render_earth%))

  (send sun-renderer set-object 'draw_sun_object)
  (send earth-renderer set-object 'draw_earth_object)
    
  (define viewer (make-object class_1%))
  (send viewer add-renderer sun-renderer)
  (send viewer add-renderer earth-renderer)
  
  (send viewer render-all)
  
  (send viewer remove-all-renderer))

