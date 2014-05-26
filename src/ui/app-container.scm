(module app-container mzscheme
  (require (lib "mred.ss" "mred")
           (lib "class.ss")
           "../view-env/viz-class.scm")
  
  (provide (all-defined))
  
  ;; frame-object: (void) -> frame-object 
  (define frame-object 
    (lambda (label)
      (let* ((frame-obj (make-object frame% label #f)))
        ;; status bar on frame
        (send frame-obj create-status-line)

        frame-obj)))
  
  ;; viz-class-object: frame -> viz-class-object
  (define viz-class-object
    (lambda (pos-expr width height) 
      (new viz-class% (parent pos-expr) 
        [min-width width]
        [min-height height])))

  ;; call viewer
  (define run-view-controller
    (lambda (frame-obj width height content-top)
      (let* ((viz (viz-class-object frame-obj width height)))
        (send viz set-scene-tree-root content-top)
        (send frame-obj show #t)))))

  