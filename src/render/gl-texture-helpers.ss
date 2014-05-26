;; By Brendan Burns, with modifications by Scott Owens, Seungkeol extracted procs from
;; original gl-frames.ss

(module gl-texture-helpers (lib "plt-pretty-big.ss" "lang")
  (require sgl/gl
           sgl/gl-vectors)  
  (provide ;set-gl-draw-fn
           ;set-gl-init-fn
           ;init-textures
           image->gl-vector
           bitmap->gl-vector
           gl-load-texture)
;           get-texture)
  
;  (define gl-draw void)
;  (define gl-init 
;    (lambda ()
;      (glShadeModel GL_SMOOTH)
;      (glClearColor 0.0 0.0 0.0 0.5)
;      (glClearDepth 1)
;      (glEnable GL_DEPTH_TEST)
;      (glDepthFunc GL_LEQUAL)
;      (glHint GL_PERSPECTIVE_CORRECTION_HINT GL_NICEST)))
  
;  (define (set-gl-draw-fn fn)
;    (set! gl-draw fn))
  
;  (define (set-gl-init-fn fn)
;    (set! gl-init fn))
    
;  ;; A function that recorrects for a new aspect ratio when the window is resized
;  (define (gl-resize width height)
;    (glViewport 0 0 width height)
;    
;    (glMatrixMode GL_PROJECTION)
;    (glLoadIdentity)
;    (gluPerspective 45 (/ width height) 0.1 100)
;    
;    (glMatrixMode GL_MODELVIEW)
;    (glLoadIdentity))
  
;  (define (recursive-handle-key list code)
;    (cond
;      ((empty? list) void)
;      ((equal? (caar list) code) ((car (cdr (car list)))))
;      (else (recursive-handle-key (rest list) code))))
;  
;  (define *textures* '())
;  
;  (define init-textures
;    (lambda (count)
;      (set! *textures* (glGenTextures count))))
  
  (define (bitmap->gl-vector bmp)
    (let* (
           (dc (instantiate bitmap-dc% (bmp)))
           (pixels (* (send bmp get-width) (send bmp get-height)))
           (vec (make-gl-ubyte-vector (* pixels 3)))
           (data (make-bytes (* pixels 4)))
           (i 0)
           )
      (send dc get-argb-pixels 0 0 (send bmp get-width) (send bmp get-height) data)
      (letrec
          ([loop
            (lambda ()
              (if (< i pixels)
                  (begin
                    (gl-vector-set! vec (* i  3) 
                                    (bytes-ref data (+ (* i 4) 1)))
                    (gl-vector-set! vec (+ (* i 3) 1) 
                                    (bytes-ref data (+ (* i 4) 2)))
                    (gl-vector-set! vec (+ (* i 3) 2) 
                                    (bytes-ref data (+ (* i 4) 3)))
                    (set! i (+ i 1))
                    (loop))))])
        (loop))
      (send dc set-bitmap #f)
      (list (send bmp get-width) (send bmp get-height) vec)))
  
  (define (image->gl-vector file) 
    (bitmap->gl-vector (make-object bitmap% file 'unknown #f)))
  
  (define gl-load-texture
    (lambda (image-vector width height min-filter mag-filter tex)
      (glBindTexture GL_TEXTURE_2D tex)
      (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER min-filter)
      (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER mag-filter)
      (let* ((new-width 128)
             (new-height 128)
             (new-img-vec (make-gl-ubyte-vector (* new-width new-height 3))))
        (gluScaleImage GL_RGB
                       width height GL_UNSIGNED_BYTE image-vector
                       new-width new-height GL_UNSIGNED_BYTE new-img-vec)
        (if (or (= min-filter GL_LINEAR_MIPMAP_NEAREST)
                (= mag-filter GL_LINEAR_MIPMAP_NEAREST))
            (gluBuild2DMipmaps GL_TEXTURE_2D 
                               3 
                               new-width 
                               new-height 
                               GL_RGB 
                               GL_UNSIGNED_BYTE 
                               new-img-vec)
            (glTexImage2D GL_TEXTURE_2D 
                          0 
                          3
                          new-width 
                          new-height
                          0 
                          GL_RGB
                          GL_UNSIGNED_BYTE
                          new-img-vec)))))
  
;  (define get-texture
;    (lambda (ix)
;      (gl-vector-ref *textures* ix)))
  )
