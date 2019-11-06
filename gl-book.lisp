;;;; gl-book.lisp 
;;
;; Copyright (c) 2019 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>


;; Permission to use, copy, modify, and/or distribute this software for any
;; purpose with or without fee is hereby granted, provided that the above
;; copyright notice and this permission notice appear in all copies.

;; THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
;; WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
;; MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
;; ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
;; WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
;; ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
;; OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

(in-package :gl-book)

(def-key-callback quit-on-escape (window key scancode action mod-keys)
  (declare (ignorable window scancode mod-keys))
  (format t "Keypress: ~a ~a ~a ~a ~a~%"
          window key scancode action mod-keys)
  (cond ((and (eq key :escape) (eq action :press))
         (set-window-should-close))))

(def-mouse-button-callback mouse-handler (window button action mod-keys)
  (declare (ignorable window button action mod-keys))
  (let ((cpos (glfw:get-cursor-position window)))
    (format t "Mouse click at ~a ~a ~a ~a ~a~%"
            cpos window button action mod-keys)))

(def-scroll-callback scroll-handler (window x y)
  (let ((cpos (glfw:get-cursor-position window)))
    (format t "Scroll at ~a ~a ~a ~a ~%" cpos window x y)))

(def-error-callback error-callback (message)
  (format t "Error: ~a~%" message))

(defun render-scene ()
  (gl:enable :line-smooth
             :polygon-smooth
             :cull-face
             :depth-test :depth-clamp
             :blend)
  (gl:depth-range -10.1 10.1)
  (gl:blend-func :one :one-minus-src-alpha)
  (gl:clear-color 0.0 0.0 0.0 1.0)
  (gl:clear :color-buffer :depth-buffer))

(defun viewer-thread-function ()
  (with-init
    (let* ((monitor (glfw:get-primary-monitor))
           (cur-mode (glfw:get-video-mode monitor))
           (cur-width (getf cur-mode '%cl-glfw3:width))
           (cur-height (getf cur-mode '%cl-glfw3:height)))
      (with-window (:title "OpenGL Scene Viewer"
                           :width cur-width
                           :height cur-height
                           :decorated t
                           ;; :monitor monitor
                           :opengl-profile :opengl-core-profile
                           :context-version-major 3
                           :context-version-minor 3
                           :opengl-forward-compat t
                           :resizable t)
        (setf %gl:*gl-get-proc-address* #'get-proc-address)
        (set-key-callback 'quit-on-escape)
        (set-error-callback 'error-callback)
        (set-mouse-button-callback 'mouse-handler)
        (set-scroll-callback 'scroll-handler)

        (let ((vaos (gl:gen-vertex-arrays 1))
              (buffers (gl:gen-buffers 1))
              (num-vertices 6)
              (vertices = (make-array (list num-vertices 2)
                                      :element-type 'single-float
                                      :initial-contents '(;; Triangle 1
                                                          (-0.9 -0.9)
                                                          (0.85 -0.9)
                                                          (-0.9 0.85)
                                                          ;; Triangle 2
                                                          (0.9 -0.85)
                                                          (0.9 0.9)
                                                          (-0.85 0.9)))))
              (gl:bind-vertex-array (car vaos))
              (gl:bind-buffer :array-buffer (car buffers))
              (gl:buffer-data :array-buffer :static-draw gl-vertices))
        (gl:clear-color 0 0 0 1.0)
        
        ;; The event loop
        (loop until (window-should-close-p)
           do
             (render-scene)
           do (swap-buffers)
           do (poll-events))))))

(defun show-viewer (&optional (in-thread nil))
  (if in-thread
      (viewer-thread-function)
      (trivial-main-thread:with-body-in-main-thread ()
        (viewer-thread-function))))
