;; Copyright (c) 2013, Jason R. Person
;; All rights reserved.
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are met: 
;;
;; 1. Redistributions of source code must retain the above copyright notice, this
;;    list of conditions and the following disclaimer. 
;; 2. Redistributions in binary form must reproduce the above copyright notice,
;;    this list of conditions and the following disclaimer in the documentation
;;    and/or other materials provided with the distribution. 
;;
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
;; ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
;; DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR
;; ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
;; (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
;; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
;; ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;
;; The views and conclusions contained in the software and documentation are those
;; of the authors and should not be interpreted as representing official policies, 
;; either expressed or implied, of the FreeBSD Project.
(in-package #:heaps)

(defmacro max-heap (n &key size contents)
  "Creates new max heap object."
  `(setf (symbol-function ,n) 
     (let* ((heap (new-max-heap ,size ,contents))
            (hsize (length heap)))
       (dlambda
         (:insert (v) 
          (incf hsize)
          (max-heap-insert heap v))
         (:get-max () 
          (if (> hsize 0) 
            (heap-max heap) nil))
         (:get-size () 
          hsize)
         (:extract-max () 
          (if (> hsize 0) 
            (let ((r (heap-extract-max heap)))
              (decf hsize)
              r)
            nil))))))

(defmacro min-heap (n &key size contents)
  "Creates new min heap object."
  `(setf (symbol-function ,n) 
     (let* ((heap (new-min-heap ,size ,contents))
            (hsize (length heap)))
       (dlambda
         (:insert (v) 
          (incf hsize)
          (min-heap-insert heap v))
         (:get-min () 
          (if (> hsize 0) 
            (heap-min heap) nil))
         (:get-size () 
          hsize)
         (:extract-min () 
          (if (> hsize 0) 
            (let ((r (heap-extract-min heap)))
              (decf hsize)
              r)
            nil))))))
