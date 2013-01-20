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

(defmacro new-min-heap (size contents)
  "Creates new max-heap object with correct params."
  (if size
     `(make-min-heap :size ,size)
     (if contents
       `(make-min-heap :contents ,contents)
       `(make-min-heap))))

(defun make-min-heap (&key size contents)
  "Builds max-heap of size or with contents."
  (let ((heap
          (progn
            (if size 
              (make-array size :fill-pointer t :adjustable t)
              (if contents 
                (make-array (length contents) :fill-pointer t :adjustable t :initial-contents contents)
                (make-array 0 :fill-pointer t :adjustable t))))))
    (build-min-heap heap)
    heap))

(defun min-heapify (h i &aux (len (length h)) (largest i))
  "Heapifies a heap with root node i."
  (let ((l (left i)) (r (right i)))
    (when (and (< l len) (< (aref h l) (aref h i)))
      (setf largest l))
    (when (and (< r len) (< (aref h r) (aref h largest)))
      (setf largest r))
    (unless (= largest i)
      (progn
        (rotatef (aref h i) (aref h largest))
        (min-heapify h largest)))))

(defun build-min-heap (h &aux (len (length h)))
  "Builds max heap from vector h."
  (loop for i from (floor (/ (- len 1) 2)) downto 0
        do (min-heapify h i)))

(defun heap-min (h)
  "Gets max from heap h."
  (aref h 0))

(defun heap-extract-min (h)
  "Extracts max from heap."
    (let ((mn (aref h 0)))
      (setf (aref h 0) (vector-pop h))
      (max-heapify h 0)
      mn))

(defun heap-decrease-key (h i)
  "Inserts value i into heap h."
  (loop while (and (> i 0) (> (aref h (parent i)) (aref h i))) do 
        (progn
          (rotatef (aref h i) (aref h (parent i)))
          (setf i (parent i)))))

(defun min-heap-insert (h key)
  "Adds key to heap h."
  (vector-push-extend key h)
  (heap-decrease-key h (- (length h) 1))
  key)
