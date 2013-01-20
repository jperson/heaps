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

(defmacro new-max-heap (size contents)
  "Creates new max-heap object with correct params."
  (if size
     `(make-max-heap :size ,size)
     (if contents
       `(make-max-heap :contents ,contents)
       `(make-max-heap))))

(defun make-max-heap (&key size contents)
  "Builds max-heap of size or with contents."
  (let ((heap
          (progn
            (if size 
              (make-array size :fill-pointer t :adjustable t)
              (if contents 
                (make-array (length contents) :fill-pointer t :adjustable t :initial-contents contents)
                (make-array 0 :fill-pointer t :adjustable t))))))
    (build-max-heap heap)
    heap))

(defun max-heapify (h i &aux (len (length h)) (largest i))
  (let ((l (left i)) (r (right i)))
    (when (and (< l len) (> (aref h l) (aref h i)))
      (setf largest l))
    (when (and (< r len) (> (aref h r) (aref h largest)))
      (setf largest r))
    (unless (= largest i)
      (progn
        (rotatef (aref h i) (aref h largest))
        (max-heapify h largest)))))

(defun build-max-heap (h &aux (len (length h)))
  "Builds max heap from vector h."
  (loop for i from (floor (/ (- len 1) 2)) downto 0
        do (max-heapify h i)))

(defun heap-max (h)
  "Gets max from heap h."
  (aref h 0))

(defun heap-extract-max (h)
  "Extracts max from heap."
    (let ((mx (vector-pop-front h)))
      (build-max-heap h)
      mx))

(defun heap-increase-key (h i)
  "Inserts value i into heap h."
  (loop while (and (> i 0) (< (aref h (parent i)) (aref h i))) do 
        (progn
          (rotatef (aref h i) (aref h (parent i)))
          (setf i (parent i)))))

(defun max-heap-insert (h key)
  "Adds key to heap h."
  (vector-push-extend key h)
  (heap-increase-key h (- (length h) 1))
  key)
