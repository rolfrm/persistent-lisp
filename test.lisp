(defcolumn fixnum 8 sb-sys:sap-ref-64)
(defcolumn single-float 4 sb-sys:sap-ref-single)
(defcolumn double-float 8 sb-sys:sap-ref-double)


(defun shorttest()
  (let ((col (make-column-single-float "test.x"))
	(col2 (make-column-fixnum "test.y"))
	(col3 (make-column-double-float "test.z")))

    (column-single-float-push col 1.0)
    (column-single-float-push col 2.0)
    (column-single-float-push col 3.0)
    (column-fixnum-push col2 1)
    (column-fixnum-push col2 2)
    (column-fixnum-push col2 3)
    (column-double-float-push col3 1d-10)
    (column-double-float-push col3 2d-10)
    (column-double-float-push col3 4d-10)
    (dotimes (i (column-single-float-count col))
      (print (column-single-float-get col i)))
    (dotimes (i (column-fixnum-count col2))
      (print (column-fixnum-get col2 i)))
    (dotimes (i (column-double-float-count col3))
      (print (column-double-float-get col3 i)))
    (print (reverse (list (column-single-float-capacity col) "/" (column-single-float-count col))))
    ))
(shorttest)


(declaim (optimize (speed 0) (space 0) (safety 3) (debug 3) (compilation-speed 0)))
(require 'sb-posix)

;(defparameter file (open "./data/table/color_alpha.value" :direction :io :if-exists :overwrite ))
;(defparameter sap (sb-posix:mmap nil 1024 sb-posix:prot-write sb-posix:map-private file 0))      

;(defparameter file (open "./lispdata" :direction :io :if-exists :overwrite :if-does-not-exist :create ))
;(write-string "                                     " file)
;(file-position file 0)
;(defparameter sap (sb-posix:mmap nil 1024 sb-posix:prot-write sb-posix:map-shared file 0))
#|
(defmacro deftable (name &rest columns)
  (declare (type symbol name))
  
  '())

(deftable particles
    (key particle :index t)
    (x single-float)
    (y single-float))

|#
#|
(defstruct (persisted-array (:constructor %make-persisted-array))
  (mptr nil :type system-area-pointer)
  (capacity 0 :type fixnum)
  (file nil :type stream))

(defun make-persisted-array(&keys (filepath nil) (stream nil))
  (assert (not (and filepath stream)))
  (when filepath
    (setf stream (open file-path :direction :io :if-exists :overwrite
		       :if-does-not-exist :create)))
  (let ((outp (%make-persisted-array :file file :capacity (file-length
							   file))))
    (persisted-array-alloc ))) ;; todo: finish this.
|#
	  
(defconstant  mremap-maymove #x1)

(defstruct (column-single-float (:constructor %make-column-single-float) (:copier nil))
  "Column of single floats."
  (data nil)
  (data-header nil)
  (file nil :type stream)
  (capacity 0 :type fixnum))

(defun concat-symbols(&rest symbols)
  (intern (format nil "~{~a~}" symbols)))

(defmacro struct-slots (instance type (&rest slots) &rest body)
  `(progn
     (declaim (type ,type instance))
     (symbol-macrolet ,(mapcar (lambda (slot) (list slot (list
							   (concat-symbols
							    type '-
							    slot)
							   instance)))
				slots)
       ,@body)))

(defun column-single-float-count (column)
  (declare (type column-single-float column))
  (struct-slots column column-single-float (data-header)
		(if data-header
		    (sb-sys:sap-ref-32 data-header 0)
		    0)))
    

(defun (setf column-single-float-count) (value column)
  (declare (type fixnum value) (type column-single-float column))
  (let ((ptr (column-single-float-data-header column)))
    (assert ptr)
    (setf (sb-sys:sap-ref-32 ptr 0) value)))

(defconstant *column-header-offset* 4 "header offset for column data. Contains data such as count.")
(defun alloc-column-single-float-data (column length &optional (truncate t))
  "Resize the allocated array to a given length. 
  This changes the capacity of the column"
  (declare (type column-single-float column)
	   (type fixnum length))
  (assert (> length 0))
  (struct-slots column column-single-float
		(data-header capacity file data)
    (when truncate
      (sb-posix:ftruncate file ( + 4 (* length 4))))
    (setf data-header
	  (if data-header
	      (sb-posix:mremap data-header (+ 4 (* capacity 4)) (+ 4 (* length 4)) mremap-maymove)
	      (sb-posix:mmap nil (+ 4 (* length 4)) sb-posix:prot-write sb-posix:map-shared file 0)))
    (setf data (sb-sys:sap+ data-header 4))
     (setf capacity length)
     ))

(defun make-column-single-float(file-path)
  (declare (type string file-path))
  (let ((file (open file-path :direction :io :if-exists :overwrite
		    :if-does-not-exist :create)))
    (let ((column 
	   (%make-column-single-float :file file))
	  (file-length (file-length file)))
      (alloc-column-single-float-data column (max 1 (/ file-length 4)) t)
      column)))

(defun column-single-float-push (column value)
  (declare (type column-single-float column)
	   (type single-float value))
  (let ((count (column-single-float-count column)))
    (when (eq (column-single-float-capacity column) count)
      (alloc-column-single-float-data column (* count 2) t))
    (let ((data (column-single-float-data column)))
      (setf (sb-sys:sap-ref-single data (* 4 count)) value))
    (incf (column-single-float-count column) 1)))

(defun column-single-float-get (column index)
  (declare (type column-single-float column)
	   (type fixnum index))
  (sb-sys:sap-ref-single (column-single-float-data column) (* 4 index)))

(defun (setf column-single-float-get) (value column index)
  (declare (type column-single-float column)
	   (type fixnum index)
	   (type single-float value))
  (setf (sb-sys:sap-ref-single (column-single-float-data column) (* 4 index)) value))

(defun shorttest()
  (let ((col (make-column-single-float "test.x")))
    (column-single-float-push col 1.0)
    (column-single-float-push col 2.0)
    (column-single-float-push col 3.0)
    (dotimes (i (column-single-float-count col))
      (print (column-single-float-get col i)))
    ))
(shorttest)

(defstruct column-integer
  (data (make-array 0 :element-type 'integer :adjustable t))
  (count 0 :type integer))

(defun column-integer-push (column value)
  (declare (type column-integer column)
	   (type integer value))
  (let ((data (column-integer-data column))
	(count (column-integer-count column)))
    (when (eq (array-total-size data) count)
      (adjust-array data (* 2 (+ count 1))))
    (setf (aref data count) value)
    (incf (column-integer-count column) 1)))

(defun column-integer-find-index (column key)
  (declare (type column-integer column)
	   (type integer key))
  (let ((size (column-integer-count column)))
    (dotimes (x size)
	  (when (eq (aref (column-integer-data column) x) key)
	    (return x)))))

(defun column-integer-get (column index)
  (declare (type column-integer column)
	   (type integer index))
  (aref (column-integer-data column) index))

(defun (setf column-integer-get) (column index value)
  (declare (type column-integer column)
	   (type integer index value))
  (setf (aref (column-integer-data column) index) value))

(defstruct particle-table
  (index-iter 0 :type integer)
  (index nil :type column-integer)
  (x nil :type column-single-float)
  (y nil :type column-single-float)
  (particle-id 0 :type integer)
  )

(defstruct particle
  (key 0 :type integer)
  (index-iter -1 :type integer)
  (index -1 :type integer)
  (table nil :type particle-table))

(defun particle-new (table)
  (declare (type particle-table table))
  (let ((part (make-particle
	       :table table
	       :key (incf (particle-table-particle-id table)))))
    (column-single-float-push (particle-table-x table) 0.0)
    (column-single-float-push (particle-table-y table) 0.0)
    (column-integer-push (particle-table-index table) (particle-key part))
    part))

(defun create-particle-table ()
  (make-particle-table
   :x (make-column-single-float "particle.x")
   :y (make-column-single-float "particle.y")
   :index (make-column-integer)))

(defun particle-get-index (particle)
  (declare (type particle particle))
  (let ((table (particle-table particle)))
    (unless (eq (particle-index-iter particle)
		(particle-table-index-iter table))
      (setf (particle-index particle) (column-integer-find-index
				       (particle-table-index table)
				       (particle-key particle)))
      (setf (particle-index-iter particle) (particle-table-index-iter table)))
    (particle-index particle)))

(defun particle-x (particle)
  (declare (type particle particle))
  (let ((column (particle-table-x (particle-table particle))))
    (column-single-float-get column (particle-get-index particle))
  ))

(defun (setf particle-x) (value particle)
  (declare (type particle particle)
	   (type single-float value))
  (let ((column (particle-table-x (particle-table particle))))
    (setf (column-single-float-get column (particle-get-index particle))
	  value))
  )

(defun particle-y (particle)
  (declare (type particle particle))
  (let ((column (particle-table-y (particle-table particle))))
    (column-single-float-get column (particle-get-index particle))
  ))

(defun (setf particle-y) (value particle)
  (declare (type particle particle)
	   (type single-float value))
  (let ((column (particle-table-y (particle-table particle))))
    (setf (column-single-float-get column (particle-get-index particle))
	  value))
  )



(defun test-columns()
  (let ((column (make-column-integer))
	(fcolumn (make-column-single-float "./testdata.bin")))
    (column-integer-push column 1)
    (column-single-float-push fcolumn 2.0)
    (column-integer-push column 3)
    (column-single-float-push fcolumn 3.0)
    (column-integer-push column 5)
    (column-single-float-push fcolumn 4.0)
    (let ((idx (column-integer-find-index column 5)))
      (print (column-integer-get column idx))
      (print (column-single-float-get fcolumn idx))
      (incf (column-single-float-get fcolumn idx) 3.33)
      (print (column-single-float-get fcolumn idx))
    )))



(defun test-particle()
  (let* ((particles (create-particle-table))
	 (p1 (particle-new particles))
	 (p2 (particle-new particles)))
    (setf (particle-x p1) 4.0)
    (setf (particle-y p1) 6.0)
    (setf (particle-x p2) 7.0)
    (setf (particle-y p2) 8.0)
    (print (list (particle-get-index p1) (particle-get-index p2)))
    (print (list (particle-x p1) (particle-y p1)))
    (print (list (particle-x p2) (particle-y p2)))
    (print (list p1 p2))))
;(test-columns)
;(test-particle)
    

#|

(deftable-key particle integer)

(deftable particles
    (key particle :index t)
    (x single-float)
    (y single-float))

(defvar p1 (table-new particles))

(setf (particles-x p1) 10)
(setf (particles-y p1) 15)

|#


;(defparameter strings (let ((proc (sb-ext:run-program "/usr/bin/curl" '("https://api-v2.soundcloud.com/users/125332894/followers?offset=0&limit=200&client_id=2t9loNQH90kzJcsFCODdigxfp325aq4z" "-s") :output :stream :wait nil)))
;			(with-open-stream (o (process-output proc))
;			  (cl-json:decode-json o))))
  #|
    (loop 
       :for line := (read-line o nil nil) 
       :while line 
       :collect line))))

(defparameter concstrs (apply #'concatenate 'string  strings))
(cl-json:decode-json-from-string concstrs)
|#
;(print strings)
