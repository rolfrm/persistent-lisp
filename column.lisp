(defconstant  mremap-maymove #x1)

(defun concat-symbols(&rest symbols)
  (intern (format nil "~{~a~}" symbols)))

(defmacro struct-slots (instance type (&rest slots) &rest body)
  `(progn
     (assert (typep ,instance ',type))
     (symbol-macrolet ,(mapcar (lambda (slot) (list slot (list
							  (concat-symbols
							   type '-
							   slot)
							  instance)))
			       slots)
       ,@body)))

(defstruct (data-column (:copier nil))
  (data nil)
  (data-header nil)
  (file nil :type stream)
  (capacity 0 :type fixnum))

(defun data-column-count (column)
  (declare (type data-column column))
    (struct-slots column data-column (data-header)
		  (if data-header
		      (sb-sys:sap-ref-32 data-header 0)
		      0)))

(defun (setf data-column-count) (value column)
  (declare (type fixnum value) ;(type data-column column)
	   )
    (struct-slots column data-column (data-header)
		  (assert data-header)
		  (setf (sb-sys:sap-ref-32 data-header 0) value)))
(let ((column-header-offset 4)) ;header offset for column data. Contains data such as count."
  (defun alloc-data-column (column type-size length &optional (truncate t))
    "Resize the allocated array to a given length. 
     This changes the capacity of the column"
    (declare ;(type data-column column)
		 (type fixnum length))
	(assert (> length 0))
	(assert (> type-size 0))
	(struct-slots column data-column
		      (data-header capacity file data)
		      (when truncate
			(sb-posix:ftruncate file ( + column-header-offset (* length type-size))))
		      (setf data-header
			    (if data-header
				(sb-posix:mremap data-header
						 (+ column-header-offset (* capacity type-size))
						 (+ column-header-offset (* length type-size)) mremap-maymove)
				(sb-posix:mmap nil (+ column-header-offset (* length type-size))
					       sb-posix:prot-write sb-posix:map-shared file 0)))
		      (setf data (sb-sys:sap+ data-header column-header-offset))
		      (setf capacity length)
		      ))
  
  (defun load-data-column(constructor type-size file-path)
    (declare (type string file-path))
    (let ((file (open file-path :direction :io :if-exists :overwrite
		      :if-does-not-exist :create)))
      ;(print (format nil "OPEN: ~a" (file-length file)))
      (let ((column (funcall constructor :file file))
	    (file-length (file-length file)))
	(alloc-data-column column type-size (max 1 (/ (- file-length column-header-offset) type-size)) t)
      column)))
  )

(defun data-column-push(column size)
  (declare (type data-column column)
	   (type fixnum size))
    (struct-slots column data-column (capacity count data)
		  (when (eq capacity count)
		    (alloc-data-column column size (* count 2) t))
		  (incf count 1)))
(defmacro defcolumn(type size accessor)
  ;ex accessor: sb-sys:sap-ref-single data
  (let ((type-name (concat-symbols 'column- type)))
    `(progn
       (defstruct (,type-name (:constructor ,(concat-symbols '%make- type-name)) (:copier nil) (:include data-column))
	 "Column of X.")

       (let ((type-size ,size))
	 
	 (defun ,(concat-symbols 'alloc- type-name '-data) (column length &optional (truncate t))
	   "Resize the allocated array to a given length. 
     This changes the capacity of the column"
	   (declare (type ,type-name column)
		    (type fixnum length))
	   (alloc-data-column column length type-size truncate))
	 
	 (defun ,(concat-symbols type-name '-count) (column)
	   (declare (type ,type-name column))
	   (data-column-count column))

	 (defun (setf ,(concat-symbols type-name '-count)) (value column)
	   (declare (type ,type-name column)
		    (fixnum value))
	   (assert (<= value (data-column-capacity column)))
	   (assert (>= value 0))
	   (setf (data-column-count column) value))
  
	 (defun ,(concat-symbols 'make- type-name) (file-path)
	   (declare (type string file-path))
	   (load-data-column #',(concat-symbols '%make- type-name) type-size file-path))

	 (defun ,(concat-symbols type-name '-push) (column value)
	   (declare (type ,type-name column)
		    (type ,type value))
	   (data-column-push column type-size)
	   (struct-slots column data-column (capacity count data)
			 (setf (,accessor data (* type-size (- count 1))) value))) 

	 (defun ,(concat-symbols type-name '-get) (column index)
	   (declare (type ,type-name column)
		    (fixnum index))
	   (struct-slots column data-column (data)
			 (,accessor data (* type-size index))))

	 (defun (setf ,(concat-symbols type-name '-get)) (value column index)
	   (declare (type ,type-name column)
		    (fixnum index)
		    (type ,type value))
	   (struct-slots column data-column (data)
			 (setf (,accessor data (* type-size index)) value)))
  ))))

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
