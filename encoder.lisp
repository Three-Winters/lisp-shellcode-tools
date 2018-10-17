;;;; This file is for encoding shellcode using a XOR encoder or custom encoder.

(ql:quickload :cl-ppcre)

;; Put your shellcode down in this variable.
(defparameter *shellcode* "")

(setf *print-base* 16)
(setf *read-base* 16)

(defun write-data (lst)
	"Debug, for printing internal data"
	(format t "~x" lst))

(defun encoder-func (lst ixor func)
	"A function that calls a function (the encoder itself) to transform data"
	(labels ((fn (lst xlst)
						 (if (not (equal lst 'nil))
								 (progn
									 (let ((byte (funcall func (car lst) ixor)))
										 (fn (cdr lst) (cons (abs byte) xlst))))
								 (nreverse xlst))))
		(fn lst 'nil)))

(defun string-to-list (str)
	"Turn a string of characters into a list"
	(if (not (streamp str))
			(string-to-list (make-string-input-stream str))
			(if (listen str)
					(cons (read str) (string-to-list str))
					'nil)))

(defun make-list-from-shellcode (str)
	"Takes the usual C \x00 codes and turns them into a lisp list"
	(string-to-list (cl-ppcre:regex-replace-all "x" str " ")))

(defun write-shellcode-to-file (bytes filepath)
	"Writes the new encoded shellcode into a binary file, ready to be read or inserted"
	(with-open-file (file filepath :direction :output :element-type 'unsigned-byte
												:if-exists :append :if-does-not-exist :create)
		(dolist (segment bytes)
			(write-byte segment file))))

(defun xor-encode (ixor filename)
	"XOR encoder"
	(write-shellcode-to-file (encoder-func (make-list-from-shellcode *shellcode*) ixor #'logxor) filename))

(defun not-encode (inot filename)
	"NOT encoder. It also does NOT work right."
	(write-shellcode-to-file (encoder-func (make-list-from-shellcode *shellcode*) inot #'lognor) filename))

(defun read-bin (filename)
	"Read a binary and print the values, C-style."
	(with-open-file (file filename :direction :input :element-type 'unsigned-byte)
		(loop :do
				 (let ((byt (read-byte file 'nil)))
					 (if (equal byt 'nil)
							 (return)
							 (format t "\\x~x" byt))))))

(defun read-bin-array (filename)
	(with-open-file (file filename :direction :input :element-type 'unsigned-byte)
		(loop :do
				 (let ((byt (read-byte file 'nil)))
					 (if (equal byt 'nil)
							 (return)
							 (format t "0x~x," byt))))))
