;;;; This file is for replacing instructions before assembly with equivalent ones to avoid fingerprinting
;;;; (i.e. shellcode polymorphism)
;;;; Only supports intel nasm syntax.

;; address of the E of magic bytes in ELF executables for interesting stuff.
(defparameter *magic-address* "0x400000")

(ql:quickload :cl-utilities)

(setf *print-base* 16)
(setf *read-base* 16)

(defparameter *polymorphs* '(("nop" . '("xchg ax,ax~%"
																				"xchg rax,rax~%"
																				"nop";end one/two byte safe nops
																				"nop dword [rax]~%"
																				"nop dword [rax+rax]~%"
																				"nop word [rax+rax]~%"
																				"push rax~%pop rax~%"
																				"inc rax~%dec rax~%"
																				"inc rbx~%dec rbx~%"))
														 
														 ("xor rax, rax" . '("sub rax,rax~%"
																								 "mov r10,rax~%sub rax,r10~%"
																								 "mov rax,[~a]~%sub rax,0x7f~%"
																								))))

;;(cdr (assoc "nop" *polymorphs* :test #'string=))
;;(nth (random length) list)

(defun magic-math (result)
	"Obfuscates any obvious values passed to registers by using an address to do arithmetic"
	(let ((x (- result 7f)))
		(if (<= x 0)
				(format t "sub ~a, 0x7f~%" (abs x))
				(format t "add ~a, 0x7f~%" x))))

(defun string-split (str)
	"Splits a string. Fancy."
	(cl-utilities:split-sequence #\Space str))

(defun de-white (str)
	"Removes whitespace from strings"
	(string-trim
	 '(#\Space #\Newline #\Backspace #\Tab
		 #\Linefeed #\Page #\Return #\Rubout)
	 str))

(defun open-file (fpath)
	"Opens the source assembly code file to polymorph"
	(let ((in (open fpath :if-does-not-exist nil)))
		(when in
			(loop :for line = (read-line in nil)
				 while line do (progn
												 (let* ((x (caddr (assoc (de-white line) *polymorphs* :test #'string=)))
																(y (nth  (random (1+ (length x))) x)))
													 (if (not (string= y nil))
															 (progn
																 (format t y *magic-address*))
															 (format t "~a~%" line)))))
			(close in))))

