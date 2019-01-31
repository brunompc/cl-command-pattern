
(in-package :maps)

;; this is my pseudo-state

(defvar *nr.of.jumps* 0)
(defvar *nr.of.shots* 0)

(defvar *commands* nil)

(defvar *current.command.id* 0)

(defclass command ()
  ())

(defgeneric execute ((command command)))
(defgeneric undo ((command command)))

;; our JUMP command...
(defclass jump-command (command)
  ())

(defmethod execute ((command jump-command))
  (format t "Jumping...~%")
  (incf *nr.of.jumps*))

(defmethod undo ((command jump-command))
  (format t "Undoing jump...~%")
  (decf *nr.of.jumps*))

;; our FIRE command

(defclass fire-command (command)
  ())

(defmethod execute ((command fire-command))
  (format t "Firing...~%")
  (incf *nr.of.shots*))

(defmethod undo ((command fire-command))
  (format t "Undoing fire...~%")
  (decf *nr.of.shots*))

;; command execution and registration

(defun perform.command (type)
  (let ((cmd (make-instance type)))
    (execute cmd)
    (setf *commands* (snoc cmd *commands*))
    (incf *current.command.id*)))

(defun perform.jump ()
  (perform.command 'jump-command))

(defun perform.fire ()
  (perform.command 'fire-command))

;; generic UNDO

(defun undo.last.command ()
  (let ((prev.command.id (- 0 1)))
    (cond ((>= *current.command.id* 0)
	   (let ((prev.command (nth *current.command.id* *commands*)))
	     (undo prev.command)
	     (decf *current.command.id*)))
	  (t
	   (format t "Nothing to Undo")))))

(defun reset.state ()
  (setf *nr.of.jumps* 0)
  (setf *nr.of.shots* 0)
  (setf *commands* nil)
  (setf *current.command.id* -1))


(defun demo ()
  (reset.state)
  (let (;;(commands nil)
	)
    (format t "Nr of jumps: ~a~%" *nr.of.jumps*)
    (format t "Nr of shots: ~a~%" *nr.of.shots*)

    (perform.jump)

    ;;(ide::trace-format)
    
    (format t "Nr of jumps: ~a~%" *nr.of.jumps*)
    (format t "Nr of shots: ~a~%" *nr.of.shots*)

    (perform.jump)

    (format t "Nr of jumps: ~a~%" *nr.of.jumps*)
    (format t "Nr of shots: ~a~%" *nr.of.shots*)
    
    (perform.fire)

    (format t "Nr of jumps: ~a~%" *nr.of.jumps*)
    (format t "Nr of shots: ~a~%" *nr.of.shots*)
    
    (undo.last.command) ;; undo a fire
    
    (format t "Nr of jumps: ~a~%" *nr.of.jumps*)
    (format t "Nr of shots: ~a~%" *nr.of.shots*)
    
    (undo.last.command) ;; undo a jump
    
    (format t "Nr of jumps: ~a~%" *nr.of.jumps*)
    (format t "Nr of shots: ~a~%" *nr.of.shots*)

    (undo.last.command) ;; undo another jump

    (undo.last.command) ;; nothing to undo
    
    ))
