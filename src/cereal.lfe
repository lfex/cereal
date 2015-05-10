(defmodule cereal
  (export all))

(defun set-raw-tty-mode (_)
  (not-loaded (LINE)))

(defun set-tty-speed (_ _ _)
  (not-loaded (LINE)))

(defun set-tty-flow (_ _)
  (not-loaded (LINE)))

(defun open-tty (_)
  (not-loaded (LINE)))

(defun close-tty (_)
  (not-loaded (LINE)))

(defun start (filename)
  (start filename '()))

(defun start (filename options)
  (let ((pid (spawn_link 'cereal 'init `(,(self) ,filename))))
    (process-options pid options)
    pid))

(defun init (pid filename)
  (let* (('ok (erlang:load_nif (cereal-util:get-so-name) 0))
         (`#(ok ,fd) (open-tty filename))
         ('ok (set-raw-tty-mode fd))
         (port (erlang:open_port `#(fd ,fd ,fd) '(binary stream))))
    (cereal-srv:run pid port)))

(defun process-options
  ((_ '())
   'done)
  ((pid (cons opt opts))
   (! pid opt)
   (process-options pid opts)))

(defun not-loaded (line)
  (exit `#(not-loaded (#(module ,(MODULE)) #(line ,line)))))

(defun stop (pid)
  (! pid #(stop)))

(defun send (pid bytes)
  (! pid `#(send ,bytes)))