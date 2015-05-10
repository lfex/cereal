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

(defun test (device)
  (let* ((`#(ok ,fd) (open-tty device))
         ('ok (set-raw-tty-mode fd)))
    (erlang:open_port `#(fd ,fd ,fd) '(binary stream))))

(defun start ()
  (init))

(defun init ()
  "Note that because this function is loading a NIF, it cannot be called
  from anywhere other than this module (thus the start function above)."
  (erlang:load_nif (cereal-util:get-so-name) 0))

(defun not-loaded (line)
  (exit `#(not-loaded (#(module ,(MODULE)) #(line ,line)))))