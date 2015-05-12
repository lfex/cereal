(defmodule cereal-const
  (export all))

(defun send () 0)
(defun connect () 1)
(defun disconnect () 2)
(defun open () 3)
(defun close () 4)
(defun speed () 5)
(defun parity-odd () 6)
(defun parity-even () 7)
(defun break () 8)

(defun server-name () 'cereal-api-server)