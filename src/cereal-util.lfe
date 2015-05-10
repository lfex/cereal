(defmodule cereal-util
  (export all))

(defun get-version ()
  (lutil:get-app-version 'cereal))

(defun get-versions ()
  (++ (lutil:get-versions)
      `(#(cereal ,(get-version)))))
