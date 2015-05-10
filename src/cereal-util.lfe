(defmodule cereal-util
  (export all))

(defun get-version ()
  (lutil:get-app-version 'cereal))

(defun get-versions ()
  (++ (lutil:get-versions)
      `(#(cereal ,(get-version)))))

(defun get-so-name ()
  (case (code:priv_dir 'cereal)
    (#(error bad_name)
     (case (filelib:is_dir (filename:join '(".." priv)))
       ('true
        (filename:join '(".." priv cereal)))
       (_
        (filename:join '(priv cereal)))))
    (dir
     (filename:join dir 'cereal))))