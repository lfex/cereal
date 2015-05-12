(defmodule cereal-server
  (behaviour gen_server)
  (export all))

(defun start (port)
  (let ((options '()))
    (gen_server:start_link
      `#(local ,(cereal-const:server-name))
      (cereal-const:callback-module)
      port
      options)))

(defun start_link (filename options)
  (cereal:load-driver)
  (prog1
    (let* ((fd (open-tty filename))
           ('ok (cereal:set-raw-tty-mode fd))
           (port (erlang:open_port `#(fd ,fd ,fd) '(binary stream))))
      (start port))
    (cereal-util:process-options options)))

(defun open-tty (filename)
  (logjam:debug (MODULE)
                'open-tty/1
                "Attempging to open TTY ~p ..."
                `(,filename))
  (try
    (let ((`#(ok ,fd) (cereal:open-tty filename)))
      fd)
    (catch
      (err
       (logjam:debug (MODULE)
                     'open-tty/1
                     "Could not open ~p: ~p"
                     `(,filename ,err))
       (error err)))))