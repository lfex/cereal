(defmodule cereal
  (export all))

(include-lib "cereal/include/records.lfe")

;;; Start-up functions

(defun start ()
  (init))

(defun init ()
  (logjam:start)
  (let* (('ok (erlang:load_nif (cereal-util:get-so-name) 0)))
    #(ok started)))

(defun run (pid filename)
  (let* ((`#(ok ,fd) (open-tty filename))
         ('ok (set-raw-tty-mode fd))
         (port (erlang:open_port `#(fd ,fd ,fd) '(binary stream))))
    (cereal-srv:run pid (make-state filename filename
                                    fd fd
                                    port port))))

;;; API

(defun open (filename)
  (open filename '()))

(defun open (filename options)
  (let ((pid (spawn_link 'cereal 'run `(,(self) ,filename))))
    (erlang:register (cereal-const:server-name) pid)
    ;;(cereal-util:process-options pid options)
    (set-options options)
    (case (is_pid pid)
      ('true #(ok opened))
      (_ `#(error ,pid)))))

(defun set-options (options)
  (set-options (whereis (cereal-const:server-name)) options))

(defun set-options (pid options)
   (logjam:debug (MODULE) 'set-options/2 "Setting options ~p ..." `(,options))
   (! pid `#(set-options ,options))
   (receive
     (#(ok options-set)
       (cereal-util:flush)
       options)
     (x `#(error ,x))))

(defun get-options ()
  (get-options (whereis (cereal-const:server-name))))

(defun get-options (pid)
  (logjam:debug (MODULE) 'get-options/1 "Getting options ...")
  (cereal-util:flush)
  (! pid #(options))
  (receive
    (`#(ok ,options) options)
    (x `#(error ,x))))

(defun get-state ()
  (get-state (whereis (cereal-const:server-name))))

(defun get-state (pid)
  (logjam:debug (MODULE) 'get-state/1 "Getting state ...")
  (cereal-util:flush)
  (! pid #(state))
  (receive
    (`#(ok ,state) (cereal-util:state->plist state))
    (x `#(error ,x))))

(defun send (bytes)
  (send (whereis (cereal-const:server-name)) bytes))

(defun send (pid bytes)
  (logjam:debug (MODULE) 'send/2 "Sending bytes ~p ..." `(,bytes))
  (! pid `#(send ,bytes))
  (receive
    (`#(data ,bytes) bytes)
    (x `#(error ,x))))

(defun info ()
  (info (whereis (cereal-const:server-name))))

(defun info (pid)
  (! pid #(info))
  (receive
    (`#(data ,data) data)
    (x `#(error ,x))))

(defun flush (pid)
  (logjam:debug (MODULE) 'flush/1 "Preparing to close cereal connection ...")
  (! pid #(flush))
  #(ok flushed))

(defun close ()
  (close (whereis (cereal-const:server-name))))

(defun close (pid)
  (logjam:debug (MODULE) 'close/1 "Preparing to close cereal connection ...")
  (! pid #(close))
  (receive
    (x
     (logjam:debug (MODULE) 'close/1 "Got ~p" `(,x))
     x)))

;;; NIF API - don't use unless you really know what you're doing (use the API
;;;           instead)

(defun set-raw-tty-mode (_)
  (cereal-util:not-loaded (MODULE) (LINE)))

(defun set-tty-speed (_ _ _)
  (cereal-util:not-loaded (MODULE) (LINE)))

(defun set-tty-flow (_ _)
  (cereal-util:not-loaded (MODULE) (LINE)))

(defun open-tty (_)
  (cereal-util:not-loaded (MODULE) (LINE)))

(defun close-tty (_)
  (cereal-util:not-loaded (MODULE) (LINE)))
