(defmodule cereal
  (behaviour supervisor)
  (export all))

(defun sup-name ()
  'cereal-supervisor)

(defun start (filename)
  (start filename '()))

(defun start (filename options)
  (logjam:start)
  (supervisor:start_link
   `#(local ,(sup-name)) (MODULE) `(,filename ,options))
  ;;(cereal-util:process-options options)
  )

(defun get-child-spec (start-args)
  (let* ((id (cereal-const:server-name))
         (start-module 'cereal-server)
         (start-call `#(,start-module start_link ,start-args))
         (restart 'permanent)
         (shutdown-timeout 2000)
         (child-type 'worker)
         (modules `(,start-module)))
    `#(,id ,start-call ,restart ,shutdown-timeout ,child-type ,modules)))

(defun init (args)
  (logjam:debug (MODULE)
                'init/1
                "Initializing supervisor with args: ~p"
                `(,args))
  (let ((children `(,(get-child-spec args)))
        (restart-strategy #(one_for_one 3 1)))
    `#(ok #(,restart-strategy ,children))))

;;; API

(defun send (bytes)
  (call-srv `#(send ,bytes)))

(defun stop ()
  (call-srv #(stop)))

(defun call-srv (msg)
  (logjam:debug (MODULE)
                'call-srv/1
                "Making call to gen_server: ~p"
                `(,msg))
  (gen_server:call (whereis (cereal-const:server-name)) msg))

;;; NIF API - don't use unless you really know what you're doing

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

(defun load-driver ()
  (logjam:debug (MODULE) 'load-driver/0 "Loading serial NIF ...")
  (logjam:debug (MODULE)
                'load-driver/0
                "Using driver: ~p"
                `(,(cereal-util:get-so-name)))
  (erlang:load_nif (cereal-util:get-so-name) 0))

(defun open-port ()

  (erlang:open_port
    `#(spawn_driver
       ,(cereal-util:get-so-name))
    '(binary stream)))