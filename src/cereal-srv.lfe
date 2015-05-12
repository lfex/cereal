(defmodule cereal-srv
  (export all))

(defun run (pid fd port)
  (receive
    (`#(,port #(data ,bytes))
     (! pid `#(data ,bytes))
     (run pid fd port))
    (`#(send ,bytes)
     (port-cmd port `(,(cereal-const:send) ,bytes))
     (run pid fd port))
    (#(connect)
     (port-cmd port `(,(cereal-const:connect)))
     (run pid fd port))
    (#(disconnect)
     (port-cmd port `(,(cereal-const:disconnect)))
     (run pid fd port))
    (`#(open ,tty)
     (port-cmd port `(,(cereal-const:open) ,tty))
     (run pid fd port))
    (#(close)
     (logjam:debug (MODULE)
                   'run/2
                   "Preparing to deregister ~p ..."
                   `(,(cereal-const:server-name)))
     ;;(port-cmd port `(,(cereal-const:close)))
     (let (('ok (cereal:close-tty fd)))
       (erlang:port_close port)
       (erlang:unregister (cereal-const:server-name))
       (! pid #(ok closed))))
    (`#(speed ,in-speed ,out-speed)
     (port-cmd port
               (list* (cereal-const:speed)
                      (cereal-util:convert-speed
                        in-speed
                        out-speed)))
     (run pid fd port))
    (`#(speed ,speed)
     (port-cmd port
               (list* (cereal-const:speed)
                      (cereal-util:convert-speed speed)))
     (run pid fd port))
    (#(parity odd)
     (port-cmd port `(,(cereal-const:parity-odd)))
     (run pid fd port))
    (#(parity even)
     (port-cmd port `(,(cereal-const:parity-even)))
     (run pid fd port))
    (#(break)
     (port-cmd port `(,(cereal-const:break)))
     (run pid fd port))
    (#(info)
     (! pid `#(data ,(erlang:port_info port)))
     (run pid fd port))
    ;; XXX is there a better way to flush than this?
    (#(flush)
      (cereal:close-tty fd)
      (cereal:open filename options))
    (`#(EXIT ,port ,why)
     (logjam:error (MODULE) 'run/2 "Exited with reason: ~p~n" `(,why))
     (exit why))
    (msg
     (logjam:info (MODULE) 'run/2 "Received unknown message: ~p~n" `(,msg))
     (run pid fd port))))


(defun port-cmd (port data)
  (logjam:debug (MODULE)
                'port-cmd/2
                "Preparing to call port_command/2 with msg ~p to ~p ..."
                `(,data ,port))
  (erlang:port_command port data))
