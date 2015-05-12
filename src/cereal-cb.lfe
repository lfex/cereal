(defmodule cereal-cb
  (export all))

(defun init (port)
  `#(ok ,port))

(defun handle_call
    ;; ((`#(,port #(data ,bytes)) caller port)
    ;;  `#(reply ,(! caller `#(data ,bytes)) ,port))
  ((`#(,port #(data ,bytes)) caller _)
   (logjam:debug (MODULE)
                 'handle_call/3
                 "Got data message: ~p" `(,bytes))
   `#(reply ,(! caller bytes) ,port))

  ((`#(data ,bytes) caller port)
   (logjam:debug (MODULE)
                 'handle_call/3
                 "Got data message: ~p" `(,bytes))
   `#(reply ,bytes ,port))
  ((`#(send ,bytes) caller port)
   `#(reply ,(send-data port bytes) ,port))
  ((#(connect) caller port)
   `#(reply ,(send-connect port) ,port))
  ((#(disconnect) caller port)
   `#(reply ,(send-disconnect port) ,port))
  ((`#(open ,tty) caller port)
   `#(reply ,(send-open port tty) ,port))
  ((`#(close) caller port)
   `#(reply ,(send-close port) ,port))
  ((`#(speed ,in-speed ,out-speed) caller port)
   `#(reply ,(send-speed port in-speed out-speed) ,port))
  ((`#(speed ,speed) caller port)
   `#(reply ,(send-speed port speed) ,port))
  ((`#(parity ,parity) caller port)
   `#(reply ,(send-parity port parity) ,port))
  ((#(break) caller port)
   `#(reply ,(send-break port) ,port))
  ((#(stop) caller port)
   (send-close port)
   `#(reply #(ok cereal-stopped) ,port))
  ((`#(EXIT ,why) caller port)
   (logjam:err (MODULE)
               'handle_call/3
               "Exited with reason: ~p~n" `(,why))
   `#(stop ,why () ,port))
  ((msg caller port)
   #(reply #(error "No callback defined for that.") ,port)))

(defun terminate (reason state)
  `#(ok (#(reason ,reason) #(state ,state))))

(defun send-data (port bytes)
  (send-serial port `(,(cereal-const:send) ,bytes)))

(defun send-connect (port)
  (send-serial port `(,(cereal-const:connect))))

(defun send-disconnect (port)
  (send-serial port `(,(cereal-const:disconnect))))

(defun send-open (port tty)
  (send-serial port `(,(cereal-const:open) ,tty)))

(defun send-close (port)
  (send-serial port `(,(cereal-const:close))))

(defun send-speed (port in-speed out-speed)
  (send-serial port (list* (cereal-const:speed)
                           (cereal-util:convert-speed
                             in-speed
                             out-speed))))

(defun send-speed (port speed)
  (send-speed port speed speed))

(defun send-parity
  ((port 'odd)
   (send-serial port `(,(cereal-const:parity-odd))))
  ((port 'even)
   (send-serial port `(,(cereal-const:parity-even))))
  ((port parity)
   (error #(error `(#(parity ,parity) "Unknown parity.")))))

(defun send-break (port)
  (send-serial port `(,(cereal-const:break))))

(defun send-serial (port msg)
  (logjam:debug (MODULE)
                'send-serial/2
                "Sending serial message ~p to ~p ..."
                `(,msg ,port))
  (! port `#(,(whereis (cereal-const:server-name)) #(command ,msg))))

  ;; (! port `#(,(self) #(command ,msg)))
  ;; (receive
  ;;   (x (logjam:debug (MODULE)
  ;;                    'send-serial/2
  ;;                    "(receive) Got data: ~p"
  ;;                    `(,x)))))

