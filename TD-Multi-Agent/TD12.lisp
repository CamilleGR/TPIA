;; TD 12
(defparameter *agents* nil)

(defun add-facette-to-slot (frame slot facette)
  (push facette (cdr (assoc slot (symbol-value frame)))))

(defun get-slot (slot frame)
  (cdr (assoc slot (symbol-value frame))))

(defun make-agent (&key name comments)
  (let ((agent (gentemp "A-")) (process-id nil))
    (set agent (list (cons 'name name)
                     (cons 'id agent)
                     (cons 'skills nil)
                     (cons 'memory nil)
                     (cons 'inbox nil)
                     (cons 'comments comments)))
    (push agent *agents*)
    
    (setq process-id
          (mp:process-run-function
           (concatenate 'string (symbol-name agent) "-scan")
           #'scan-messages
           agent))
    (push (cons 'process process-id) (symbol-value agent))
    agent))

(defun scan-messages (agent &aux message)
  (loop
    (mp:process-wait "idle: waiting for incoming message" #'input-message? agent)
    (setq message (get-input-message agent))
    (process-message agent message)))

(defun input-message? (agent)
  (cdr (assoc 'inbox (symbol-value agent)))) 

(defun get-input-message (agent)
  (pop (cdr (assoc 'inbox (symbol-value agent)))))


(setq AG-0 (make-agent :name "Vendeur" :comments "Agent vendeur"))

(defun process-message (agent msg)
  (format #.*standard-output* "~%Message '~A' recu par l'agent '~A' !" msg agent))

(sleep 1)
(add-facette-to-slot AG-0 'inbox "Hello Mr !!")
