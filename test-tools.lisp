
;;;  This file contains code borrowed from:
;;;  URL: http://github.com/fukamachi/clack

;;;  Therefore, is freely distributable under the LLGPL License.


(in-package :cl-user)
(defpackage webhax-test-tools
  (:use :cl)
  (:import-from :clack.util
   :find-handler)
  (:export
   #:localhost
   #:file-size
   #:get-header
   #:test-app
   #:with-clack-app))
(in-package :webhax-test-tools)

(defvar *clack-test-handler* :hunchentoot
  "Backend Handler to run tests on. String or Symbol are allowed.")
(defvar *clack-test-port* 4242
  "HTTP port number of Handler.")
(defvar *enable-debug-p* t)

(defun port-available-p (port &key (host "127.0.0.1"))
  (let (socket)
    (unwind-protect
         (handler-case (setq socket (usocket:socket-listen
                                      host port :reuse-address t))
           (usocket:address-in-use-error () nil))
      (when socket
        (usocket:socket-close socket)
        T))))

(defun get-header (headers key)
  (let ((val (assoc key headers)))
    (values (cdr val) (not (null val)))))

(defun file-size (file)
  (with-open-file (in file :direction :input)
    (file-length in)))

(defun localhost (&optional path)
  (format nil
          "http://localhost:~D/~:[~;~:*~A~]"
          *clack-test-access-port*
          path))

(defun test-app (app client); &optional desc)
  "Test Clack Application."
  (loop repeat 5
        until (port-available-p *clack-test-port*)
        do (sleep 0.1)
        finally
        (unless (port-available-p *clack-test-port*)
          (error "Port ~D is already in use." *clack-test-port*)))
  (let* ((handler (find-handler *clack-test-handler*))
         (debug *enable-debug-p*)
         (acceptor (bt:make-thread
                    (lambda ()
                      (funcall (intern (string '#:run) handler)
                               app
                               :port *clack-test-port*
                               :debug debug))
                    :initial-bindings
                    `((*clack-test-port* . ,*clack-test-port*)))))
    ;;(when desc (diag desc))
    (sleep 0.5)
    (unwind-protect
        (funcall client)
      (when (bt:thread-alive-p acceptor)
        (bt:destroy-thread acceptor)
        (loop until (port-available-p *clack-test-port*) do
          (sleep 0.1))))))

(defvar *clack-test-access-port* *clack-test-port*
  "Port of localhost to request.
Use if you want to set another port. The default is `*clack-test-port*`.")

(defmacro with-clack-app (app &body body)
  `(test-app ,app
             (lambda ()
               ,@body)))



;;;FIXME: might want to check for:
;;; #+thread-support
