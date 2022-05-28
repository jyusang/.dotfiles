(defun message-init-time ()
  (message "Init time: %s"
           (format "%.2fs" (float-time (time-subtract
                                        after-init-time
                                        before-init-time)))))
