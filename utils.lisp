;;;; utils.lisp

(in-package #:webs)

(defun slurp-file (path)
  "Read entire file as string."
  (uiop:read-file-string path))

(defun file-string (path)
  "Read entire file as byte sequence."
  (with-open-file (stream path)
    (let ((val (make-string (file-length stream))))
      (read-sequence val stream)
      val)))

(defun resolve-system-file (file system)
  "Return the path of FILE relative to current system."
  (uiop:merge-pathnames* file (asdf:system-source-directory (asdf:find-system system))))