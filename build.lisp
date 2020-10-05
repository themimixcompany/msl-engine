;;;; build.lisp

(in-package #:cl-user)

(load-all-patches)

(let ((asdf #P"./scripts/asdf.lisp"))
  (when (probe-file asdf)
    (hcl:compile-file-if-needed asdf :load t)))

#-asdf
(let ((asdf-init
	(merge-pathnames ".quicklisp/local-projects/asdf.lisp" (user-homedir-pathname))))
  (when (probe-file asdf-init)
    (hcl:compile-file-if-needed asdf-init :load t)))

#-quicklisp
(let ((quicklisp-init
        (merge-pathnames #P".quicklisp/setup.lisp" (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

(unless (fboundp 'utf-8-file-encoding)
  (defun utf-8-file-encoding (pathname ef-spec buffer length)
    (declare (ignore pathname buffer length))
    (system:merge-ef-specs ef-spec :utf-8))
  (compile 'utf-8-file-encoding))

(setq system:*file-encoding-detection-algorithm*
      '(utf-8-file-encoding))
(lw:set-default-character-element-type 'character)
(fli:set-locale)

(asdf:load-system :streams :force t)
(streams:build)
