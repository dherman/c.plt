#lang setup/infotab
(define name "c")
(define blurb
  (list "Tools for manipulating C, including header extraction for the FFI."))
(define scribblings '(("scribblings/c.scrbl" (multi-page))))
(define categories '(misc metaprogramming system devtools))
(define version "0.4")
(define primary-file "main.ss")
(define release-notes
  (list '(p "Bugfix: fixed " (a ([href "http://planet.plt-scheme.org/trac/ticket/202"]) "parser bug 202") ". "
            "The " (i "ParameterDeclaration") " production is now complete. This involved a subtle restriction of "
            "a popular but non-standard extension that allows " (i "TypedefName") " to be "
            "used in many places that expect " (i "Identifier") ".")))
;; recent dependencies:
;;   4.1.3.8: `procedure-rename'
;;   4.1.4.3: `at-exp' language
;;   4.1.4.3: synthesized source location data in syntax objects no longer ignored for #f source
;;   4.1.4.3: logging no longer gets disabled in DrScheme (not a hard dependency)
(define required-core-version "4.1.4.3")
(define repositories '("4.x"))
