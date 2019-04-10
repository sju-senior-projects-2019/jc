#lang br

(define-macro (hawk-module-begin PARSED-EXPR ...)
  #'(#%module-begin
     PARSED-EXPR ...))
(provide (rename-out (hawk-module-begin #%module-begin))) 