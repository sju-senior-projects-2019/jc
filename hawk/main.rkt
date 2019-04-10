#lang racket/base

(require hawk/expander (for-syntax racket/base syntax/parse))
(module reader syntax/module-reader hawk)
;(module hawk hawk/expander)
(provide (all-from-out racket/base)
         #%datum ;May have to change
         #%module-begin ;Must change
         #%app ;May have to change
         #%top ;Don't have to change
         #%top-interaction) ;Don't have to change




