#lang racket

(require (only-in xml write-xexpr))
(require "../common.rkt" "../datafile.rkt" "../syntax/types.rkt" "../pareto.rkt"
         "common.rkt" "plot.rkt" "../syntax/read.rkt" "../sandbox.rkt")

(provide make-bench-page)

(define (make-bench-page result out output? fpcore? profile?)
  ;; HTML cruft
  (fprintf out "<!doctype html>\n")
  (write-xexpr
   `(html
     (head
      (title "Herbie results")
      (meta ((charset "utf-8")))
      (script ([src "https://unpkg.com/mathjs@4.4.2/dist/math.min.js"]))
      (script ([src "https://unpkg.com/d3@6.7.0/dist/d3.min.js"]))
      (script ([src "https://unpkg.com/@observablehq/plot@0.4.3/dist/plot.umd.min.js"])))
      (link ([rel "stylesheet"] [type "text/css"] [href "../report.css"]))
      (script ([src "interactive.js"]))
      (script ([src "../report.js"]))
      (script ([src "../bench-page.js"][type "module"]))
     (body))
   out))

