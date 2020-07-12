
\\
\\ The FOR macro by Ramil Farkshatov 2015-10-14
\\
\\

\*

The FOR1 macro with a negative step size

The WHILE-DO macro

The REPEAT-UNTIL macro 

Dr Antti J Ylikoski 2020-07-12

*\



(define for-expand
  Var Start Step End Code
      -> (let F (gensym (protect A))
              Self (protect Self)
                 [let F [/. Self Var [if [<= Var End]
                                         [do [do | Code]
                                                [Self Self [+ Var Step]]]
                                              []]]
                                 [F F Start]]))



(defmacro for-macro
  [for [Var Start Step End] | Code] -> (for-expand Var Start Step End Code)
  [for [Var Start End] | Code] -> (for-expand Var Start 1 End Code)
  [for [Var End] | Code] -> (for-expand Var 0 1 End Code))


\*

Usage: (for (Var Start Step End) Code...)
       (for (Var Start End) Code...) \\ Step is 1
       (for (Var End) Code...) \\ Start is 0, Step is 1

(for (X 0 2 30) (output "~A~%" X))
(for (Y 0 10) (output "~A~%" Y))
(for (Z 5) (output "~A~%" Z))

*\


\\ The FOR1 macro with a negative increment



(define for1-expand
  Var Start Step End Code
      -> (let F (gensym (protect A))
              Self (protect Self)
                 [let F [/. Self Var [if [>= Var End]
                                         [do [do | Code]
                                                [Self Self [+ Var Step]]]
                                              []]]
                                 [F F Start]]))



(defmacro for1-macro
  [for1 [Var Start Step End] | Code] -> (for1-expand Var Start Step End Code)
  [for1 [Var Start End] | Code] -> (for1-expand Var Start -1 End Code)
  [for1 [Var End] | Code] -> (for1-expand Var 0 -1 End Code))


\*

Usage: (for1 (Var Start Step End) Code...)
       (for1 (Var Start End) Code...) \\ Step is -1
       (for1 (Var End) Code...) \\ Start is 0, Step is -1

(for1 (X 30 -2 0) (output "~A~%" X))
(for1 (Y 10 0) (output "~A~%" Y))
(for1 (Z -5) (output "~A~%" Z))

*\


\\ The WHILE-DO macro



(define while-expand
  WhileCondition Code
      -> (let F (gensym (protect A))
              Self (protect Self)
                 [let F [/. Self [if WhileCondition
                                         [do [do | Code]
                                                [Self Self]]
                                              []]]
                                 [F F]]))



(defmacro while-macro
  [while WhileCondition do Code] -> (while-expand WhileCondition Code))


\*

(do
  (set *var* 0)
  (while (<= (value *var*) 10)
         do
	 (do
	     (output "~%~S" (value *var*))
	     (set *var* (+ 1 (value *var*))))))



*\



\\ the REPEAT-UNTIL macro



(define repeat-expand
  RepeatCondition Code
      -> (let F (gensym (protect A))
              Self (protect Self)
	      [do
	         [do | Code]
                 [let F [/. Self [if [not RepeatCondition]
                                         [do [do | Code]
                                                [Self Self]]
                                              []]]
                                 [F F]]]))



(defmacro repeat-macro
  [repeat Code until RepeatCondition] -> (repeat-expand RepeatCondition Code))


\*

(do
  (set *var* 0)
  (repeat
         (do
	     (output "~%~S" (value *var*))
	     (set *var* (+ 1 (value *var*))))
	  until (> (value *var*) 10)))


*\

