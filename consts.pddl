(define (constraints GripperCst)
(:cdomain ident)

(:NecessarlyBefore (move pick-up) )

(:PossiblyBefore (action1 action2) )

(:Fill (action1 ressource action2))

(:choice (action1 action2))

(:ImmediatlyLeadsTo (action1 action2))

(:EventualyLeadsTo (action1 action2))

(:Parallel (action1 action2))
)
