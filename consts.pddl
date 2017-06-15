(define (constraints GripperCst)
(:cdomain GRIPPER)

(:NecessarlyBefore (move pick-up) )

(:PossiblyBefore (action1 action2) )

(:choice (action1 action2))

(:ImmediatlyLeadsTo (action1 action2))

(:EventuallyLeadsTo (action1 action2))

(:Parallel (action1 action2))


)
