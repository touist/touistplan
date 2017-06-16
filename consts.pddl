(define (constraints GripperCst)
(:cdomain GRIPPER)

(:NecessarlyBefore (move, pick-up) )

(:PossiblyBefore (action1, action2) )

(:choice (action1, action1))

(:ImmediatlyLeadsTo (action5, action6))

(:EventuallyLeadsTo (action7, action8))

(:Parallel (action9 ,action10))

(:Fill (action11, act12, action13))

(:Parallel (move1, action10, act1))


)
