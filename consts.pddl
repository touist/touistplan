(define (constraints GripperCst)
(:cdomain GRIPPER)

(:NecessarlyBefore (pick-up  ball2 rooma left) (drop  ball2 roomb left))

(:PossiblyBefore (drop  ball1 roomb left) (pick-up ball2 rooma left) )

(:Choice (pick-up ball1 rooma left) (pick-up ball1 rooma right))

(:Parallel (pick-up ball1 ?y left) (pick-up ball2 ?y right))

(:ImmediatlyLeadsTo (pick-up ball1 ?y left) (move ?a ?b))

(:EventuallyLeadsTo (pick-up  ?x rooma ?z) (drop  ?x roomb ?z))



)
