#lang racket

(provide db-name^ db-funs^)

(define-signature db-name^ (db-name))
(define-signature db-funs^
  (error-insert!
   get-all-groups-with-frequency
   group-find
   group->messages
   group->solutions
   solution->group
   solution-insert!
   solution-upvote!
   solution-downvote!
   clear-all-collections!
   get-all-types
   type->groups))