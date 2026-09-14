#lang sicp
; assume the keys can be ordered
; each node in the tree will consist of (key, value) pair and a ptr to the left subtree and right subtree
; compare the key to the current node. if equal return the associated value, if less than recurse on left subtree;
; if greater than recurse on right subtree
; keep the tree balanced so that looks up are logarithmic times