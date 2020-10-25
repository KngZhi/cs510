open OUnit2
open Proc.Ds
open Proc.Interp

(* A few test cases *)
let tests_let = [
  "int"  >:: (fun _ -> assert_equal (Ok (NumVal 22)) (interp "22"));
  "add"  >:: (fun _ -> assert_equal (Ok (NumVal 22)) (interp "11+11"));
  "adds" >:: (fun _ -> assert_equal (Ok (NumVal 22)) (interp "(10+1)+(5+6)"));
  "let"  >:: (fun _ -> assert_equal (Ok (NumVal 22)) (interp "let x=22 in x"));
  "lets" >:: (fun _ -> assert_equal (Ok (NumVal 22))
                 (interp "let x = 0 in let x = 22 in x"));
    "add"  >:: (fun _ -> assert_equal
                   (*                 ~printer:string_of_expval *)
                 (Ok (NumVal 22))
                 (interp "11+11"));

  "adds" >:: (fun _ -> assert_equal
                 (Ok (NumVal 22))
                 (interp "(10+1)+(5+6)"));

  "subs" >:: (fun _ -> assert_equal
                 (Ok (NumVal 20))
                 (interp "(10-1)+(5+6)"));

  "mults" >:: (fun _ -> assert_equal
                 (Ok (NumVal 21))
                 (interp "(10*1)+(5+6)"));

  "divs" >:: (fun _ -> assert_equal
                 (Ok (NumVal 16))
                 (interp "(10/2)+(5+6)"));

  "let"  >:: (fun _ -> assert_equal
                 (Ok (NumVal 44))
                 (interp "let x=22 in x+x"));

  "lets" >:: (fun _ -> assert_equal
                 (Ok (NumVal 22))
                 (interp "let x = 0 in let x = 22 in (x+x)/2"));
]


let tests_proc = [
  "int"  >:: (fun _ -> assert_equal (Ok (NumVal 3))
                 (interp "(proc (x) { x+1 } 2)"))
]


let tests_extensions = [
  "abs_0"  >:: (fun _ -> assert_equal
                 (Ok (NumVal 0))
                 (interp "abs(0)"));

  "abs_pos"  >:: (fun _ -> assert_equal
                 (Ok (NumVal 22))
                 (interp "abs(22)"));

  "abs_neg"  >:: (fun _ -> assert_equal
                 (Ok (NumVal 22))
                 (interp "abs((-22))"));

  "emptylist" >:: (fun _ -> assert_equal
                 (Ok (ListVal []))
                 (interp "emptylist"));

  "cons_singleton" >:: (fun _ -> assert_equal                
                 (Ok (ListVal [NumVal 1]))
                 (interp "cons(1, emptylist)"));

  "cons_list" >:: (fun _ -> assert_equal                 
                 (Ok (ListVal [NumVal 3; NumVal 2; NumVal 1]))
                 (interp "cons(3, cons(2, cons(1, emptylist)))"));

  "hd_singleton" >:: (fun _ -> assert_equal                 
                 (Ok (NumVal 1))
                 (interp "hd(cons(1, emptylist))"));

  "hd_list" >:: (fun _ -> assert_equal               
                 (Ok (NumVal 3))
                 (interp "hd(cons(3, cons(2, cons(1, emptylist))))"));

  "tl_singleton" >:: (fun _ -> assert_equal                 
                 (Ok (ListVal []))
                 (interp "tl(cons(1, emptylist))"));

  "tl_list1" >:: (fun _ -> assert_equal                 
                 (Ok (ListVal [NumVal 1]))
                 (interp "tl(cons(3, cons(1, emptylist)))"));

  "tl_list1" >:: (fun _ -> assert_equal                 
                 (Ok (ListVal [NumVal 2; NumVal 1]))
                 (interp "tl(cons(3, cons(2, cons(1, emptylist))))"));

  "caset_empty" >:: (fun _ -> assert_equal                 
                 (Ok (TreeVal Empty))
                 (interp "caseT emptytree of { emptytree -> emptytree, node(a, l, r) -> l } "));

  "caset_empty_other_types" >:: (fun _ -> assert_equal                 
                 (Ok (NumVal 3))
                 (interp "caseT emptytree of { emptytree -> 3, node(a, l, r) -> l } "));

  "caset_left" >:: (fun _ -> assert_equal                 
                 (Ok (TreeVal (Node (NumVal 5, Empty, Empty))))
                 (interp "let t = node(emptylist, node(5, emptytree, emptytree), emptytree) in 
                    caseT t of { emptytree -> emptytree, node(a, l, r) -> l } "));
  "caset_right" >:: (fun _ -> assert_equal                 
                 (Ok (TreeVal (Node (NumVal 3, Empty, Empty))))
                 (interp "let t = node(emptylist, 
                    node(5, emptytree, emptytree), 
                    node(3, emptytree, emptytree)) in 
                    caseT t of { emptytree -> emptytree, node(a, l, r) -> r } "));
  "caset_using_list_as_key" >:: (fun _ -> assert_equal                 
                 (Ok (TreeVal (Node (ListVal [NumVal 5], Empty, Empty))))
                 (interp "let t = node(1,
                    node(cons(5, emptylist), emptytree, emptytree), 
                    node(3, emptytree, emptytree)) in 
                    caseT t of { emptytree -> emptytree, node(a, l, r) -> l } "));
  "caset_using_if" >:: (fun _ -> assert_equal                 
                 (Ok (TreeVal (Node (ListVal [NumVal 5], Empty, Empty))))
                 (interp "let t = node(emptylist,
                    node(cons(5, emptylist), emptytree, emptytree), 
                    node(3, emptytree, emptytree)) in 
                    caseT t of { emptytree -> emptytree, 
                    node(a, l, r) -> if empty?(a) then l else r } "));
  "caset_using_hw03" >:: (fun _ -> assert_equal                 
                 (Ok (NumVal 99))
                 (interp "let t = node(emptylist , node(cons(5, cons(2, cons(1, emptylist ))), emptytree , node(emptylist, emptytree , emptytree ) ), node(tl(cons(5, emptylist )), node(cons (10, cons(9, cons(8, emptylist ))), emptytree , emptytree), node(emptylist , node(cons(9, emptylist), emptytree , emptytree), emptytree)))
                          in caseT t of { 
                            emptytree -> 10,
                            node(a,l,r) -> 
                              if empty?(a) 
                                then caseT l of 
                                  { 
                                      emptytree -> 21, 
                                      node(b,ll ,rr) -> 
                                        if empty?(b) 
                                          then 4 
                                          else 
                                            if zero?(hd(b)) 
                                            then 22 
                                            else  99
                                  }
                                else 5
                            }"));

  "null_true" >:: (fun _ -> assert_equal                 
                 (Ok (BoolVal true))
                 (interp "empty?(tl(cons(1, emptylist)))"));

  "null_false" >:: (fun _ -> assert_equal               
                 (Ok (BoolVal false))
                 (interp "empty?(tl(cons(2, cons(1, emptylist))))"));
]

let _ = run_test_tt_main ("suite" >::: (tests_let @ tests_proc @ tests_extensions))
