type expr =
    True
  | False
  | And of expr * expr
  | Or of expr * expr
  | Not of expr
  | If of expr * expr * expr
  | Zero
  | Succ of expr
  | Pred of expr
  | IsZero of expr

  type exprval = 
  | Bool of bool
  | Nat of int