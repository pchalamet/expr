module AbstractSyntaxTree
 
type Function =
    | Plus
    | Minus
    | Times
    | Divide

type Expression = 
  | Number of float 
  | Symbol of string
  | Field of string * string
  | Function of Function * Expression * Expression 
