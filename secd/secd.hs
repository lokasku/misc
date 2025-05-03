data Value
  = VAL_NIL
  | VAL_NUM Integer
  | VAL_BOOL Bool
  | VAL_PAIR Value Value
  | CLOSURE String [Instruction] Map
  deriving (Show, Eq)

data Instruction
  = OP_NIL
  | OP_LD String
  | OP_LDC Value
  | OP_LAM String [Instruction]
  | OP_LAB String [Instruction]
  | OP_REC String [Instruction]
  | OP_AP
  | OP_RET
  | OP_SEL [Instruction] [Instruction]
  | OP_CAR
  | OP_CDR
  | OP_CONS
  | OP_ATOM
  | OP_ADD
  | OP_MUL
  | OP_EQ
  | OP_STOP
  deriving (Show, Eq)

type Program = [Instruction]

type Map = [(String, Value)]

data SECD
  = SECD
      { stack :: [Value],
        env :: Map,
        code :: Program,
        dump :: SECD
      }
  | Empty
  deriving (Show, Eq)

proc :: SECD -> SECD
proc secd@SECD {stack, env, code = x : xs, dump} = case x of
  OP_NIL -> secd {stack = VAL_NIL : stack, code = xs}
  OP_LD name -> case lookup name env of
    Just val -> secd {stack = val : stack, code = xs}
    Nothing -> error $ "Undefined variable: " ++ name
  OP_LDC c -> secd {stack = c : stack, code = xs}
  OP_LAM name body ->
    let closure = CLOSURE name body env
     in secd {stack = closure : stack, code = xs}
  OP_AP -> case stack of
    (fun : arg : s') -> case fun of
      CLOSURE name body env' ->
        secd
          { stack = [],
            env = (name, arg) : env',
            code = body,
            dump = secd {stack = s', code = xs}
          }
      _ -> error "Invalid function application : Not a closure"
    _ -> error "Invalid stack for OP_AP"
  OP_RET -> case dump of
    Empty -> error "Empty dump"
    SECD {stack = s', env = e', code = cs', dump = d'} ->
      case stack of
        (res : _) -> SECD {stack = res : s', env = e', code = cs', dump = d'}
        [] -> error "Empty stack for OP_RET"
  OP_LAB name body ->
    let closure = CLOSURE name body ((name, closure) : env)
    in secd {env = (name, closure) : env, code = xs}
  OP_REC name body ->
    let recClosure = CLOSURE name body ((name, recClosure) : env)
    in secd {stack = recClosure : stack, code = xs}
  OP_SEL if_case else_case ->
    let (pred : r) = stack
     in case pred of
          VAL_BOOL bool ->
            let new_code = (if bool then if_case else else_case) ++ xs
             in secd {stack = r, code = new_code}
          _ -> error "OP_SEL expected a boolean"
  OP_CONS -> case stack of
    (x : y : rest) ->
      let pair = VAL_PAIR x y
       in secd {stack = pair : rest, code = xs}
    _ -> error "OP_CONS expected at least two arguments"
  OP_CAR -> case stack of
    (h : t) -> case h of
      VAL_PAIR car _ -> secd {stack = car : t, code = xs}
      _ -> error "OP_CAR exepected PAIR"
    _ -> error "OP_CAR expected one argument"
  OP_CDR -> case stack of
    (h : t) -> case h of
      VAL_PAIR _ cdr -> secd {stack = cdr : t, code = xs}
      _ -> error "OP_CDR expected PAIR"
    _ -> error "OP_CDR expected one argument"
  OP_ATOM -> case stack of
    (h : t) -> case h of
      VAL_PAIR _ _ -> secd {stack = VAL_BOOL False : t, code = xs}
      _ -> secd {stack = VAL_BOOL True : t, code = xs}
    _ -> error "OP_ATOM expected one argument"
  OP_EQ -> case stack of
    (lhs : rhs : t) ->
      if (lhs == rhs)
        then secd {stack = VAL_BOOL True : t, code = xs}
        else secd {stack = VAL_BOOL False : t, code = xs}
    _ -> error "OP_EQ expected two arguments"
  OP_ADD -> case stack of
    (VAL_NUM x : VAL_NUM y : t) -> secd {stack = VAL_NUM (x + y) : t, code = xs}
    _ -> error "OP_ADD expected two integers"
  OP_MUL -> case stack of
    (VAL_NUM x : VAL_NUM y : t) -> secd {stack = VAL_NUM (x * y) : t, code = xs}
    _ -> error "OP_MUL expected two integers"
  _ -> secd {code = xs}

procAll :: SECD -> IO SECD
procAll Empty = return Empty
procAll secd@SECD {code = x : xs} =
  if x == OP_STOP
    then do return secd
    else procAll . proc $ secd

empty code =
  SECD
    { stack = [],
      env = [],
      code,
      dump = Empty
    }

var =
  let code =
        [ OP_LAB "x" [OP_LDC $ VAL_NUM 6],
          OP_LD "x",
          OP_STOP
        ]
   in empty code

cond =
  let code =
        [ OP_LDC $ VAL_BOOL True,
          OP_SEL [OP_LDC $ VAL_NUM 1] [OP_LDC $ VAL_NUM 2],
          OP_STOP
        ]
   in empty code

id' =
  let code =
        [ OP_LAB "id" [OP_LAM "x" [OP_LD "x", OP_RET]],
          OP_LDC $ VAL_BOOL True,
          OP_LD "id",
          OP_AP,
          OP_STOP
        ]
   in empty code

-- fact :: SECD
-- fact =
--     empty
--     [ OP_LAB
--         "fact"
--         [ OP_REC
--             "n"
--             [ OP_LD "n",
--                 OP_LDC (VAL_NUM 0),
--                 OP_EQ,
--                 OP_SEL
--                 [OP_LDC (VAL_NUM 1)]
--                 [ OP_LD "n",
--                     OP_LD "n",
--                     OP_LDC (VAL_NUM (-1)),
--                     OP_ADD,
--                     OP_LD "fact",
--                     OP_AP,
--                     OP_MUL
--                 ],
--                 OP_RET
--             ]
--         ],
--         OP_LDC (VAL_NUM 5),
--         OP_LD "fact",
--         OP_AP,
--         OP_STOP
--     ]

fact :: SECD
fact =
  empty
    [ OP_LAB
        "fact"
        [ OP_LD "n",
          OP_LDC (VAL_NUM 0),
          OP_EQ,
          OP_SEL
            [OP_LDC (VAL_NUM 1)]
            [ OP_LD "n",
              OP_LD "n",
              OP_LDC (VAL_NUM (-1)),
              OP_ADD,
              OP_LD "fact",
              OP_AP,
              OP_MUL
            ],
          OP_RET
        ],
      OP_LDC (VAL_NUM 5),
      OP_LD "fact",
      OP_AP,
      OP_STOP
    ]

cons =
  let code =
        [ OP_LDC VAL_NIL,
          OP_LDC $ VAL_NUM 1,
          OP_CONS,
          OP_LDC $ VAL_NUM 2,
          OP_CONS,
          OP_LDC $ VAL_NUM 3,
          OP_CONS,
          OP_STOP
        ]
   in empty code

add =
  let code =
        [ OP_LDC $ VAL_NUM 2,
          OP_LDC $ VAL_NUM 2,
          OP_ADD,
          OP_STOP
        ]
   in empty code

main :: IO SECD
main = procAll $ fact
