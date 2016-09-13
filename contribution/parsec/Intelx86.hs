module Intelx86 where


data SizeSpec = Byte | Word | Dword
              deriving ( Show )

data Register = Eax | Ax | Ah | Al
              | Ebx | Bx | Bh | Bl
              | Ecx | Cx | Ch | Cl
              | Edx | Dx | Dh | Dl
              | Ebp | Bp
              | Esi | Si
              | Edi | Di
              | Esp | Sp
              deriving ( Show, Enum, Read )

data Operand = Constant Integer
             | Memory SizeSpec Operand Operand
             | Register Register
             deriving ( Show )

data Instruction = Mov  Operand Operand
                 | Add  Operand Operand
                 | Or   Operand Operand
                 | Xor  Operand Operand
                 | And  Operand Operand
                 | Push Operand
                 | Pop  Operand
                 deriving ( Show )
          