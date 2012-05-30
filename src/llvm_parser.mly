%{

  let label_of_ident = function
  | LLVM.Id_Global v -> Prim.label ("@" ^ Prim.string_of_var v)
  | LLVM.Id_Local  v -> Prim.label ("%" ^ Prim.string_of_var v)

%}

%token EOL
%token <int> NUMBER

%token <string> GLOBAL LOCAL
%token LPAREN LCURLY LSQUARE RSQUARE RCURLY RPAREN
%token STAR EQ COMMA

%token DEFINE

%token <int> I
%token VOID TLABEL

%token LOAD STORE BR RET ALLOCA ALIGN PHI ADD
%token <string> LABEL

%start <LLVM.prog> prog

%%

prog:
  | m = separated_nonempty_list(EOL+,proc) {m}

proc:
  | DEFINE
      ret_typ = typ name = ident LPAREN args=separated_list(COMMA, arg) RPAREN RCURLY EOL
      instrs = separated_list(EOL+,instr) EOL
      LCURLY
      { let name = label_of_ident name in
        {LLVM. ret_typ; name; args; instrs; }
      }

typ:
  | i = I { LLVM.Typ_I i }
  | VOID { LLVM.Typ_Void }
  | TLABEL { LLVM.Typ_Label }
  | t = typ STAR { LLVM.Typ_Pointer t }

ident:
  | g = GLOBAL { LLVM.Id_Global (Prim.var g) }
  | l = LOCAL { LLVM.Id_Local (Prim.var l) }

arg:
  | t = typ i = ident { (t,i) }

phi_assoc:
  | v = value COMMA l = ident { (v, label_of_ident l) }

instr:
  | l = LABEL { LLVM.Label (Prim.label ("%" ^ l)) }
  (* When in doubt, try another hack!   ^^^^^^^ *)
  | i = ident EQ PHI t = typ assocs = separated_list(COMMA, phi_assoc)
    {LLVM.Phi (i, t, assocs) }
  | i = ident EQ ALLOCA t = typ COMMA ALIGN NUMBER { LLVM.Alloca (i, t) }
  | i = ident EQ ADD t = typ v1 = value COMMA v2 = value
    {LLVM.Add (i, t, v1, v2) }
  | STORE tv = typ v = value COMMA ti = typ i = ident
    { LLVM.Store ((ti, i), (tv, v)) }
  | RET t = typ v = value {LLVM.Ret (t,v)}
  | BR TLABEL i = ident {LLVM.Br_1 (label_of_ident i)}
  | BR t = typ v = value COMMA TLABEL vt = ident COMMA TLABEL vf = ident
    { assert (t = LLVM.Typ_I 1);
      LLVM.Br (v, label_of_ident vt, label_of_ident vf)
    }

value:
  | i = ident { LLVM.Vvar i }
  | n = NUMBER { LLVM.Vconst n }

