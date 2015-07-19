
import Language.Coq.Pretty
import Language.Coq.Syntax

listOf :: Term -> Term
listOf t = TermApp (TermQualId (Ident "list")) [t]

coqFoldr :: Sentence
coqFoldr =
  let a  = TermQualId (Ident "A")
      b  = TermQualId (Ident "B")
      c  = TermQualId (Ident "c")
      f  = TermQualId (Ident "foldr")
      n  = TermQualId (Ident "n")
      x  = TermQualId (Ident "x")
      xs = TermQualId (Ident "xs")
  in
    SentenceFixpoint . Fixpoint $
      [ FixpointBody "foldr"
          [BinderNameType [NameIdent "A", NameIdent "B"] (TermSort Type),
           BinderNameType [NameIdent "c"] (TermFunction a $ TermFunction b b),
           BinderNameType [NameIdent "n"] b,
           BinderNameType [NameIdent "xs"] (listOf a)
          ]
          (Just (Struct "xs"))
          b
          (TermMatch
            (MatchItem xs Nothing Nothing)
            Nothing
            [ Equation
                (PatCtor (Ident "nil") []) n,
              Equation
                (PatCtor (Ident "cons") ["x","xs"])
                (TermApp c [x,TermApp f [a,b,c,n,xs]])
            ])
      ]

main :: IO ()
main = putDoc (pretty $ Root [coqFoldr])
