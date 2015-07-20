{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Language.Coq.Pretty (
  module Language.Coq.Pretty,
  module Text.PrettyPrint.ANSI.Leijen
  ) where

import Prelude hiding ((<$>))

import Language.Coq.Syntax

import Text.PrettyPrint.ANSI.Leijen hiding (Pretty,pretty,prettyList)

class Pretty a where
  -- | Pretty-print something in isolation.
  pretty :: a -> Doc
  -- | Pretty-print something in a precedence context.
  prettyPrec :: Int -> a -> Doc
  pretty = prettyPrec 0
  prettyPrec _ = pretty
  prettyList :: [a] -> [Doc]
  prettyList = map pretty

instance Pretty Identifier where
  pretty = text

instance Pretty Root where
  pretty (Root ss) =
    vsep (prettyList ss) <$> empty

instance Pretty Sentence where
  pretty (SentenceDefinition def) = pretty def
  pretty (SentenceInductive ind) = pretty ind
  pretty (SentenceFixpoint fix) = pretty fix
  pretty (SentenceAssertionProof ass prf) =
    pretty ass <$> pretty prf
  pretty (SentenceSection id phrs) =
    vsep
      [ text "Section" <+> pretty id <> char '.',
        indent 2 (vsep $ prettyList phrs),
        text "End"  <+> pretty id <> char '.'
      ]
  pretty (SentenceOpaque id) =
    text "Opaque" <+> pretty id <> char '.'
  pretty (SentenceHintResolve id) =
    text "Hint Resolve" <+> pretty id <> char '.'
  pretty (SentenceHintRewrite tm) =
    text "Hint Rewrite" <+> pretty tm <> char '.'
  pretty (SentenceScheme scheme) = pretty scheme
  pretty (SentenceCombinedScheme id ids) =
    hsep
      ([ text "Combined Scheme",
        pretty id,
        text "from"
       ] ++ punctuate comma (prettyList ids))
     <> char '.'

prettyType :: Maybe Term -> [Doc]
prettyType Nothing   = []
prettyType (Just tm) = [colon, pretty tm]

prettyReturnType :: Maybe Term -> [Doc]
prettyReturnType Nothing   = []
prettyReturnType (Just tm) = [text "return", pretty tm]

prettyMbAnnotation :: Maybe Annotation -> [Doc]
prettyMbAnnotation Nothing   = []
prettyMbAnnotation (Just tm) = [pretty tm]

instance Pretty Annotation where
  pretty (Struct id) = braces (text "struct" <+> pretty id)

instance Pretty Definition where
  pretty (Definition id bds ty tm) =
    hsep
      ([ text "Definition", pretty id] ++
       prettyList bds ++ prettyType ty ++
       [ text ":=", pretty tm]) <> char '.'


instance Pretty Scheme where
  pretty (Scheme bodies) =
    vsep
      (zipWith (<+>)
         (text "Scheme" : repeat (text "with"))
         (prettyList bodies))
    <> char '.'

instance Pretty SchemeBody where
  pretty (SchemeInduction id ty) =
    hsep
    [ pretty id,
      text ":= Induction for",
      pretty ty,
      text "Sort",
      text "Prop"
    ]

instance Pretty Inductive where
  pretty (Inductive bodies) =
    vsep
      (zipWith (<+>)
         (text "Inductive" : repeat (text "with"))
         (prettyList bodies))
    <> char '.'

instance Pretty InductiveBody where
  pretty (InductiveBody id bds ty ctors) =
    hsep
      ([ pretty id ] ++
       prettyList bds ++
       [ char ':'
       , pretty ty
       , text ":="
       ]) <$>
    indent 2 (vsep $ prettyList ctors)

instance Pretty InductiveCtor where
  pretty (InductiveCtor id bds ty) =
    case ty of
      Just ty ->
        vsep [char '|' <+> pretty id,
              indent 4 (hsep (prettyList bds) <+> colon),
              indent 4 (pretty ty)
             ]
      Nothing ->
        hang 2 (char '|' <+> pretty id <+> hsep (prettyList bds))

instance Pretty Fixpoint where
  pretty (Fixpoint bodies) =
    vsep
      (zipWith (<+>)
         (text "Fixpoint" : repeat (text "with"))
         (prettyList bodies))
    <> char '.'

instance Pretty FixpointBody where
  pretty (FixpointBody id bds mbAnno ty tm) =
    hsep
      ([ pretty id ] ++
       prettyList bds ++
       prettyMbAnnotation mbAnno ++
       [ char ':'
       , pretty ty
       , text ":="
       ]) <$>
    indent 2 (pretty tm)

instance Pretty Binder where
  pretty (BinderName nm) = pretty nm
  pretty (BinderNameType nms tm) =
    parens . hsep $ [hsep $ prettyList nms, colon, pretty tm]

instance Pretty Name where
  pretty (NameIdent id) = pretty id
  pretty (NameUnderscore) = char '_'

instance Pretty QualId where
  pretty (Ident id) = pretty id

instance Pretty Term where
  pretty (TermApp fun args) =
    parens $ foldl (<+>) (pretty fun) (prettyList args)
  pretty (TermNum num) = text (show num)
  pretty (TermQualId id) = pretty id
  pretty (TermSort srt) = pretty srt
  pretty (TermMatch item mbTy eqns) =
    vsep
      [ hsep
          ([ text "match"
           , pretty item ] ++
           prettyReturnType mbTy ++
           [ text "with" ])
      , indent 2 (vsep $ prettyList eqns)
      , text "end"
      ]
  pretty (TermFunction source target) =
    pretty source <+> text "->" <+> pretty target
  pretty (TermForall bds tm) = parens $
    hsep ([ text "forall" ] ++ prettyList bds ++ [ char ',' ]) <$>
    indent 2 (pretty tm)
  pretty (TermAnd tms) = vsep (punctuate (text " /\\ ") (prettyList tms))
  pretty (TermEq l r)  = pretty l <+> char '=' <$> pretty r

instance Pretty MatchItem where
  pretty (MatchItem tm mbName mbIn) =
     hsep [ pretty tm ] -- TODO

instance Pretty Equation where
  pretty (Equation pat body) =
    hsep [text "|", pretty pat, text "=>", pretty body]

instance Pretty Pattern where
  pretty (PatCtor id fields) =
    hsep (pretty id : prettyList fields)

instance Pretty Sort where
  pretty (Prop) = text "Prop"
  pretty (Set)  = text "Set"
  pretty (Type) = text "Type"

instance Pretty Assertion where
  pretty (Assertion kw id bds ty) =
    vsep
      [ hsep
          [ pretty kw,
            pretty id,
            hsep $ prettyList bds,
            colon
          ],
        indent 2 (pretty ty) <> char '.'
      ]

instance Pretty AssertionKeyword where
  pretty AssTheorem     = text "Theorem"
  pretty AssLemma       = text "Lemma"
  pretty AssRemark      = text "Remark"
  pretty AssFact        = text "Fact"
  pretty AssCorollary   = text "Corollary"
  pretty AssProposition = text "Proposition"
  pretty AssDefinition  = text "Definition"
  pretty AssExample     = text "Example"

instance Pretty Proof where
  pretty (ProofWith with steps) =
    vsep
      [ text "Proof with" <+> hsep (punctuate (text "; ") (prettyList with)) <+> char '.',
        indent 2 . vsep $ map (<> char '.') (prettyList steps),
        text "Defined."
      ]
  pretty (ProofDefined steps) =
    vsep
      [ text "Proof.",
        indent 2 . vsep $ map (<> char '.') (prettyList steps),
        text "Defined."
      ]
  pretty (ProofQed steps) =
    vsep
      [ text "Proof.",
        indent 2 . vsep $ map (<> char '.') (prettyList steps),
        text "Qed."
      ]

instance Pretty ProofStep where
  pretty (PrHintResolve id)       = text "Hint Resolve" <+> pretty id
  pretty (PrInduction id)         = text "induction" <+> pretty id
  pretty (PrCrushInd)             = text "crush_ind"
  pretty (PrApply tm)             = text "apply" <+> parens (pretty tm)
  pretty (PrSeq steps)            = hsep (punctuate (char ';') (prettyList steps))
  pretty (PrIntros ids)           = hsep (text "intros":map pretty ids)
  pretty (PrTry step)             = text "try" <+> parens (pretty step)
  pretty (PrConstructor)          = text "constructor"
  pretty (PrAuto)                 = text "auto"
  pretty (PrFail)                 = text "fail"
  pretty (PrInversion id)         = text "inversion" <+> pretty id
  pretty (PrSubst)                = text "subst"
  pretty (PrSimpl)                = text "simpl"
  pretty (PrRepeat step)          = text "repeat" <+> parens (pretty step)
  pretty (PrRewrite tm)           = text "rewrite" <+> parens (pretty tm)
  pretty (PrRewriter)             = text "rewriter"
  pretty (PrEasy)                 = text "easy"
  pretty (PrTactic s ts)          = text s <+> hsep (map (parens . pretty) ts)
  pretty (PrPoseProof tm)         = text "pose proof" <+> parens (pretty tm)
  pretty (PrPoseProofAs tm id)    = hsep
                                      [ text "pose proof",
                                        parens (pretty tm),
                                        text "as",
                                        pretty id
                                      ]
  pretty (PrBullet lvl steps) = char c <+>
                                    vsep (punctuate (char '.')
                                            (prettyList steps))
    where c                   = case lvl of
                0 -> '-'
                1 -> '+'
                2 -> '*'
  pretty (PrDestruct tm) = text "destruct" <+> parens (pretty tm)
