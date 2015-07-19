

module Language.Coq.Syntax where

type Identifier = String

-- Annotation --------------------------------------------------
data Annotation = Struct Identifier
                deriving (Eq,Read,Show)
-- Assertion ---------------------------------------------------
data Assertion = Assertion AssertionKeyword Identifier [Binder] Term
               deriving (Eq,Read,Show)
-- AssertionKeyword --------------------------------------------
data AssertionKeyword = AssTheorem
                      | AssLemma
                      | AssRemark
                      | AssFact
                      | AssCorollary
                      | AssProposition
                      | AssDefinition
                      | AssExample
                      deriving (Eq,Read,Show)
-- Binder ------------------------------------------------------
data Binder = BinderName Name
            | BinderNameType [Name] Term
            deriving (Eq,Read,Show)
-- Definition --------------------------------------------------
data Definition = Definition Identifier [Binder] (Maybe Term) Term
                deriving (Eq,Read,Show)
-- Equation ----------------------------------------------------
data Equation = Equation Pattern Term
              deriving (Eq,Read,Show)
-- Fixpoint ----------------------------------------------------
data Fixpoint = Fixpoint [FixpointBody]
              deriving (Eq,Read,Show)
-- FixpointBody ------------------------------------------------
data FixpointBody = FixpointBody Identifier [Binder] (Maybe Annotation) Term Term
                  deriving (Eq,Read,Show)
-- Inductive ---------------------------------------------------
data Inductive = Inductive [InductiveBody]
               deriving (Eq,Read,Show)
-- InductiveBody -----------------------------------------------
data InductiveBody = InductiveBody Identifier [Binder] Term [InductiveCtor]
                   deriving (Eq,Read,Show)
-- InductiveCtor -----------------------------------------------
data InductiveCtor = InductiveCtor Identifier [Binder] (Maybe Term)
                   deriving (Eq,Read,Show)
-- MatchItem ---------------------------------------------------
data MatchItem = MatchItem Term (Maybe Name) (Maybe Term)
               deriving (Eq,Read,Show)
-- Name --------------------------------------------------------
data Name = NameIdent Identifier
          | NameUnderscore
          deriving (Eq,Read,Show)
-- Pattern -----------------------------------------------------
data Pattern = PatCtor QualId [Identifier]
             deriving (Eq,Read,Show)
-- Proof -------------------------------------------------------
data Proof = ProofWith [ProofStep] [ProofStep]
           | ProofDefined [ProofStep]
           | ProofQed [ProofStep]
           deriving (Eq,Read,Show)
-- ProofStep ---------------------------------------------------
data ProofStep = PrHintResolve Identifier
               | PrInduction Identifier
               | PrCrushInd
               | PrApply Term
               | PrSeq [ProofStep]
               | PrIntros [Identifier]
               | PrTry ProofStep
               | PrConstructor
               | PrAuto
               | PrFail
               | PrInversion Identifier
               | PrSubst
               | PrSimpl
               | PrRepeat ProofStep
               | PrRewrite Term
               | PrRewriter
               | PrEasy
               | PrTactic Identifier [Term]
               | PrPoseProof Term
               | PrPoseProofAs Term Identifier
               | PrBullet Int [ProofStep]
               | PrDestruct Term
               deriving (Eq,Read,Show)
-- QualId ------------------------------------------------------
data QualId = Ident Identifier
            deriving (Eq,Read,Show)
-- Root --------------------------------------------------------
data Root = Root [Sentence]
          deriving (Eq,Read,Show)
-- Scheme ------------------------------------------------------
data Scheme = Scheme [SchemeBody]
            deriving (Eq,Read,Show)
-- SchemeBody --------------------------------------------------
data SchemeBody = SchemeInduction Identifier Identifier
                deriving (Eq,Read,Show)
-- Sentence ----------------------------------------------------
data Sentence = SentenceDefinition Definition
              | SentenceInductive Inductive
              | SentenceFixpoint Fixpoint
              | SentenceAssertionProof Assertion Proof
              | SentenceSection Identifier [Sentence]
              | SentenceOpaque Identifier
              | SentenceHintResolve Identifier
              | SentenceHintRewrite Term
              | SentenceScheme Scheme
              | SentenceCombinedScheme Identifier [Identifier]
              deriving (Eq,Read,Show)
-- Sort --------------------------------------------------------
data Sort = Prop
          | Set
          | Type
          deriving (Eq,Read,Show)
-- Term --------------------------------------------------------
data Term = TermApp Term [Term]
          | TermNum Int
          | TermQualId QualId
          | TermSort Sort
          | TermFunction Term Term
          | TermForall [Binder] Term
          | TermAnd [Term]
          | TermEq Term Term
          | TermMatch MatchItem (Maybe Term) [Equation]
          deriving (Eq,Read,Show)
