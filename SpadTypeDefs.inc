  -- vim:ft=axiom:

  I ==> Integer
  DF ==> DoubleFloat
  NNI ==> NonNegativeInteger
  PI ==> PositiveInteger
  SEX ==> SExpression

  PF ==> PrintableForm

  N ==> SpadNode
  APP ==> SpadApply(N)
  ASS ==> SpadAssign(N)
  CE ==> SpadCondExpr(N)
  SC ==> SpadCase(N)
  FN ==> SpadFunction(N)
  FT ==> SpadFunctor(N)
  IM ==> SpadImport(N)
  IT ==> SpadIterator(N)
  LP ==> SpadLoop(N)
  MP ==> SpadMarkerPath
  MR ==> SpadMarker(N)
  MT ==> SpadMappingType(N)
  SM ==> SpadMacro(N)
  NR ==> SpadNodeRef(N)
  RT ==> SpadRecordType(N)
  SEG ==> SpadSegment(N)
  SEQ ==> SpadSequence(N)
  ST ==> SpadSubType(N)
  TC ==> SpadTypeCoerce(N)
  TD ==> SpadTypeDecl(N)
  TEH ==> SpadTypeHas(N)
  TEI ==> SpadTypeIs(N)
  TEP ==> SpadTypePretend(N)
  TVL ==> SpadTypeValue(N)
  TI ==> SpadTypeInfo(N)
  TG ==> SpadTypeGuard(N)
  TO ==> SpadTypeOrigin(N)
  TS ==> SpadTypeSelect(N)
  TV ==> SpadTypeVar(N)
  UT ==> SpadUnionType(N)
  SW ==> SpadWhere(N)

  SUBS ==> AssociationList(TV, List(N))
  URES ==> Union(SUBS, "failed")

  ENV ==> SpadEnvironment
  EKEY ==> Union(Symbol, APP)
  EVAL ==> Record(types : List(N), info : N, facts : List(N))

  TN ==> SpadTypeNode
  TNA ==> SpadTypeNodeArray(CTX)
  TR ==> SpadTypeRule
  CTX ==> SpadTypeTree
