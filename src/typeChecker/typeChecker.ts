import * as es from 'estree'
import {
  Context,
  TypeAnnotatedNode,
  Primitive,
  Variable,
  Pair,
  List,
  ForAll,
  Type,
  FunctionType,
  SourceError,
  TypeEnvironment
} from '../types'
import {
  TypeError,
  InternalTypeError,
  UnifyError,
  InternalDifferentNumberArgumentsError,
  InternalCyclicReferenceError
} from './internalTypeErrors'
import { InvalidArgumentTypesError, CyclicReferenceError } from '../errors/typeErrors'

/** Name of Unary negative builtin operator */
const NEGATIVE_OP = '-_1'
let typeIdCounter = 0

/**
 * Called before and after type inference. First to add typeVar attribute to node, second to resolve
 * the type
 * FunctionDeclaration nodes have the functionTypeVar attribute as well
 * @param node
 * @param constraints: undefined for first call
 */
/* tslint:disable cyclomatic-complexity */
function traverse(node: TypeAnnotatedNode<es.Node>, constraints?: Constraint[]) {
  if (node === null) {
    // this happens in a holey array [,,,,,]
    return
  }
  if (constraints && node.typability !== 'Untypable') {
    try {
      node.inferredType = applyConstraints(node.inferredType as Type, constraints)
      node.typability = 'Typed'
    } catch (e) {
      if (isInternalTypeError(e) && !(e instanceof InternalCyclicReferenceError)) {
        typeErrors.push(new TypeError(node, e))
      }
    }
  } else {
    node.inferredType = tVar(typeIdCounter)
    typeIdCounter++
  }
  switch (node.type) {
    case 'Program': {
      node.body.forEach(nodeBody => {
        traverse(nodeBody, constraints)
      })
      break
    }
    case 'UnaryExpression': {
      traverse(node.argument, constraints)
      break
    }
    case 'LogicalExpression': // both cases are the same
    case 'BinaryExpression': {
      traverse(node.left, constraints)
      traverse(node.right, constraints)
      break
    }
    case 'ExpressionStatement': {
      traverse(node.expression, constraints)
      break
    }
    case 'BlockStatement':
      throw Error('Block statements not supported for x-slang')
    case 'WhileStatement':
      throw Error('While statements not supported for x-slang')
    case 'ForStatement':
      throw Error('For statements not supported for x-slang')
    case 'ConditionalExpression': // both cases are the same
    case 'IfStatement':
      throw Error('If statements not supported for x-slang')
    case 'CallExpression':
      throw Error('Call statements not supported for x-slang')
    case 'ReturnStatement':
      throw Error('Return statements not supported for x-slang')
    case 'VariableDeclaration':
      throw Error('Variable statements not supported for x-slang')
    case 'ArrowFunctionExpression':
      throw Error('Arrow functions not supported for x-slang')
    case 'FunctionDeclaration':
      throw Error('Function declarations  not supported for x-slang')
    case 'AssignmentExpression':
      throw Error('Assignments expressions not supported for x-slang')
    case 'ArrayExpression':
      throw Error('Array expressions not supported for x-slang')
    case 'MemberExpression':
      throw Error('Member expressions not supported for x-slang')
    default:
      return
  }
}

function isPair(type: Type): type is Pair {
  return type.kind === 'pair'
}

function isList(type: Type): type is List {
  return type.kind === 'list'
}

function getListType(type: Type): Type | null {
  if (isList(type)) {
    return type.elementType
  }
  return null
}

function isInternalTypeError(error: any) {
  return error instanceof InternalTypeError
}

// Type Definitions
// Our type environment maps variable names to types.
// it also remembers if names weer declared as const or let
type Env = TypeEnvironment

type Constraint = [Variable, Type]
let typeErrors: SourceError[] = []
/**
 * An additional layer of typechecking to be done right after parsing.
 * @param program Parsed Program
 */
export function typeCheck(
  program: TypeAnnotatedNode<es.Program>,
  context: Context
): [TypeAnnotatedNode<es.Program>, SourceError[]] {
  typeIdCounter = 0
  typeErrors = []
  const env: Env = context.typeEnvironment
  const constraints: Constraint[] = []
  traverse(program)
  try {
    infer(program, env, constraints, true)
  } catch {
    // we ignore the errors here since
    // they would have already been processed
  }
  traverse(program, constraints)
  return [program, typeErrors]
}

/**
 * Generate a fresh type variable
 * @param typeVar
 */
function freshTypeVar(typeVar: Variable): Variable {
  const newVarId = typeIdCounter
  typeIdCounter++
  return {
    ...typeVar,
    name: `T${newVarId}`
  }
}

/**
 * Replaces all instances of type variables in the type of a polymorphic type
 */
function fresh(monoType: Type, subst: { [typeName: string]: Variable }): Type {
  switch (monoType.kind) {
    case 'primitive':
      return monoType
    case 'list':
      return {
        kind: 'list',
        elementType: fresh(monoType.elementType, subst)
      }
    case 'array':
      return {
        kind: 'array',
        elementType: fresh(monoType.elementType, subst)
      }
    case 'pair':
      return {
        kind: 'pair',
        headType: fresh(monoType.headType, subst),
        tailType: fresh(monoType.tailType, subst)
      }
    case 'variable':
      return subst[monoType.name]
    case 'function':
      return {
        ...monoType,
        parameterTypes: monoType.parameterTypes.map(argType => fresh(argType, subst)),
        returnType: fresh(monoType.returnType, subst)
      }
  }
}

/** Union of free type variables */
function union(a: Variable[], b: Variable[]): Variable[] {
  const sum = [...a]
  b.forEach(newVal => {
    if (sum.findIndex(val => val.name === newVal.name) === -1) {
      sum.push(newVal)
    }
  })
  return sum
}

function freeTypeVarsInType(type: Type): Variable[] {
  switch (type.kind) {
    case 'primitive':
      return []
    case 'list':
      return freeTypeVarsInType(type.elementType)
    case 'array':
      return freeTypeVarsInType(type.elementType)
    case 'pair':
      return union(freeTypeVarsInType(type.headType), freeTypeVarsInType(type.tailType))
    case 'variable':
      return [type]
    case 'function':
      return union(
        type.parameterTypes.reduce((acc, currentType) => {
          return union(acc, freeTypeVarsInType(currentType))
        }, []),
        freeTypeVarsInType(type.returnType)
      )
  }
}

function extractFreeVariablesAndGenFresh(polyType: ForAll): Type {
  const monoType = polyType.polyType
  const freeTypeVars = freeTypeVarsInType(monoType)
  const substitutions = {}
  freeTypeVars.forEach(val => {
    substitutions[val.name] = freshTypeVar(val)
  })
  return fresh(monoType, substitutions)
}

/**
 * Going down the DAG that is the constraint list
 */
function applyConstraints(type: Type, constraints: Constraint[]): Type {
  switch (type.kind) {
    case 'primitive': {
      return type
    }
    case 'pair': {
      const pairHeadType = applyConstraints(type.headType, constraints)
      const pairTailType = applyConstraints(type.tailType, constraints)
      if (pairTailType.kind === 'list' && pairHeadType === getListType(pairTailType)) {
        return tList(pairHeadType)
      } else {
        return tPair(pairHeadType, pairTailType)
      }
    }
    case 'list': {
      const elementType = applyConstraints(type.elementType, constraints)
      return {
        kind: 'list',
        elementType
      }
    }
    case 'array': {
      const elementType = applyConstraints(type.elementType, constraints)
      return {
        kind: 'array',
        elementType
      }
    }
    case 'variable': {
      for (const constraint of constraints) {
        const LHS = constraint[0]
        const RHS = constraint[1]
        if (LHS.name === type.name) {
          if (contains(RHS, LHS.name)) {
            if (isPair(RHS) && LHS === RHS.tailType) {
              return {
                kind: 'list',
                elementType: RHS.headType
              }
            } else if (LHS.kind === 'variable' && LHS === getListType(RHS)) {
              return {
                kind: 'list',
                elementType: LHS
              }
            }
            throw new InternalCyclicReferenceError(type.name)
          }
          return applyConstraints(constraint[1], constraints)
        }
      }
      return type
    }
    case 'function': {
      return {
        ...type,
        parameterTypes: type.parameterTypes.map(fromType =>
          applyConstraints(fromType, constraints)
        ),
        returnType: applyConstraints(type.returnType, constraints)
      }
    }
  }
}

/**
 * Check if a type contains a reference to a name, to check for an infinite type
 * e.g. A = B -> A
 * @param type
 * @param name
 */
function contains(type: Type, name: string): boolean {
  switch (type.kind) {
    case 'primitive':
      return false
    case 'pair':
      return contains(type.headType, name) || contains(type.tailType, name)
    case 'array':
    case 'list':
      return contains(type.elementType, name)
    case 'variable':
      return type.name === name
    case 'function':
      const containedInParamTypes = type.parameterTypes.some(currentType =>
        contains(currentType, name)
      )
      return containedInParamTypes || contains(type.returnType, name)
  }
}

function occursOnLeftInConstraintList(
  LHS: Variable,
  constraints: Constraint[],
  RHS: Type
): Constraint[] {
  for (const constraint of constraints) {
    if (constraint[0].name === LHS.name) {
      // when LHS occurs earlier in original constrain list
      return addToConstraintList(constraints, [RHS, constraint[1]])
    }
  }
  if (RHS.kind === 'variable') {
    if (LHS.constraint === 'addable' && RHS.constraint === 'none') {
      // We need to modify the type of the RHS so that it is at least as specific as the LHS
      // this is so we are going from least to most specific as we recursively try to determine
      // type of a type variable
      RHS.constraint = LHS.constraint
    }
  }
  if (LHS !== RHS) constraints.push([LHS, RHS])
  return constraints
}

function cannotBeResolvedIfAddable(LHS: Variable, RHS: Type): boolean {
  return (
    LHS.constraint === 'addable' &&
    RHS.kind !== 'variable' &&
    !(RHS.kind === 'primitive' && (RHS.name === 'string' || RHS.name === 'number'))
  )
}

function addToConstraintList(constraints: Constraint[], [LHS, RHS]: [Type, Type]): Constraint[] {
  if (LHS.kind === 'primitive' && RHS.kind === 'primitive' && LHS.name === RHS.name) {
    // if t is base type and t' also base type of the same kind, do nothing
    return constraints
  } else if (LHS.kind !== 'variable' && RHS.kind === 'variable') {
    // if t is not a type var and t' is type var, then swap order
    return addToConstraintList(constraints, [RHS, LHS])
  } else if (LHS.kind === 'variable') {
    RHS = applyConstraints(RHS, constraints)
    if ((RHS.kind === 'primitive' || RHS.kind === 'variable') && LHS.name === RHS.name) {
      // if t is type var and S(t') is a type var with same name, do nothing
      return constraints
    } else if (RHS.kind === 'pair' && LHS === RHS.tailType) {
      // if t is type var and S(t') = Pair(t'',t), add t = List(t'')
      addToConstraintList(constraints, [LHS, tList(RHS.headType)])
    } else if (RHS.kind === 'pair' && RHS.tailType.kind === 'list') {
      // if t = type var and t' = Pair(T1, List<T2>), add T1 = T2 and t = List(T1)
      const newConstraints = addToConstraintList(constraints, [
        RHS.headType,
        getListType(RHS.tailType)!
      ])
      return addToConstraintList(newConstraints, [LHS, tList(RHS.headType)])
    } else if (contains(RHS, LHS.name)) {
      // if t is tpye var and S(t') is function, list or pair type and t contained in S(t'), throw
      // recursive definition error
      throw new InternalCyclicReferenceError(LHS.name)
    }
    if (cannotBeResolvedIfAddable(LHS, RHS)) {
      throw new UnifyError(LHS, RHS)
    }
    return occursOnLeftInConstraintList(LHS, constraints, applyConstraints(RHS, constraints))
  } else if (LHS.kind === 'function' && RHS.kind === 'function') {
    if (LHS.parameterTypes.length !== RHS.parameterTypes.length) {
      throw new InternalDifferentNumberArgumentsError(
        RHS.parameterTypes.length,
        LHS.parameterTypes.length
      )
    }
    let newConstraints = constraints
    for (let i = 0; i < LHS.parameterTypes.length; i++) {
      newConstraints = addToConstraintList(newConstraints, [
        LHS.parameterTypes[i],
        RHS.parameterTypes[i]
      ])
    }
    newConstraints = addToConstraintList(newConstraints, [LHS.returnType, RHS.returnType])
    return newConstraints
  } else if (LHS.kind === 'pair' && RHS.kind === 'pair') {
    // if t = Pair<T1, T2> and t' = Pair<T3, T4>, add T1 = T3 and T2 = T4
    const newConstraints = addToConstraintList(constraints, [LHS.headType, RHS.headType])
    return addToConstraintList(newConstraints, [LHS.tailType, RHS.tailType])
  } else if (LHS.kind === 'list' && RHS.kind === 'list') {
    // if t = List<T1> and t' = List<T2>, add T1 = T2
    return addToConstraintList(constraints, [LHS.elementType, RHS.elementType])
  } else if (LHS.kind === 'list' && RHS.kind === 'pair') {
    // if t = List<T1> and t' = Pair<T2, T3>, add t' = Pair<T1, List<T1>>
    return addToConstraintList(constraints, [RHS, tPair(LHS.elementType, LHS)])
  } else if (RHS.kind === 'list' && LHS.kind === 'pair') {
    // if t = Pair<T1, T2> and t' = List<T3>, add t = Pair<T3, List<T3>>
    return addToConstraintList(constraints, [LHS, tPair(RHS.elementType, RHS)])
  } else if (LHS.kind === 'array' && RHS.kind === 'array') {
    // if t = Array<T1> and t' = Array<T2>, add T1 = T2
    return addToConstraintList(constraints, [LHS.elementType, RHS.elementType])
  }
  throw new UnifyError(LHS, RHS)
}

function statementHasReturn(node: es.Node): boolean {
  switch (node.type) {
    case 'IfStatement': {
      return statementHasReturn(node.consequent) || statementHasReturn(node.alternate!)
    }
    case 'BlockStatement': {
      return node.body.some(stmt => statementHasReturn(stmt))
    }
    case 'ForStatement':
    case 'WhileStatement': {
      return statementHasReturn(node.body)
    }
    case 'ReturnStatement': {
      return true
    }
    default: {
      return false
    }
  }
}

// These are the only two possible kinds of value returning statements when excluding return statements
function stmtHasValueReturningStmt(node: es.Node): boolean {
  switch (node.type) {
    case 'ExpressionStatement': {
      return true
    }
    case 'IfStatement': {
      return (
        stmtHasValueReturningStmt(node.consequent) || stmtHasValueReturningStmt(node.alternate!)
      )
    }
    case 'BlockStatement': {
      return node.body.some(stmt => stmtHasValueReturningStmt(stmt))
    }
    case 'ForStatement':
    case 'WhileStatement': {
      return stmtHasValueReturningStmt(node.body)
    }
    default: {
      return false
    }
  }
}

/**
 * The following is the index of the node whose value will be the value of the block itself.
 * At the top level and if we are currently in the last value returning stmt of the parent block stmt,
 * we will use the last value returning statement of the current block. Anywhere else, we will use
 * either the first return statement or the last statement in the block otherwise
 */
function returnBlockValueNodeIndexFor(
  node: es.Program | es.BlockStatement,
  isTopLevelAndLastValStmt: boolean
): number {
  const lastStatementIndex = node.body.length - 1
  if (isTopLevelAndLastValStmt) {
    for (let index = lastStatementIndex; index >= 0; index--) {
      if (stmtHasValueReturningStmt(node.body[index])) {
        return index
      }
    }
    // in the case there are no value returning statements in the body
    // return the last statement
    return lastStatementIndex
  } else {
    return node.body.findIndex((currentNode, index) => {
      return index === lastStatementIndex || statementHasReturn(currentNode)
    })
  }
}

function lookupType(name: string, env: Env): Type | ForAll | undefined {
  for (let i = env.length - 1; i >= 0; i--) {
    if (env[i].typeMap.has(name)) {
      return env[i].typeMap.get(name)
    }
  }
  return undefined
}

function pushEnv(env: Env) {
  env.push({ typeMap: new Map(), declKindMap: new Map() })
}

/* tslint:disable cyclomatic-complexity */
function infer(
  node: TypeAnnotatedNode<es.Node>,
  env: Env,
  constraints: Constraint[],
  isTopLevelAndLastValStmt: boolean = false
): Constraint[] {
  try {
    return _infer(node, env, constraints, isTopLevelAndLastValStmt)
  } catch (e) {
    if (e instanceof InternalCyclicReferenceError) {
      typeErrors.push(new CyclicReferenceError(node))
      return constraints
    }
    throw e
  }
}

/* tslint:disable cyclomatic-complexity */
function _infer(
  node: TypeAnnotatedNode<es.Node>,
  env: Env,
  constraints: Constraint[],
  isTopLevelAndLastValStmt: boolean = false
): Constraint[] {
  const storedType = node.inferredType as Variable
  switch (node.type) {
    case 'UnaryExpression': {
      const op = node.operator === '-' ? NEGATIVE_OP : node.operator
      const funcType = lookupType(op, env) as FunctionType // in either case its a monomorphic type
      const argNode = node.argument as TypeAnnotatedNode<es.Node>
      const argType = argNode.inferredType as Variable
      const receivedTypes: Type[] = []
      let newConstraints = infer(argNode, env, constraints)
      receivedTypes.push(applyConstraints(argNode.inferredType!, newConstraints))
      try {
        newConstraints = addToConstraintList(newConstraints, [tFunc(argType, storedType), funcType])
      } catch (e) {
        if (e instanceof UnifyError) {
          const expectedTypes = funcType.parameterTypes
          typeErrors.push(
            new InvalidArgumentTypesError(node, [argNode], expectedTypes, receivedTypes)
          )
          return newConstraints
        }
      }
      return newConstraints
    }
    case 'LogicalExpression': // both cases are the same
    case 'BinaryExpression': {
      const envType = lookupType(node.operator, env)!
      const opType = envType.kind === 'forall' ? extractFreeVariablesAndGenFresh(envType) : envType
      const leftNode = node.left as TypeAnnotatedNode<es.Node>
      const leftType = leftNode.inferredType as Variable
      const rightNode = node.right as TypeAnnotatedNode<es.Node>
      const rightType = rightNode.inferredType as Variable

      const argNodes = [leftNode, rightNode]
      let newConstraints = constraints
      const receivedTypes: Type[] = []
      argNodes.forEach(argNode => {
        newConstraints = infer(argNode, env, newConstraints)
        receivedTypes.push(applyConstraints(argNode.inferredType!, newConstraints))
      })
      try {
        newConstraints = addToConstraintList(constraints, [
          tFunc(leftType, rightType, storedType),
          opType
        ])
      } catch (e) {
        if (e instanceof UnifyError) {
          const expectedTypes = (opType as FunctionType).parameterTypes
          typeErrors.push(
            new InvalidArgumentTypesError(node, argNodes, expectedTypes, receivedTypes)
          )
        }
      }
      return newConstraints
    }
    case 'ExpressionStatement': {
      return infer(node.expression, env, addToConstraintList(constraints, [storedType, tUndef]))
    }
    case 'ReturnStatement':
      throw Error('Return statements not supported for x-slang')
    case 'WhileStatement':
      throw Error('Return statements not supported for x-slang')
    case 'ForStatement':
      throw Error('Return statements not supported for x-slang')
    case 'BlockStatement':
      throw Error('Block statements not supported for x-slang')
    case 'Program': {
      pushEnv(env)
      const lastStatementIndex = node.body.length - 1
      const returnValNodeIndex = returnBlockValueNodeIndexFor(node, isTopLevelAndLastValStmt)
      const lastDeclNodeIndex = -1
      const lastNode = node.body[returnValNodeIndex] as TypeAnnotatedNode<es.Node>
      const lastNodeType = (isTopLevelAndLastValStmt && lastNode.type === 'ExpressionStatement'
        ? (lastNode.expression as TypeAnnotatedNode<es.Node>).inferredType
        : lastNode.inferredType) as Variable
      let newConstraints = addToConstraintList(constraints, [storedType, lastNodeType])
      for (let i = 0; i <= lastDeclNodeIndex; i++) {
        if (i === returnValNodeIndex) {
          newConstraints = infer(node.body[i], env, newConstraints, isTopLevelAndLastValStmt)
        } else {
          newConstraints = infer(node.body[i], env, newConstraints)
        }
      }
      for (let i = lastDeclNodeIndex + 1; i <= lastStatementIndex; i++) {
        // for the last statement, if it is an if statement, pass down isLastStatementinBlock variable
        const checkedNode = node.body[i]
        if (i === returnValNodeIndex) {
          newConstraints = infer(checkedNode, env, newConstraints, isTopLevelAndLastValStmt)
        } else {
          newConstraints = infer(checkedNode, env, newConstraints)
        }
      }
      return newConstraints
    }
    case 'Literal': {
      const literalVal = node.value
      const typeOfLiteral = typeof literalVal
      if (literalVal === null) {
        return addToConstraintList(constraints, [storedType, tList(tVar(typeIdCounter++))])
      } else if (typeOfLiteral === 'number') {
        return addToConstraintList(constraints, [storedType, tNumber])
      } else if (typeOfLiteral === 'boolean') {
        return addToConstraintList(constraints, [storedType, tBool])
      } else if (typeOfLiteral === 'string') {
        return addToConstraintList(constraints, [storedType, tString])
      }
      throw Error('Unexpected literal type')
    }
    case 'Identifier':
      throw Error('Identifiers not supported for x-slang')
    case 'ConditionalExpression': // both cases are the same
      throw Error('Conditional expressions not supported for x-slang')
    case 'IfStatement':
      throw Error('If statements not supported for x-slang')
    case 'ArrowFunctionExpression':
      throw Error('Arrow functions not supported for x-slang')
    case 'VariableDeclaration':
      throw Error('Variable declarations not supported for x-slang')
    case 'FunctionDeclaration':
      throw Error('Function declarations not supported for x-slang')
    case 'CallExpression':
      throw Error('Call expressions not supported for x-slang')
    case 'AssignmentExpression':
      throw Error('Assignment expressions not supported for x-slang')
    case 'ArrayExpression':
      throw Error('Array expressions not supported for x-slang')
    case 'MemberExpression':
      throw Error('Member expressions not supported for x-slang')
    default:
      return addToConstraintList(constraints, [storedType, tUndef])
  }
}

// =======================================
// Private Helper Parsing Functions
// =======================================

function tPrimitive(name: Primitive['name']): Primitive {
  return {
    kind: 'primitive',
    name
  }
}

export function tVar(name: string | number): Variable {
  return {
    kind: 'variable',
    name: `T${name}`,
    constraint: 'none'
  }
}

function tAddable(name: string): Variable {
  return {
    kind: 'variable',
    name: `${name}`,
    constraint: 'addable'
  }
}

function tPair(var1: Type, var2: Type): Pair {
  return {
    kind: 'pair',
    headType: var1,
    tailType: var2
  }
}

function tList(var1: Type): List {
  return {
    kind: 'list',
    elementType: var1
  }
}

export function tForAll(type: Type): ForAll {
  return {
    kind: 'forall',
    polyType: type
  }
}

const tBool = tPrimitive('boolean')
const tNumber = tPrimitive('number')
const tString = tPrimitive('string')
const tUndef = tPrimitive('undefined')

function tFunc(...types: Type[]): FunctionType {
  const parameterTypes = types.slice(0, -1)
  const returnType = types.slice(-1)[0]
  return {
    kind: 'function',
    parameterTypes,
    returnType
  }
}

const predeclaredNames: [string, Type | ForAll][] = []

const primitiveFuncs: [string, Type | ForAll][] = [
  [NEGATIVE_OP, tFunc(tNumber, tNumber)],
  ['!', tFunc(tBool, tBool)],
  ['&&', tForAll(tFunc(tBool, tVar('T'), tVar('T')))],
  ['||', tForAll(tFunc(tBool, tVar('T'), tVar('T')))],
  ['<', tForAll(tFunc(tAddable('A'), tAddable('A'), tBool))],
  ['<=', tForAll(tFunc(tAddable('A'), tAddable('A'), tBool))],
  ['>', tForAll(tFunc(tAddable('A'), tAddable('A'), tBool))],
  ['>=', tForAll(tFunc(tAddable('A'), tAddable('A'), tBool))],
  ['+', tForAll(tFunc(tAddable('A'), tAddable('A'), tAddable('A')))],
  ['%', tFunc(tNumber, tNumber, tNumber)],
  ['-', tFunc(tNumber, tNumber, tNumber)],
  ['*', tFunc(tNumber, tNumber, tNumber)],
  ['/', tFunc(tNumber, tNumber, tNumber)]
]

export function createTypeEnvironment(): Env {
  const initialTypeMappings = [...predeclaredNames, ...primitiveFuncs]

  return [
    {
      typeMap: new Map(initialTypeMappings),
      declKindMap: new Map(initialTypeMappings.map(val => [val[0], 'const']))
    }
  ]
}
