import { ancestor, base, FullWalkerCallback } from '../utils/walkers'
import * as es from 'estree'
import { Context, TypeAnnotatedNode } from '../types'
import { getVariableDecarationName } from '../utils/astCreator'

class Declaration {
  public accessedBeforeDeclaration: boolean = false
  constructor(public isConstant: boolean) {}
}

export function validateAndAnnotate(
  program: es.Program,
  context: Context
): TypeAnnotatedNode<es.Program> {
  const accessedBeforeDeclarationMap = new Map<es.Node, Map<string, Declaration>>()
  const scopeHasCallExpressionMap = new Map<es.Node, boolean>()
  function processBlock(node: es.Program | es.BlockStatement) {
    const initialisedIdentifiers = new Map<string, Declaration>()
    scopeHasCallExpressionMap.set(node, false)
    accessedBeforeDeclarationMap.set(node, initialisedIdentifiers)
  }
  function processFunction(node: es.FunctionDeclaration | es.ArrowFunctionExpression) {
    accessedBeforeDeclarationMap.set(
      node,
      new Map((node.params as es.Identifier[]).map(id => [id.name, new Declaration(false)]))
    )
    scopeHasCallExpressionMap.set(node, false)
  }

  // initialise scope of variables
  ancestor(program as es.Node, {
    Program: processBlock,
    BlockStatement: processBlock,
    FunctionDeclaration: processFunction,
    ArrowFunctionExpression: processFunction,
    ForStatement(forStatement: es.ForStatement, ancestors: es.Node[]) {}
  })

  function validateIdentifier(id: es.Identifier, ancestors: es.Node[]) {
    const name = id.name
    for (let i = ancestors.length - 1; i >= 0; i--) {
      const a = ancestors[i]
      const map = accessedBeforeDeclarationMap.get(a)
      if (map?.has(name)) {
        map.get(name)!.accessedBeforeDeclaration = true
        break
      }
    }
  }
  const customWalker = {
    ...base,
    VariableDeclarator(node: es.VariableDeclarator, st: never, c: FullWalkerCallback<never>) {
      // don't visit the id
      if (node.init) {
        c(node.init, st, 'Expression')
      }
    }
  }
  ancestor(
    program,
    {
      VariableDeclaration(node: TypeAnnotatedNode<es.VariableDeclaration>, ancestors: es.Node[]) {
        const lastAncestor = ancestors[ancestors.length - 2]
        const name = getVariableDecarationName(node)
        const accessedBeforeDeclaration = accessedBeforeDeclarationMap.get(lastAncestor)!.get(name)!
          .accessedBeforeDeclaration
        node.typability = accessedBeforeDeclaration ? 'Untypable' : 'NotYetTyped'
      },
      Identifier: validateIdentifier,
      FunctionDeclaration(node: TypeAnnotatedNode<es.FunctionDeclaration>, ancestors: es.Node[]) {
        // a function declaration can be typed if there are no function calls in the same scope before it
        const lastAncestor = ancestors[ancestors.length - 2]
        node.typability = scopeHasCallExpressionMap.get(lastAncestor) ? 'Untypable' : 'NotYetTyped'
      },
      Pattern(node: es.Pattern, ancestors: es.Node[]) {},
      CallExpression(call: es.CallExpression, ancestors: es.Node[]) {
        for (let i = ancestors.length - 1; i >= 0; i--) {
          const a = ancestors[i]
          if (scopeHasCallExpressionMap.has(a)) {
            scopeHasCallExpressionMap.set(a, true)
            break
          }
        }
      }
    },
    customWalker
  )

  return program
}
