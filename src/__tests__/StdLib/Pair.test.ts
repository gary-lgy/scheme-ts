import { ExpressibleValue } from '../../interpreter/ExpressibleValue'
import { prepareContext, runUntilDone } from '../../testHelpers'
import { Variant } from '../../types'

const list = "'((((1 2) 3 4) (5 6) 7 8) ((9 10) 11 12) (13) 14 15)"

describe.each<Variant>(['base', 'no-tco', 'macro'])('miscellaneous library features', variant => {
  function evaluateUntilDone(code: string): ExpressibleValue {
    const context = prepareContext(variant)
    return runUntilDone(code, context).value
  }

  test('caar', () => {
    expect(evaluateUntilDone(`(caar  ${list})`)).toMatchInlineSnapshot(`
    Object {
      "head": Object {
        "head": Object {
          "type": "EVNumber",
          "value": 1,
        },
        "tail": Object {
          "head": Object {
            "type": "EVNumber",
            "value": 2,
          },
          "tail": Object {
            "type": "EVEmptyList",
          },
          "type": "EVPair",
        },
        "type": "EVPair",
      },
      "tail": Object {
        "head": Object {
          "type": "EVNumber",
          "value": 3,
        },
        "tail": Object {
          "head": Object {
            "type": "EVNumber",
            "value": 4,
          },
          "tail": Object {
            "type": "EVEmptyList",
          },
          "type": "EVPair",
        },
        "type": "EVPair",
      },
      "type": "EVPair",
    }
  `)
  })

  test('cadr', () => {
    expect(evaluateUntilDone(`(cadr  ${list})`)).toMatchInlineSnapshot(`
    Object {
      "head": Object {
        "head": Object {
          "type": "EVNumber",
          "value": 9,
        },
        "tail": Object {
          "head": Object {
            "type": "EVNumber",
            "value": 10,
          },
          "tail": Object {
            "type": "EVEmptyList",
          },
          "type": "EVPair",
        },
        "type": "EVPair",
      },
      "tail": Object {
        "head": Object {
          "type": "EVNumber",
          "value": 11,
        },
        "tail": Object {
          "head": Object {
            "type": "EVNumber",
            "value": 12,
          },
          "tail": Object {
            "type": "EVEmptyList",
          },
          "type": "EVPair",
        },
        "type": "EVPair",
      },
      "type": "EVPair",
    }
  `)
  })

  test('cdar', () => {
    expect(evaluateUntilDone(`(cdar  ${list})`)).toMatchInlineSnapshot(`
    Object {
      "head": Object {
        "head": Object {
          "type": "EVNumber",
          "value": 5,
        },
        "tail": Object {
          "head": Object {
            "type": "EVNumber",
            "value": 6,
          },
          "tail": Object {
            "type": "EVEmptyList",
          },
          "type": "EVPair",
        },
        "type": "EVPair",
      },
      "tail": Object {
        "head": Object {
          "type": "EVNumber",
          "value": 7,
        },
        "tail": Object {
          "head": Object {
            "type": "EVNumber",
            "value": 8,
          },
          "tail": Object {
            "type": "EVEmptyList",
          },
          "type": "EVPair",
        },
        "type": "EVPair",
      },
      "type": "EVPair",
    }
  `)
  })

  test('cddr', () => {
    expect(evaluateUntilDone(`(cddr  ${list})`)).toMatchInlineSnapshot(`
    Object {
      "head": Object {
        "head": Object {
          "type": "EVNumber",
          "value": 13,
        },
        "tail": Object {
          "type": "EVEmptyList",
        },
        "type": "EVPair",
      },
      "tail": Object {
        "head": Object {
          "type": "EVNumber",
          "value": 14,
        },
        "tail": Object {
          "head": Object {
            "type": "EVNumber",
            "value": 15,
          },
          "tail": Object {
            "type": "EVEmptyList",
          },
          "type": "EVPair",
        },
        "type": "EVPair",
      },
      "type": "EVPair",
    }
  `)
  })

  test('caaar', () => {
    expect(evaluateUntilDone(`(caaar  ${list})`)).toMatchInlineSnapshot(`
    Object {
      "head": Object {
        "type": "EVNumber",
        "value": 1,
      },
      "tail": Object {
        "head": Object {
          "type": "EVNumber",
          "value": 2,
        },
        "tail": Object {
          "type": "EVEmptyList",
        },
        "type": "EVPair",
      },
      "type": "EVPair",
    }
  `)
  })

  test('caadr', () => {
    expect(evaluateUntilDone(`(caadr  ${list})`)).toMatchInlineSnapshot(`
    Object {
      "head": Object {
        "type": "EVNumber",
        "value": 9,
      },
      "tail": Object {
        "head": Object {
          "type": "EVNumber",
          "value": 10,
        },
        "tail": Object {
          "type": "EVEmptyList",
        },
        "type": "EVPair",
      },
      "type": "EVPair",
    }
  `)
  })

  test('cadar', () => {
    expect(evaluateUntilDone(`(cadar  ${list})`)).toMatchInlineSnapshot(`
    Object {
      "head": Object {
        "type": "EVNumber",
        "value": 5,
      },
      "tail": Object {
        "head": Object {
          "type": "EVNumber",
          "value": 6,
        },
        "tail": Object {
          "type": "EVEmptyList",
        },
        "type": "EVPair",
      },
      "type": "EVPair",
    }
  `)
  })

  test('caddr', () => {
    expect(evaluateUntilDone(`(caddr  ${list})`)).toMatchInlineSnapshot(`
    Object {
      "head": Object {
        "type": "EVNumber",
        "value": 13,
      },
      "tail": Object {
        "type": "EVEmptyList",
      },
      "type": "EVPair",
    }
  `)
  })

  test('cdaar', () => {
    expect(evaluateUntilDone(`(cdaar  ${list})`)).toMatchInlineSnapshot(`
    Object {
      "head": Object {
        "type": "EVNumber",
        "value": 3,
      },
      "tail": Object {
        "head": Object {
          "type": "EVNumber",
          "value": 4,
        },
        "tail": Object {
          "type": "EVEmptyList",
        },
        "type": "EVPair",
      },
      "type": "EVPair",
    }
  `)
  })

  test('cdadr', () => {
    expect(evaluateUntilDone(`(cdadr  ${list})`)).toMatchInlineSnapshot(`
    Object {
      "head": Object {
        "type": "EVNumber",
        "value": 11,
      },
      "tail": Object {
        "head": Object {
          "type": "EVNumber",
          "value": 12,
        },
        "tail": Object {
          "type": "EVEmptyList",
        },
        "type": "EVPair",
      },
      "type": "EVPair",
    }
  `)
  })

  test('cddar', () => {
    expect(evaluateUntilDone(`(cddar  ${list})`)).toMatchInlineSnapshot(`
    Object {
      "head": Object {
        "type": "EVNumber",
        "value": 7,
      },
      "tail": Object {
        "head": Object {
          "type": "EVNumber",
          "value": 8,
        },
        "tail": Object {
          "type": "EVEmptyList",
        },
        "type": "EVPair",
      },
      "type": "EVPair",
    }
  `)
  })

  test('cdddr', () => {
    expect(evaluateUntilDone(`(cdddr  ${list})`)).toMatchInlineSnapshot(`
    Object {
      "head": Object {
        "type": "EVNumber",
        "value": 14,
      },
      "tail": Object {
        "head": Object {
          "type": "EVNumber",
          "value": 15,
        },
        "tail": Object {
          "type": "EVEmptyList",
        },
        "type": "EVPair",
      },
      "type": "EVPair",
    }
  `)
  })

  test('caaaar', () => {
    expect(evaluateUntilDone(`(caaaar  ${list})`)).toMatchInlineSnapshot(`
    Object {
      "type": "EVNumber",
      "value": 1,
    }
  `)
  })

  test('caaadr', () => {
    expect(evaluateUntilDone(`(caaadr  ${list})`)).toMatchInlineSnapshot(`
    Object {
      "type": "EVNumber",
      "value": 9,
    }
  `)
  })

  test('caadar', () => {
    expect(evaluateUntilDone(`(caadar  ${list})`)).toMatchInlineSnapshot(`
    Object {
      "type": "EVNumber",
      "value": 5,
    }
  `)
  })

  test('caaddr', () => {
    expect(evaluateUntilDone(`(caaddr  ${list})`)).toMatchInlineSnapshot(`
    Object {
      "type": "EVNumber",
      "value": 13,
    }
  `)
  })

  test('cadaar', () => {
    expect(evaluateUntilDone(`(cadaar  ${list})`)).toMatchInlineSnapshot(`
    Object {
      "type": "EVNumber",
      "value": 3,
    }
  `)
  })

  test('cadadr', () => {
    expect(evaluateUntilDone(`(cadadr  ${list})`)).toMatchInlineSnapshot(`
    Object {
      "type": "EVNumber",
      "value": 11,
    }
  `)
  })

  test('caddar', () => {
    expect(evaluateUntilDone(`(caddar  ${list})`)).toMatchInlineSnapshot(`
    Object {
      "type": "EVNumber",
      "value": 7,
    }
  `)
  })

  test('cadddr', () => {
    expect(evaluateUntilDone(`(cadddr  ${list})`)).toMatchInlineSnapshot(`
    Object {
      "type": "EVNumber",
      "value": 14,
    }
  `)
  })

  test('cdaaar', () => {
    expect(evaluateUntilDone(`(cdaaar  ${list})`)).toMatchInlineSnapshot(`
    Object {
      "head": Object {
        "type": "EVNumber",
        "value": 2,
      },
      "tail": Object {
        "type": "EVEmptyList",
      },
      "type": "EVPair",
    }
  `)
  })

  test('cdaadr', () => {
    expect(evaluateUntilDone(`(cdaadr  ${list})`)).toMatchInlineSnapshot(`
    Object {
      "head": Object {
        "type": "EVNumber",
        "value": 10,
      },
      "tail": Object {
        "type": "EVEmptyList",
      },
      "type": "EVPair",
    }
  `)
  })

  test('cdadar', () => {
    expect(evaluateUntilDone(`(cdadar  ${list})`)).toMatchInlineSnapshot(`
    Object {
      "head": Object {
        "type": "EVNumber",
        "value": 6,
      },
      "tail": Object {
        "type": "EVEmptyList",
      },
      "type": "EVPair",
    }
  `)
  })

  test('cdaddr', () => {
    expect(evaluateUntilDone(`(cdaddr  ${list})`)).toMatchInlineSnapshot(`
    Object {
      "type": "EVEmptyList",
    }
  `)
  })

  test('cddaar', () => {
    expect(evaluateUntilDone(`(cddaar  ${list})`)).toMatchInlineSnapshot(`
    Object {
      "head": Object {
        "type": "EVNumber",
        "value": 4,
      },
      "tail": Object {
        "type": "EVEmptyList",
      },
      "type": "EVPair",
    }
  `)
  })

  test('cddadr', () => {
    expect(evaluateUntilDone(`(cddadr  ${list})`)).toMatchInlineSnapshot(`
    Object {
      "head": Object {
        "type": "EVNumber",
        "value": 12,
      },
      "tail": Object {
        "type": "EVEmptyList",
      },
      "type": "EVPair",
    }
  `)
  })

  test('cdddar', () => {
    expect(evaluateUntilDone(`(cdddar  ${list})`)).toMatchInlineSnapshot(`
    Object {
      "head": Object {
        "type": "EVNumber",
        "value": 8,
      },
      "tail": Object {
        "type": "EVEmptyList",
      },
      "type": "EVPair",
    }
  `)
  })

  test('cddddr', () => {
    expect(evaluateUntilDone(`(cddddr  ${list})`)).toMatchInlineSnapshot(`
    Object {
      "head": Object {
        "type": "EVNumber",
        "value": 15,
      },
      "tail": Object {
        "type": "EVEmptyList",
      },
      "type": "EVPair",
    }
  `)
  })
})
