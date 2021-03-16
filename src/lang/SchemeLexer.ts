// Generated from ./src/lang/Scheme.g4 by ANTLR 4.9.0-SNAPSHOT
// @ts-ignore

import { ATN } from 'antlr4ts/atn/ATN'
import { ATNDeserializer } from 'antlr4ts/atn/ATNDeserializer'
import { LexerATNSimulator } from 'antlr4ts/atn/LexerATNSimulator'
import { CharStream } from 'antlr4ts/CharStream'
import { Lexer } from 'antlr4ts/Lexer'
import * as Utils from 'antlr4ts/misc/Utils'
import { Vocabulary } from 'antlr4ts/Vocabulary'
import { VocabularyImpl } from 'antlr4ts/VocabularyImpl'

export class SchemeLexer extends Lexer {
  public static readonly T__0 = 1
  public static readonly T__1 = 2
  public static readonly STRING = 3
  public static readonly NUMBER = 4
  public static readonly BOOL = 5
  public static readonly WHITESPACE = 6
  public static readonly IDENTIFIER = 7

  // tslint:disable:no-trailing-whitespace
  public static readonly channelNames: string[] = ['DEFAULT_TOKEN_CHANNEL', 'HIDDEN']

  // tslint:disable:no-trailing-whitespace
  public static readonly modeNames: string[] = ['DEFAULT_MODE']

  public static readonly ruleNames: string[] = [
    'T__0',
    'T__1',
    'STRING',
    'NUMBER',
    'BOOL',
    'WHITESPACE',
    'IDENTIFIER',
    'LETTER',
    'DIGIT',
    'IDENTIFIER_INITIAL',
    'IDENTIFIER_SUBSEQUENT'
  ]

  private static readonly _LITERAL_NAMES: Array<string | undefined> = [undefined, "'('", "')'"]
  private static readonly _SYMBOLIC_NAMES: Array<string | undefined> = [
    undefined,
    undefined,
    undefined,
    'STRING',
    'NUMBER',
    'BOOL',
    'WHITESPACE',
    'IDENTIFIER'
  ]
  public static readonly VOCABULARY: Vocabulary = new VocabularyImpl(
    SchemeLexer._LITERAL_NAMES,
    SchemeLexer._SYMBOLIC_NAMES,
    []
  )

  // @Override
  // @NotNull
  public get vocabulary(): Vocabulary {
    return SchemeLexer.VOCABULARY
  }
  // tslint:enable:no-trailing-whitespace

  constructor(input: CharStream) {
    super(input)
    this._interp = new LexerATNSimulator(SchemeLexer._ATN, this)
  }

  // @Override
  public get grammarFileName(): string {
    return 'Scheme.g4'
  }

  // @Override
  public get ruleNames(): string[] {
    return SchemeLexer.ruleNames
  }

  // @Override
  public get serializedATN(): string {
    return SchemeLexer._serializedATN
  }

  // @Override
  public get channelNames(): string[] {
    return SchemeLexer.channelNames
  }

  // @Override
  public get modeNames(): string[] {
    return SchemeLexer.modeNames
  }

  public static readonly _serializedATN: string =
    '\x03\uC91D\uCABA\u058D\uAFBA\u4F53\u0607\uEA8B\uC241\x02\tY\b\x01\x04' +
    '\x02\t\x02\x04\x03\t\x03\x04\x04\t\x04\x04\x05\t\x05\x04\x06\t\x06\x04' +
    '\x07\t\x07\x04\b\t\b\x04\t\t\t\x04\n\t\n\x04\v\t\v\x04\f\t\f\x03\x02\x03' +
    '\x02\x03\x03\x03\x03\x03\x04\x03\x04\x03\x04\x03\x04\x07\x04"\n\x04\f' +
    '\x04\x0E\x04%\v\x04\x03\x04\x03\x04\x03\x05\x05\x05*\n\x05\x03\x05\x06' +
    '\x05-\n\x05\r\x05\x0E\x05.\x03\x05\x03\x05\x06\x053\n\x05\r\x05\x0E\x05' +
    '4\x05\x057\n\x05\x03\x06\x03\x06\x03\x06\x03\x06\x05\x06=\n\x06\x03\x07' +
    '\x06\x07@\n\x07\r\x07\x0E\x07A\x03\x07\x03\x07\x03\b\x03\b\x07\bH\n\b' +
    '\f\b\x0E\bK\v\b\x03\t\x05\tN\n\t\x03\n\x03\n\x03\v\x03\v\x05\vT\n\v\x03' +
    '\f\x03\f\x05\fX\n\f\x02\x02\x02\r\x03\x02\x03\x05\x02\x04\x07\x02\x05' +
    '\t\x02\x06\v\x02\x07\r\x02\b\x0F\x02\t\x11\x02\x02\x13\x02\x02\x15\x02' +
    '\x02\x17\x02\x02\x03\x02\x07\x04\x02$$^^\x04\x02--//\x05\x02\v\f\x0F\x0F' +
    '""\x04\x02C\\c|\x05\x02,-//11\x02_\x02\x03\x03\x02\x02\x02\x02\x05\x03' +
    '\x02\x02\x02\x02\x07\x03\x02\x02\x02\x02\t\x03\x02\x02\x02\x02\v\x03\x02' +
    '\x02\x02\x02\r\x03\x02\x02\x02\x02\x0F\x03\x02\x02\x02\x03\x19\x03\x02' +
    '\x02\x02\x05\x1B\x03\x02\x02\x02\x07\x1D\x03\x02\x02\x02\t)\x03\x02\x02' +
    '\x02\v<\x03\x02\x02\x02\r?\x03\x02\x02\x02\x0FE\x03\x02\x02\x02\x11M\x03' +
    '\x02\x02\x02\x13O\x03\x02\x02\x02\x15S\x03\x02\x02\x02\x17W\x03\x02\x02' +
    '\x02\x19\x1A\x07*\x02\x02\x1A\x04\x03\x02\x02\x02\x1B\x1C\x07+\x02\x02' +
    '\x1C\x06\x03\x02\x02\x02\x1D#\x07$\x02\x02\x1E\x1F\x07^\x02\x02\x1F"' +
    '\v\x02\x02\x02 "\n\x02\x02\x02!\x1E\x03\x02\x02\x02! \x03\x02\x02\x02' +
    '"%\x03\x02\x02\x02#!\x03\x02\x02\x02#$\x03\x02\x02\x02$&\x03\x02\x02' +
    "\x02%#\x03\x02\x02\x02&'\x07$\x02\x02'\b\x03\x02\x02\x02(*\t\x03\x02" +
    '\x02)(\x03\x02\x02\x02)*\x03\x02\x02\x02*,\x03\x02\x02\x02+-\x05\x13\n' +
    '\x02,+\x03\x02\x02\x02-.\x03\x02\x02\x02.,\x03\x02\x02\x02./\x03\x02\x02' +
    '\x02/6\x03\x02\x02\x0202\x070\x02\x0213\x05\x13\n\x0221\x03\x02\x02\x02' +
    '34\x03\x02\x02\x0242\x03\x02\x02\x0245\x03\x02\x02\x0257\x03\x02\x02\x02' +
    '60\x03\x02\x02\x0267\x03\x02\x02\x027\n\x03\x02\x02\x0289\x07%\x02\x02' +
    '9=\x07v\x02\x02:;\x07%\x02\x02;=\x07h\x02\x02<8\x03\x02\x02\x02<:\x03' +
    '\x02\x02\x02=\f\x03\x02\x02\x02>@\t\x04\x02\x02?>\x03\x02\x02\x02@A\x03' +
    '\x02\x02\x02A?\x03\x02\x02\x02AB\x03\x02\x02\x02BC\x03\x02\x02\x02CD\b' +
    '\x07\x02\x02D\x0E\x03\x02\x02\x02EI\x05\x15\v\x02FH\x05\x17\f\x02GF\x03' +
    '\x02\x02\x02HK\x03\x02\x02\x02IG\x03\x02\x02\x02IJ\x03\x02\x02\x02J\x10' +
    '\x03\x02\x02\x02KI\x03\x02\x02\x02LN\t\x05\x02\x02ML\x03\x02\x02\x02N' +
    '\x12\x03\x02\x02\x02OP\x042;\x02P\x14\x03\x02\x02\x02QT\x05\x11\t\x02' +
    'RT\t\x06\x02\x02SQ\x03\x02\x02\x02SR\x03\x02\x02\x02T\x16\x03\x02\x02' +
    '\x02UX\x05\x15\v\x02VX\x05\x13\n\x02WU\x03\x02\x02\x02WV\x03\x02\x02\x02' +
    'X\x18\x03\x02\x02\x02\x0F\x02!#).46<AIMSW\x03\b\x02\x02'
  public static __ATN: ATN
  public static get _ATN(): ATN {
    if (!SchemeLexer.__ATN) {
      SchemeLexer.__ATN = new ATNDeserializer().deserialize(
        Utils.toCharArray(SchemeLexer._serializedATN)
      )
    }

    return SchemeLexer.__ATN
  }
}
