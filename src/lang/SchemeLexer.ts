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
  public static readonly T__2 = 3
  public static readonly T__3 = 4
  public static readonly T__4 = 5
  public static readonly T__5 = 6
  public static readonly STRING = 7
  public static readonly NUMBER = 8
  public static readonly BOOL = 9
  public static readonly IDENTIFIER = 10
  public static readonly WHITESPACE = 11
  public static readonly COMMENT = 12

  // tslint:disable:no-trailing-whitespace
  public static readonly channelNames: string[] = ['DEFAULT_TOKEN_CHANNEL', 'HIDDEN']

  // tslint:disable:no-trailing-whitespace
  public static readonly modeNames: string[] = ['DEFAULT_MODE']

  public static readonly ruleNames: string[] = [
    'T__0',
    'T__1',
    'T__2',
    'T__3',
    'T__4',
    'T__5',
    'STRING',
    'NUMBER',
    'BOOL',
    'IDENTIFIER',
    'WHITESPACE',
    'COMMENT',
    'LETTER',
    'DIGIT',
    'IDENTIFIER_INITIAL',
    'IDENTIFIER_SUBSEQUENT',
    'PECULIAR_IDENTIFIER'
  ]

  private static readonly _LITERAL_NAMES: Array<string | undefined> = [
    undefined,
    "'('",
    "')'",
    "'''",
    "'`'",
    "','",
    "',@'"
  ]
  private static readonly _SYMBOLIC_NAMES: Array<string | undefined> = [
    undefined,
    undefined,
    undefined,
    undefined,
    undefined,
    undefined,
    undefined,
    'STRING',
    'NUMBER',
    'BOOL',
    'IDENTIFIER',
    'WHITESPACE',
    'COMMENT'
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
    '\x03\uC91D\uCABA\u058D\uAFBA\u4F53\u0607\uEA8B\uC241\x02\x0E\x80\b\x01' +
    '\x04\x02\t\x02\x04\x03\t\x03\x04\x04\t\x04\x04\x05\t\x05\x04\x06\t\x06' +
    '\x04\x07\t\x07\x04\b\t\b\x04\t\t\t\x04\n\t\n\x04\v\t\v\x04\f\t\f\x04\r' +
    '\t\r\x04\x0E\t\x0E\x04\x0F\t\x0F\x04\x10\t\x10\x04\x11\t\x11\x04\x12\t' +
    '\x12\x03\x02\x03\x02\x03\x03\x03\x03\x03\x04\x03\x04\x03\x05\x03\x05\x03' +
    '\x06\x03\x06\x03\x07\x03\x07\x03\x07\x03\b\x03\b\x03\b\x03\b\x07\b7\n' +
    '\b\f\b\x0E\b:\v\b\x03\b\x03\b\x03\t\x05\t?\n\t\x03\t\x06\tB\n\t\r\t\x0E' +
    '\tC\x03\t\x03\t\x06\tH\n\t\r\t\x0E\tI\x05\tL\n\t\x03\n\x03\n\x03\n\x03' +
    '\n\x05\nR\n\n\x03\v\x03\v\x07\vV\n\v\f\v\x0E\vY\v\v\x03\v\x05\v\\\n\v' +
    '\x03\f\x06\f_\n\f\r\f\x0E\f`\x03\f\x03\f\x03\r\x03\r\x07\rg\n\r\f\r\x0E' +
    '\rj\v\r\x03\r\x03\r\x03\x0E\x03\x0E\x03\x0F\x03\x0F\x03\x10\x03\x10\x05' +
    '\x10t\n\x10\x03\x11\x03\x11\x03\x11\x05\x11y\n\x11\x03\x12\x03\x12\x03' +
    '\x12\x03\x12\x05\x12\x7F\n\x12\x02\x02\x02\x13\x03\x02\x03\x05\x02\x04' +
    '\x07\x02\x05\t\x02\x06\v\x02\x07\r\x02\b\x0F\x02\t\x11\x02\n\x13\x02\v' +
    '\x15\x02\f\x17\x02\r\x19\x02\x0E\x1B\x02\x02\x1D\x02\x02\x1F\x02\x02!' +
    '\x02\x02#\x02\x02\x03\x02\b\x04\x02$$^^\x04\x02--//\x05\x02\v\f\x0F\x0F' +
    '""\x04\x02\f\f\x0F\x0F\b\x02&(,,11<<>A`a\x06\x02##--/0BB\x02\x89\x02' +
    '\x03\x03\x02\x02\x02\x02\x05\x03\x02\x02\x02\x02\x07\x03\x02\x02\x02\x02' +
    '\t\x03\x02\x02\x02\x02\v\x03\x02\x02\x02\x02\r\x03\x02\x02\x02\x02\x0F' +
    '\x03\x02\x02\x02\x02\x11\x03\x02\x02\x02\x02\x13\x03\x02\x02\x02\x02\x15' +
    '\x03\x02\x02\x02\x02\x17\x03\x02\x02\x02\x02\x19\x03\x02\x02\x02\x03%' +
    "\x03\x02\x02\x02\x05'\x03\x02\x02\x02\x07)\x03\x02\x02\x02\t+\x03\x02" +
    '\x02\x02\v-\x03\x02\x02\x02\r/\x03\x02\x02\x02\x0F2\x03\x02\x02\x02\x11' +
    '>\x03\x02\x02\x02\x13Q\x03\x02\x02\x02\x15[\x03\x02\x02\x02\x17^\x03\x02' +
    '\x02\x02\x19d\x03\x02\x02\x02\x1Bm\x03\x02\x02\x02\x1Do\x03\x02\x02\x02' +
    '\x1Fs\x03\x02\x02\x02!x\x03\x02\x02\x02#~\x03\x02\x02\x02%&\x07*\x02\x02' +
    "&\x04\x03\x02\x02\x02'(\x07+\x02\x02(\x06\x03\x02\x02\x02)*\x07)\x02" +
    '\x02*\b\x03\x02\x02\x02+,\x07b\x02\x02,\n\x03\x02\x02\x02-.\x07.\x02\x02' +
    '.\f\x03\x02\x02\x02/0\x07.\x02\x0201\x07B\x02\x021\x0E\x03\x02\x02\x02' +
    '28\x07$\x02\x0234\x07^\x02\x0247\v\x02\x02\x0257\n\x02\x02\x0263\x03\x02' +
    '\x02\x0265\x03\x02\x02\x027:\x03\x02\x02\x0286\x03\x02\x02\x0289\x03\x02' +
    '\x02\x029;\x03\x02\x02\x02:8\x03\x02\x02\x02;<\x07$\x02\x02<\x10\x03\x02' +
    '\x02\x02=?\t\x03\x02\x02>=\x03\x02\x02\x02>?\x03\x02\x02\x02?A\x03\x02' +
    '\x02\x02@B\x05\x1D\x0F\x02A@\x03\x02\x02\x02BC\x03\x02\x02\x02CA\x03\x02' +
    '\x02\x02CD\x03\x02\x02\x02DK\x03\x02\x02\x02EG\x070\x02\x02FH\x05\x1D' +
    '\x0F\x02GF\x03\x02\x02\x02HI\x03\x02\x02\x02IG\x03\x02\x02\x02IJ\x03\x02' +
    '\x02\x02JL\x03\x02\x02\x02KE\x03\x02\x02\x02KL\x03\x02\x02\x02L\x12\x03' +
    '\x02\x02\x02MN\x07%\x02\x02NR\x07v\x02\x02OP\x07%\x02\x02PR\x07h\x02\x02' +
    'QM\x03\x02\x02\x02QO\x03\x02\x02\x02R\x14\x03\x02\x02\x02SW\x05\x1F\x10' +
    '\x02TV\x05!\x11\x02UT\x03\x02\x02\x02VY\x03\x02\x02\x02WU\x03\x02\x02' +
    '\x02WX\x03\x02\x02\x02X\\\x03\x02\x02\x02YW\x03\x02\x02\x02Z\\\x05#\x12' +
    '\x02[S\x03\x02\x02\x02[Z\x03\x02\x02\x02\\\x16\x03\x02\x02\x02]_\t\x04' +
    '\x02\x02^]\x03\x02\x02\x02_`\x03\x02\x02\x02`^\x03\x02\x02\x02`a\x03\x02' +
    '\x02\x02ab\x03\x02\x02\x02bc\b\f\x02\x02c\x18\x03\x02\x02\x02dh\x07=\x02' +
    '\x02eg\n\x05\x02\x02fe\x03\x02\x02\x02gj\x03\x02\x02\x02hf\x03\x02\x02' +
    '\x02hi\x03\x02\x02\x02ik\x03\x02\x02\x02jh\x03\x02\x02\x02kl\b\r\x02\x02' +
    'l\x1A\x03\x02\x02\x02mn\x04c|\x02n\x1C\x03\x02\x02\x02op\x042;\x02p\x1E' +
    '\x03\x02\x02\x02qt\x05\x1B\x0E\x02rt\t\x06\x02\x02sq\x03\x02\x02\x02s' +
    'r\x03\x02\x02\x02t \x03\x02\x02\x02uy\x05\x1F\x10\x02vy\x05\x1D\x0F\x02' +
    'wy\t\x07\x02\x02xu\x03\x02\x02\x02xv\x03\x02\x02\x02xw\x03\x02\x02\x02' +
    'y"\x03\x02\x02\x02z\x7F\t\x03\x02\x02{|\x070\x02\x02|}\x070\x02\x02}' +
    '\x7F\x070\x02\x02~z\x03\x02\x02\x02~{\x03\x02\x02\x02\x7F$\x03\x02\x02' +
    '\x02\x11\x0268>CIKQW[`hsx~\x03\b\x02\x02'
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
