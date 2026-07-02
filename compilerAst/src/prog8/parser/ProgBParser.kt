package prog8.parser

import org.antlr.v4.runtime.*
import prog8.ast.Module
import prog8.ast.antlr.Antlr2KotlinVisitorQB
import prog8.code.core.Position
import prog8.code.source.SourceCode


/**
 * Parser for ProgB (.pb) files - BASIC-style syntax for Prog8.
 * This parser produces the same AST as Prog8Parser, allowing both
 * syntaxes to be used interchangeably.
 */
object ProgBParser {

    fun parseModule(src: SourceCode): Module {
        val errorListener = CollectingErrorListener(src)
        val lexer = Prog8QBLexer(CharStreams.fromString(src.text, src.origin))
        lexer.removeErrorListeners()
        lexer.addErrorListener(errorListener)
        val tokens = CommonTokenStream(lexer)
        val parser = Prog8QBParser(tokens)
        parser.errorHandler = DefaultErrorStrategy()
        parser.removeErrorListeners()
        parser.addErrorListener(errorListener)

        val parseTree = parser.module()

        if(errorListener.hasErrors()) {
            throw MultipleParseErrors(errorListener.getErrors())
        }

        val visitor = Antlr2KotlinVisitorQB(src)
        val visitorResult = visitor.visit(parseTree)
        return visitorResult as Module
    }


    private class CollectingErrorListener(private val src: SourceCode): BaseErrorListener() {
        private val errors = mutableListOf<ParseError>()

        private fun RecognitionException.getPosition(): Position {
            val offending = this.offendingToken ?: return Position(src.origin, 1, 1, 1)

            if (offending.line <= 0 || offending.charPositionInLine < 0) {
                return Position(src.origin, 1, 1, 1)
            }

            val line = offending.line
            val startCol = offending.charPositionInLine + 1

            val endCol = if (offending.type == Token.EOF ||
                             offending.startIndex < 0 || offending.stopIndex < 0) {
                startCol
            } else if (offending.line == line) {
                offending.charPositionInLine + (offending.stopIndex - offending.startIndex) + 1
            } else {
                maxOf(startCol, offending.charPositionInLine + 1)
            }

            return Position(src.origin, line, startCol, endCol)
        }

        override fun syntaxError(recognizer: Recognizer<*, *>?, offendingSymbol: Any?, line: Int, charPositionInLine: Int, msg: String, e: RecognitionException?) {
            if (e == null) {
                val error = ParseError(msg, Position(src.origin, line, charPositionInLine+1, charPositionInLine+1), RuntimeException("parse error"))
                errors.add(error)
            } else {
                if(e.offendingToken==null) {
                    val error = ParseError(msg, Position(src.origin, line, charPositionInLine+1, charPositionInLine+1), e)
                    errors.add(error)
                } else {
                    val error = ParseError(msg, e.getPosition(), e)
                    errors.add(error)
                }
            }
        }

        fun getErrors(): List<ParseError> = errors.toList()
        fun hasErrors(): Boolean = errors.isNotEmpty()
    }

}
