package miniplc0java.tokenizer;

import miniplc0java.error.TokenizeError;
import miniplc0java.error.ErrorCode;
import miniplc0java.util.Pos;

import java.util.HashMap;

public class Tokenizer {

    private StringIter it;
    private HashMap<String, TokenType> keyWords;

    public Tokenizer(StringIter it) {
        this.it = it;
        keyWords = new HashMap<String, TokenType>();
        keyWords.put("fn", TokenType.FN_KW);
        keyWords.put("let", TokenType.LET_KW);
        keyWords.put("const", TokenType.CONST_KW);
        keyWords.put("as", TokenType.AS_KW);
        keyWords.put("while", TokenType.WHILE_KW);
        keyWords.put("if", TokenType.IF_KW);
        keyWords.put("else", TokenType.ELSE_KW);
        keyWords.put("return", TokenType.RETURN_KW);
        keyWords.put("int", TokenType.INT);
    }

    // 这里本来是想实现 Iterator<Token> 的，但是 Iterator 不允许抛异常，于是就这样了
    /**
     * 获取下一个 Token
     *
     * @return
     * @throws TokenizeError 如果解析有异常则抛出
     */
    public Token nextToken() throws TokenizeError {
        it.readAll();
        int flag = 0;
        // 跳过之前的所有空白字符
        //skipSpaceCharacters();
        while (!it.isEOF() && (it.getCurrentChar() == '\n' || it.getCurrentChar() == ' ' || it.getCurrentChar() == '\t')) {
            it.nextChar();
            flag = 1;
        }

        if (it.isEOF()) {
            return new Token(TokenType.EOF, "", it.currentPos(), it.currentPos());
        }

        char peek = it.peekChar();

        if (Character.isDigit(peek)) {
            return lexUInt();
        } else if (Character.isAlphabetic(peek) || isUnderlineMark(peek)) {
            return lexIdentOrKeyword();
        } else if (isQuotationMark(peek)) {
            return lexStringLiteral();
        } else {
            return lexOperatorOrUnknown();
        }
    }

    /**
     * Julian
     * 对字符串常量进行词法分析 STRING_LITERAL
     * 文法如下:
     * escape_sequence -> '\' [\\"'nrt]  表示6个转义字符 \\ \" \' \n \t \r
     * string_regular_char -> [^"\\]
     * STRING_LITERAL -> '"' (string_regular_char | escape_sequence)* '"'
     *
     * 也就是说 一个字符串可以是一个
     * 1.空串 ""
     * 2.普通的字符串 "This is just a common sentence."
     * 3.带有转义字符的字符串 "This is a sentence with some escape_sequence \r\t\n\\\"\'"
     *
     * 为了方便之后检查正则表达式:
     * "(\\[\\"'nrt]|[^"\\])*" https://tool.oschina.net/regex/
     * @return
     */
    private Token lexStringLiteral() throws TokenizeError {
        StringBuilder stringBuilder = new StringBuilder();
        Pos intStartPos = it.currentPos();
        Pos intEndPos = it.nextPos();
        char peekChar = it.peekChar();
        char previousChar = peekChar;
        stringBuilder.append(peekChar);
        it.nextChar();
        peekChar = it.peekChar();
        while (true) {
            if (peekChar == '"') {
                if (previousChar != '\\') {
                    stringBuilder.append(peekChar);
                    it.nextChar();
                    break;
                }
            } else if (peekChar == '\\') {
                char peepNextChar = it.seeNextChar();
                if (peepNextChar != '\\' && peepNextChar != '\'' && peepNextChar != '"' && peepNextChar != 'n'
                        && peepNextChar != 'r' && peepNextChar != 't') {
                    throw new TokenizeError(ErrorCode.InvalidInput, it.previousPos());
                }
            }
            previousChar = peekChar;
            stringBuilder.append(peekChar);
            it.nextChar();
            peekChar = it.peekChar();
        }
        var tmpStringLiteral = stringBuilder.toString();
        Token retToken = new Token(TokenType.STRING_LITERAL, tmpStringLiteral, intStartPos, intEndPos);
        return retToken;
    }

    /**
     * 复用了 miniplc0 的代码
     * @return
     * @throws TokenizeError
     */
    private Token lexUInt() throws TokenizeError {
        // 请填空：
        // 直到查看下一个字符不是数字为止:
        // -- 前进一个字符，并存储这个字符
        //
        // 解析存储的字符串为无符号整数
        // 解析成功则返回无符号整数类型的token，否则返回编译错误
        //
        // Token 的 Value 应填写数字的值
        StringBuilder stringBuilder = new StringBuilder();
        Pos intStartPos = it.currentPos();
        Pos intEndPos = it.nextPos();

        while (Character.isDigit(it.peekChar())) {
            intEndPos = it.nextPos();
            char currentChar = it.getCurrentChar();
            stringBuilder.append(currentChar);
            it.nextChar();
        }
        int retValue = Integer.parseInt(stringBuilder.toString());
        Token retToken = new Token(TokenType.UINT_LITERAL, retValue, intStartPos, intEndPos);
        return retToken;
    }

    /**
     * 基本复用了miniplc0的代码
     * 111 行多加入了一个针对词法的判断条件而已
     * @return
     * @throws TokenizeError
     */
    private Token lexIdentOrKeyword() throws TokenizeError {
        // 请填空：
        // 直到查看下一个字符不是数字或字母为止:
        // -- 前进一个字符，并存储这个字符
        //
        // 尝试将存储的字符串解释为关键字
        // -- 如果是关键字，则返回关键字类型的 token
        // -- 否则，返回标识符
        //
        // Token 的 Value 应填写标识符或关键字的字符串
        StringBuilder stringBuilder = new StringBuilder();
        Pos identOrKeywordStartPos = it.currentPos();
        Pos identOrKeywordEndPos = it.nextPos();

        if (Character.isAlphabetic(it.peekChar()) || isUnderlineMark(it.peekChar())) {
            stringBuilder.append(it.getCurrentChar());
            identOrKeywordEndPos = it.nextPos();
            it.nextChar();
            while (Character.isAlphabetic(it.peekChar()) || Character.isDigit(it.peekChar()) || isUnderlineMark(it.peekChar())) {
                char currentChar = it.getCurrentChar();
                stringBuilder.append(currentChar);
                identOrKeywordEndPos = it.nextPos();
                it.nextChar();
            }
        }
        String tmpToken = stringBuilder.toString();
        if (keyWords.containsKey(tmpToken)) {
            Token retToken = new Token(keyWords.get(tmpToken), tmpToken, identOrKeywordStartPos, identOrKeywordEndPos);
            return retToken;
        }

        Token retToken = new Token(TokenType.IDENT, tmpToken, identOrKeywordStartPos, identOrKeywordEndPos);
        return retToken;
    }

    private Token lexOperatorOrUnknown() throws TokenizeError {
        char caseCharacter = it.nextChar();
        char peepNextChar;
        Pos startPos;
        switch (caseCharacter) {
            case '+':
                return new Token(TokenType.PLUS, '+', it.previousPos(), it.currentPos());

            case '-':
                // 填入返回语句
                /**
                 * 这里peepNextChar其实应该是seeNextChar但是上面指针动了一下就不能seeNextChar了
                 * 下面的peepNextChar同理
                 */
                peepNextChar = it.getCurrentChar();
                startPos = it.previousPos();
                if (peepNextChar == '>') {
                    it.nextChar();
                    return new Token(TokenType.ARROW, "->", startPos, it.currentPos());
                }
                return new Token(TokenType.MINUS, '-', it.previousPos(), it.currentPos());

            case '*':
                // 填入返回语句
                return new Token(TokenType.MUL, '*', it.previousPos(), it.currentPos());

            case '/':
                // 填入返回语句
                return new Token(TokenType.DIV, '/', it.previousPos(), it.currentPos());



            case '!':
                // 填入返回语句
                startPos = it.previousPos();
                peepNextChar = it.getCurrentChar();
                if (peepNextChar == '=') {
                    it.nextChar();
                    return new Token(TokenType.NEQ, "!=", startPos, it.currentPos());
                } else {
                    throw new TokenizeError(ErrorCode.InvalidInput, it.previousPos());
                }

            case '<':
                // 填入返回语句
                startPos = it.previousPos();
                peepNextChar = it.getCurrentChar();
                if (peepNextChar == '=') {
                    it.nextChar();
                    return new Token(TokenType.LE, "<=", startPos, it.currentPos());
                }
                return new Token(TokenType.LT, '<', startPos, it.currentPos());

            case '>':
                // 填入返回语句
                startPos = it.previousPos();
                peepNextChar = it.getCurrentChar();
                if (peepNextChar == '=') {
                    it.nextChar();
                    return new Token(TokenType.GE, ">=", startPos, it.currentPos());
                }
                return new Token(TokenType.GT, '>', startPos, it.currentPos());


            case ';':
                // 填入返回语句
                return new Token(TokenType.SEMICOLON, ';', it.previousPos(), it.currentPos());
            // 填入更多状态和返回语句
            case '(':
                // 填入返回语句
                return new Token(TokenType.L_PAREN, '(', it.previousPos(), it.currentPos());

            case ')':
                // 填入返回语句
                return new Token(TokenType.R_PAREN, ')', it.previousPos(), it.currentPos());

            case '{':
                // 填入返回语句
                return new Token(TokenType.L_BRACE, '{', it.previousPos(), it.currentPos());

            case '}':
                // 填入返回语句
                return new Token(TokenType.R_BRACE, '}', it.previousPos(), it.currentPos());

            case '=':
                // 填入返回语句
                startPos = it.previousPos();
                peepNextChar = it.getCurrentChar();
                if (peepNextChar == '=') {
                    it.nextChar();
                    return new Token(TokenType.EQ, "==", startPos, it.currentPos());
                }
                return new Token(TokenType.ASSIGN, '=', startPos, it.currentPos());

            case ',':
                // 填入返回语句
                return new Token(TokenType.COMMA, ',', it.previousPos(), it.currentPos());

            case ':':
                // 填入返回语句
                return new Token(TokenType.COLON, ':', it.previousPos(), it.currentPos());

            default:
                // 不认识这个输入，摸了
                throw new TokenizeError(ErrorCode.InvalidInput, it.previousPos());
        }
    }

    private void skipSpaceCharacters() {
        while (!it.isEOF() && Character.isWhitespace(it.peekChar())) {
            it.nextChar();
        }
    }

    /**
     * Julian
     * 判断字符是不是双引号
     * @param character
     * @return
     */
    public boolean isQuotationMark(char character) {
        if (character == '"') {
            return true;
        }
        return false;
    }

    /**
     * Julian
     * 判断字符是不是下划线
     * @param character
     * @return
     */
    public boolean isUnderlineMark(char character) {
        if (character == '_') {
            return true;
        }
        return false;
    }
}
