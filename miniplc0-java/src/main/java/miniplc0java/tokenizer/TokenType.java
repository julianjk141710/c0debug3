package miniplc0java.tokenizer;

public enum TokenType {
    /** 空 */
    None,

    /**
     *  3 个类型
     */
    INT,

    /** UINT_LITERAL无符号整数 */
    UINT_LITERAL,
    /** IDENT标识符 */
    IDENT,
    /** STRING_LITERAL字符串常量 */
    STRING_LITERAL,

    /**
     *  8 个关键字
     */
    /** 函数fn */
    FN_KW,
    /** let */
    LET_KW,
    /** const */
    CONST_KW,
    /** AS_KW */
    AS_KW,
    /** WHILE_KW */
    WHILE_KW,
    /** IF_KW */
    IF_KW,
    /** ELSE_KW */
    ELSE_KW,
    /** RETURN_KW */
    RETURN_KW,

    /**
     *  19 个运算符
     */
    /** 加号 */
    PLUS,
    /** 减号 */
    MINUS,
    /** 乘号 */
    MUL,
    /** 除号 */
    DIV,
    /** 等号 */
    EQ,
    /** 赋值 */
    ASSIGN,
    /** 不等号 */
    NEQ,
    /** 小于号 */
    LT,
    /** 大于号 */
    GT,
    /** 小于等于号 */
    LE,
    /** 大于等于号 */
    GE,
    /** 左括号 */
    L_PAREN,
    /** 右括号 */
    R_PAREN,
    /** 左大括号 */
    L_BRACE,
    /** 右大括号 */
    R_BRACE,
    /** 箭头指针 */
    ARROW,
    /** 逗号 */
    COMMA,
    /** 冒号 */
    COLON,
    /** 分号 */
    SEMICOLON,
    /** 文件尾 */
    EOF;

    @Override
    public String toString() {
        switch (this) {
            case None:
                return "NullToken";
            case FN_KW:
                return "FnSign";
            case LET_KW:
                return "LetSign";
            case CONST_KW:
                return "ConstSign";
            case AS_KW:
                return "AsSign";
            case WHILE_KW:
                return "WhileSign";
            case IF_KW:
                return "IfSign";
            case ELSE_KW:
                return "ElseSign";
            case RETURN_KW:
                return "ReturnSign";
            case PLUS:
                return "PlusSign";
            case MINUS:
                return "MinusSign";
            case MUL:
                return "MulSign";
            case DIV:
                return "DivSign";
            case ASSIGN:
                return "AssignSign";
            case EQ:
                return "EqualSign";
            case NEQ:
                return "NotEqualSign";
            case LT:
                return "LessThanSign";
            case GT:
                return "GreaterThanSign";
            case LE:
                return "LessEqualSign";
            case GE:
                return "GreaterEqualSign";
            case L_PAREN:
                return "L_ParenSign";
            case R_PAREN:
                return "R_ParenSign";
            case L_BRACE:
                return "L_BraceSign";
            case R_BRACE:
                return "R_BraceSign";
            case ARROW:
                return "ArrowSign";
            case COMMA:
                return "CommaSign";
            case COLON:
                return "ColonSign";
            case SEMICOLON:
                return "SemicolonSign";
            case UINT_LITERAL:
                return "Uint_Literal";
            case STRING_LITERAL:
                return "String_Literal";
            case IDENT:
                return "Ident";
            case INT:
                return "Int";
            default:
                return "InvalidToken";
        }
    }
}
